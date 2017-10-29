#include "Parser.h"
#include "Lexer.h"
#include "PoolAllocator.h"
#include <assert.h>
#include <string.h>
#include <stdio.h>
#include <stdlib.h>

void traverseAST(FileAST *root);
u32 process_scope_variables(Scope *scope);
bool infer_types(VariableDeclarationAST *decl);
TypeAST *deduceType(ExpressionAST *expr);

void ErrorAST(BaseAST *ast, const char *msg)
{
    printf("Error %s:%d - %s", ast->filename, ast->line_num, msg);
    exit(1);
}

static VariableDeclarationAST *findVariable(TextType name, Scope *scope)
{
    // trivial recursive case, could not be found
    if (scope == nullptr) return nullptr; 

    for (auto d : scope->decls) {
        if (!strcmp(name, d->varname)) return d;
    }
    return findVariable(name, scope->parent);
}

void copyASTloc(BaseAST *src, BaseAST *dst)
{
    dst->filename = src->filename;
    dst->line_num = src->line_num;
}

TypeAST * deduceType(ExpressionAST *expr) 
{
    switch (expr->ast_type) {
    case AST_FUNCTION_CALL: {
        FunctionCallAST *a = (FunctionCallAST *)expr;
        VariableDeclarationAST *decl = findVariable(a->function_name, a->scope);

        if (decl == nullptr) {
            ErrorAST(expr, "Function name could not be found on this scope\n");
        }
        // do not recurse on inferring types as this could cause infinite recursion
        if (decl->specified_type == nullptr) {
            return nullptr;
        }

        if (decl->specified_type->ast_type != AST_FUNCTION_TYPE) {
            ErrorAST(expr, "Cannot perform a function call on a variable that is not a function\n");
        }
        FunctionTypeAST *fundecl = (FunctionTypeAST *)decl->specified_type;
        if (!fundecl->return_type) {
            ErrorAST(expr, "Cannot use the return value of a void function\n");
        }
        return fundecl->return_type;
    }
    case AST_IDENTIFIER: {
        IdentifierAST *a = (IdentifierAST *)expr;
        VariableDeclarationAST *decl = findVariable(a->name, a->scope);

        if (decl == nullptr) {
            ErrorAST(expr, "Variable name could not be found on this scope\n");
        }

        if ((decl->filename == expr->filename) &&
            (decl->line_num > expr->line_num) && 
            !(decl->flags & DECL_FLAG_IS_CONSTANT) )
        {
            ErrorAST(expr, "The variable used in this declaration appears after the current declaration, this is only allowed for constants\n");
        }

        // do not recurse on inferring types as this could cause infinite recursion
        return decl->specified_type; 
    }
    case AST_CONSTANT_NUMBER: {
        ConstantNumberAST *cons = (ConstantNumberAST *)expr;
        DirectTypeAST *direct_type = new DirectTypeAST();
        copyASTloc(expr, direct_type);
        direct_type->type = cons->type;
        return direct_type;
    }
    case AST_CONSTANT_STRING: {
        DirectTypeAST *direct_type = new DirectTypeAST();
        copyASTloc(expr, direct_type);
        direct_type->type = BASIC_TYPE_STRING;
        return direct_type;
    }
    case AST_BINARY_OPERATION: {
        return deduceType(((BinaryOperationAST *)expr)->lhs);
    }
    default:
        assert("We should never be here, we could not parse this type\n");
    }
    return nullptr;
}

u32 process_scope_variables(Scope * scope)
{
    u32 untyped_vars = 0;
    for (auto &decl : scope->decls) {
        if (decl->specified_type == nullptr) {
            if (!infer_types(decl)) {
                untyped_vars++;
            }
        }
    }
    return untyped_vars;
}

void process_all_scope_variables(Scope *scope)
{
    u32 untyped_vars, prev_vars = 0;

    do {
        untyped_vars = process_scope_variables(scope);
        if (prev_vars == 0) {
            prev_vars = untyped_vars;
        } else if ((prev_vars == untyped_vars) && prev_vars > 0) {
            printf("Could not resolve types for variables\n");
            exit(1);
        }
    } while (untyped_vars > 0);
}

bool infer_types(VariableDeclarationAST *decl)
{
    if (decl->specified_type) return true;
    assert(decl->definition); // if we do not have a type we must have something to compare against
    switch (decl->definition->ast_type) {
    case AST_FUNCTION_DEFINITION: {
        FunctionDefinitionAST *fundef = (FunctionDefinitionAST *)decl->definition;
        decl->specified_type = fundef->declaration;
        decl->flags |= DECL_FLAG_HAS_BEEN_INFERRED;
        return true;
    }
    default: {
        // expect this to be an expression, and the expression type needs to be deduced
        // operation, literal, function call, etc
        TypeAST *t = deduceType((ExpressionAST *)decl->definition);
        if (t != nullptr) {
            decl->specified_type = t;
            decl->flags |= DECL_FLAG_HAS_BEEN_INFERRED;
            return true;
        }
        break;
    }
    }
    return false;
}

static void setASTloc(Parser *p, BaseAST *ast)
{
    SrcLocation loc;
    p->lex->getLocation(loc);
    ast->line_num = loc.line;
    ast->char_num = loc.col;
    ast->filename = p->lex->getFilename();
}

static void setASTinfo(Parser *p, BaseAST *ast)
{
    setASTloc(p, ast);
    ast->scope = p->current_scope;
}

void Parser::Error(const char *msg)
{
	SrcLocation loc;
	lex->getLocation(loc);
	printf("Error %s:%d - %s", lex->getFilename(), loc.line, msg);
	exit(1);
}

void Parser::MustMatchToken(TOKEN_TYPE type, char *msg)
{
    if (!lex->checkToken(type)) {
        char err[128] = {};
        u32 offset = 0;
        if (msg) {
            offset = snprintf(err, sizeof(err), "%s\n    ", msg);
        }
        snprintf(err + offset, sizeof(err) - offset, "Token %s was expected, but we found: %s\n", 
            TokenTypeToStr(type), TokenTypeToStr(lex->getTokenType()));
        Error(err);
    }
    lex->consumeToken();
}

void Parser::AddDeclarationToScope(VariableDeclarationAST * decl)
{
    for (auto d : current_scope->decls) {
        if (!strcmp(d->varname, decl->varname)) {
            char err[128];
            snprintf(err, sizeof(err), "Error, variable [%s] is already defined in the scope", decl->varname);
            Error(err);
        }
    }
    current_scope->decls.push_back(decl);
}


static bool isAssignmentOperator(TOKEN_TYPE type)
{
    return (type == TK_ASSIGN)
        || (type == TK_MUL_ASSIGN)
        || (type == TK_DIV_ASSIGN)
        || (type == TK_MOD_ASSIGN)
        || (type == TK_ADD_ASSIGN)
        || (type == TK_SUB_ASSIGN)
        || (type == TK_LEFT_ASSIGN)
        || (type == TK_RIGHT_ASSIGN)
        || (type == TK_AND_ASSIGN)
        || (type == TK_XOR_ASSIGN)
        || (type == TK_OR_ASSIGN);
}

static bool isBinOperator(TOKEN_TYPE type)
{
    return (type == TK_EQ)
        || (type == TK_LEQ)
        || (type == TK_GEQ)
        || (type == TK_NEQ)
        || (type == TK_LT)
        || (type == TK_GT)
        || (type == TK_RSHIFT)
        || (type == TK_LSHIFT)
        || (type == TK_STAR)
        || (type == TK_DIV)
        || (type == TK_MOD)
        || (type == TK_PIPE)
        || (type == TK_DOUBLE_PIPE)
        || (type == TK_HAT)
        || (type == TK_AMP)
        || (type == TK_DOUBLE_AMP)
        || (type == TK_PLUS)
        || (type == TK_MINUS);
}

static u32 getPrecedence(TOKEN_TYPE t)
{
    switch (t) {
    case TK_STAR:
    case TK_DIV:
    case TK_MOD:
        return 1;
    case TK_MINUS:
    case TK_PLUS:
        return 2;
    case TK_LSHIFT:
    case TK_RSHIFT:
        return 3;
    case TK_LT:
    case TK_GT:
    case TK_LEQ:
    case TK_GEQ:
        return 4;
    case TK_EQ:
    case TK_NEQ:
        return 5;
    case TK_AMP:
        return 6;
    case TK_HAT:
        return 7;
    case TK_PIPE:
        return 8;
    case TK_DOUBLE_AMP:
        return 9;
    case TK_DOUBLE_PIPE:
        return 10;
    }
    return 0;
}

static bool isVariableTypeToken(TOKEN_TYPE t)
{
    return (t == TK_BOOL)
        || (t == TK_INT)
        || (t == TK_U8)
        || (t == TK_U16)
        || (t == TK_U32)
        || (t == TK_U64)
        || (t == TK_S8)
        || (t == TK_S16)
        || (t == TK_S32)
        || (t == TK_S64)
        || (t == TK_FLOAT)
        || (t == TK_F32)
        || (t == TK_F64)
        || (t == TK_STRING);
}

TypeAST *Parser::parseDirectType()
{
    // @TODO: support pointer, arrays, etc
    Token t;
    lex->lookaheadToken(t);
    if ((t.type != TK_IDENTIFIER) && !isVariableTypeToken(t.type)) {
        char err[128] = {};
        snprintf(err, sizeof(err), "Variable type token could not be found, but we found: %s\n",
            TokenTypeToStr(t.type));
        Error(err);
    }
    lex->consumeToken();

    DirectTypeAST *type = new DirectTypeAST();
    setASTinfo(this, type);
    type->name = t.string;
    switch (t.type) {
    case TK_STRING:
        type->type = BASIC_TYPE_STRING;
        break;
    case TK_U8:
        type->type = BASIC_TYPE_U8;
        break;
    case TK_U16:
        type->type = BASIC_TYPE_U16;
        break;
    case TK_U32:
        type->type = BASIC_TYPE_U32;
        break;
    case TK_INT:
    case TK_U64:
        type->type = BASIC_TYPE_U64;
        break;
    case TK_S8:
        type->type = BASIC_TYPE_S8;
        break;
    case TK_S16:
        type->type = BASIC_TYPE_S16;
        break;
    case TK_S32:
        type->type = BASIC_TYPE_S32;
        break;
    case TK_S64:
        type->type = BASIC_TYPE_S64;
        break;
    case TK_F32:
        type->type = BASIC_TYPE_F32;
        break;
    case TK_FLOAT:
    case TK_F64:
        type->type = BASIC_TYPE_F64;
        break;
    case TK_IDENTIFIER :
        assert(!"Identifier types, custom types are not implemented");
    }

    // @TODO: support custom types
    return type;
}

TypeAST * Parser::parseType()
{
    if (lex->checkToken(TK_OPEN_PAREN)) {
        return parseFunctionDeclaration();
    }

    return parseDirectType();
}

ArgumentDeclarationAST *Parser::parseArgumentDeclaration()
{
    Token t;
    lex->lookaheadToken(t);
    ArgumentDeclarationAST *arg = new ArgumentDeclarationAST();
    setASTinfo(this, arg);
    MustMatchToken(TK_IDENTIFIER, "Argument declaration needs to start with an identifier");
    arg->name = t.string;

    MustMatchToken(TK_COLON, "Argument declaration needs a colon between identifier and type");

    arg->type = parseType();
    return arg;
}

FunctionTypeAST *Parser::parseFunctionDeclaration()
{
    MustMatchToken(TK_OPEN_PAREN, "Function declarations need a parenthesis");

    FunctionTypeAST *fundec = new FunctionTypeAST();
    setASTinfo(this, fundec);

    while (!lex->checkToken(TK_CLOSE_PAREN)) {
        ArgumentDeclarationAST *arg = Parser::parseArgumentDeclaration();
        fundec->arguments.push_back(arg);
        if (lex->checkToken(TK_COMMA)) {
            lex->consumeToken();
        } else if (!lex->checkToken(TK_CLOSE_PAREN)) {
            Error("Comma must be used in between parameters in a function\n");
        }
    }
    lex->consumeToken();
    // Now do the return value, which can be empty
    if (lex->checkToken(TK_RETURN_ARROW)) {
        lex->consumeToken();
        fundec->return_type = parseType();
    }
    return fundec;
}

ReturnStatementAST *Parser::parseReturnStatement()
{
    ReturnStatementAST *ret = new ReturnStatementAST();
    setASTinfo(this, ret);
    MustMatchToken(TK_RETURN);

    ret->ret = parseExpression();
    MustMatchToken(TK_SEMICOLON, "Statement needs to end in semicolon");
    return ret;
}

StatementAST *Parser::parseStatement()
{
    TOKEN_TYPE cur_type;
    StatementAST *statement = nullptr;

    cur_type = lex->getTokenType();
    if (cur_type == TK_IDENTIFIER) {
        // could be a variable definition or a statement
        if (lex->checkAheadToken(TK_COLON,1)
            || lex->checkAheadToken(TK_DOUBLE_COLON, 1)
            || lex->checkAheadToken(TK_IMPLICIT_ASSIGN, 1)) {
            statement = parseDeclaration();
        } else if (lex->checkAheadToken(TK_OPEN_PAREN, 1)) {
            // this is a function call, parse it as such
            statement = parseFunctionCall();
            MustMatchToken(TK_SEMICOLON, "Statement needs to end in semicolon");
        } else {
            statement = parseExpression();
            MustMatchToken(TK_SEMICOLON, "Statement needs to end in semicolon");
        }
        return statement;
    } else if (cur_type == TK_OPEN_BRACKET) {
        return parseStatementBlock();
    } else if (cur_type == TK_RETURN) {
        return parseReturnStatement();
    } else {
        return parseExpression();
    }
}

StatementBlockAST *Parser::parseStatementBlock()
{
    if (!lex->checkToken(TK_OPEN_BRACKET)) {
        Error("Was expecting an open bracket\n");
    }
    lex->consumeToken(); // consume the {
    StatementBlockAST *block = new StatementBlockAST();
    setASTinfo(this, block);
    // push scope
    block->scope.parent = current_scope;
    current_scope = &block->scope;

    while (!lex->checkToken(TK_CLOSE_BRACKET)) {
        StatementAST *statement = nullptr;
        statement = parseStatement();
        block->statements.push_back(statement);
        if (lex->checkToken(TK_LAST_TOKEN)) {
            Error("Failed to find a matching close bracket");
        }
    }
    lex->consumeToken(); // match }
    // pop scope
    current_scope = current_scope->parent;
    return block;
}


FunctionDefinitionAST *Parser::parseFunctionDefinition()
{
    FunctionDefinitionAST *fundef = new FunctionDefinitionAST();
    setASTinfo(this, fundef);

    fundef->declaration = parseFunctionDeclaration();
    fundef->function_body = parseStatementBlock();
    return fundef;
}

FunctionCallAST * Parser::parseFunctionCall()
{
    FunctionCallAST *funcall = new FunctionCallAST();
    Token t;
    setASTinfo(this, funcall);
    lex->getNextToken(t);

    assert(t.type == TK_IDENTIFIER);
    funcall->function_name = t.string;

    MustMatchToken(TK_OPEN_PAREN);
    while (!lex->checkToken(TK_CLOSE_PAREN)) {
        ExpressionAST * expr = parseExpression();
        funcall->args.push_back(expr);
        if (lex->checkToken(TK_COMMA)) {
            lex->consumeToken();
        } 
    }

    MustMatchToken(TK_CLOSE_PAREN);

    return funcall;
}

ExpressionAST * Parser::parseLiteral()
{
    Token t;
    lex->getNextToken(t);

    if (t.type == TK_IDENTIFIER) {
        IdentifierAST *ex = new IdentifierAST();
        setASTinfo(this, ex);
        ex->name = t.string;
        return ex;
    } else if ((t.type == TK_NUMBER) || (t.type == TK_FNUMBER)) {
        ConstantNumberAST *ex = new ConstantNumberAST();
        setASTinfo(this, ex);

        if (t.type == TK_NUMBER) {
            ex->type = BASIC_TYPE_U64;
            ex->pl.pu64 = t.pl.pu64;
        } else {
            ex->type = BASIC_TYPE_F64;
            ex->pl.pf64 = t.pl.pf64;
        }
        return ex;
    } else if ((t.type == TK_STRING)) {
        ConstantStringAST *str = new ConstantStringAST();
        setASTinfo(this, str);

        str->str = t.string;
        return str;
    }
    //@TODO: have an error message here
    printf("Could not parse a expression!");
    return nullptr;
}

ExpressionAST * Parser::parseUnaryExpression()
{
    Token t;
    lex->lookaheadToken(t);

    if (t.type == TK_IDENTIFIER) {
        if (lex->checkAheadToken(TK_OPEN_PAREN, 1)) {
            return parseFunctionCall();
        }
    } 
    return parseLiteral();
}

ExpressionAST *Parser::parseBinOpExpressionRecursive(u32 oldprec, ExpressionAST *lhs)
{
    TOKEN_TYPE cur_type;

    while (1) {
        cur_type = lex->getTokenType();
        if (isBinOperator(cur_type)) {
            u32 cur_prec = getPrecedence(cur_type);
            if (cur_prec < oldprec) {
                return lhs;
            } else {
                lex->consumeToken();
                ExpressionAST *rhs = parseUnaryExpression();
                if (isBinOperator(lex->getTokenType())) {
                    u32 newprec = getPrecedence(lex->getTokenType());
                    if (cur_prec < newprec) {
                        rhs = parseBinOpExpressionRecursive(cur_prec + 1, rhs);
                    }
                } 
                BinaryOperationAST *bin = new BinaryOperationAST();
                setASTinfo(this, bin);
                bin->lhs = lhs;
                bin->rhs = rhs;
                bin->op = cur_type;
                lhs = bin;
            }
        } else {
            break;
        }
    }
    return lhs;
}

ExpressionAST *Parser::parseBinOpExpression()
{
    ExpressionAST *lhs = parseUnaryExpression();
    return parseBinOpExpressionRecursive( 0, lhs);
}

ExpressionAST * Parser::parseAssignmentExpression()
{
    ExpressionAST *lhs = parseBinOpExpression();
    TOKEN_TYPE type;

    type = lex->getTokenType();
    if (isAssignmentOperator(type)) {
        // @TODO : we would need to have here a RHS evaluation
        lex->consumeToken();
        AssignmentAST *assign = new AssignmentAST();
        setASTinfo(this, assign);
        assign->lhs = lhs;
        assign->op = type;
        assign->rhs = parseAssignmentExpression();
        return assign;
    } 
    return lhs;
}

ExpressionAST * Parser::parseExpression()
{
    Token t;
    lex->lookaheadToken(t);
    if (t.type == TK_OPEN_PAREN) {
        lex->getNextToken(t);
        ExpressionAST *expr = parseExpression();
        lex->getNextToken(t);
        if (t.type != TK_CLOSE_PAREN) {
            Error("Cound not find a matching close parentesis\n");
        }
        return expr;
    }
    return parseAssignmentExpression();
}

DefinitionAST *Parser::parseDefinition()
{
    Token t;
    lex->lookaheadToken(t);
    if (t.type == TK_OPEN_PAREN) {
        // ok, this could be an expression or a function definition!
        // hard to tell, so we will try both and see which one produces a result
        u32 lex_pos = lex->getTokenStreamPosition();
        FunctionDefinitionAST *func_def = parseFunctionDefinition();
        if (func_def != nullptr) {
            return func_def;
        }

        // @TODO: use lookahead instead of token rewind
        // if we are here, means that we could not parse the function definition
        // it has to be an expression, but we need to reset the parsing stream
        lex->setTokenStreamPosition(lex_pos);
        // fall through to the normal expression parsing
    }
    return parseExpression();
}

VariableDeclarationAST * Parser::parseDeclaration()
{
    Token t;
    lex->getNextToken(t);
    VariableDeclarationAST *decl = new VariableDeclarationAST();
    setASTinfo(this, decl);

    if (t.type != TK_IDENTIFIER) {
        Error("Identifier expected but not found\n");
    }

    decl->varname = t.string;
    lex->lookaheadToken(t);
    if (t.type == TK_COLON) {
        lex->consumeToken();
        // we are doing a variable declaration
        decl->specified_type = parseType();
        lex->lookaheadToken(t);
    } 
    
    if (t.type == TK_DOUBLE_COLON) {
        decl->flags |= DECL_FLAG_IS_CONSTANT;
    }

    if ((t.type == TK_ASSIGN) || (t.type == TK_DOUBLE_COLON)
        || (t.type == TK_IMPLICIT_ASSIGN)) {
        lex->consumeToken();
        // we are doing an assignment or initial value
        decl->definition = parseDefinition();
        lex->lookaheadToken(t);
    } else if (t.type != TK_SEMICOLON) {
        Error("Declaration is malformed");
    }
    if (!decl->definition || decl->definition->needsSemiColon()) {
        // @TODO: support compiler flags and others here
        if (t.type != TK_SEMICOLON) {
            Error("Declaration needs to end with a semicolon\n");
        }
        lex->getNextToken(t);
    }
    AddDeclarationToScope(decl);
    return decl;
}

// first version, just return a list of AST
FileAST *Parser::Parse(const char *filename, PoolAllocator *pool)
{
	Lexer lex;
    this->lex = &lex;
    FileAST *file_inst = new FileAST();

    lex.setPoolAllocator(pool);

	lex.openFile(filename);
    lex.parseFile();

    current_scope = &file_inst->scope;
    current_scope->parent = nullptr;

	while (!lex.checkToken(TK_LAST_TOKEN)) {
		// we got a token, figure out what AST to build
        VariableDeclarationAST *d = parseDeclaration();
        file_inst->items.push_back(d);
                
        //lex->getNextToken(t);
        //t.print();
	}

    this->lex = nullptr;
	return file_inst;
}

void traverseAST(StatementBlockAST *root)
{
    process_all_scope_variables(&root->scope);
    for (auto stmt : root->statements) {
        if (stmt->ast_type == AST_VARIABLE_DECLARATION) {
            auto decl = (VariableDeclarationAST *)stmt;
            if ((decl->definition) && (decl->definition->ast_type == AST_FUNCTION_DEFINITION)) {
                FunctionDefinitionAST *fundef = (FunctionDefinitionAST *)decl->definition;
                traverseAST(fundef->function_body);
            }
        } else if (stmt->ast_type == AST_STATEMENT_BLOCK) {
            traverseAST((StatementBlockAST *)stmt);
        }
    }
}

void traverseAST(FileAST *root)
{
    process_all_scope_variables(&root->scope);
    for (auto &decl : root->items) {
        if ((decl->definition) && (decl->definition->ast_type == AST_FUNCTION_DEFINITION)) {
            FunctionDefinitionAST *fundef = (FunctionDefinitionAST *)decl->definition;
            traverseAST(fundef->function_body);
        }
    }
}
