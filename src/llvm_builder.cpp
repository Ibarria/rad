#include "llvm/ADT/APFloat.h"
#include "llvm/ADT/STLExtras.h"
#include "llvm/IR/BasicBlock.h"
#include "llvm/IR/Constants.h"
#include "llvm/IR/DerivedTypes.h"
#include "llvm/IR/Function.h"
#include "llvm/IR/IRBuilder.h"
#include "llvm/IR/LLVMContext.h"
#include "llvm/IR/LegacyPassManager.h"
#include "llvm/IR/Module.h"
#include "llvm/IR/Type.h"
#include "llvm/IR/Verifier.h"
#include "llvm/IR/GlobalVariable.h"
#include "llvm/Support/FileSystem.h"
#include "llvm/Support/Host.h"
#include "llvm/Support/raw_ostream.h"
#include "llvm/Support/TargetRegistry.h"
#include "llvm/Support/TargetSelect.h"
#include "llvm/Target/TargetMachine.h"
#include "llvm/Target/TargetOptions.h"

#include "llvm_builder.h"
#include "assert.h"
#include "AST.h"
#include "Timer.h"
#include "Profiler.h"
#include "os.h"

using namespace llvm;

// function on llvm_backend.cpp
int link_object(const char *obj_file, ImportsHash &imports);

// These should be wrapped in some kinda class... some day
static LLVMContext TheContext;
static IRBuilder<> Builder(TheContext);
static Module *TheModule = nullptr;

static Function *llvm_function = nullptr;

static void generateCode(BaseAST *ast);

static void printBasicBlock(BasicBlock *bb)
{    
    printf("  Block %s has %d instructions\n", bb->getName().str().c_str(), (int)bb->size());
}

static void printFunction(Function *f)
{
    for (auto it = f->getBasicBlockList().begin(); it != f->getBasicBlockList().end(); it++) {
        printBasicBlock(&(*it));
    }
    f->print(outs());
    outs().flush();
}

static void generateFunctionPrototype(VariableDeclarationAST *decl)
{
    if (decl->codegen) return;

    assert(isFunctionDeclaration(decl));

    auto func_decl = (FunctionDefinitionAST *)decl->definition;
    auto ft = (FunctionTypeAST *)decl->specified_type;
    // we need to define the function prototype here first
    // and then proceed with the body if we have it
    generateCode(decl->specified_type);
    auto llvm_type = (llvm::FunctionType *)decl->specified_type->llvm_type;
    Function *llvm_func = Function::Create(llvm_type, Function::ExternalLinkage,
        decl->varname, TheModule);

    // ensure llvm knows the name of arguments
    u32 idx = 0;
    for (auto &llvm_arg : llvm_func->args()) {
        llvm_arg.setName(ft->arguments[idx]->varname);
        idx++;
    }

    decl->codegen = llvm_func;
}

static void allocateVariable(VariableDeclarationAST *decl)
{
    if (isFunctionDeclaration(decl)) {
        generateFunctionPrototype(decl);
    }

    // declared functions need no variable allocation
    if (isFunctionDefinition(decl)) return;
    if (isFunctionForeign(decl)) return;

    // we need the type of the variable to be part of llvm
    generateCode(decl->specified_type);

    if (isGlobalDeclaration(decl)) {
        bool isConstant = isConstantDeclaration(decl);
        llvm::Constant *initializer = nullptr;
        if (isConstant || decl->definition) {
            generateCode(decl->definition);
            initializer = (llvm::Constant *) decl->definition->codegen;
        } else {
            initializer = ConstantAggregateZero::get(decl->specified_type->llvm_type);
        }
        auto gv = new GlobalVariable(*TheModule, decl->specified_type->llvm_type, isConstant, 
            GlobalValue::CommonLinkage, initializer, decl->varname);
        decl->codegen = gv;
    } else {
        IRBuilder<> TmpB(&llvm_function->getEntryBlock(),
                 llvm_function->getEntryBlock().begin());
        Value *arraySize = nullptr;
        auto AllocA = TmpB.CreateAlloca(decl->specified_type->llvm_type, arraySize, decl->varname);

        if (decl->flags & DECL_FLAG_IS_FUNCTION_ARGUMENT) {
            // codegen should be here because it is generated in the function definition generation
            assert(decl->codegen);
            TmpB.CreateStore(decl->codegen, AllocA);
        }

        decl->codegen = AllocA;
    }
}

static Value *generateIdentifierCode(IdentifierAST *id)
{
    if (id->next != nullptr) {
        // this is the case of an ARRAY or STRUCT access, we need GEP
        std::vector<Value *> idx;
        auto decl = id->decl;
        BaseAST *loop_ast = id->next;
        Value *val = ConstantInt::get(Type::getInt32Ty(TheContext), APInt(32, 0));
        idx.push_back(val);
        
        while(loop_ast != nullptr) {
            switch(loop_ast->ast_type) {
                case AST_ARRAY_ACCESS: {
                    auto loop_aa = (ArrayAccessAST *)loop_ast;
                    ArrayTypeAST *atype = (ArrayTypeAST *)getDefinedType(loop_aa->prev);
                    assert(atype->ast_type == AST_ARRAY_TYPE);
                    if (isSizedArray(atype) || isDynamicArray(atype)) {
                        // Sized and Dynamic arrays are implemented as a struct and requires
                        // one more level of indirection
                        idx.push_back(ConstantInt::get(Type::getInt32Ty(TheContext), 0));
                    }
                    generateCode(loop_aa->array_exp);
                    idx.push_back(loop_aa->array_exp->codegen);
                    loop_ast = loop_aa->next;
                    break;
                }
                case AST_STRUCT_ACCESS: {
                    auto loop_sac = (StructAccessAST *)loop_ast;
                    assert(!"Struct access in LLVM is not implemented yet");
                    break;
                }
                default:
                    assert(!"We should never get here!");
            }
        }
        Value *gep = Builder.CreateGEP(
            // decl->specified_type->llvm_type, 
            decl->codegen, idx);
        return gep;
    } else {
        return id->decl->codegen;
    }
}

static void generateCode(BaseAST *ast)
{
    switch (ast->ast_type)
    {
    case AST_FILE: {
        auto f = (FileAST *)ast;

        for (auto decl : f->global_scope.decls) {
            allocateVariable(decl);
        }

        for (auto item : f->items) {
            switch (item->ast_type) {
            case AST_VARIABLE_DECLARATION: {
                generateCode(item);
                break;
            }
            case AST_RUN_DIRECTIVE: {
                // Run directives either have already been executed, or in top level they do nothing
                break;
            }
            default:
                assert(!"Unsupported type on top level expression");
            }
        }
        break;
    }
    case AST_STATEMENT_BLOCK: {
        auto stmt_block = (StatementBlockAST *)ast;

        if (stmt_block->codegen) return;

        for (auto decl : stmt_block->block_scope.decls) {
            allocateVariable(decl);
        }

        for (auto item : stmt_block->statements) {
            generateCode(item);
        }
        break;
    }
    case AST_IF_STATEMENT: {
        auto ifst = (IfStatementAST *)ast;
        // This code is taken from http://llvm.org/docs/tutorial/LangImpl05.html
        generateCode(ifst->condition);

        Value *cond = ifst->condition->codegen;
        cond = Builder.CreateICmpEQ(cond, ConstantInt::getTrue(TheContext), "if_cond");

        Function *lfunc = Builder.GetInsertBlock()->getParent();

        BasicBlock *then_block = BasicBlock::Create(TheContext, "then", lfunc);
        BasicBlock *else_block = BasicBlock::Create(TheContext, "else");
        BasicBlock *merge_block = BasicBlock::Create(TheContext, "ifcont");        

        Builder.CreateCondBr(cond, then_block, else_block);

        Builder.SetInsertPoint(then_block);

        generateCode(ifst->then_branch);
        Value *then_val = ifst->then_branch->codegen;

        Builder.CreateBr(merge_block);
        then_block = Builder.GetInsertBlock();

        llvm_function->getBasicBlockList().push_back(else_block);
        Builder.SetInsertPoint(else_block);
        if (ifst->else_branch) generateCode(ifst->else_branch);

        Builder.CreateBr(merge_block);
        else_block = Builder.GetInsertBlock();

        llvm_function->getBasicBlockList().push_back(merge_block);
        Builder.SetInsertPoint(merge_block);

        break;
    }
    case AST_RETURN_STATEMENT: {
        auto ret_stmt = (ReturnStatementAST *)ast;
        if (ret_stmt->ret != nullptr) {
            generateCode(ret_stmt->ret);
            Builder.CreateRet(ret_stmt->ret->codegen);
        } else {
            Builder.CreateRetVoid();
        }
        break;
    }
    case AST_FUNCTION_DEFINITION: {
        auto fundef = (FunctionDefinitionAST *)ast;
        assert(!"LLVM function definition is not implemented!");
        break;
    }
    case AST_FUNCTION_CALL: {
        auto funcall = (FunctionCallAST *)ast;
        std::vector<Value *> ArgsV;
        for (auto arg : funcall->args) {
            generateCode(arg);
            assert(arg->codegen);
            ArgsV.push_back(arg->codegen);
        }
        // The function call should not have a name if the return value is void
        auto llvm_call = Builder.CreateCall(funcall->fundef->var_decl->codegen, ArgsV);
        funcall->codegen = llvm_call;
        break;
    }
    case AST_IDENTIFIER: {
        auto id = (IdentifierAST *)ast;
        id->codegen = Builder.CreateLoad(generateIdentifierCode(id), id->name);
        break;
    }
    case AST_LITERAL: {
        auto lit = (LiteralAST *)ast;
        generateCode(lit->typeAST);
        switch (lit->typeAST->basic_type) {
        case BASIC_TYPE_INTEGER: {
            auto llvm_val = ConstantInt::get(
                lit->typeAST->llvm_type, 
                APInt(lit->typeAST->size_in_bytes * 8, 
                    (lit->typeAST->isSigned ? lit->_s64 : lit->_u64), 
                    lit->typeAST->isSigned));
            lit->codegen = llvm_val;
            break;
        }
        case BASIC_TYPE_FLOATING: {
            auto llvm_val = ConstantFP::get(lit->typeAST->llvm_type, lit->_f64);
            lit->codegen = llvm_val;
            break;
        }
        case BASIC_TYPE_BOOL: {
            auto llvm_val = ConstantInt::get(lit->typeAST->llvm_type, APInt(1, lit->_bool));
            lit->codegen = llvm_val;
            break;
        }
        case BASIC_TYPE_STRING: {
            auto llvm_str = (Constant *)Builder.CreateGlobalStringPtr(lit->str);
            auto llvm_sz = ConstantInt::get(Type::getInt64Ty(TheContext), APInt(64, strlen(lit->str)));
            std::vector<Constant *> members;
            members.push_back(llvm_str);
            members.push_back(llvm_sz);
            auto llvm_struct = ConstantStruct::get((StructType *)lit->typeAST->llvm_type, members);
            lit->codegen = llvm_struct;
            break;
        }
        default:
            assert(!"Unknonw literal type!");
        }
        break;
    }
    case AST_BINARY_OPERATION: {
        auto binop = (BinaryOperationAST *)ast;
        generateCode(binop->lhs);
        generateCode(binop->rhs);
        switch (binop->op) {
        case TK_EQ: {
            if (isTypeInteger(binop->lhs->expr_type) || isTypeBoolean(binop->lhs->expr_type) ||
                isTypePointer(binop->lhs->expr_type)) {
                binop->codegen = Builder.CreateICmpEQ(binop->lhs->codegen, binop->rhs->codegen);
            } else if (isTypeFloating(binop->lhs->expr_type)) {
                binop->codegen = Builder.CreateFCmpOEQ(binop->lhs->codegen, binop->rhs->codegen);
            } else { 
                assert(!"Type not supported for EQ comparison!"); 
            }
            break;
        }
        case TK_LEQ: {
            if (isTypePointer(binop->lhs->expr_type) || isTypeBoolean(binop->lhs->expr_type)) {
                binop->codegen = Builder.CreateICmpULE(binop->lhs->codegen, binop->rhs->codegen);
            } else if (isTypeInteger(binop->lhs->expr_type)) {
                auto dt = (DirectTypeAST *)binop->lhs->expr_type;
                if (dt->isSigned) {
                    binop->codegen = Builder.CreateICmpSLE(binop->lhs->codegen, binop->rhs->codegen);
                } else {
                    binop->codegen = Builder.CreateICmpULE(binop->lhs->codegen, binop->rhs->codegen);
                }
            } else if (isTypeFloating(binop->lhs->expr_type)) {
                binop->codegen = Builder.CreateFCmpOLE(binop->lhs->codegen, binop->rhs->codegen);
            } else {
                assert(!"Type not supported for LEQ comparison!");
            }
            break;
        }
        case TK_GEQ: {
            if (isTypePointer(binop->lhs->expr_type) || isTypeBoolean(binop->lhs->expr_type)) {
                binop->codegen = Builder.CreateICmpULE(binop->lhs->codegen, binop->rhs->codegen);
            } else if (isTypeInteger(binop->lhs->expr_type)) {
                auto dt = (DirectTypeAST *)binop->lhs->expr_type;
                if (dt->isSigned) {
                    binop->codegen = Builder.CreateICmpSGE(binop->lhs->codegen, binop->rhs->codegen);
                } else {
                    binop->codegen = Builder.CreateICmpUGE(binop->lhs->codegen, binop->rhs->codegen);
                }
            } else if (isTypeFloating(binop->lhs->expr_type)) {
                binop->codegen = Builder.CreateFCmpOGE(binop->lhs->codegen, binop->rhs->codegen);
            } else {
                assert(!"Type not supported for GEQ comparison!");
            }
            break;
        }
        case TK_NEQ: {
            if (isTypeInteger(binop->lhs->expr_type) || isTypeBoolean(binop->lhs->expr_type) ||
                isTypePointer(binop->lhs->expr_type)) {
                binop->codegen = Builder.CreateICmpNE(binop->lhs->codegen, binop->rhs->codegen);
            } else if (isTypeFloating(binop->lhs->expr_type)) {
                binop->codegen = Builder.CreateFCmpONE(binop->lhs->codegen, binop->rhs->codegen);
            } else {
                assert(!"Type not supported for NEQ comparison!");
            }
            break;
        }
        case TK_LT: {
            if (isTypePointer(binop->lhs->expr_type) || isTypeBoolean(binop->lhs->expr_type)) {
                binop->codegen = Builder.CreateICmpULT(binop->lhs->codegen, binop->rhs->codegen);
            } else if (isTypeInteger(binop->lhs->expr_type)) {
                auto dt = (DirectTypeAST *)binop->lhs->expr_type;
                if (dt->isSigned) {
                    binop->codegen = Builder.CreateICmpSLT(binop->lhs->codegen, binop->rhs->codegen);
                } else {
                    binop->codegen = Builder.CreateICmpULT(binop->lhs->codegen, binop->rhs->codegen);
                }
            } else if (isTypeFloating(binop->lhs->expr_type)) {
                binop->codegen = Builder.CreateFCmpOLT(binop->lhs->codegen, binop->rhs->codegen);
            } else {
                assert(!"Type not supported for LT comparison!");
            }
            break;
        }
        case TK_GT: {
            if (isTypePointer(binop->lhs->expr_type) || isTypeBoolean(binop->lhs->expr_type)) {
                binop->codegen = Builder.CreateICmpUGT(binop->lhs->codegen, binop->rhs->codegen);
            } else if (isTypeInteger(binop->lhs->expr_type)) {
                auto dt = (DirectTypeAST *)binop->lhs->expr_type;
                if (dt->isSigned) {
                    binop->codegen = Builder.CreateICmpSGT(binop->lhs->codegen, binop->rhs->codegen);
                } else {
                    binop->codegen = Builder.CreateICmpUGT(binop->lhs->codegen, binop->rhs->codegen);
                }
            } else if (isTypeFloating(binop->lhs->expr_type)) {
                binop->codegen = Builder.CreateFCmpOGT(binop->lhs->codegen, binop->rhs->codegen);
            } else {
                assert(!"Type not supported for GT comparison!");
            }
            break;
        }
        case TK_STAR: {
            if (isTypePointer(binop->lhs->expr_type) || isTypeBoolean(binop->lhs->expr_type)) {
                assert(!"Pointers and booleans cannot be multiplied!");
            } else if (isTypeInteger(binop->lhs->expr_type) || isTypeFloating(binop->lhs->expr_type)) {
                binop->codegen = Builder.CreateMul(binop->lhs->codegen, binop->rhs->codegen);
            } else {
                assert(!"Type not supported for division!");
            }
            break;
        }
        case TK_DIV: {
            if (isTypePointer(binop->lhs->expr_type) || isTypeBoolean(binop->lhs->expr_type)) {
                assert(!"Pointers and booleans cannot be divided!");
            } else if (isTypeInteger(binop->lhs->expr_type)) {
                binop->codegen = Builder.CreateUDiv(binop->lhs->codegen, binop->rhs->codegen);
            } else if (isTypeFloating(binop->lhs->expr_type)) {
                binop->codegen = Builder.CreateFDiv(binop->lhs->codegen, binop->rhs->codegen);
            } else {
                assert(!"Type not supported for division!");
            }
            break;
        }
        case TK_MOD: {
            assert(!"TK_MOD has not been implemented for llvm!");
            break;
        }
        case TK_PLUS: {
            binop->codegen = Builder.CreateAdd(binop->lhs->codegen, binop->rhs->codegen);
            break;
        }
        case TK_MINUS: {
            binop->codegen = Builder.CreateSub(binop->lhs->codegen, binop->rhs->codegen);
            break;
        }
        default:
            assert(!"Token operation is not recognized/supported!");
        }
        break;
    }
    case AST_UNARY_OPERATION: {
        auto unop = (UnaryOperationAST *)ast;
        generateCode(unop->expr);
        unop->codegen = Builder.CreateNot(unop->expr->codegen);
        break;
    }
    case AST_ASSIGNMENT: {
        auto assign = (AssignmentAST *)ast;
        // This can work for identifiers but not for arrays or structs
        generateCode(assign->rhs);
        if (assign->lhs->ast_type == AST_IDENTIFIER) {
            auto id = (IdentifierAST *)assign->lhs;
            Builder.CreateStore(assign->rhs->codegen, generateIdentifierCode(id));
        } else {
            assert(!"Not implemented complex assignment");
        }
        break;
    }
    case AST_VARIABLE_DECLARATION: {
        auto decl = (VariableDeclarationAST *)ast;

        // all variables should have their alloc (or prototype) done already
        assert(decl->codegen);

        switch (decl->specified_type->ast_type) {
        case AST_FUNCTION_TYPE: {
            // @TODO: differentiate between top level functions
            // and lambda (these need rename)

            auto func_decl = (FunctionDefinitionAST *)decl->definition;
            auto ft = (FunctionTypeAST *)decl->specified_type;

            // If it is a Foreign function, nothing else to do;
            if (ft->isForeign) return;

            Function *llvm_func = (Function *)decl->codegen;

            u32 arg_index = 0;
            auto& arg_decls = func_decl->function_body->block_scope.decls;
            for (auto& llvm_arg : llvm_func->args()) {
                arg_decls[arg_index++]->codegen = &llvm_arg;
            }
            // If it is not foreign, we should do the body here
            // Create a new basic block to start insertion into.
            BasicBlock *BB = BasicBlock::Create(TheContext, "entry", llvm_func);
            Builder.SetInsertPoint(BB);

            Function *old_func = llvm_function;
            llvm_function = llvm_func;

            generateCode(func_decl->function_body);

            if (isVoidType(ft->return_type)) {
                Builder.CreateRetVoid();
            }

            verifyFunction(*llvm_func);
            llvm_function = old_func;

            break;
        }
        case AST_DIRECT_TYPE: {
            // Global variables do not require the initialization, it is done in another section.
            if (isGlobalDeclaration(decl)) return;

            generateCode(decl->specified_type);
            // The variable has already been allocated, now we might have to initialize it
            if (!decl->definition) return;

            generateCode(decl->definition);
            Builder.CreateStore(decl->definition->codegen, decl->codegen);

            break;
        }
        case AST_POINTER_TYPE: {
            // @TODO: check if we can merge POINTER TYPE with DIRECT_TYPE
            // Global variables do not require the initialization, it is done in another section.
            if (isGlobalDeclaration(decl)) return;

            generateCode(decl->specified_type);
            // The variable has already been allocated, now we might have to initialize it
            if (!decl->definition) return;

            generateCode(decl->definition);
            Builder.CreateStore(decl->definition->codegen, decl->codegen);

            break;            
        }
        case AST_ARRAY_TYPE: {
            // Global variables do not require the initialization, it is done in another section.
            if (isGlobalDeclaration(decl)) return;

            generateCode(decl->specified_type);
            // The variable has already been allocated, now we might have to initialize it
            if (!decl->definition) return;

            generateCode(decl->definition);
            Builder.CreateStore(decl->definition->codegen, decl->codegen);

            break;
        }
        default:
            assert(!"Type of variable declaration on llvm is not supported!");
        }
        break;
    }
    case AST_STRUCT_DEFINITION: {
        auto defn = (StructDefinitionAST *)ast;
        assert(!"Struct definition is not supported on llvm yet");
        break;
    }
    case AST_ARRAY_ACCESS: {
        auto aa = (ArrayAccessAST *)ast;
        generateCode(aa->array_exp);
//        Builder.CreateGEP()
        assert(!"Array access is not supported on llvm yet");
        break;
    }
    case AST_STRUCT_ACCESS: {
        auto sac = (StructAccessAST *)ast;
        assert(!"Struct access is not supported on llvm yet");
        break;
    }
    case AST_FUNCTION_TYPE: {
        auto ftype = (FunctionTypeAST *)ast;
        std::vector<Type *> func_args;
        for (auto arg : ftype->arguments) {
            generateCode(arg->specified_type);
            func_args.push_back(arg->specified_type->llvm_type);
        }
        generateCode(ftype->return_type);
        auto llvm_ft = FunctionType::get(ftype->return_type->llvm_type, // return type
            func_args,
            ftype->hasVariableArguments);

        ftype->llvm_type = llvm_ft;
        break;
    }
    case AST_DIRECT_TYPE: {
        auto dtype = (DirectTypeAST *)ast;
        if (dtype->llvm_type) return;

        switch (dtype->basic_type) {
        case BASIC_TYPE_BOOL: {
            dtype->llvm_type = Type::getInt1Ty(TheContext);
            break;
        }
        case BASIC_TYPE_CUSTOM: {
            assert(!"Custom types not supported on llvm yet");
            break;
        }
        case BASIC_TYPE_FLOATING: {
            switch (dtype->size_in_bytes) {
            case 4: {
                dtype->llvm_type = Type::getFloatTy(TheContext);
                break;
            }
            case 8: {
                dtype->llvm_type = Type::getDoubleTy(TheContext);
                break;
            }
            default:
                assert(!"Unsupported byte number for floating point");
            }
            break;
        }
        case BASIC_TYPE_INTEGER: {
            switch (dtype->size_in_bytes) {
            case 1: {
                dtype->llvm_type = Type::getInt8Ty(TheContext);
                break;
            }
            case 2: {
                dtype->llvm_type = Type::getInt16Ty(TheContext);
                break;
            }
            case 4: {
                dtype->llvm_type = Type::getInt32Ty(TheContext);
                break;
            }
            case 8: {
                dtype->llvm_type = Type::getInt64Ty(TheContext);
                break;
            }
            default:
                assert(!"Unsupported byte number for integer");
            }
            break;
        }
        case BASIC_TYPE_STRING: {
            // A string is a special case of a struct type
            std::vector<Type *> struct_members;
            struct_members.push_back(Type::getInt8PtrTy(TheContext)); // the char *
            struct_members.push_back(Type::getInt64Ty(TheContext)); // the size
            dtype->llvm_type = StructType::create(TheContext, struct_members);
            break;
        }
        case BASIC_TYPE_VOID: {
            dtype->llvm_type = Type::getVoidTy(TheContext);
            return;
        }
        default:
            assert(!"Unknonw basic type on llvm generation");
        }
        break;
    }
    case AST_POINTER_TYPE: {
        auto ptype = (PointerTypeAST *)ast;
        if (ptype->llvm_type) return;
        if (!isDirectType(ptype->points_to_type)) {
            assert(!"Only pointers to direct types are supported for now");
        }
        auto dtype = (DirectTypeAST *)ptype->points_to_type;
        generateCode(dtype);
        assert(dtype->llvm_type != nullptr);
        ptype->llvm_type = dtype->llvm_type->getPointerTo();
        break;
    }
    case AST_ARRAY_TYPE: {
        auto atype = (ArrayTypeAST *)ast;
        generateCode(atype->array_of_type);
        assert(atype->array_of_type->llvm_type != nullptr);
        if (isStaticArray(atype)) {
            atype->llvm_type = ArrayType::get(atype->array_of_type->llvm_type, atype->num_elems);
        } else {
            // common parts for sized and dynamic arrays
            std::vector<Type *> array_members;
            array_members.push_back(atype->array_of_type->llvm_type->getPointerTo()); // .data
            array_members.push_back(Type::getInt64Ty(TheContext)); // This is .count
            const char *llvm_name = "SizedArray";
            if (isDynamicArray(atype)) {
                array_members.push_back(Type::getInt64Ty(TheContext)); // This is .reserved_size                
                llvm_name = "DynamicArray";
            }
            atype->llvm_type = StructType::create(TheContext, array_members, llvm_name);

            atype->llvm_type->print(outs(), false);
        }
        break;
    }
    case AST_STRUCT_TYPE: {
        auto stype = (StructTypeAST *)ast;
        assert(!"Structure type is not supported on llvm yet");
        break;
    }
    case AST_RUN_DIRECTIVE: {
        auto run = (RunDirectiveAST *)ast;
        if (isVoidType(run->expr_type)) break;

        assert(run->new_ast->ast_type == AST_LITERAL);
        auto lit = (LiteralAST *)run->new_ast;

        generateCode(lit);
        run->codegen = lit->codegen;
        break;
    }
    default:
        assert(!"Unknonw AST type on LLVM generation");
    }
}


/*

C:\code\c2>..\llvm\build\Release\bin\llc -march=x86-64 -mattr=help
Available CPUs for this target:

amdfam10       - Select the amdfam10 processor.
athlon         - Select the athlon processor.
athlon-4       - Select the athlon-4 processor.
athlon-fx      - Select the athlon-fx processor.
athlon-mp      - Select the athlon-mp processor.
athlon-tbird   - Select the athlon-tbird processor.
athlon-xp      - Select the athlon-xp processor.
athlon64       - Select the athlon64 processor.
athlon64-sse3  - Select the athlon64-sse3 processor.
atom           - Select the atom processor.
barcelona      - Select the barcelona processor.
bdver1         - Select the bdver1 processor.
bdver2         - Select the bdver2 processor.
bdver3         - Select the bdver3 processor.
bdver4         - Select the bdver4 processor.
bonnell        - Select the bonnell processor.
broadwell      - Select the broadwell processor.
btver1         - Select the btver1 processor.
btver2         - Select the btver2 processor.
c3             - Select the c3 processor.
c3-2           - Select the c3-2 processor.
cannonlake     - Select the cannonlake processor.
core-avx-i     - Select the core-avx-i processor.
core-avx2      - Select the core-avx2 processor.
core2          - Select the core2 processor.
corei7         - Select the corei7 processor.
corei7-avx     - Select the corei7-avx processor.
generic        - Select the generic processor.
geode          - Select the geode processor.
goldmont       - Select the goldmont processor.
haswell        - Select the haswell processor.
i386           - Select the i386 processor.
i486           - Select the i486 processor.
i586           - Select the i586 processor.
i686           - Select the i686 processor.
icelake        - Select the icelake processor.
ivybridge      - Select the ivybridge processor.
k6             - Select the k6 processor.
k6-2           - Select the k6-2 processor.
k6-3           - Select the k6-3 processor.
k8             - Select the k8 processor.
k8-sse3        - Select the k8-sse3 processor.
knl            - Select the knl processor.
knm            - Select the knm processor.
lakemont       - Select the lakemont processor.
nehalem        - Select the nehalem processor.
nocona         - Select the nocona processor.
opteron        - Select the opteron processor.
opteron-sse3   - Select the opteron-sse3 processor.
penryn         - Select the penryn processor.
pentium        - Select the pentium processor.
pentium-m      - Select the pentium-m processor.
pentium-mmx    - Select the pentium-mmx processor.
pentium2       - Select the pentium2 processor.
pentium3       - Select the pentium3 processor.
pentium3m      - Select the pentium3m processor.
pentium4       - Select the pentium4 processor.
pentium4m      - Select the pentium4m processor.
pentiumpro     - Select the pentiumpro processor.
prescott       - Select the prescott processor.
sandybridge    - Select the sandybridge processor.
silvermont     - Select the silvermont processor.
skx            - Select the skx processor.
skylake        - Select the skylake processor.
skylake-avx512 - Select the skylake-avx512 processor.
slm            - Select the slm processor.
westmere       - Select the westmere processor.
winchip-c6     - Select the winchip-c6 processor.
winchip2       - Select the winchip2 processor.
x86-64         - Select the x86-64 processor.
yonah          - Select the yonah processor.
znver1         - Select the znver1 processor.

Available features for this target:

16bit-mode                    - 16-bit mode (i8086).
32bit-mode                    - 32-bit mode (80386).
3dnow                         - Enable 3DNow! instructions.
3dnowa                        - Enable 3DNow! Athlon instructions.
64bit                         - Support 64-bit instructions.
64bit-mode                    - 64-bit mode (x86_64).
adx                           - Support ADX instructions.
aes                           - Enable AES instructions.
atom                          - Intel Atom processors.
avx                           - Enable AVX instructions.
avx2                          - Enable AVX2 instructions.
avx512bitalg                  - Enable AVX-512 Bit Algorithms.
avx512bw                      - Enable AVX-512 Byte and Word Instructions.
avx512cd                      - Enable AVX-512 Conflict Detection Instructions.
avx512dq                      - Enable AVX-512 Doubleword and Quadword Instructions.
avx512er                      - Enable AVX-512 Exponential and Reciprocal Instructions.
avx512f                       - Enable AVX-512 instructions.
avx512ifma                    - Enable AVX-512 Integer Fused Multiple-Add.
avx512pf                      - Enable AVX-512 PreFetch Instructions.
avx512vbmi                    - Enable AVX-512 Vector Byte Manipulation Instructions.
avx512vbmi2                   - Enable AVX-512 further Vector Byte Manipulation Instructions.
avx512vl                      - Enable AVX-512 Vector Length eXtensions.
avx512vnni                    - Enable AVX-512 Vector Neural Network Instructions.
avx512vpopcntdq               - Enable AVX-512 Population Count Instructions.
bmi                           - Support BMI instructions.
bmi2                          - Support BMI2 instructions.
broadwell                     - Intel Broadwell processors.
cannonlake                    - Intel Cannonlake processors.
clflushopt                    - Flush A Cache Line Optimized.
clwb                          - Cache Line Write Back.
clzero                        - Enable Cache Line Zero.
cmov                          - Enable conditional move instructions.
cx16                          - 64-bit with cmpxchg16b.
ermsb                         - REP MOVS/STOS are fast.
f16c                          - Support 16-bit floating point conversion instructions.
fast-gather                   - Indicates if gather is reasonably fast..
fast-lzcnt                    - LZCNT instructions are as fast as most simple integer ops.
fast-partial-ymm-or-zmm-write - Partial writes to YMM/ZMM registers are fast.
fast-scalar-fsqrt             - Scalar SQRT is fast (disable Newton-Raphson).
fast-shld-rotate              - SHLD can be used as a faster rotate.
fast-vector-fsqrt             - Vector SQRT is fast (disable Newton-Raphson).
fma                           - Enable three-operand fused multiple-add.
fma4                          - Enable four-operand fused multiple-add.
fsgsbase                      - Support FS/GS Base instructions.
fxsr                          - Support fxsave/fxrestore instructions.
gfni                          - Enable Galois Field Arithmetic Instructions.
glm                           - Intel Goldmont processors.
haswell                       - Intel Haswell processors.
ibt                           - Support CET Indirect-Branch-Tracking instructions.
icelake                       - Intel Icelake processors.
idivl-to-divb                 - Use 8-bit divide for positive values less than 256.
idivq-to-divl                 - Use 32-bit divide for positive values less than 2^32.
knl                           - Intel Knights Landing processors.
lea-sp                        - Use LEA for adjusting the stack pointer.
lea-uses-ag                   - LEA instruction needs inputs at AG stage.
lwp                           - Enable LWP instructions.
lzcnt                         - Support LZCNT instruction.
macrofusion                   - Various instructions can be fused with conditional branches.
mmx                           - Enable MMX instructions.
movbe                         - Support MOVBE instruction.
mpx                           - Support MPX instructions.
mwaitx                        - Enable MONITORX/MWAITX timer functionality.
pad-short-functions           - Pad short functions.
pclmul                        - Enable packed carry-less multiplication instructions.
pku                           - Enable protection keys.
popcnt                        - Support POPCNT instruction.
prefetchwt1                   - Prefetch with Intent to Write and T1 Hint.
prfchw                        - Support PRFCHW instructions.
rdrnd                         - Support RDRAND instruction.
rdseed                        - Support RDSEED instruction.
rtm                           - Support RTM instructions.
sahf                          - Support LAHF and SAHF instructions.
sgx                           - Enable Software Guard Extensions.
sha                           - Enable SHA instructions.
shstk                         - Support CET Shadow-Stack instructions.
skx                           - Intel Skylake Server processors.
skylake                       - Intel Skylake processors.
slm                           - Intel Silvermont processors.
slow-3ops-lea                 - LEA instruction with 3 ops or certain registers is slow.
slow-incdec                   - INC and DEC instructions are slower than ADD and SUB.
slow-lea                      - LEA instruction with certain arguments is slow.
slow-pmulld                   - PMULLD instruction is slow.
slow-shld                     - SHLD instruction is slow.
slow-two-mem-ops              - Two memory operand instructions are slow.
slow-unaligned-mem-16         - Slow unaligned 16-byte memory access.
slow-unaligned-mem-32         - Slow unaligned 32-byte memory access.
soft-float                    - Use software floating point features..
sse                           - Enable SSE instructions.
sse-unaligned-mem             - Allow unaligned memory operands with SSE instructions.
sse2                          - Enable SSE2 instructions.
sse3                          - Enable SSE3 instructions.
sse4.1                        - Enable SSE 4.1 instructions.
sse4.2                        - Enable SSE 4.2 instructions.
sse4a                         - Support SSE 4a instructions.
ssse3                         - Enable SSSE3 instructions.
tbm                           - Enable TBM instructions.
vaes                          - Promote selected AES instructions to AVX512/AVX registers.
vpclmulqdq                    - Enable vpclmulqdq instructions.
x87                           - Enable X87 float instructions.
xop                           - Enable XOP instructions.
xsave                         - Support xsave instructions.
xsavec                        - Support xsavec instructions.
xsaveopt                      - Support xsaveopt instructions.
xsaves                        - Support xsaves instructions.
*/

extern "C" DLLEXPORT void llvm_compile(FileAST *root, const char *obj_file, double &codegenTime, double &bingenTime, double &linkTime, bool option_llvm_print)
{   
    Timer timer;

    CPU_SAMPLE("LLVM compile");

    timer.startTimer();
    TheModule = new Module("jai", TheContext);
    
    // Initialize the target registry etc.
    InitializeAllTargetInfos();
    InitializeAllTargets();
    InitializeAllTargetMCs();
    InitializeAllAsmParsers();
    InitializeAllAsmPrinters();

    auto TargetTriple = sys::getDefaultTargetTriple();
    TheModule->setTargetTriple(TargetTriple);

    std::string Error;
    auto Target = TargetRegistry::lookupTarget(TargetTriple, Error);

    auto CPU = "corei7-avx";
    // Should this be avx2 or such?
    auto Features = "";

    TargetOptions opt;
    auto RM = Optional<Reloc::Model>();
    auto TheTargetMachine =
        Target->createTargetMachine(TargetTriple, CPU, Features, opt, RM);

    TheModule->setDataLayout(TheTargetMachine->createDataLayout());

    // Do the actual compile
    generateCode(root);

    codegenTime = timer.stopTimer();

    timer.startTimer();

    {
        std::error_code EC;
        raw_fd_ostream dest(obj_file, EC, sys::fs::F_None);

        if (EC) {
            errs() << "Could not open file: " << EC.message();
            assert(false);
            return;
        }

        legacy::PassManager pass;
        auto FileType = TargetMachine::CGFT_ObjectFile;

        if (TheTargetMachine->addPassesToEmitFile(pass, dest, FileType)) {
            errs() << "TheTargetMachine can't emit a file of this type";
            assert(false);
            return;
        }

        pass.run(*TheModule);

        dest.flush();

        bingenTime = timer.stopTimer();
    }

    outs() << "Object generation [" << obj_file << "] completed, now linking...\n";
    outs().flush();

    {
        CPU_SAMPLE("LLVM external link");
        timer.startTimer();
        link_object(obj_file, root->imports);
        linkTime = timer.stopTimer();
    }

    if (option_llvm_print) {
        TheModule->print(outs(), nullptr);
        outs().flush();
    }
}

