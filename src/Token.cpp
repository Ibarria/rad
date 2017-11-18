#include "Token.h"
#include <stdlib.h>
#include <string.h>
#include <stdio.h>

//Token::~Token()
//{
//	type = INVALID;
//}

void Token::clear()
{
	type = TK_INVALID;
    string = nullptr;
}


void Token::print()
{
	printf("Token %03d:%03d type %s", loc.line, loc.col, TokenTypeToStr(type));
	switch (type) {
	case TK_NUMBER:
		printf(" %" U64FMT "u", _u64);
		break;
    case TK_FNUMBER:
        printf(" %f", _f64);
        break;
    case TK_IDENTIFIER:
	case TK_STRING:
		printf(" %s", string);
		break;
	case TK_CHAR:
		printf(" %c", (char)_u64);
		break;
	}
	printf("\n");
}

#define CASE_TOKEN_TYPE(a) case a: return #a
const char * TokenTypeToStr(TOKEN_TYPE type)
{
	switch (type) {
		CASE_TOKEN_TYPE(TK_LAST_TOKEN);
		CASE_TOKEN_TYPE(TK_NUMBER);
        CASE_TOKEN_TYPE(TK_FNUMBER);
        CASE_TOKEN_TYPE(TK_IDENTIFIER);
		CASE_TOKEN_TYPE(TK_EQ);
        CASE_TOKEN_TYPE(TK_LEQ);
        CASE_TOKEN_TYPE(TK_GEQ);
        CASE_TOKEN_TYPE(TK_NEQ);
		CASE_TOKEN_TYPE(TK_LT);
        CASE_TOKEN_TYPE(TK_GT);
        CASE_TOKEN_TYPE(TK_RSHIFT);
        CASE_TOKEN_TYPE(TK_LSHIFT);
		CASE_TOKEN_TYPE(TK_ASSIGN);
        CASE_TOKEN_TYPE(TK_IMPLICIT_ASSIGN);
        CASE_TOKEN_TYPE(TK_MUL_ASSIGN);
        CASE_TOKEN_TYPE(TK_DIV_ASSIGN);
        CASE_TOKEN_TYPE(TK_MOD_ASSIGN);
        CASE_TOKEN_TYPE(TK_ADD_ASSIGN);
        CASE_TOKEN_TYPE(TK_SUB_ASSIGN);
        CASE_TOKEN_TYPE(TK_LEFT_ASSIGN);
        CASE_TOKEN_TYPE(TK_RIGHT_ASSIGN);
        CASE_TOKEN_TYPE(TK_AND_ASSIGN);
        CASE_TOKEN_TYPE(TK_XOR_ASSIGN);
        CASE_TOKEN_TYPE(TK_OR_ASSIGN);
        CASE_TOKEN_TYPE(TK_OPEN_PAREN);
        CASE_TOKEN_TYPE(TK_CLOSE_PAREN);
        CASE_TOKEN_TYPE(TK_OPEN_BRACKET);
        CASE_TOKEN_TYPE(TK_CLOSE_BRACKET);
        CASE_TOKEN_TYPE(TK_OPEN_SQBRACKET);
        CASE_TOKEN_TYPE(TK_CLOSE_SQBRACKET);
        CASE_TOKEN_TYPE(TK_RETURN_ARROW);
        CASE_TOKEN_TYPE(TK_SEMICOLON);
        CASE_TOKEN_TYPE(TK_COLON);
        CASE_TOKEN_TYPE(TK_DOUBLE_COLON);
        CASE_TOKEN_TYPE(TK_PERIOD);
        CASE_TOKEN_TYPE(TK_DOUBLE_PERIOD);
        CASE_TOKEN_TYPE(TK_HASH);
        CASE_TOKEN_TYPE(TK_STAR);
        CASE_TOKEN_TYPE(TK_DIV);
        CASE_TOKEN_TYPE(TK_MOD);
        CASE_TOKEN_TYPE(TK_HAT);
        CASE_TOKEN_TYPE(TK_PIPE);
        CASE_TOKEN_TYPE(TK_AMP);
        CASE_TOKEN_TYPE(TK_DOUBLE_AMP);
        CASE_TOKEN_TYPE(TK_PLUS);
        CASE_TOKEN_TYPE(TK_DOUBLE_PLUS);
        CASE_TOKEN_TYPE(TK_MINUS);
        CASE_TOKEN_TYPE(TK_DOUBLE_MINUS);
        CASE_TOKEN_TYPE(TK_TRIPLE_MINUS);
        CASE_TOKEN_TYPE(TK_COMMA);
        CASE_TOKEN_TYPE(TK_BANG);
        CASE_TOKEN_TYPE(TK_STRING);
        CASE_TOKEN_TYPE(TK_STRING_KEYWORD);
        CASE_TOKEN_TYPE(TK_CHAR);
        CASE_TOKEN_TYPE(TK_IF);
        CASE_TOKEN_TYPE(TK_FOR);
        CASE_TOKEN_TYPE(TK_RETURN);
        CASE_TOKEN_TYPE(TK_BOOL);
        CASE_TOKEN_TYPE(TK_TRUE);
        CASE_TOKEN_TYPE(TK_FALSE);
        CASE_TOKEN_TYPE(TK_INT);
        CASE_TOKEN_TYPE(TK_U8);
        CASE_TOKEN_TYPE(TK_U16);
        CASE_TOKEN_TYPE(TK_U32);
        CASE_TOKEN_TYPE(TK_U64);
        CASE_TOKEN_TYPE(TK_S8);
        CASE_TOKEN_TYPE(TK_S16);
        CASE_TOKEN_TYPE(TK_S32);
        CASE_TOKEN_TYPE(TK_S64);
        CASE_TOKEN_TYPE(TK_FLOAT);
        CASE_TOKEN_TYPE(TK_F32);
        CASE_TOKEN_TYPE(TK_F64);
        CASE_TOKEN_TYPE(TK_IMPORT);
        CASE_TOKEN_TYPE(TK_LOAD);
        CASE_TOKEN_TYPE(TK_RUN);
        CASE_TOKEN_TYPE(TK_FOREIGN);
    }
        return "UNKNOWN";   
}                       

