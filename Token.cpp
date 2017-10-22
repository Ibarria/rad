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
	type = INVALID;
}

//Token::Token(const Token & rhs)
//{
//	*this = rhs;
//}
//
//Token::Token(Token && rhs)
//{
//	*this = rhs;
//}
//
//Token & Token::operator=(Token && rhs)
//{
//	if (type == STRING || type == IDENTIFIER) {
//		if (pl.pstr) {
//			free(pl.pstr);
//			pl.pstr = nullptr;
//		}
//	}
//	type = rhs.type;
//	pl = rhs.pl;
//	rhs.pl.pstr = nullptr;
//	return *this;
//}
//
//Token & Token::operator=(Token const & rhs)
//{
//	if (type == STRING || type == IDENTIFIER) {
//		if (pl.pstr) {
//			free(pl.pstr);
//			pl.pstr = nullptr;
//		}
//	}
//	type = rhs.type;
//	if (type == STRING || type == IDENTIFIER) {
//		u32 s = (u32)strlen(rhs.pl.pstr);
//		pl.pstr = new char[s + 1];
//		for (u32 i = 0; i < s; i++) pl.pstr[i] = rhs.pl.pstr[i];
//	} else {
//		pl = rhs.pl;
//	}
//	return *this;
//}

void Token::print()
{
	printf("Token %03lld:%03lld type %s", loc.line, loc.col, TokenTypeToStr(type));
	switch (type) {
	case NUMBER:
		printf(" %d", pl.pu32);
		break;
	case IDENTIFIER:
	case STRING:
		printf(" %s", str.c_str());
		break;
	case CHAR:
		printf(" %c", pl.pu32);
		break;
	}
	printf("\n");
}

#define CASE_TOKEN_TYPE(a) case a: return #a
const char * TokenTypeToStr(TOKEN_TYPE type)
{
	switch (type) {
		CASE_TOKEN_TYPE(LAST_TOKEN);
		CASE_TOKEN_TYPE(NUMBER);
		CASE_TOKEN_TYPE(IDENTIFIER);
		CASE_TOKEN_TYPE(EQ);
		CASE_TOKEN_TYPE(LEQ);
		CASE_TOKEN_TYPE(GEQ);
		CASE_TOKEN_TYPE(NEQ);
		CASE_TOKEN_TYPE(LT);
		CASE_TOKEN_TYPE(GT);
		CASE_TOKEN_TYPE(RSHIFT);
		CASE_TOKEN_TYPE(LSHIFT);
		CASE_TOKEN_TYPE(ASSIGN);
        CASE_TOKEN_TYPE(MUL_ASSIGN);
        CASE_TOKEN_TYPE(DIV_ASSIGN);
        CASE_TOKEN_TYPE(MOD_ASSIGN);
        CASE_TOKEN_TYPE(ADD_ASSIGN);
        CASE_TOKEN_TYPE(SUB_ASSIGN);
        CASE_TOKEN_TYPE(LEFT_ASSIGN);
        CASE_TOKEN_TYPE(RIGHT_ASSIGN);
        CASE_TOKEN_TYPE(AND_ASSIGN);
        CASE_TOKEN_TYPE(XOR_ASSIGN);
        CASE_TOKEN_TYPE(OR_ASSIGN);
        CASE_TOKEN_TYPE(OPEN_PAREN);
        CASE_TOKEN_TYPE(CLOSE_PAREN);
		CASE_TOKEN_TYPE(OPEN_BRACKET);
		CASE_TOKEN_TYPE(CLOSE_BRACKET);
		CASE_TOKEN_TYPE(OPEN_SQBRACKET);
		CASE_TOKEN_TYPE(CLOSE_SQBRACKET);
        CASE_TOKEN_TYPE(OPEN_CURLYBRACKET);
        CASE_TOKEN_TYPE(CLOSE_CURLYBRACKET);
        CASE_TOKEN_TYPE(SEMICOLON);
		CASE_TOKEN_TYPE(COLON);
        CASE_TOKEN_TYPE(DOUBLE_COLON);
        CASE_TOKEN_TYPE(PERIOD);
        CASE_TOKEN_TYPE(DOUBLE_PERIOD);
        CASE_TOKEN_TYPE(HASH);
		CASE_TOKEN_TYPE(STAR);
		CASE_TOKEN_TYPE(DIV);
		CASE_TOKEN_TYPE(MOD);
        CASE_TOKEN_TYPE(HAT);
        CASE_TOKEN_TYPE(PIPE);
        CASE_TOKEN_TYPE(AMP);
        CASE_TOKEN_TYPE(DOUBLE_AMP);
        CASE_TOKEN_TYPE(PLUS);
        CASE_TOKEN_TYPE(DOUBLE_PLUS);
        CASE_TOKEN_TYPE(MINUS);
        CASE_TOKEN_TYPE(DOUBLE_MINUS);
        CASE_TOKEN_TYPE(TRIPLE_MINUS);
        CASE_TOKEN_TYPE(COMMA);
		CASE_TOKEN_TYPE(BANG);
		CASE_TOKEN_TYPE(STRING);
		CASE_TOKEN_TYPE(CHAR);
	}
	return "UNKNOWN";
}
