#pragma once

#include "mytypes.h"
#include "SrcLocation.h"
#include <string>

enum TOKEN_TYPE {
	INVALID,
	LAST_TOKEN,
	NUMBER,
    FNUMBER,
	IDENTIFIER,
	EQ,
	LEQ,
	GEQ,
	NEQ,
	LT,
	GT,
	RSHIFT,
	LSHIFT,
	ASSIGN, 
    MUL_ASSIGN,
    DIV_ASSIGN,
    MOD_ASSIGN,
    ADD_ASSIGN,
    SUB_ASSIGN,
    LEFT_ASSIGN,
    RIGHT_ASSIGN,
    AND_ASSIGN,
    XOR_ASSIGN,
    OR_ASSIGN,
    OPEN_PAREN,
	CLOSE_PAREN,
	OPEN_BRACKET,
	CLOSE_BRACKET,
	OPEN_SQBRACKET,
	CLOSE_SQBRACKET,
    OPEN_CURLYBRACKET,
    CLOSE_CURLYBRACKET,
    SEMICOLON,
	COLON,
    DOUBLE_COLON,
	PERIOD,
    DOUBLE_PERIOD,
    HASH,
	STAR,
	DIV,
	MOD,
    HAT, 
    PIPE,
    DOUBLE_PIPE,
	AMP,
    DOUBLE_AMP,
	PLUS,
    DOUBLE_PLUS,
    MINUS,
    DOUBLE_MINUS,
    TRIPLE_MINUS,
    COMMA,
	BANG,
	STRING,
	CHAR,
    OPEN_BLOCK_COMMENT,
    CLOSE_BLOCK_COMMENT,
    LINE_COMMENT
};


class Token
{
public: 
	TOKEN_TYPE type;
	SrcLocation loc;
	Token(TOKEN_TYPE t) : type(t) {}
    Token() : type(INVALID) {}
	//~Token();
	union payload {
		u32 pu32;
		u64 pu64;
		s32 ps32;
		s64 ps64;
		f32 pf32;
		f64 pf64;
	} pl;
    std::string str;

	void clear();

	//Token(const Token &rhs);
	//Token(Token &&rhs);
	//Token& operator=(Token &&rhs);
	//Token& operator=(Token const &rhs);

	void print();
};

const char * TokenTypeToStr(TOKEN_TYPE);
