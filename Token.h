#pragma once

#include "mytypes.h"

enum TOKEN_TYPE {
	EOF,
	NUMBER,
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
	OPEN_PAREN,
	CLOSE_PAREN,
	OPEN_BRACKET,
	CLOSE_BRACKET,
	OPEN_SQBRACKET,
	CLOSE_SQBRACKET,
	SEMICOLON,
	COLON,
	PERIOD,
	HASH,
	STAR,
	DIV,
	MOD,
	AMP,
	PLUS,
	MINUS,
	STRING,
	CHAR
};


class Token
{
public: 
	TOKEN_TYPE type;
	Token(TOKEN_TYPE t) : type(t) {
		pl.pstr = nullptr;
	};
	Token() : type(EOF) {
		pl.pstr = nullptr;
	}
	~Token();
	union payload {
		u32 pu32;
		u64 pu64;
		s32 ps32;
		s64 ps64;
		f32 pf32;
		f64 pf64;
		char * pstr;
	} pl;
};

