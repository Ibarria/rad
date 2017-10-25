#pragma once

#include "mytypes.h"
#include "SrcLocation.h"
#include "TextType.h"

#include "TokenType.h"

class Token
{
public: 
	TOKEN_TYPE type;
	SrcLocation loc;
	union payload {
		u32 pu32;
		u64 pu64;
		s32 ps32;
		s64 ps64;
		f32 pf32;
		f64 pf64;
	} pl;
    TextType string;

    Token(TOKEN_TYPE t) : type(t) {}
    Token() : type(TK_INVALID) {}

	void clear();
	void print();
};

const char * TokenTypeToStr(TOKEN_TYPE);
