#pragma once

#include "mytypes.h"
#include "SrcLocation.h"
#include "TextType.h"

#include "TokenType.h"

class Token
{
public: 
	TOKEN_TYPE type = TK_INVALID;
	SrcLocation loc;
    u64 _u64 = 0;
    f64 _f64 = 0.0;
    TextType string = nullptr;
    bool _bool = false;

    Token(TOKEN_TYPE t) : type(t) {}
    Token() : type(TK_INVALID) {}

	void clear();
	void print();
};

const char * TokenTypeToStr(TOKEN_TYPE);
