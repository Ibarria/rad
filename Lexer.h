#pragma once

#include "Token.h"
#include "FileData.h"

class Lexer
{
	FileData file;

	void consumeWhiteSpace();


public:
	Lexer();
	~Lexer();
	bool openFile(const char *filename);
	void getNextToken(Token &tok);
};