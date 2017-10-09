#pragma once

#include "Token.h"
#include "FileData.h"

class Lexer
{
	FileData file;

	void consumeWhiteSpace();
	void Error(const char *msg);

public:
	Lexer();
	~Lexer();
	bool openFile(const char *filename);
	void getNextToken(Token &tok);
};