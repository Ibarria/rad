#pragma once

#include "FileData.h"
#include "Token.h"

class Lexer
{
	FileData file;

	void consumeWhiteSpace();


public:
	Lexer();
	~Lexer();
	void openFile(FileData &file);
	Token getNextToken();
};