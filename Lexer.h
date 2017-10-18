#pragma once

#include "Token.h"
#include "FileData.h"

class Lexer
{
    static const int MAX_NESTED_COMMENT = 16;
	FileData file;
    SrcLocation nested_comment_stack[MAX_NESTED_COMMENT];
    u8 num_nested;
	void consumeWhiteSpace();
	void Error(const char *msg);

public:
	Lexer();
	~Lexer();
	bool openFile(const char *filename);
	void getNextToken(Token &tok);
	void getLocation(SrcLocation &loc) const;
	const char * getFilename() const;
};