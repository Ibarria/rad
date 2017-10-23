#pragma once

#include "Token.h"
#include "FileData.h"
#include <vector>

class Lexer
{
    static const int MAX_NESTED_COMMENT = 16;
	FileData file;
    SrcLocation nested_comment_stack[MAX_NESTED_COMMENT];
    u8 num_nested;
    std::vector<Token> tokens;
    unsigned int token_index;
	void consumeWhiteSpace();
	void Error(const char *msg);
    void getNextTokenInternal(Token &tok);
    bool parseStringToken(char *input, Token &tok);
    void parseNumber(Token &tok, char c);
public:
	Lexer();
	~Lexer();
	bool openFile(const char *filename);
    void parseFile();
	void getNextToken(Token &tok);
    void lookaheadToken(Token &tok);
    void lookNaheadToken(Token &tok, unsigned int ahead);
    void consumeToken();
	void getLocation(SrcLocation &loc) const;
    unsigned int getTokenStreamPosition() const;
    void setTokenStreamPosition(unsigned int index);
	const char * getFilename() const;
};