#pragma once

#include "Token.h"
#include "FileData.h"
#include "PoolAllocator.h"
#include "Array.h"

class Lexer
{
    static const int MAX_NESTED_COMMENT = 16;
	FileData *file = nullptr;
    SrcLocation nested_comment_stack[MAX_NESTED_COMMENT];
    u8 num_nested;
    Array<Token> tokens;
    unsigned int token_index;
	void consumeWhiteSpace();
	void Error(const char *msg, ...);
    void getNextTokenInternal(Token &tok);
    bool parseStringToken(char *input, Token &tok);
    void parseNumber(Token &tok, char c);
    PoolAllocator *pool;
    TextType filename;
public:
	Lexer();
	~Lexer();
    void setPoolAllocator(PoolAllocator *p) { pool = p; }
	bool openFile(const char *filename);
    bool loadString(const char *str, u64 size);
    void parseFile();
    void getCurrentToken(Token &tok);
    void getNextToken(Token &tok);
    void lookaheadToken(Token &tok);
    void lookNaheadToken(Token &tok, unsigned int ahead);
    void consumeToken();
    bool checkToken(TOKEN_TYPE t) const { return tokens[token_index].type == t; }
    bool checkAheadToken(TOKEN_TYPE t, u32 ahead) const { return t == tokens[token_index + ahead].type; }
    TOKEN_TYPE getTokenType() const { return tokens[token_index].type; }
	void getLocation(SrcLocation &loc) const;
    unsigned int getTokenStreamPosition() const;
    void setTokenStreamPosition(unsigned int index);
    TextType getFilename() { return filename; }
    FileData *getFileData() { return file; }
};