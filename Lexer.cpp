#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#undef EOF

#include "Lexer.h"

inline bool isWhiteSpace(char c)
{
	return ((c == ' ') ||
		(c == '\t') ||
		(c == '\n') ||
		(c == '\r'));
}

inline bool isNewLine(char c)
{
	return ((c == '\n'));
}

inline bool isNumber(char c)
{
	return ((c >= '0') && (c <= '9'));
}

inline bool isAlpha(char c)
{
	return ((c >= 'a') && (c <= 'z')) ||
		((c >= 'A') && (c <= 'Z'));
}

void Lexer::consumeWhiteSpace()
{
	char c = 0;
	while (file.getc(c)) {
		if (!isWhiteSpace(c)) {
			file.rewind_one();
			return;
		}
	}
}

Lexer::Lexer()
{
}

Lexer::~Lexer()
{
}

bool Lexer::openFile(const char * filename)
{
	return file.open(filename);
}

void Lexer::getNextToken(Token &tok)
{
	char c = 0;
	tok.clear();

	while(1) {
		consumeWhiteSpace();
		if (!file.getc(c)) {
			tok.type = EOF;
			return;
		}

		if (isNumber(c)) {
			// this is a number token
			u64 total = c - '0';

			while (file.peek(c) && isNumber(c)) {
				total = total * 10 + (c - '0');
				file.getc(c);
			}
			tok.type = NUMBER;
			tok.pl.pu64 = total;
		} else if (isAlpha(c) || (c == '_')) {
			// this can indicate that an identifier starts
			char buff[256] = {};
			unsigned int i = 0;
			buff[i++] = c;
			while (file.peek(c) && (isAlpha(c) || isNumber(c) || (c == '_')) && (i<255)) {
				buff[i++] = c;
				file.getc(c);
			}
			tok.type = IDENTIFIER;
			tok.pl.pstr = new char[i + 1];
			memcpy(tok.pl.pstr, buff, i + 1);
		} else if (c == '"') {
			// this marks the start of a string
			char *s = new char[1024];
			u32 i = 0;
			while (file.getc(c) && (c != '"') && (!isNewLine(c))) {
				s[i++] = c;
				if (i >= 1024 - 1) {
					printf("Error: Found string too long to parse\n");
					exit(1);
				}
			}
			if (isNewLine(c)) {
				printf("Error: Newlines are not allowed inside a quoted string\n");
				exit(1);
			}
			tok.type = STRING;
			tok.pl.pstr = s;
			return;
		} else if (c == '\'') {
			// this marks a character

		} else if (c == '/') {
			// this can mean a line comment or multi line comment (or just a division)
			if (!file.peek(c)) {
				tok.type = DIV;
				return;
			}
			if (c != '/') {
				// this is not a comment, just return the DIV operator
				tok.type = DIV;
				return;
			}
			// we are in a comment situation, advance the pointer
			file.getc(c);
			// this will consume all characters until the newline is found, or end of file
			while (file.getc(c) && !isNewLine(c));
		} else if (c == '=') {
			// this can be the assign or equals operator
			if (!file.peek(c)) {
				tok.type = ASSIGN;
				return;
			}
			if (c != '=') {
				// this is not a comment, just return the DIV operator
				tok.type = ASSIGN;
				return;
			} else {
				// We have to consume the second = since it is part of EQ
				file.getc(c);
				tok.type = EQ;
				return;
			}
		} else if (c == '<') {
			// this can be the LEQ or LT command or LSHIFT
			if (!file.peek(c)) {
				tok.type = LT;
				return;
			}
			if (c == '<') {
				// this is a LSHIFT
				file.getc(c);
				tok.type = LSHIFT;
				return;
			} else if (c == '=') {				
				file.getc(c);
				tok.type = LEQ;
				return;
			} else {
				tok.type = LT;
				return; 
			}
		} else if (c == '>') {
			// this can be the GEQ or GT command or RSHIFT
			if (!file.peek(c)) {
				tok.type = GT;
				return;
			}
			if (c == '>') {
				// this is a RSHIFT
				file.getc(c);
				tok.type = RSHIFT;
				return;
			} else if (c == '=') {
				file.getc(c);
				tok.type = GEQ;
				return;
			} else {
				tok.type = GT;
				return;
			}
		} else if (c == '!') {
			// this can be NEQ or just the bang unary operator

		} else {
			// here we handle the case of each individual character in a simple switch
			switch (c) {
			case '(':
				tok.type = OPEN_PAREN;
				break;
			case ')':
				tok.type = CLOSE_PAREN;
				break;
			case '{':
				tok.type = OPEN_BRACKET;
				break;
			case '}':
				tok.type = CLOSE_BRACKET;
				break;
			case '[':
				tok.type = OPEN_SQBRACKET;
				break;
			case ']':
				tok.type = CLOSE_SQBRACKET;
				break;
			case ';':
				tok.type = SEMICOLON;
				break;
			case ':':
				tok.type = COLON;
				break;
			case '.':
				tok.type = PERIOD;
				break;
			case '#':
				tok.type = HASH;
				break;
			case '*':
				tok.type = STAR;
				break;
			case '%':
				tok.type = MOD;
				break;
			case '&':
				tok.type = AMP;
				break;
			case '+':
				tok.type = PLUS;
				break;
			case '-':
				tok.type = MINUS;
				break;
			case ',':
				tok.type = COMMA;
				break;
			default:
				printf("We should never get here. Character: %c\n", c);
				exit(1);
			}
		}

		if (tok.type != INVALID) {
			return;
			// if we have a valid token return it, otherwise continue. This handles comments, etc
		}
	}
}
