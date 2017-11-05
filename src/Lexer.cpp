#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <assert.h>
#include <math.h>
#include <stdarg.h>
#include "Lexer.h"

inline bool isWhiteSpace(char c)
{
	return ((c == ' ') ||
		(c == '\t') ||
		(c == '\n') ||
		(c == '\r'));
}

struct CompilerDirective {
    const char *directive;
    TOKEN_TYPE type;
} compiler_directives[] = {
    { "import",  TK_IMPORT},
    { "load",    TK_LOAD },
    { "run",     TK_RUN },
    { "foreign", TK_IMPORT },

    { nullptr,    TK_INVALID }
};

static void ConvertIdentifierToCompilerDirective(Token &tok, char *buff)
{
    CompilerDirective *k;
    for (k = compiler_directives; k->directive != nullptr; k++) {
        if (!strcmp(k->directive, buff)) {
            tok.type = k->type;
            return;
        }
    }
}


struct ReservedKeyword {
    const char *keyword;
    TOKEN_TYPE type;
} reserved_keywords[] = {
    { "return",   TK_RETURN},
    { "if",       TK_IF },
    { "for",      TK_FOR },
    { "bool",     TK_BOOL },
    { "true",     TK_TRUE },
    { "false",    TK_FALSE },
    { "string",   TK_STRING_KEYWORD },
    { "int",      TK_INT },
    { "u8",       TK_U8 },
    { "u16",      TK_U16 },
    { "u32",      TK_U32 },
    { "u64",      TK_U64 },
    { "s8",       TK_S8 },
    { "s16",      TK_S16 },
    { "s32",      TK_S32 },
    { "s64",      TK_S64 },
    { "float",    TK_FLOAT },
    { "f32",      TK_F32 },
    { "f64",      TK_F64 },

    { nullptr,    TK_INVALID}
};

static void ConvertIdentifierToReservedKeyword(Token &tok, char *buff)
{
    ReservedKeyword *k; 
    for (k = reserved_keywords; k->keyword != nullptr; k++) {
        if (!strcmp(k->keyword, buff)) {
            tok.type = k->type;
            return;
        }
    }
}

struct StringToken {
    const char *str;
    TOKEN_TYPE token;
} strTok[] = {
    { "<<=", TK_LEFT_ASSIGN},
    { "<<",  TK_LSHIFT },
    { "<=",  TK_LEQ },
    { "<",   TK_LT },

    { "==",  TK_EQ },
    { "=",   TK_ASSIGN},

    { ">>=", TK_RIGHT_ASSIGN },
    { ">>",  TK_RSHIFT },
    { ">=",  TK_GEQ },
    { ">",   TK_GT },

    { "::",  TK_DOUBLE_COLON },
    { ":=",  TK_IMPLICIT_ASSIGN },
    { ":",   TK_COLON },

    { "*=",  TK_MUL_ASSIGN },
    { "*/",  TK_CLOSE_BLOCK_COMMENT },
    { "*",   TK_STAR},

    { "/=",  TK_DIV_ASSIGN },
    { "//",  TK_LINE_COMMENT },
    { "/*",  TK_OPEN_BLOCK_COMMENT },
    { "/",   TK_DIV },

    { "+=",  TK_ADD_ASSIGN },
    { "++",  TK_DOUBLE_PLUS },
    { "+",   TK_PLUS },

    { "---", TK_TRIPLE_MINUS },
    { "-=",  TK_SUB_ASSIGN },
    { "->",  TK_RETURN_ARROW },
    { "--",  TK_DOUBLE_MINUS },
    { "-",   TK_MINUS },

    { "&=",  TK_AND_ASSIGN },
    { "&&",  TK_DOUBLE_AMP },
    { "&",   TK_AMP },

    { "^=",  TK_XOR_ASSIGN },
    { "^",   TK_HAT },

    { "|=",  TK_OR_ASSIGN },
    { "||",  TK_DOUBLE_PIPE },
    { "|",   TK_PIPE },

    { "..",  TK_DOUBLE_PERIOD },
    { ".",   TK_PERIOD},

    { "!=",  TK_NEQ },
    { "!",   TK_BANG },

    { ";",   TK_SEMICOLON },
    { "(",   TK_OPEN_PAREN },
    { ")",   TK_CLOSE_PAREN },
    { "[",   TK_OPEN_SQBRACKET },
    { "]",   TK_CLOSE_SQBRACKET },
    { "{",   TK_OPEN_BRACKET },
    { "}",   TK_CLOSE_BRACKET },
    { "#",   TK_HASH },
    { "%",   TK_MOD },
    { ",",   TK_COMMA },

    { nullptr, TK_INVALID}
};

const char * TokenTypeToCOP(TOKEN_TYPE type)
{
    for (auto &st : strTok) {
        if (st.token == type) {
            return st.str;
        }
    }
    assert("We should never be here, using this function wrong");
    return nullptr;
}

bool Lexer::parseStringToken(char *input, Token &tok)
{
    // find the first existence of the first character of input in our array
    struct StringToken *st;
    st = strTok;
    while (st->str != nullptr) {
        if (input[0] == st->str[0]) {
            break;
        }
        st++;
    }
    if (st->str == nullptr) return false;

    // we assume that input is for sure a string of less than 4 characters
    assert(strlen(input) < 4);

    while ((st->str != nullptr) && (st->str[0] == input[0])) {
        if (!strncmp(st->str, input, strlen(st->str))) {
            // we found a match, our table assigns the first match lenghtwise
            tok.type = st->token;
            char c;
            int i, s;
            s = (int)strlen(st->str); 
            // the for loop starts with 1 because we usually have consumed 1 char
            for (i = 1; i < s; i++) file.getc(c);
            return true;
        }
        st++;
    }
    return false;
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

inline bool isHex(char c)
{
    return isNumber(c) || ((c >= 'A') && (c <= 'F'))
        || ((c >= 'a') && (c <= 'f'));
}

void Lexer::parseNumber(Token & tok, char c)
{
    // this is a number token
    u64 total = c - '0';
    double dtotal = 0;
    int base = 10;
    bool decimal = false;
    int dec_index = 0;

    file.peek(c);
    if (c == 'x') {
        if (total != 0) {
            Error("Hex numbers must start with prefix 0x, the compiler saw %d%c\n", (int)total, c);
        }
        base = 16;
    }

    while (file.peek(c)) {
        if (c == '.') {
            // handle the conversion to float right now
            if (base == 16) Error("Hex numbers cannot have a period in them\n");
            file.getc(c);
            decimal = true;
            dtotal = (double)total;
            continue;
        }
        if ((base == 10) && !isNumber(c) && isAlpha(c)) {
            Error("Numbers need a space at the end\n");
        }
        if ((base == 16) && !isHex(c) && isAlpha(c)) {
            Error("Numbers need a space at the end\n");
        }
        if (!isAlpha(c) && !isNumber(c)) {
            break;
        }
        if (!decimal) {
            total = total * base;
            if (isNumber(c)) {
                total += c - '0';
            } else {
                c &= 0xDF; // this makes this character lowercase
                total += c - 'A';
            }
        } else {
            dec_index++;
            float num = float(c - '0');
            dtotal = dtotal + num / pow(10, dec_index);
        }
        file.getc(c);
    }
    if (decimal) {
        tok._f64 = dtotal;
        tok.type = TK_FNUMBER;
    } else {
        tok._u64 = total;
        tok.type = TK_NUMBER;
    }
}

void Lexer::consumeWhiteSpace()
{
	char c = 0;
	while (file.peek(c)) {
		if (!isWhiteSpace(c)) {			
			return;
		} else {
			file.getc(c);
		}
	}
}

void Lexer::Error(const char * msg, ...)
{
    va_list args;
	SrcLocation loc;
	file.getLocation(loc);
	printf("%s(%d): error: ", file.getFilename(), loc.line);

    va_start(args, msg);
    vprintf(msg, args);
    va_end(args);
	exit(1);
}

Lexer::Lexer()
{
    num_nested = 0;
    pool = nullptr;
}

Lexer::~Lexer()
{
}

bool Lexer::openFile(const char * filename)
{
	return file.open(filename);
}

void Lexer::parseFile()
{
    Token tok;
    filename = CreateTextType(pool, file.getFilename());

    while (tok.type != TK_LAST_TOKEN) {
        getNextTokenInternal(tok);
        tokens.push_back(tok);
    }
    file.close();
    token_index = 0;
    // this is a small optimization to not check for
    // indices on tokens
    tokens.push_back(tok);
    tokens.push_back(tok);
    tokens.push_back(tok);
}

void Lexer::getNextToken(Token &tok)
{
    if (token_index == tokens.size()) {
        tok.clear();
        tok.type = TK_LAST_TOKEN;
        return;
    }
    tok = tokens[token_index++];
}

void Lexer::getNextTokenInternal(Token &tok)
{
	char c = 0;

	tok.clear();

	while(1) {
		consumeWhiteSpace();
		if (!file.getc(c)) {
			tok.type = TK_LAST_TOKEN;
			return;
		}

        // Get the location in the token, the one for the first character of the token
		file.getLocation(tok.loc);

		if (isNumber(c)) {
            parseNumber(tok, c);
		} else if (isAlpha(c) || (c == '_')) {
			// this can indicate that an identifier starts
			char buff[256] = {};
			unsigned int i = 0;
			buff[i++] = c;
			while (file.peek(c) && (isAlpha(c) || isNumber(c) || (c == '_')) && (i<255)) {
				buff[i++] = c;
				file.getc(c);
			}
			tok.type = TK_IDENTIFIER;
            buff[i] = 0;
            ConvertIdentifierToReservedKeyword(tok, buff);
            if (tok.type == TK_IDENTIFIER) tok.string = CreateTextType(pool, buff);
        } else if (c == '"') {
			// this marks the start of a string
            char *s = new char[1024];
            u32 i = 0;
			while (file.getc(c) && (c != '"') && (!isNewLine(c))) {
				s[i++] = c;
				if (i >= 1024 - 1) {
					Error("Found string too long to parse\n");
					exit(1);
				}
			}
			if (isNewLine(c)) {
				Error("Newlines are not allowed inside a quoted string\n");
				exit(1);
			}
            s[i++] = 0;
			tok.type = TK_STRING;
            tok.string = CreateTextType(pool, s);
            delete [] s;
            return;
        } else if (c == '\'') {
            // this marks a character
            if (!file.getc(c)) {
                Error("Character was not defined before end of file\n");
            }
            tok.type = TK_CHAR;
            tok._u64 = c;
            if (!file.getc(c)) {
                Error("Character was not defined before end of file\n");
            }
            if (c != '\'') {
                Error("Character definitions can only be a single character\n");
            }
            return;
        } else if (c == '#') {
            char buff[256] = {};
            unsigned int i = 0;
            while (file.peek(c) && (isAlpha(c) || (c == '_')) && (i<255)) {
                buff[i++] = c;
                file.getc(c);
            }
            tok.type = TK_INVALID;
            buff[i] = 0;
            ConvertIdentifierToCompilerDirective(tok, buff);
            if (tok.type == TK_INVALID) {
                Error("Found invalid compiler directive: #%s\n", buff);
            }
            return;
        } else {
            // we are going to do a bit of lookahead in the string
            char input[4] = {};
            input[0] = c;
            file.lookAheadTwo(&input[1]);
            if (!parseStringToken(input, tok)) {
                // at this point, all other tokens must be in this form
                Error("Token not recognized : [%s]\n", input); 
            }

            if (tok.type == TK_LINE_COMMENT) {
                // we are in a comment situation, advance the pointer
                file.getc(c);
                // this will consume all characters until the newline is found, or end of file
                while (file.getc(c) && !isNewLine(c));
                tok.clear();
                continue;
            } else if (tok.type == TK_OPEN_BLOCK_COMMENT) {
                // this is a multi line comment, move until we find a star (and then a slash)
                bool cont = file.getc(c);
                file.getLocation(nested_comment_stack[num_nested]);
                num_nested++;
                while (cont) {
                    while ((cont = file.getc(c)) && (c != '*') && (c != '/'));
                    if (!cont) {
                        tok.type = TK_LAST_TOKEN;
                        return;
                    }
                    if (c == '/') {
                        // possible nested comment
                        cont = file.getc(c);
                        if (cont && (c == '*')) {
                            if (num_nested + 1 >= MAX_NESTED_COMMENT) {
                                Error("You have reached the maximum number of nested comments: %d\n", MAX_NESTED_COMMENT);
                            }
                            file.getLocation(nested_comment_stack[num_nested]);
                            num_nested++;
                        }
                    } else {
                        cont = file.getc(c);
                        if (cont && (c == '/')) {
                            num_nested--;
                            if (num_nested == 0) break;
                        }
                    }
                    // just fall through and parse a new token
                }
                // just continue and parse a new token
                tok.clear();
                continue;
            }
        }
                        
		if (tok.type != TK_INVALID) {
			return;
			// if we have a valid token return it, otherwise continue. This handles comments, etc
		}
	}
}

void Lexer::lookaheadToken(Token & tok)
{
    if (token_index == tokens.size()) {
        tok.clear();
        tok.type = TK_LAST_TOKEN;
        return;
    }
    tok = tokens[token_index];
}

void Lexer::lookNaheadToken(Token & tok, unsigned int ahead)
{
    unsigned int index = token_index + ahead;
    tok = tokens[index];
}

void Lexer::consumeToken()
{
    token_index++;
}

void Lexer::getLocation(SrcLocation & loc) const
{
    loc = tokens[token_index].loc;
}

unsigned int Lexer::getTokenStreamPosition() const
{
    return token_index;
}

void Lexer::setTokenStreamPosition(unsigned int index)
{
    token_index = index;
}
