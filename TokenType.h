#pragma once

enum TOKEN_TYPE {
    TK_INVALID,
    TK_LAST_TOKEN,
    TK_NUMBER,
    TK_FNUMBER,
    TK_IDENTIFIER,
    TK_EQ,
    TK_LEQ,
    TK_GEQ,
    TK_NEQ,
    TK_LT,
    TK_GT,
    TK_RSHIFT,
    TK_LSHIFT,
    TK_ASSIGN,
    TK_IMPLICIT_ASSIGN,
    TK_MUL_ASSIGN,
    TK_DIV_ASSIGN,
    TK_MOD_ASSIGN,
    TK_ADD_ASSIGN,
    TK_SUB_ASSIGN,
    TK_LEFT_ASSIGN,
    TK_RIGHT_ASSIGN,
    TK_AND_ASSIGN,
    TK_XOR_ASSIGN,
    TK_OR_ASSIGN,
    TK_OPEN_PAREN,
    TK_CLOSE_PAREN,
    TK_OPEN_BRACKET,
    TK_CLOSE_BRACKET,
    TK_OPEN_SQBRACKET,
    TK_CLOSE_SQBRACKET,
    TK_OPEN_CURLYBRACKET,
    TK_CLOSE_CURLYBRACKET,
    TK_RETURN_ARROW,
    TK_SEMICOLON,
    TK_COLON,
    TK_DOUBLE_COLON,
    TK_PERIOD,
    TK_DOUBLE_PERIOD,
    TK_HASH,
    TK_STAR,
    TK_DIV,
    TK_MOD,
    TK_HAT,
    TK_PIPE,
    TK_DOUBLE_PIPE,
    TK_AMP,
    TK_DOUBLE_AMP,
    TK_PLUS,
    TK_DOUBLE_PLUS,
    TK_MINUS,
    TK_DOUBLE_MINUS,
    TK_TRIPLE_MINUS,
    TK_COMMA,
    TK_BANG,
    TK_STRING,
    TK_CHAR,
    TK_IF,
    TK_FOR,
    TK_RETURN,
    TK_BOOL,
    TK_INT,
    TK_U8,
    TK_U16,
    TK_U32,
    TK_U64,
    TK_S8,
    TK_S16,
    TK_S32,
    TK_S64,
    TK_FLOAT,
    TK_F32,
    TK_F64,
    TK_OPEN_BLOCK_COMMENT,
    TK_CLOSE_BLOCK_COMMENT,
    TK_LINE_COMMENT
};

const char * TokenTypeToStr(TOKEN_TYPE type);
const char * TokenTypeToCOP(TOKEN_TYPE type);