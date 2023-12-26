/*! \file scanner.h
    \brief Functions and datatypes used by clox's scanner.
*/

#ifndef CLOX_SCANNER_H
#define CLOX_SCANNER_H

/**
 * \enum TokenType
 * \brief Enum type for all types of Token in the Lox language.
 */
typedef enum {
  // Single-character tokens.
  TOKEN_LEFT_PAREN,
  TOKEN_RIGHT_PAREN,
  TOKEN_LEFT_BRACE,
  TOKEN_RIGHT_BRACE,
  TOKEN_COMMA,
  TOKEN_DOT,
  TOKEN_MINUS,
  TOKEN_PLUS,
  TOKEN_SEMICOLON,
  TOKEN_SLASH,
  TOKEN_STAR,
  // One or two character tokens.
  TOKEN_BANG,
  TOKEN_BANG_EQUAL,
  TOKEN_EQUAL,
  TOKEN_EQUAL_EQUAL,
  TOKEN_GREATER,
  TOKEN_GREATER_EQUAL,
  TOKEN_LESS,
  TOKEN_LESS_EQUAL,
  // Literals.
  TOKEN_IDENTIFIER,
  TOKEN_STRING,
  TOKEN_NUMBER,
  // Keywords.
  TOKEN_AND,
  TOKEN_CLASS,
  TOKEN_ELSE,
  TOKEN_FALSE,
  TOKEN_FOR,
  TOKEN_FUN,
  TOKEN_IF,
  TOKEN_NIL,
  TOKEN_OR,
  TOKEN_PRINT,
  TOKEN_RETURN,
  TOKEN_SUPER,
  TOKEN_THIS,
  TOKEN_TRUE,
  TOKEN_VAR,
  TOKEN_WHILE,

  TOKEN_ERROR,
  TOKEN_EOF
} TokenType;

/**
 * \struct Token
 * \brief Strucure used to represent a token in the source code.
 */
typedef struct {
  TokenType type;    /**< The type of the token */
  const char* start; /**< Pointer to the start of the token in the code */
  size_t length;     /**< Length of the token */
  uint32_t line;     /**< Line where the token appears in the source code */
} Token;

/**
 * \brief Initializes clox's scanner.
 *
 * \param source Pointer to the source code's string in memory.
 */
void initScanner(const char* source);

/**
 * \brief Scans the next available token in the source code.
 *
 * \return The scanned token.
 */
Token scanToken();

#endif
