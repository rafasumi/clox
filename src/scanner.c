/*! \file scanner.c
    \brief Definitions of functions from scanner.h
*/

#include <string.h>

#include "common.h"
#include "scanner.h"

/**
 * \struct Scanner
 * \brief Strucure that represents clox's scanner and its necessary attributes.
 */
typedef struct {
  const char* start; /**< Pointer to the start of the next lexeme in the code */
  const char* current; /**< Pointer to the current character being scanned */
  uint32_t line;       /**< Current line in the source code being scanned */
} Scanner;

Scanner scanner;

void initScanner(const char* source) {
  scanner.start = source;
  scanner.current = source;
  scanner.line = 1;
}

/**
 * \brief Determines if the scanner is at the end of the source code.
 *
 * \return Boolean value to indicate if the scanner is at the end
 */
static bool isAtEnd() {
  return *scanner.current == '\0';
}

/**
 * \brief Creates a Token with a given type based on the current state of the
 * scanner.
 *
 * \param type The type of the Token that will be created
 *
 * \return The created Token
 */
static Token makeToken(const TokenType type) {
  Token token;
  token.type = type;
  token.start = scanner.start;
  token.length = (size_t)(scanner.current - scanner.start);
  token.line = scanner.line;

  return token;
}

/**
 * \brief Creates a Token of the TOKEN_ERROR type with a given error message.
 * The error message is stored as the token's lexeme.
 *
 * \param message Error message to be stored in the token.
 *
 * \return The created Token
 */
static Token errorToken(const char* message) {
  Token token;
  token.type = TOKEN_ERROR;
  token.start = message;
  token.length = strlen(message);
  token.line = scanner.line;

  return token;
}

/**
 * \brief Consumes the next character in the source code and updates the
 * "current" pointer.
 *
 * \return The next character in the source code
 */
static char advance() {
  scanner.current++;

  return scanner.current[-1];
}

/**
 * \brief Conditionally consumes the next character in the source code if it is
 * equal to a given expected character.
 *
 * \param expected The expected character
 *
 * \return Boolean value that indicates if the character was consumed
 */
static bool match(const char expected) {
  if (isAtEnd())
    return false;

  if (*scanner.current != expected)
    return false;

  scanner.current++;

  return true;
}

/**
 * \brief Returns the next character in the source code without consuming it.
 *
 * \return The next character in the source code
 */
static char peek() {
  return *scanner.current;
}

/**
 * \brief Returns the character that is two positions ahead without consuming
 * them.
 *
 * \return The character two positions ahead
 */
static char peekNext() {
  if (isAtEnd())
    return '\0';

  return scanner.current[1];
}

/**
 * \brief Skips whitespaces and line breaks in the source code. Also updates the
 * current line in the Scanner.
 */
static void skipWhitespace() {
  while (true) {
    char c = peek();
    switch (c) {
    case ' ':
    case '\r':
    case '\t':
      advance();
      break;
    case '\n':
      scanner.line++;
      advance();
      break;
    case '/': {
      if (peekNext() == '/') {
        while (peek() != '\n' && !isAtEnd())
          advance();
      } else {
        return;
      }

      break;
    }
    default:
      return;
    }
  }
}

/**
 * \brief Creates a TOKEN_STRING Token if there is a correctly terminated string
 * in the source code. Otherwise, returns a TOKEN_ERROR Token.
 *
 * \return The created Token
 */
static Token string() {
  while (peek() != '"' && !isAtEnd()) {
    if (peek() == '\n')
      scanner.line++;

    advance();
  }

  if (isAtEnd())
    return errorToken("Unterminated string.");

  // Consumes the closing quote
  advance();

  return makeToken(TOKEN_STRING);
}

/**
 * \brief Determines if a given character is a digit.
 *
 * \param c Character to be considered
 *
 * \return Boolean value
 */
static bool isDigit(const char c) {
  return c >= '0' && c <= '9';
}

/**
 * \brief Determines if a given character is a either alphabetic or underscore.
 * In other words, determines if the character is appropriate for use in
 * identifiers.
 *
 * \param c Character to be considered
 *
 * \return Boolean value
 */
static bool isAlpha(const char c) {
  return (c >= 'a' && c <= 'z') || (c >= 'A' && c <= 'Z') || c == '_';
}

/**
 * \brief Creates a TOKEN_NUMBER Token by consuming digits in the source code.
 *
 * \return The created Token
 */
static Token number() {
  while (isDigit(peek()))
    advance();

  // Look for fractional part
  if (peek() == '.' && isDigit(peekNext())) {
    // Consumes the '.'
    advance();

    while (isDigit(peek()))
      advance();
  }

  return makeToken(TOKEN_NUMBER);
}

/**
 * \brief Checks if a given substring from the source code is equal to a
 * specified Lox keyword. If so, returns the appropriate TokenType. Otherwise
 * returns the TOKEN_IDENTIFIER type.
 *
 * \param start Index of the start of the substring
 * \param length Length of the substring
 * \param rest String that contains the part of the keyword that hasn't been
 * checked in the substring
 * \param type The appropriate TokenType for the keyword
 *
 * \return The TokenType for the substring
 */
static TokenType checkKeyword(const size_t start, const size_t length,
                              const char* rest, TokenType type) {
  if ((size_t)(scanner.current - scanner.start) == start + length &&
      memcmp(scanner.start + start, rest, length) == 0) {
    return type;
  }

  return TOKEN_IDENTIFIER;
}

/**
 * \brief Returns the appropriate type for an identifier Token in the source
 * code. This function is able to determine if the identifier is a Lox keyword.
 *
 * \return The TokenType for the identifier
 */
static TokenType identifierType() {
  switch (scanner.start[0]) {
  case 'a':
    return checkKeyword(1, 2, "nd", TOKEN_AND);
  case 'c':
    if (scanner.current - scanner.start > 1) {
      switch (scanner.start[1]) {
      case 'l':
        return checkKeyword(2, 3, "ass", TOKEN_CLASS);
      case 'o':
        return checkKeyword(2, 3, "nst", TOKEN_CONST);
      }
    }
    
    break;
  case 'e':
    return checkKeyword(1, 3, "lse", TOKEN_ELSE);
  case 'f':
    if (scanner.current - scanner.start > 1) {
      switch (scanner.start[1]) {
      case 'a':
        return checkKeyword(2, 3, "lse", TOKEN_FALSE);
      case 'o':
        return checkKeyword(2, 1, "r", TOKEN_FOR);
      case 'u':
        return checkKeyword(2, 1, "n", TOKEN_FUN);
      }
    }

    break;
  case 'i':
    return checkKeyword(1, 1, "f", TOKEN_IF);
  case 'n':
    return checkKeyword(1, 2, "il", TOKEN_NIL);
  case 'o':
    return checkKeyword(1, 1, "r", TOKEN_OR);
  case 'p':
    return checkKeyword(1, 4, "rint", TOKEN_PRINT);
  case 'r':
    return checkKeyword(1, 5, "eturn", TOKEN_RETURN);
  case 's':
    return checkKeyword(1, 4, "uper", TOKEN_SUPER);
  case 't':
    if (scanner.current - scanner.start > 1) {
      switch (scanner.start[1]) {
      case 'h':
        return checkKeyword(2, 2, "is", TOKEN_THIS);
      case 'r':
        return checkKeyword(2, 2, "ue", TOKEN_TRUE);
      }
    }

    break;
  case 'v':
    return checkKeyword(1, 2, "ar", TOKEN_VAR);
  case 'w':
    return checkKeyword(1, 4, "hile", TOKEN_WHILE);
  }

  return TOKEN_IDENTIFIER;
}

/**
 * \brief Creates an identifier Token with the appropriate TokenType.
 *
 * \return The created Token.
 */
static Token identifier() {
  while (isAlpha(peek()) || isDigit(peek()))
    advance();

  return makeToken(identifierType());
}

Token scanToken() {
  skipWhitespace();

  scanner.start = scanner.current;

  if (isAtEnd())
    return makeToken(TOKEN_EOF);

  char c = advance();

  if (isAlpha(c))
    return identifier();

  if (isDigit(c))
    return number();

  switch (c) {
  case '(':
    return makeToken(TOKEN_LEFT_PAREN);
  case ')':
    return makeToken(TOKEN_RIGHT_PAREN);
  case '{':
    return makeToken(TOKEN_LEFT_BRACE);
  case '}':
    return makeToken(TOKEN_RIGHT_BRACE);
  case ';':
    return makeToken(TOKEN_SEMICOLON);
  case ',':
    return makeToken(TOKEN_COMMA);
  case '.':
    return makeToken(TOKEN_DOT);
  case ':':
    return makeToken(TOKEN_COLON);
  case '?':
    return makeToken(TOKEN_QUESTION);
  case '-':
    return makeToken(TOKEN_MINUS);
  case '+':
    return makeToken(TOKEN_PLUS);
  case '/':
    return makeToken(TOKEN_SLASH);
  case '*':
    return makeToken(TOKEN_STAR);
  case '!':
    return makeToken(match('=') ? TOKEN_BANG_EQUAL : TOKEN_BANG);
  case '=':
    return makeToken(match('=') ? TOKEN_EQUAL_EQUAL : TOKEN_EQUAL);
  case '<':
    return makeToken(match('=') ? TOKEN_LESS_EQUAL : TOKEN_LESS);
  case '>':
    return makeToken(match('=') ? TOKEN_GREATER_EQUAL : TOKEN_GREATER);
  case '"':
    return string();
  }

  return errorToken("Unexpected character.");
}
