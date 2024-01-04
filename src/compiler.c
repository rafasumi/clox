#include "compiler.h"
#include "common.h"
#include "scanner.h"
#include <stdlib.h>

#ifdef DEBUG
#include "debug.h"
#endif

/**
 * \struct Parser
 * \brief Strucure that contains relevant data and flags to clox's parser
 */
typedef struct {
  Token current;  /**< Token being currently parsed */
  Token previous; /**< Token that was previously parsed */
  bool hadError;  /**< Boolean flag to indicate if there were any parsing errors
                   */
  bool panicMode; /**< Boolean flag to indicate if the compiler is in panic mode
                   */
} Parser;

/**
 * \enum Precedence
 * \brief Enum type for different levels of precedence for tokens in Lox's
 * grammar
 */
typedef enum {
  PREC_NONE,
  PREC_ASSIGNMENT, /**< = */
  PREC_OR,         /**< or */
  PREC_AND,        /**< and */
  PREC_EQUALITY,   /**< == != */
  PREC_COMPARISON, /**< < > <= >= */
  PREC_TERM,       /**< + - */
  PREC_FACTOR,     /**< * / */
  PREC_UNARY,      /**< ! - */
  PREC_CALL,       /**< . () */
  PREC_PRIMARY
} Precedence;

/**
 * \var ParseFn
 * \brief Type definition for a function pointer type to be used by the parser
 */
typedef void (*ParseFn)();

/**
 * \struct ParseRule
 * \brief Strucure used to establish rules on how to parse a given token
 */
typedef struct {
  ParseFn prefix; /**< Function that compiles a prefix expression starting with
                     the specified token */

  ParseFn infix; /**< Function that compiles an infix expression whose left
                    operand is followed by the specified token */

  Precedence precedence; /**< Precedence of an infix expression that uses the
                            specified token as an operator */
} ParseRule;

Parser parser;
Chunk* compilingChunk;

/**
 * \brief Returns a pointer to chunk of bytecode that is the current target of
 * the compiler
 *
 * \return Pointer to the chunk
 */
static Chunk* currentChunk() {
  return compilingChunk;
}

/**
 * \brief Reports an error at a given token and also updates the error flags in
 * the Parser struct.
 *
 * \param token Token where the error happened
 * \param message Error message to be reported
 *
 */
static void errorAt(const Token* token, const char* message) {
  if (parser.panicMode)
    return;

  parser.panicMode = true;
  eprintf("[line %d] Error", token->line);

  if (token->type == TOKEN_EOF) {
    eprintf(" at end");
  } else if (token->type != TOKEN_ERROR) {
    eprintf(" at '%.*s'", (int)token->length, token->start);
  }

  eprintf(": %s\n", message);
  parser.hadError = true;
}

/**
 * \brief Helper function to report an error at the previous token of the Parser
 *
 * \param message Error message to be reported
 *
 */
static void error(const char* message) {
  errorAt(&parser.previous, message);
}

/**
 * \brief Helper function to report an error at the current token of the Parser
 *
 * \param message Error message to be reported
 *
 */
static void errorAtCurrent(const char* message) {
  errorAt(&parser.current, message);
}

/**
 * \brief Advances in the scanner's token stream and stores it for later use
 * in the "current" attribute of the Parser.
 *
 * Also updates the "previous" attribute. If there were any lexical errors, this
 * function will report them
 *
 */
static void advance() {
  parser.previous = parser.current;

  while (true) {
    parser.current = scanToken();

    if (parser.current.type != TOKEN_ERROR)
      break;

    errorAtCurrent(parser.current.start);
  }
}

/**
 * \brief Consumes the current token of the parser if it has a given type.
 * Otherwise, doesn't consume the token and reports an error
 *
 * \param type Expected type of the token to be consumed
 * \param message Error message to be reported if the type is not as expected
 *
 */
static void consume(const TokenType type, const char* message) {
  if (parser.current.type == type) {
    advance();
    return;
  }

  errorAtCurrent(message);
}

/**
 * \brief Adds a bytecode to the target chunk
 *
 * \param byte Bytecode to be added
 *
 */
static void emitByte(const uint8_t byte) {
  writeChunk(currentChunk(), byte, parser.previous.line);
}

/**
 * \brief Adds a bytecode and its one-byte operand to the target chunk
 *
 * \param byte1 Bytecode to be added
 * \param byte2 Operand byte
 *
 */
static void emitBytes(const uint8_t byte1, const uint8_t byte2) {
  emitByte(byte1);
  emitByte(byte2);
}

/**
 * \brief Adds an OP_RETURN bytecode to the target chunk
 *
 */
static void emitReturn() {
  emitByte(OP_RETURN);
}

/**
 * \brief Adds a constant value to the constants array of the target chunk.
 *
 * It also emits the appropriate bytecode for the offset of the value, depending
 * on the size of the constants array.
 *
 * If the array is too large, an error will be reported.
 *
 * \param value Constant value to be added
 */
static void emitConstant(const Value value) {
  size_t offset = addConstant(currentChunk(), value);

  if (offset <= UINT8_MAX) {
    emitBytes(OP_CONSTANT, (uint8_t)offset);
  } else if (offset <= UINT24_MAX) {
    emitByte(OP_CONSTANT_LONG);
    emitByte((uint8_t)(offset & UINT8_MAX));
    emitByte((uint8_t)((offset >> 8) & UINT8_MAX));
    emitByte((uint8_t)((offset >> 16) & UINT8_MAX));
  } else {
    error("Too many constants in one chunk");
    emitBytes(OP_CONSTANT, 0);
  }
}

/**
 * \brief Wraps up the compiling process. In debug mode, it will also
 * disassemble the compiled chunk
 *
 */
static void endCompiler() {
  emitReturn();

#ifdef DEBUG
  if (!parser.hadError) {
    disassembleChunk(currentChunk(), "code");
  }
#endif
}

// --------- Forward declarations ---------
static void expression();
static ParseRule* getRule(TokenType type);
static void parsePrecedence(Precedence precedence);
// ----------------------------------------

/**
 * \brief Function to parse a binary expression
 *
 */
static void binary() {
  TokenType operatorType = parser.previous.type;
  ParseRule* rule = getRule(operatorType);

  // Compile the right operand
  // Use one higher level of precedence because binary operators are left
  // associative
  parsePrecedence((Precedence)(rule->precedence + 1));

  switch (operatorType) {
  case TOKEN_BANG_EQUAL:
    emitByte(OP_NOT_EQUAL);
    break;
  case TOKEN_EQUAL_EQUAL:
    emitByte(OP_EQUAL);
    break;
  case TOKEN_GREATER:
    emitByte(OP_GREATER);
    break;
  case TOKEN_GREATER_EQUAL:
    emitByte(OP_GTE);
    break;
  case TOKEN_LESS:
    emitByte(OP_LESS);
    break;
  case TOKEN_LESS_EQUAL:
    emitByte(OP_LTE);
    break;
  case TOKEN_PLUS:
    emitByte(OP_ADD);
    break;
  case TOKEN_MINUS:
    emitByte(OP_SUBTRACT);
    break;
  case TOKEN_STAR:
    emitByte(OP_MULTIPLY);
    break;
  case TOKEN_SLASH:
    emitByte(OP_DIVIDE);
    break;
  default:
    return; // Unreachable.
  }
}

/**
 * \brief Function to parse a literal expression
 *
 */
static void literal() {
  switch (parser.previous.type) {
  case TOKEN_FALSE:
    emitByte(OP_FALSE);
    break;
  case TOKEN_NIL:
    emitByte(OP_NIL);
    break;
  case TOKEN_TRUE:
    emitByte(OP_TRUE);
    break;
  default:
    return; // Unreachable
  }
}

/**
 * \brief Function to parse a grouping expression
 *
 */
static void grouping() {
  // Compile the expression within the parenthesis
  expression();

  consume(TOKEN_RIGHT_PAREN, "Expect ')' after expression.");
}

/**
 * \brief Function to parse a number expression
 *
 */
static void number() {
  double value = strtod(parser.previous.start, NULL);
  emitConstant(NUMBER_VAL(value));
}

/**
 * \brief Function to parse an unary expression
 *
 */
static void unary() {
  TokenType operatorType = parser.previous.type;

  // Compile the operand
  parsePrecedence(PREC_UNARY);

  switch (operatorType) {
  case TOKEN_BANG:
    emitByte(OP_NOT);
    break;
  case TOKEN_MINUS:
    emitByte(OP_NEGATE);
    break;
  default: // Unreachable
    return;
  }
}

// This array is used to specify parse rules for each token in the grammar. It
// is needed to apply Vaughan Pratt’s “top-down operator precedence parsing”
ParseRule rules[] = {
    [TOKEN_LEFT_PAREN] = {grouping, NULL, PREC_NONE},
    [TOKEN_RIGHT_PAREN] = {NULL, NULL, PREC_NONE},
    [TOKEN_LEFT_BRACE] = {NULL, NULL, PREC_NONE},
    [TOKEN_RIGHT_BRACE] = {NULL, NULL, PREC_NONE},
    [TOKEN_COMMA] = {NULL, NULL, PREC_NONE},
    [TOKEN_DOT] = {NULL, NULL, PREC_NONE},
    [TOKEN_MINUS] = {unary, binary, PREC_TERM},
    [TOKEN_PLUS] = {NULL, binary, PREC_TERM},
    [TOKEN_SEMICOLON] = {NULL, NULL, PREC_NONE},
    [TOKEN_SLASH] = {NULL, binary, PREC_FACTOR},
    [TOKEN_STAR] = {NULL, binary, PREC_FACTOR},
    [TOKEN_BANG] = {unary, NULL, PREC_NONE},
    [TOKEN_BANG_EQUAL] = {NULL, binary, PREC_EQUALITY},
    [TOKEN_EQUAL] = {NULL, NULL, PREC_NONE},
    [TOKEN_EQUAL_EQUAL] = {NULL, binary, PREC_EQUALITY},
    [TOKEN_GREATER] = {NULL, binary, PREC_COMPARISON},
    [TOKEN_GREATER_EQUAL] = {NULL, binary, PREC_COMPARISON},
    [TOKEN_LESS] = {NULL, binary, PREC_COMPARISON},
    [TOKEN_LESS_EQUAL] = {NULL, binary, PREC_COMPARISON},
    [TOKEN_IDENTIFIER] = {NULL, NULL, PREC_NONE},
    [TOKEN_STRING] = {NULL, NULL, PREC_NONE},
    [TOKEN_NUMBER] = {number, NULL, PREC_NONE},
    [TOKEN_AND] = {NULL, NULL, PREC_NONE},
    [TOKEN_CLASS] = {NULL, NULL, PREC_NONE},
    [TOKEN_ELSE] = {NULL, NULL, PREC_NONE},
    [TOKEN_FALSE] = {literal, NULL, PREC_NONE},
    [TOKEN_FOR] = {NULL, NULL, PREC_NONE},
    [TOKEN_FUN] = {NULL, NULL, PREC_NONE},
    [TOKEN_IF] = {NULL, NULL, PREC_NONE},
    [TOKEN_NIL] = {literal, NULL, PREC_NONE},
    [TOKEN_OR] = {NULL, NULL, PREC_NONE},
    [TOKEN_PRINT] = {NULL, NULL, PREC_NONE},
    [TOKEN_RETURN] = {NULL, NULL, PREC_NONE},
    [TOKEN_SUPER] = {NULL, NULL, PREC_NONE},
    [TOKEN_THIS] = {NULL, NULL, PREC_NONE},
    [TOKEN_TRUE] = {literal, NULL, PREC_NONE},
    [TOKEN_VAR] = {NULL, NULL, PREC_NONE},
    [TOKEN_WHILE] = {NULL, NULL, PREC_NONE},
    [TOKEN_ERROR] = {NULL, NULL, PREC_NONE},
    [TOKEN_EOF] = {NULL, NULL, PREC_NONE},
};

/**
 * \brief Keeps parsing expressions that have a greater precedence level than a
 * specified level.
 *
 * Based on the "rules" table, the function is able to determine if a token is
 * used as a prefix or as an infix within an expression.
 *
 * \param precedence The lower bound for the precedence level of the expressions
 * that will be parsed
 *
 */
static void parsePrecedence(Precedence precedence) {
  advance();
  ParseFn prefixRule = getRule(parser.previous.type)->prefix;
  if (prefixRule == NULL) {
    error("Expect expression.");
    return;
  }

  prefixRule();

  while (precedence <= getRule(parser.current.type)->precedence) {
    advance();
    ParseFn infixRule = getRule(parser.previous.type)->infix;
    infixRule();
  }
}

/**
 * \brief Obtains the parse rules for a token of a given type
 *
 * \param type Token type whose rules must be fetched
 *
 */
static ParseRule* getRule(TokenType type) {
  return &rules[type];
}

/**
 * \brief Parses an expression
 *
 */
static void expression() {
  parsePrecedence(PREC_ASSIGNMENT);
}

bool compile(const char* source, Chunk* chunk) {
  initScanner(source);

  compilingChunk = chunk;

  parser.hadError = false;
  parser.panicMode = false;

  advance();
  expression();
  consume(TOKEN_EOF, "Expect end of expression.");

  endCompiler();

  return !parser.hadError;
}
