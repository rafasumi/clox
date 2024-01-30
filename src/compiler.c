/*! \file compiler.c
    \brief Definitions of functions from compiler.h
*/

#include "compiler.h"
#include "common.h"
#include "global.h"
#include "object.h"
#include "scanner.h"

#include <stdlib.h>
#include <string.h>

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
typedef void (*ParseFn)(const bool canAssign);

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

typedef struct {
  Token name;
  int32_t depth;
} Local;

typedef struct {
  Local locals[UINT16_COUNT];
  uint32_t localCount;
  int32_t scopeDepth;
} Compiler;

// --------- Module variables ---------
Parser parser;
Compiler* current = NULL;
Chunk* compilingChunk;
// ------------------------------------

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
 * \brief Check if the type of the current token is equal to a given type.
 *
 * \param type Expected type of the current token
 *
 * \return Boolean value that indicates if the types are the same
 */
static bool check(const TokenType type) {
  return parser.current.type == type;
}

/**
 * \brief Consumes the current token if it has a given type
 *
 * \param type Expected type of the current token
 *
 * \return Boolean value that indicates if the token was consumed
 */
static bool match(const TokenType type) {
  if (!check(type))
    return false;

  advance();

  return true;
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
 * \brief Emits an instruction that takes an operand which is an offset. This
 * instruction must have two versions, one for an 8-bit offset and the other
 * for a 24-bit offset.
 *
 * The function receives both versions of the instruction and is able to
 * determine which of them will be emitted based on the size of the offset.
 *
 * \param instruction Version of the instruction that takes an 8-bit offset
 * \param instructionLong Version of the instruction that takes a 24-bit offset
 * \param offset Offset operand
 *
 */
static void emitOffsetOperandInstruction(const uint8_t instruction,
                                         const uint32_t offset) {
  if (offset <= UINT8_MAX) {
    emitBytes(instruction, (uint8_t)offset);
  } else {
    // Can safely assume that the offset is a 24-bit integer if it is greater
    // than UINT8_MAX, as this must be checked by the caller
    
    // This is a bit a of a hack, as the long version of an instruction will
    // always be the next enum type in the OpCode Enum
    emitByte(instruction + 1);
    
    emitByte((uint8_t)(offset & UINT8_MAX));
    emitByte((uint8_t)((offset >> 8) & UINT8_MAX));
    emitByte((uint8_t)((offset >> 16) & UINT8_MAX));
  }
}

/**
 * \brief Adds an OP_RETURN bytecode to the target chunk
 *
 */
static void emitReturn() {
  emitByte(OP_RETURN);
}

/**
 * \brief Adds a constant value to the chunk's constant array. The function will
 * produce an error if there are too many constants in the array.
 *
 * \param value Constant value to be added
 *
 * \return Offset of the value in the constants array, if it was correctly
 * added. 0 otherwise.
 */
static uint32_t makeConstant(const Value value) {
  uint32_t offset = addConstant(currentChunk(), value);

  if (offset > UINT24_MAX) {
    error("Too many constants in one chunk");
    return 0;
  }

  return offset;
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
  uint32_t offset = makeConstant(value);

  emitOffsetOperandInstruction(OP_CONSTANT, offset);
}

static void initCompiler(Compiler* compiler) {
  compiler->localCount = 0;
  compiler->scopeDepth = 0;
  current = compiler;
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

static void beginScope() {
  current->scopeDepth++;
}

static void endScope() {
  current->scopeDepth--;

  while (current->localCount > 0 &&
         current->locals[current->localCount - 1].depth > current->scopeDepth) {
    emitByte(OP_POP);
    current->localCount--;
  }
}

// --------- Forward declarations ---------
static void expression();
static void statement();
static void declaration();
static ParseRule* getRule(TokenType type);
static void parsePrecedence(Precedence precedence);
// ----------------------------------------

/**
 * \brief Function to parse a binary expression
 *
 */
static void binary(const bool canAssign) {
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
static void literal(const bool canAssign) {
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
static void grouping(const bool canAssign) {
  // Compile the expression within the parenthesis
  expression();

  consume(TOKEN_RIGHT_PAREN, "Expect ')' after expression.");
}

/**
 * \brief Function to parse a number expression
 *
 */
static void number(const bool canAssign) {
  double value = strtod(parser.previous.start, NULL);
  emitConstant(NUMBER_VAL(value));
}

/**
 * \brief Function to parse a string expression
 *
 */
static void string(const bool canAssign) {
  emitConstant(OBJ_VAL(
      copyString(parser.previous.start + 1, parser.previous.length - 2)));
}

/**
 * \brief Takes a given token and creates a new global variable with its lexeme
 * as an identifier
 *
 * \param name Identifier token
 *
 * \return Offset of the new identifier in the globalValues array
 */
static uint32_t identifierConstant(const Token* name) {
  ObjString* identifier = copyString(name->start, name->length);

  Value globalValuesOffset;
  if (tableGet(&vm.globalNames, identifier, &globalValuesOffset)) {
    // The identifier has already been added to the globalValues array, so we
    // just return its offset

    return (uint32_t)AS_NUMBER(globalValuesOffset);
  }

  // Watch out for double free of identifier
  writeGlobalVarArray(&vm.globalValues, UNDEFINED_GLOBAL(identifier));

  if (vm.globalValues.count > UINT24_MAX) {
    error("Too many many defined identifiers");
    return 0;
  }

  uint32_t offset = (uint32_t)vm.globalValues.count - 1;
  tableSet(&vm.globalNames, identifier, NUMBER_VAL((double)offset));

  return offset;
}

static bool identifiersEqual(const Token* a, const Token* b) {
  if (a->length != b->length)
    return false;

  return memcmp(a->start, b->start, a->length) == 0;
}

static uint32_t resolveLocal(Compiler* compiler, const Token* name) {
  for (int i = compiler->localCount - 1; i >= 0; --i) {
    Local* local = &compiler->locals[i];
    if (identifiersEqual(name, &local->name)) {
      if (local->depth == -1)
        error("Can't read local variable in its own initializer.");

      return i;
    }
  }

  return -1;
}

static void addLocal(const Token name) {
  if (current->localCount == UINT16_COUNT) {
    error("Too many variables in function.");
    return;
  }

  Local* local = &current->locals[current->localCount++];
  local->name = name;
  local->depth = -1;
}

static void declareVariable() {
  // Doesn't do anything for the top-level global scope
  if (current->scopeDepth == 0)
    return;

  Token* name = &parser.previous;
  for (int i = current->localCount - 1; i >= 0; --i) {
    Local* local = &current->locals[i];
    if (local->depth != -1 && local->depth < current->scopeDepth)
      break;

    if (identifiersEqual(name, &local->name))
      error("Already a variable with this name in this scope");
  }

  addLocal(*name);
}

/**
 * \brief Auxiliary function that parses an expression that uses an identifier.
 *
 * The function determines if the expression will be parsed as an assignment
 * or as a usage of the variable based on the \p canAssign parameter.
 *
 * \param name Identifier token
 * \param canAssign Boolean flag that indicates if the identifier has already
 * been defined
 *
 */
static void namedVariable(const Token name, const bool canAssign) {
  uint8_t getOp, setOp;
  int32_t arg = resolveLocal(current, &name);
  if (arg != -1) {
    getOp = OP_GET_LOCAL;
    setOp = OP_SET_LOCAL;
  } else {
    arg = identifierConstant(&name);
    getOp = OP_GET_GLOBAL;
    setOp = OP_SET_GLOBAL;
  }

  if (canAssign && match(TOKEN_EQUAL)) {
    // Assignment expression

    expression();

    emitOffsetOperandInstruction(setOp, (uint32_t)arg);
  } else {
    emitOffsetOperandInstruction(getOp, (uint32_t)arg);
  }
}

/**
 * \brief Function to parse an expression that uses an identifier.
 *
 * \param canAssign Boolean flag that indicates if the identifier has already
 * been defined
 *
 */
static void variable(const bool canAssign) {
  namedVariable(parser.previous, canAssign);
}

/**
 * \brief Function to parse an unary expression
 *
 */
static void unary(const bool canAssign) {
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
    [TOKEN_IDENTIFIER] = {variable, NULL, PREC_NONE},
    [TOKEN_STRING] = {string, NULL, PREC_NONE},
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

  bool canAssign = precedence <= PREC_ASSIGNMENT;
  prefixRule(canAssign);

  while (precedence <= getRule(parser.current.type)->precedence) {
    advance();
    ParseFn infixRule = getRule(parser.previous.type)->infix;
    infixRule(canAssign);
  }

  if (canAssign && match(TOKEN_EQUAL)) {
    error("Invalid assignment target.");
  }
}

/**
 * \brief Auxiliary function used to parse a variable and create it in memory.
 *
 * \param errorMessage Error message to be emitted if the variable can't be
 * consumed
 *
 * \return The offset of the variable's value in the globalValues array
 */
static uint32_t parseVariable(const char* errorMessage) {
  consume(TOKEN_IDENTIFIER, errorMessage);

  declareVariable();

  if (current->scopeDepth > 0)
    return 0;

  return identifierConstant(&parser.previous);
}

static void markInitialized() {
  current->locals[current->localCount - 1].depth = current->scopeDepth;
}

/**
 * \brief Emits the instruction to define a global variable, based on its offset
 * in the globalValues array.
 *
 * \param globalOffset Offset of the variable in the globalValues array
 *
 */
static void defineVariable(uint32_t globalOffset) {
  if (current->scopeDepth > 0) {
    markInitialized();
    return;
  }

  emitOffsetOperandInstruction(OP_DEFINE_GLOBAL, globalOffset);
}

/**
 * \brief Obtains the parse rules for a token of a given type.
 *
 * \param type Token type whose rules must be fetched
 *
 * \return A function pointer to the parse rule for the given type
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

static void block() {
  while (!check(TOKEN_RIGHT_BRACE) && !check(TOKEN_EOF)) {
    declaration();
  }

  consume(TOKEN_RIGHT_BRACE, "Expect '}' after block.");
}

/**
 * \brief Function used to parse a variable declaration.
 *
 * If the variable is not initialized in the declaration, it's value is set to
 * nil.
 *
 */
static void varDeclaration() {
  uint32_t globalOffset = parseVariable("Expect variable name.");

  if (match(TOKEN_EQUAL)) {
    expression();
  } else {
    emitByte(OP_NIL);
  }

  consume(TOKEN_SEMICOLON, "Expect ';' after variable declaration");
  defineVariable(globalOffset);
}

/**
 * \brief Function used to parse an expression statement
 *
 */
static void expressionStatement() {
  expression();
  consume(TOKEN_SEMICOLON, "Expect ';' after expression");
  emitByte(OP_POP);
}

/**
 * \brief Function used to parse a print statement.
 *
 * Prints in Lox are statements and not function calls.
 *
 */
static void printStatement() {
  expression();
  consume(TOKEN_SEMICOLON, "Expect ';' after value");
  emitByte(OP_PRINT);
}

/**
 * \brief Function used for error synchronization.
 *
 * The function will exit panic mode and consume tokens until it reaches a
 * synchronization point. Statement boundaries are used as synchronization
 * points.
 *
 */
static void synchronize() {
  parser.panicMode = false;

  while (parser.current.type != TOKEN_EOF) {
    if (parser.previous.type == TOKEN_SEMICOLON)
      return;

    switch (parser.current.type) {
    case TOKEN_CLASS:
    case TOKEN_FUN:
    case TOKEN_VAR:
    case TOKEN_FOR:
    case TOKEN_IF:
    case TOKEN_WHILE:
    case TOKEN_PRINT:
    case TOKEN_RETURN:
      return;
    default:
      break;
    }

    advance();
  }
}

/**
 * \brief Function used to parse a declaration.
 *
 */
static void declaration() {
  if (match(TOKEN_VAR)) {
    varDeclaration();
  } else {
    statement();
  }

  if (parser.panicMode)
    synchronize();
}

/**
 * \brief Function used to parse a statement.
 *
 */
static void statement() {
  if (match(TOKEN_PRINT)) {
    printStatement();
  } else if (match(TOKEN_LEFT_BRACE)) {
    beginScope();
    block();
    endScope();
  } else {
    expressionStatement();
  }
}

bool compile(const char* source, Chunk* chunk) {
  initScanner(source);

  Compiler compiler;
  initCompiler(&compiler);

  compilingChunk = chunk;

  parser.hadError = false;
  parser.panicMode = false;

  advance();

  while (!match(TOKEN_EOF)) {
    declaration();
  }

  endCompiler();

  return !parser.hadError;
}
