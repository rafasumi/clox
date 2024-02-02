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

typedef uint8_t ConstFlag;
#define CONST_UNKNOWN 2

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

/**
 * \struct Local
 * \brief Strucure used to represent a local variable in memory
 */
typedef struct {
  Token name;    /**< Name of the variable */
  int32_t depth; /**< Scope depth of where the variable was declared */
  bool isConst;  /**< Flag that indicates if it is a constant variable */
} Local;

/**
 * \struct Compiler
 * \brief Strucure used to store relevant state for the clox compiler
 */
typedef struct {
  Local locals[UINT16_COUNT]; /**< Array used to store local variables */
  uint32_t localCount; /**< Current number of local variables in locals */
  int32_t scopeDepth;  /**< Scope depth of the code being compiled */
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

static int32_t emitJump(uint8_t instruction) {
  emitByte(instruction);
  
  // Placeholder bytes for backpatching
  emitByte(UINT8_MAX);
  emitByte(UINT8_MAX);
  
  // Returns the offset of the emitted instruction in the chunk
  return currentChunk()->count - 2;
}

/**
 * \brief Emits an instruction that takes an operand which is an offset. This
 * instruction must have two versions, one for an 8-bit offset and the other
 * for a 24-bit offset.
 *
 * The function receives only the 8-bit version of the instruction and assumes
 * that the 24-bit version will be the next instruction in the OpCode Enum.
 *
 * The function determines which version of the instruction to emit based on the
 * size of the offset.
 *
 * It is assumed that the value of \p offset can fit in an 24-bit integer. Thus
 * this condition must be checked by the caller.
 *
 * \param instruction Version of the instruction that takes an 8-bit offset
 * \param offset Offset operand
 *
 */
static void emitOffsetOperandInstruction(const uint8_t instruction,
                                         const uint32_t offset) {
  if (offset <= UINT8_MAX) {
    emitBytes(instruction, (uint8_t)offset);
  } else {
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

static void patchJump(int32_t offset) {
  // -2 to adjust for the bytecode of the jump offset itself
  int32_t jump = currentChunk()->count - offset - 2;

  if (jump > UINT16_MAX) {
    error("Too much code to jump over.");
  }

  currentChunk()->code[offset] = jump & UINT8_MAX;
  currentChunk()->code[offset + 1] = (jump >> 8) & UINT8_MAX;
}

/**
 * \brief Initializes the compiler's state.
 *
 * \param compiler Pointer to the compiler's state struct
 */
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

/**
 * \brief Begin's a new scope level and updates the compiler's state
 * accordingly.
 *
 */
static void beginScope() {
  current->scopeDepth++;
}

/**
 * \brief Ends the current scope level, updates the compiler's state
 * accordingly and emits the appropriate instructions.
 *
 */
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
 * as an identifier.
 *
 * If the global identifier has already been declared and the \p isConst flag is
 * CONST_UNKOWN, the value of the flag is set to the same value as the
 * variable's flag.
 *
 * \param name Identifier token
 * \param isConst Pointer to a flag variable that indicates if the variable
 * is a constant
 *
 * \return Offset of the new identifier in the globalValues array
 */
static uint32_t identifierConstant(const Token* name, ConstFlag* isConst) {
  ObjString* identifier = copyString(name->start, name->length);

  Value globalValuesOffset;
  if (tableGet(&vm.globalNames, identifier, &globalValuesOffset)) {
    // The identifier has already been added to the globalValues array, so we
    // just return its offset

    uint32_t offset = (uint32_t)AS_NUMBER(globalValuesOffset);

    if (*isConst == CONST_UNKNOWN) {
      *isConst = (ConstFlag)vm.globalValues.vars[offset].isConst;
    } else {
      // A global identifier might change its constant modifier if it is
      // redefined. Thus, we must update its state

      vm.globalValues.vars[offset].isConst = (bool)(*isConst);
    }

    return offset;
  }

  // Watch out for double free of identifier
  writeGlobalVarArray(&vm.globalValues,
                      UNDEFINED_GLOBAL(identifier, (bool)(*isConst)));

  if (vm.globalValues.count > UINT24_MAX) {
    error("Too many many defined identifiers");
    return 0;
  }

  uint32_t offset = (uint32_t)vm.globalValues.count - 1;
  tableSet(&vm.globalNames, identifier, NUMBER_VAL((double)offset));

  return offset;
}

/**
 * \brief Verifies if two Token identifiers are equal.
 *
 * \param a First identifier
 * \param b Second identifier
 *
 * \return Boolean value that indicates whether both identifiers are equal
 */
static bool identifiersEqual(const Token* a, const Token* b) {
  if (a->length != b->length)
    return false;

  return memcmp(a->start, b->start, a->length) == 0;
}

/**
 * \brief Resolves and validates a local variable.
 *
 * The function will look for the given identifier in the locals array. If it is
 * found and it doesn't use itself in its own initializer (meaning that it has
 * already been initialized), the function succeeds.
 *
 * \param compiler Pointer to the compiler's state struct
 * \param name Name of the local variable to be resolved
 * \param isConst Pointer to a boolean flag that will be set if the variable is
 * resolved
 *
 * \return Offset of the local variable in the locals array
 */
static uint32_t resolveLocal(Compiler* compiler, const Token* name,
                             bool* isConst) {
  for (int i = compiler->localCount - 1; i >= 0; --i) {
    Local* local = &compiler->locals[i];
    if (identifiersEqual(name, &local->name)) {
      if (local->depth == -1)
        error("Can't read local variable in its own initializer.");

      *isConst = local->isConst;

      return i;
    }
  }

  return -1;
}

/**
 * \brief Adds a local variable to the locals array.
 *
 * \param name Name of the local variable to be added
 * \param isConst Boolean flag that indicates if it is a constant variable
 *
 */
static void addLocal(const Token name, const bool isConst) {
  if (current->localCount == UINT16_COUNT) {
    error("Too many variables in function.");
    return;
  }

  Local* local = &current->locals[current->localCount++];
  local->name = name;
  local->depth = -1;
  local->isConst = isConst;
}

/**
 * \brief Declares a local variable.
 *
 * The declared variable cannot have the same name as another variable in the
 * same scope. If it doesn't, the local variable is added to the locals array.
 *
 * \param isConst Boolean flag that indicates if it is a constant variable
 *
 */
static void declareVariable(const bool isConst) {
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

  addLocal(*name, isConst);
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
  bool isConst;

  int32_t arg = resolveLocal(current, &name, &isConst);
  if (arg != -1) {
    getOp = OP_GET_LOCAL;
    setOp = OP_SET_LOCAL;
  } else {
    // If can't be resolved as a local, than it is resolved as a global

    ConstFlag flag = CONST_UNKNOWN;
    arg = identifierConstant(&name, &flag);
    isConst = (bool)flag;

    getOp = OP_GET_GLOBAL;
    setOp = OP_SET_GLOBAL;
  }

  if (canAssign && match(TOKEN_EQUAL)) {
    // Assignment expression

    if (isConst) {
      error("Can't assign to constant variable.");
      return;
    }

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
    [TOKEN_CONST] = {NULL, NULL, PREC_NONE},
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
 * \param isConst Boolean flag that indicates if the variable is a constant
 *
 * \return The offset of the variable's value in the globalValues array
 */
static uint32_t parseVariable(const char* errorMessage, const bool isConst) {
  consume(TOKEN_IDENTIFIER, errorMessage);

  declareVariable(isConst);

  if (current->scopeDepth > 0)
    return 0;

  ConstFlag flag = (ConstFlag)isConst;
  return identifierConstant(&parser.previous, &flag);
}

/**
 * \brief Marks the last added variable in the locals array as initialized.
 *
 * This means that the scope depth of the variable is set to the current scope
 * depth.
 *
 */
static void markInitialized() {
  current->locals[current->localCount - 1].depth = current->scopeDepth;
}

/**
 * \brief Defines a variable.
 *
 * If it is a global variable, then it Emits the instruction to define it based
 * on its offset in the globalValues array.
 *
 * If it is a local variable, then it is marked as initialized.
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

/**
 * \brief Parses a block of code surrounded by braces.
 *
 */
static void block() {
  while (!check(TOKEN_RIGHT_BRACE) && !check(TOKEN_EOF)) {
    declaration();
  }

  consume(TOKEN_RIGHT_BRACE, "Expect '}' after block.");
}

/**
 * \brief Function used to parse a variable declaration.
 *
 * If the variable is not initialized in the declaration and it is not a
 * constant, it's value is set to nil.
 *
 * Constant variables must be initialized in their declarations.
 *
 */
static void varDeclaration(bool isConst) {
  uint32_t globalOffset = parseVariable("Expect variable name.", isConst);

  if (match(TOKEN_EQUAL)) {
    expression();
  } else {
    if (isConst) {
      error("Constant variable must be initialized at declaration.");
      return;
    }

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

static void ifStatement() {
  consume(TOKEN_LEFT_PAREN, "Expect '(' after 'if'.");
  expression();
  consume(TOKEN_RIGHT_PAREN, "Expect ')' after condition.");

  int thenJump = emitJump(OP_JUMP_IF_FALSE);
  
  emitByte(OP_POP); // Pop the condition value from the stack
  statement();
  int elseJump = emitJump(OP_JUMP);

  patchJump(thenJump);

  emitByte(OP_POP); // Pop the condition value from the stack
  if (match(TOKEN_ELSE))
    statement();
  patchJump(elseJump);
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
    case TOKEN_CONST:
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
  if (match(TOKEN_VAR) || match(TOKEN_CONST)) {
    varDeclaration(parser.previous.type == TOKEN_CONST);
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
  } else if (match(TOKEN_IF)) {
    ifStatement();
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
