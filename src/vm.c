/*! \file vm.c
    \brief Definitions of functions from vm.h
*/

#include "vm.h"
#include "common.h"
#include "compiler.h"
#include "memory.h"
#include "object.h"

#ifdef DEBUG
#include "debug.h"
#endif

#include <stdarg.h>
#include <string.h>

VM vm;

/**
 * \brief Resets the value stack by moving the stack pointer to its start.
 */
static void resetStack() {
  vm.stackTop = vm.stack;
}

/**
 * \brief Variadic function used to report a runtime error in the appropriate
 * line
 *
 * \param format Format string for the error message
 * \param ... Arguments for format specification
 */
static void runtimeError(const char* format, ...) {
  va_list args;
  va_start(args, format);
  vfprintf(stderr, format, args);
  va_end(args);
  fputs("\n", stderr);

  size_t instruction = vm.ip - vm.chunk->code - 1;
  int line = getLine(vm.chunk, instruction);
  eprintf("[line %d] in script\n", line);
  resetStack();
}

void initVM() {
  resetStack();
  vm.objects = NULL;
}

void freeVM() {
  freeObjects();
}

void push(const Value value) {
  *vm.stackTop = value;
  vm.stackTop++;
}

Value pop() {
  vm.stackTop--;
  return *vm.stackTop;
}

/**
 * \brief Returns the value at a certain distance in the stack without popping
 * it
 *
 * \param distance How far down the stack the target the desired value is
 *
 * \return The peeked Value
 */
static Value peek(const int32_t distance) {
  return vm.stackTop[-1 - distance];
}

/**
 * \brief Returns whether a Value is falsey or not
 *
 * \param value The Value in consideration
 *
 * \return Boolean value that indicates if the parameter is falsey
 */
static bool isFalsey(const Value value) {
  return IS_NIL(value) || (IS_BOOL(value) && AS_BOOL(value) == false);
}

/**
 * \brief Concatenates the next two value at the value stack, assuming that they
 * are strings.
 * 
 * This function has to allocate a new string for the concatenated string.
 *
 */
static void concatenate() {
  ObjString* right = AS_STRING(pop());
  ObjString* left = AS_STRING(pop());

  size_t length = left->length + right->length;
  ObjString* concatenatedString = allocateString(length);

  memcpy(concatenatedString->chars, left->chars, left->length);
  memcpy(concatenatedString->chars + left->length, right->chars, right->length);
  concatenatedString->chars[length] = '\0';

  push(OBJ_VAL(concatenatedString));
}

/**
 * \brief Helper function that runs the instructions in the VM.
 *
 * \return Result of the interpretation process
 */
static InterpretResult run() {
// Read a single bytecode from the VM and updates the instruction pointer
#define READ_BYTE() (*(vm.ip++))

// Read a constant from the chunk based on the 8-bit offset at the bytecode
// array
#define READ_CONSTANT() (vm.chunk->constants.values[READ_BYTE()])

// Read a constant from the chunk based on the 24-bit offset at the bytecode
// array
#define READ_CONSTANT_LONG()                                                   \
  (vm.chunk->constants                                                         \
       .values[READ_BYTE() | ((READ_BYTE()) << 8) | ((READ_BYTE()) << 16)])

// Apply a binary operation based on the two next values at stack and on the
// type of the operands
#define BINARY_OP(valueType, op)                                               \
  do {                                                                         \
    if (!IS_NUMBER(peek(0)) || !IS_NUMBER(peek(1))) {                          \
      runtimeError("Operands must be numbers.");                               \
      return INTERPRET_RUNTIME_ERROR;                                          \
    }                                                                          \
    double right = AS_NUMBER(pop());                                           \
    double left = AS_NUMBER(pop());                                            \
    push(valueType(left op right));                                            \
  } while (false)

#ifdef DEBUG
  printf("\n=== execution trace ===");
#endif

  while (true) {

#ifdef DEBUG
    printf("          ");
    for (Value* slot = vm.stack; slot < vm.stackTop; ++slot) {
      printf("[");
      printValue(*slot);
      printf("]");
    }
    printf("\n");

    disassembleInstruction(vm.chunk, (size_t)(vm.ip - vm.chunk->code));
#endif

    uint8_t instruction;
    switch (instruction = READ_BYTE()) {
    case OP_CONSTANT: {
      Value constant = READ_CONSTANT();
      push(constant);
      break;
    }
    case OP_CONSTANT_LONG: {
      Value constant = READ_CONSTANT_LONG();
      push(constant);
      break;
    }
    case OP_NIL:
      push(NIL_VAL);
      break;
    case OP_TRUE:
      push(BOOL_VAL(true));
      break;
    case OP_FALSE:
      push(BOOL_VAL(false));
      break;
    case OP_NOT:
      vm.stackTop[-1] = BOOL_VAL(isFalsey(vm.stackTop[-1]));
      break;
    case OP_NEGATE:
      if (!IS_NUMBER(peek(0))) {
        runtimeError("Operand must be a number.");
        return INTERPRET_RUNTIME_ERROR;
      }

      vm.stackTop[-1] = NUMBER_VAL(-AS_NUMBER(vm.stackTop[-1]));
      break;
    case OP_EQUAL: {
      Value left = pop();
      Value right = pop();
      push(BOOL_VAL(valuesEqual(left, right)));
      break;
    }
    case OP_NOT_EQUAL: {
      Value left = pop();
      Value right = pop();
      push(BOOL_VAL(!valuesEqual(left, right)));
      break;
    }
    case OP_GREATER:
      BINARY_OP(BOOL_VAL, >);
      break;
    case OP_GTE:
      BINARY_OP(BOOL_VAL, >=);
      break;
    case OP_LESS:
      BINARY_OP(BOOL_VAL, <);
      break;
    case OP_LTE:
      BINARY_OP(BOOL_VAL, <=);
      break;
    case OP_ADD:
      if (IS_NUMBER(peek(0)) && IS_NUMBER(peek(1))) {
        double right = AS_NUMBER(pop());
        double left = AS_NUMBER(pop());
        push(NUMBER_VAL(left + right));
      } else if (IS_STRING(peek(0)) && IS_STRING(peek(1))) {
        concatenate();
      } else {
        runtimeError("Operands must be two numbers or two strings.");
        return INTERPRET_RUNTIME_ERROR;
      }

      break;
    case OP_SUBTRACT:
      BINARY_OP(NUMBER_VAL, -);
      break;
    case OP_MULTIPLY:
      BINARY_OP(NUMBER_VAL, *);
      break;
    case OP_DIVIDE:
      BINARY_OP(NUMBER_VAL, /);
      break;
    case OP_RETURN:
      printValue(pop());
      printf("\n");
      return INTERPRET_OK;
    default:
      break;
    }
  }

#undef READ_BYTE
#undef READ_CONSTANT
#undef READ_CONSTANT_LONG
#undef BINARY_OP
}

InterpretResult interpret(const char* source) {
  Chunk chunk;
  initChunk(&chunk);

  if (!compile(source, &chunk)) {
    freeChunk(&chunk);
    return INTERPRET_COMPILE_ERROR;
  }

  vm.chunk = &chunk;
  vm.ip = vm.chunk->code;

  InterpretResult result = run();

  freeChunk(&chunk);
  return result;
}
