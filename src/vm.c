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
  initTable(&vm.globalNames);
  initGlobalVarArray(&vm.globalValues);
  initTable(&vm.strings);
}

void freeVM() {
  freeTable(&vm.globalNames);
  freeGlobalVarArray(&vm.globalValues);
  freeTable(&vm.strings);
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
  char* chars = ALLOCATE(char, length + 1);
  memcpy(chars, left->chars, left->length);
  memcpy(chars + left->length, right->chars, right->length);
  chars[length] = '\0';

  ObjString* concatenatedString = takeString(chars, length);

  push(OBJ_VAL(concatenatedString));
}

/**
 * \brief Auxiliary function that gets the value of global variable based on its
 * offset in the globalValues array. The value is pushed to the stack.
 *
 * \param offset The offset of the variable in the globalValues array
 *
 * \return Boolean value that indicates if there were any errors when fetching
 * the variable
 *
 */
static bool getGlobal(const uint32_t offset) {
  GlobalVar var = vm.globalValues.vars[offset];

  if (IS_UNDEFINED(var.value)) {
    runtimeError("Undefined variable '%s'.", var.identifier->chars);
    return false;
  }

  push(var.value);
  return true;
}

/**
 * \brief Auxiliary function that defines a global variable by setting its
 * value.
 *
 * \param offset The offset of the variable in the globalValues array
 */
static void defineGlobal(const uint32_t offset) {
  vm.globalValues.vars[offset].value = pop();
}

/**
 * \brief Auxiliary function that sets the value of a global variable only if it
 * has already been defined.
 *
 * \param offset The offset of the variable in the globalValues array
 *
 * \return Boolean value that indicates if there were any errors
 */
static bool setGlobal(const uint32_t offset) {
  GlobalVar* var = vm.globalValues.vars + offset;

  if (IS_UNDEFINED(var->value)) {
    runtimeError("Undefined variable '%s'.", var->identifier->chars);
    return false;
  }

  var->value = peek(0);
  return true;
}

/**
 * \brief Helper function that runs the instructions in the VM.
 *
 * \return Result of the interpretation process
 */
static InterpretResult run() {
// Read a single bytecode from the chunk and updates the instruction pointer
#define READ_BYTE() (*(vm.ip++))

// Read a 24-bit (or 3 bytes) operand from the chunk
#define READ_LONG_OPERAND()                                                    \
  READ_BYTE() | ((READ_BYTE()) << 8) | ((READ_BYTE()) << 16)

#define READ_SHORT() \
  (vm.ip += 2, (uint16_t)(vm.ip[-2] | (vm.ip[-1] << 8)))

// Read a constant from the chunk based on the 8-bit offset at the bytecode
// array
#define READ_CONSTANT() (vm.chunk->constants.values[READ_BYTE()])

// Read a constant from the chunk based on the 24-bit offset at the bytecode
// array
#define READ_CONSTANT_LONG() (vm.chunk->constants.values[READ_LONG_OPERAND()])

// Read a string from the constants array with an 8-bit offset
#define READ_STRING() AS_STRING(READ_CONSTANT())

// Read a string from the constants array with a 24-bit offset
#define READ_STRING_LONG() AS_STRING(READ_CONSTANT_LONG())

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
    case OP_POP:
      pop();
      break;
    case OP_GET_LOCAL: {
      uint8_t slot = READ_BYTE();
      push(vm.stack[slot]);
      break;
    }
    case OP_GET_LOCAL_LONG: {
      uint32_t slot = READ_LONG_OPERAND();
      push(vm.stack[slot]);
      break;
    }
    case OP_SET_LOCAL: {
      uint8_t slot = READ_BYTE();
      vm.stack[slot] = peek(0);
      break;
    }
    case OP_SET_LOCAL_LONG: {
      uint32_t slot = READ_LONG_OPERAND();
      vm.stack[slot] = peek(0);
      break;
    }
    case OP_GET_GLOBAL:
      if (!getGlobal(READ_BYTE()))
        return INTERPRET_RUNTIME_ERROR;
      break;
    case OP_GET_GLOBAL_LONG:
      if (!getGlobal(READ_LONG_OPERAND()))
        return INTERPRET_RUNTIME_ERROR;
      break;
    case OP_DEFINE_GLOBAL:
      defineGlobal(READ_BYTE());
      break;
    case OP_DEFINE_GLOBAL_LONG:
      defineGlobal(READ_LONG_OPERAND());
      break;
    case OP_SET_GLOBAL:
      if (!setGlobal(READ_BYTE()))
        return INTERPRET_RUNTIME_ERROR;
      break;
    case OP_SET_GLOBAL_LONG:
      if (!setGlobal(READ_LONG_OPERAND()))
        return INTERPRET_RUNTIME_ERROR;
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
    case OP_PRINT: {
      printValue(pop());
      printf("\n");
      break;
    }
    case OP_JUMP: {
      uint16_t offset = READ_SHORT();
      vm.ip += offset;
      break;
    }
    case OP_JUMP_IF_FALSE: {
      uint16_t offset = READ_SHORT();
      if (isFalsey(peek(0)))
        vm.ip += offset;
      break;
    }
    case OP_RETURN:
      // Exit the interpreter
      return INTERPRET_OK;
    default:
      break;
    }
  }

#undef READ_BYTE
#undef READ_LONG_OPERAND
#undef READ_SHORT
#undef READ_CONSTANT
#undef READ_CONSTANT_LONG
#undef READ_STRING
#undef READ_STRING_LONG
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
