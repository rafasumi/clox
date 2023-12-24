/*! \file vm.c
    \brief Definitions of function from vm.h
*/

#include "vm.h"
#include "common.h"
#include "compiler.h"
#include "debug.h"

VM vm;

/**
 * \brief Resets the value stack by moving the stack pointer to its start.
 */
static void resetStack() { vm.stackTop = vm.stack; }

void initVM() { resetStack(); }

void freeVM() {}

void push(const Value value) {
  *vm.stackTop = value;
  vm.stackTop++;
}

Value pop() {
  vm.stackTop--;
  return *vm.stackTop;
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

// Apply a binary operation based on the two next values at stack
#define BINARY_OP(op)                                                          \
  do {                                                                         \
    double right = pop();                                                      \
    double left = pop();                                                       \
    push(left op right);                                                       \
  } while (false)

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
    case OP_NEGATE:
      Value* target = vm.stackTop - 1;
      *target = -(*target);
      break;
    case OP_ADD:
      BINARY_OP(+);
      break;
    case OP_SUBTRACT:
      BINARY_OP(-);
      break;
    case OP_MULTIPLY:
      BINARY_OP(*);
      break;
    case OP_DIVIDE:
      BINARY_OP(/);
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
  compile(source);
  return INTERPRET_OK;
}
