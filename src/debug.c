/*! \file debug.c
    \brief Definitions of functions from debug.h
*/

#include "debug.h"
#include "value.h"
#include "vm.h"

void disassembleChunk(const Chunk* chunk, const char* name) {
  printf("=== %s ===\n", name);

  for (size_t offset = 0; offset < chunk->count;) {
    offset = disassembleInstruction(chunk, offset);
  }
}

/**
 * \brief Display a simple instruction at a given offset. The instruction must
 * have no operands.
 *
 * \param name Name of the instruction
 * \param offset Offset of the instruction within the bytecode array
 *
 * \return Offset of the next instruction
 */
static size_t simpleInstruction(const char* name, const size_t offset) {
  printf("%s\n", name);

  return offset + 1;
}

/**
 * \brief Display an instruction at a given offset along with the instruction's
 * 8-bit operand.
 *
 * \param name Name of the instruction
 * \param chunk Pointer to the chunk of bytecode that contains the instruction
 * \param offset Offset of the instruction within the bytecode array
 *
 * \return Offset of the next instruction
 */
static size_t byteInstruction(const char* name, const Chunk* chunk,
                              const size_t offset) {
  uint8_t slot = chunk->code[offset + 1];
  printf("%-16s %4d\n", name, slot);

  return offset + 2;
}

/**
 * \brief Display an instruction at a given offset along with the instruction's
 * 24-bit operand.
 *
 * \param name Name of the instruction
 * \param chunk Pointer to the chunk of bytecode that contains the instruction
 * \param offset Offset of the instruction within the bytecode array
 *
 * \return Offset of the next instruction
 */
static size_t byteLongInstruction(const char* name, const Chunk* chunk,
                                  const size_t offset) {
  uint32_t slot = (chunk->code[offset + 3] << 16) |
                  (chunk->code[offset + 2] << 8) | chunk->code[offset + 1];
  printf("%-16s %4d\n", name, slot);

  return offset + 4;
}

/**
 * \brief Display an OP_CONSTANT instruction at a given offset. This instruction
 * has one 8-bit operand.
 *
 * \param name Name of the instruction
 * \param chunk Pointer to the chunk of bytecode that contains the instruction
 * \param offset Offset of the instruction within the bytecode array
 *
 * \return Offset of the next instruction
 */
static size_t constantInstruction(const char* name, const Chunk* chunk,
                                  const size_t offset) {
  uint8_t constantOffset = chunk->code[offset + 1];
  printf("%-16s %4d '", name, constantOffset);

  printValue(chunk->constants.values[constantOffset]);
  printf("'\n");

  return offset + 2;
}

/**
 * \brief Display an OP_CONSTANT_LONG instruction at a given offset. This
 * instruction has three 8-bit operands.
 *
 * \param name Name of the instruction
 * \param chunk Pointer to the chunk of bytecode that contains the instruction
 * \param offset Offset of the instruction within the bytecode array
 *
 * \return Offset of the next instruction
 */
static size_t constantLongInstruction(const char* name, const Chunk* chunk,
                                      const size_t offset) {
  uint32_t constantOffset = (chunk->code[offset + 3] << 16) |
                            (chunk->code[offset + 2] << 8) |
                            chunk->code[offset + 1];

  printf("%-16s %4d '", name, constantOffset);

  printValue(chunk->constants.values[constantOffset]);
  printf("'\n");

  return offset + 4;
}

/**
 * \brief Display an instruction which handles global variables. This
 * instruction has one 8-bit operand.
 *
 * \param name Name of the instruction
 * \param chunk Pointer to the chunk of bytecode that contains the instruction
 * \param offset Offset of the instruction within the bytecode array
 *
 * \return Offset of the next instruction
 */
static size_t globalInstruction(const char* name, const Chunk* chunk,
                                const size_t offset) {
  uint8_t globalOffset = chunk->code[offset + 1];

  printf("%-16s %4d '", name, globalOffset);
  printValue(vm.globalValues.vars[globalOffset].value);
  printf("'\n");

  return offset + 2;
}

/**
 * \brief Display an instruction which handles global variables. This
 * instruction has three 8-bit operands.
 *
 * \param name Name of the instruction
 * \param chunk Pointer to the chunk of bytecode that contains the instruction
 * \param offset Offset of the instruction within the bytecode array
 *
 * \return Offset of the next instruction
 */
static size_t globalLongInstruction(const char* name, const Chunk* chunk,
                                    const size_t offset) {
  uint32_t globalOffset = (chunk->code[offset + 3] << 16) |
                          (chunk->code[offset + 2] << 8) |
                          chunk->code[offset + 1];

  printf("%-16s %4d '", name, globalOffset);
  printValue(vm.globalValues.vars[globalOffset].value);
  printf("'\n");

  return offset + 4;
}

size_t disassembleInstruction(const Chunk* chunk, const size_t offset) {
  printf("%04ld ", offset);

  uint32_t line = getLine(chunk, offset);
  if (offset > 0 && line == getLine(chunk, offset - 1)) {
    printf("   | ");
  } else {
    printf("%4d ", line);
  }

  uint8_t instruction = chunk->code[offset];

  switch (instruction) {
  case OP_CONSTANT:
    return constantInstruction("OP_CONSTANT", chunk, offset);
  case OP_CONSTANT_LONG:
    return constantLongInstruction("OP_CONSTANT_LONG", chunk, offset);
  case OP_NIL:
    return simpleInstruction("OP_NIL", offset);
  case OP_TRUE:
    return simpleInstruction("OP_TRUE", offset);
  case OP_FALSE:
    return simpleInstruction("OP_FALSE", offset);
  case OP_POP:
    return simpleInstruction("OP_POP", offset);
  case OP_GET_LOCAL:
    return byteInstruction("OP_GET_LOCAL", chunk, offset);
  case OP_GET_LOCAL_LONG:
    return byteLongInstruction("OP_GET_LOCAL_LONG", chunk, offset);
  case OP_SET_LOCAL:
    return byteInstruction("OP_GET_LOCAL", chunk, offset);
  case OP_SET_LOCAL_LONG:
    return byteLongInstruction("OP_SET_LOCAL_LONG", chunk, offset);
  case OP_GET_GLOBAL:
    return globalInstruction("OP_GET_GLOBAL", chunk, offset);
  case OP_GET_GLOBAL_LONG:
    return globalLongInstruction("OP_GET_GLOBAL_LONG", chunk, offset);
  case OP_DEFINE_GLOBAL:
    return globalInstruction("OP_DEFINE_GLOBAL", chunk, offset);
  case OP_DEFINE_GLOBAL_LONG:
    return globalLongInstruction("OP_DEFINE_GLOBAL_LONG", chunk, offset);
  case OP_SET_GLOBAL:
    return globalInstruction("OP_SET_GLOBAL", chunk, offset);
  case OP_SET_GLOBAL_LONG:
    return globalLongInstruction("OP_SET_GLOBAL_LONG", chunk, offset);
  case OP_NOT:
    return simpleInstruction("OP_NOT", offset);
  case OP_NEGATE:
    return simpleInstruction("OP_NEGATE", offset);
  case OP_EQUAL:
    return simpleInstruction("OP_EQUAL", offset);
  case OP_NOT_EQUAL:
    return simpleInstruction("OP_NOT_EQUAL", offset);
  case OP_GREATER:
    return simpleInstruction("OP_GREATER", offset);
  case OP_GTE:
    return simpleInstruction("OP_GTE", offset);
  case OP_LESS:
    return simpleInstruction("OP_LESS", offset);
  case OP_LTE:
    return simpleInstruction("OP_LTE", offset);
  case OP_ADD:
    return simpleInstruction("OP_ADD", offset);
  case OP_SUBTRACT:
    return simpleInstruction("OP_SUBTRACT", offset);
  case OP_MULTIPLY:
    return simpleInstruction("OP_MULTIPLY", offset);
  case OP_DIVIDE:
    return simpleInstruction("OP_DIVIDE", offset);
  case OP_PRINT:
    return simpleInstruction("OP_PRINT", offset);
  case OP_RETURN:
    return simpleInstruction("OP_RETURN", offset);
  default:
    printf("Unknown opcode %d\n", instruction);
    return offset + 1;
  }
}
