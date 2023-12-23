/*! \file debug.c
    \brief Definitions of function from debug.h
*/

#include "debug.h"
#include "value.h"
#include <stdio.h>

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
 * \brief Display an OP_CONSTANT instruction at a given offset. This instruction
 * has one 8-bit operand
 *
 * \param name Name of the instruction
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
  case OP_NEGATE:
    return simpleInstruction("OP_NEGATE", offset);
  case OP_ADD:
    return simpleInstruction("OP_ADD", offset);
  case OP_SUBTRACT:
    return simpleInstruction("OP_SUBTRACT", offset);
  case OP_MULTIPLY:
    return simpleInstruction("OP_MULTIPLY", offset);
  case OP_DIVIDE:
    return simpleInstruction("OP_DIVIDE", offset);
  case OP_RETURN:
    return simpleInstruction("OP_RETURN", offset);
  default:
    printf("Unknown opcode %d\n", instruction);
    return offset + 1;
  }
}
