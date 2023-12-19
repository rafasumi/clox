#include "debug.h"
#include "value.h"
#include <stdio.h>

void disassembleChunk(const Chunk* chunk, const char* name) {
  printf("=== %s ===\n", name);

  for (size_t offset = 0; offset < chunk->count;) {
    offset = disassembleInstruction(chunk, offset);
  }
}

static size_t simpleInstruction(const char* name, const size_t offset) {
  printf("%s\n", name);

  return offset + 1;
}

static size_t constantInstruction(const char* name, const Chunk* chunk,
                                    const size_t offset) {
  uint8_t constantOffset = chunk->code[offset + 1];
  printf("%-16s %4d '", name, constantOffset);

  printValue(chunk->constants.values[constantOffset]);
  printf("'\n");

  return offset + 2;
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
  case OP_RETURN:
    return simpleInstruction("OP_RETURN", offset);
  default:
    printf("Unknown opcode %d\n", instruction);
    return offset + 1;
  }
}
