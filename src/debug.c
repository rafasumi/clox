#include "debug.h"
#include "value.h"
#include <stdio.h>

void disassembleChunk(const Chunk* chunk, const char* name) {
  printf("=== %s ===\n", name);

  for (uint32_t offset = 0; offset < chunk->count;) {
    offset = disassembleInstruction(chunk, offset);
  }
}

static uint32_t simpleInstruction(const char* name, const uint32_t offset) {
  printf("%s\n", name);

  return offset + 1;
}

static uint32_t constantInstruction(const char* name, const Chunk* chunk,
                                    const uint32_t offset) {
  uint8_t constantOffset = chunk->code[offset + 1];
  printf("%-16s %4d '", name, constantOffset);

  printValue(chunk->constants.values[constantOffset]);
  printf("'\n");

  return offset + 2;
}

uint32_t disassembleInstruction(const Chunk* chunk, const uint32_t offset) {
  printf("%04d ", offset);

  if (offset > 0 && chunk->lines[offset] == chunk->lines[offset - 1]) {
    printf("   | ");
  } else {
    printf("%4d ", chunk->lines[offset]);
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
