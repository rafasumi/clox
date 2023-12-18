#include "debug.h"
#include <stdio.h>

void disassembleChunk(Chunk* chunk, const char* name) {
  printf("=== %s ===\n", name);

  for (uint32_t offset; offset < chunk->count;) {
    offset = disassembleInstruction(chunk, offset);
  }
}

static uint32_t simpleInstruction(const char* name, uint32_t offset) {
  printf("%s\n", name);

  return offset + 1;
}

uint32_t disassembleInstruction(Chunk* chunk, uint32_t offset) {
  printf("%04d ", offset);

  uint8_t instruction = chunk->code[offset];

  switch (instruction) {
  case OP_RETURN:
    return simpleInstruction("OP_RETURN", offset);
  default:
    printf("Unknown opcode %d\n", instruction);
    return offset + 1;
  }
}
