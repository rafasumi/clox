#include "chunk.h"
#include "memory.h"
#include <stdlib.h>

void initChunk(Chunk* chunk) {
  chunk->count = 0;
  chunk->capacity = 0;
  chunk->code = NULL;
  chunk->lines = NULL;
  initValueArray(&chunk->constants);
}

void freeChunk(Chunk* chunk) {
  FREE_ARRAY(uint8_t, chunk->code, chunk->capacity);
  FREE_ARRAY(uint32_t, chunk->lines, chunk->capacity);
  freeValueArray(&chunk->constants);
  initChunk(chunk);
}

void writeChunk(Chunk* chunk, const uint8_t byte, const uint32_t line) {
  if (chunk->capacity <= chunk->count) {
    uint32_t oldCapacity = chunk->capacity;
    chunk->capacity = GROW_CAPACITY(oldCapacity);
    chunk->code =
        GROW_ARRAY(uint8_t, chunk->code, oldCapacity, chunk->capacity);
    chunk->lines =
        GROW_ARRAY(uint32_t, chunk->lines, oldCapacity, chunk->capacity);
  }

  chunk->code[chunk->count] = byte;
  chunk->lines[chunk->count] = line;
  ++chunk->count;
}

uint32_t addConstant(Chunk* chunk, const Value value) {
  writeValueArray(&chunk->constants, value);

  return chunk->constants.count - 1;
}
