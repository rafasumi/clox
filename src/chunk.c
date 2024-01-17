/*! \file chunk.c
    \brief Definitions of functions from chunk.h
*/

#include "chunk.h"
#include "memory.h"
#include <stdlib.h>

void initChunk(Chunk* chunk) {
  chunk->count = 0;
  chunk->capacity = 0;
  chunk->code = NULL;

  chunk->linesCount = 0;
  chunk->linesCapacity = 0;
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
    size_t oldCapacity = chunk->capacity;
    chunk->capacity = GROW_CAPACITY(oldCapacity);
    chunk->code =
        GROW_ARRAY(uint8_t, chunk->code, oldCapacity, chunk->capacity);
  }

  chunk->code[chunk->count] = byte;
  ++chunk->count;

  if (chunk->linesCount > 0 &&
      chunk->lines[chunk->linesCount - 1].line == line) {
    return;
  }

  if (chunk->linesCapacity <= chunk->linesCount) {
    size_t oldCapacity = chunk->linesCapacity;
    chunk->linesCapacity = GROW_CAPACITY(oldCapacity);
    chunk->lines =
        GROW_ARRAY(LineInfo, chunk->lines, oldCapacity, chunk->linesCapacity);
  }

  LineInfo* lineInfo = &chunk->lines[chunk->linesCount];
  ++chunk->linesCount;
  lineInfo->startOffset = chunk->count - 1;
  lineInfo->line = line;
}

uint32_t getLine(const Chunk* chunk, const size_t instructionOffset) {
  size_t start = 0;
  size_t end = chunk->linesCount - 1;

  while (true) {
    size_t mid = (start + end) / 2;
    LineInfo* lineInfo = &chunk->lines[mid];

    if (instructionOffset < lineInfo->startOffset) {
      end = mid - 1;
    } else if (mid == chunk->linesCount - 1 ||
               instructionOffset < chunk->lines[mid + 1].startOffset) {
      return lineInfo->line;
    } else {
      start = mid + 1;
    }
  }
}

uint32_t addConstant(Chunk* chunk, const Value value) {
  writeValueArray(&chunk->constants, value);

  return (uint32_t)chunk->constants.count - 1;
}
