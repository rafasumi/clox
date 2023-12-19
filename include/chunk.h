#ifndef CLOX_CHUNK_H
#define CLOX_CHUNK_H

#include "common.h"
#include "value.h"

typedef enum { OP_CONSTANT, OP_CONSTANT_LONG, OP_RETURN } OpCode;

typedef struct {
  size_t startOffset;
  uint32_t line;
} LineInfo;

typedef struct {
  size_t count;
  size_t capacity;
  uint8_t* code;

  size_t linesCount;
  size_t linesCapacity;
  LineInfo* lines;

  ValueArray constants;
} Chunk;

void initChunk(Chunk* chunk);
void freeChunk(Chunk* chunk);
void writeChunk(Chunk* chunk, const uint8_t byte, const uint32_t line);

uint32_t getLine(const Chunk* chunk, size_t instructionOffset);

void writeConstant(Chunk* chunk, const Value value, const uint32_t line);
size_t addConstant(Chunk* chunk, const Value value);

#endif
