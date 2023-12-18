#ifndef CLOX_CHUNK_H
#define CLOX_CHUNK_H

#include "common.h"

typedef enum { OP_RETURN } OpCode;

typedef struct {
  uint32_t count;
  uint32_t capacity;
  uint8_t* code;
} Chunk;

void initChunk(Chunk* chunk);
void freeChunk(Chunk* chunk);
void writeChunk(Chunk* chunk, const uint8_t byte);

#endif
