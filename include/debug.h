#ifndef CLOX_DEBUG_H
#define CLOX_DEBUG_H

#include "chunk.h"

void disassembleChunk(const Chunk* chunk, const char* name);
uint32_t disassembleInstruction(const Chunk* chunk, const uint32_t offset);

#endif
