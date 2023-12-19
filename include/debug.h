#ifndef CLOX_DEBUG_H
#define CLOX_DEBUG_H

#include "chunk.h"

void disassembleChunk(const Chunk* chunk, const char* name);
size_t disassembleInstruction(const Chunk* chunk, const size_t offset);

#endif
