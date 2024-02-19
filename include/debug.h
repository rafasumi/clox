/*! \file debug.h
    \brief Functions useful for debugging information produced by the
   interpreter
*/

#ifndef CLOX_DEBUG_H
#define CLOX_DEBUG_H

#include "chunk.h"

/**
 * \brief Disassemble and print information for all instruction in a given chunk
 *
 * \param chunk Pointer to the chunk of bytecode
 * \param name Name of the chunk
 */
void disassembleChunk(const Chunk* chunk, const char* name);

/**
 * \brief Disassemble and print information for an instruction at a given offset
 *
 * \param chunk Pointer to the chunk of bytecode
 * \param offset Offset of the instruction within the bytecode array
 *
 * \return Offset of the next instruction
 */
size_t disassembleInstruction(const Chunk* chunk, size_t offset);

#endif
