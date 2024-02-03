/*! \file chunk.h
    \brief Functions and data types for manipulating chunks of bytecode.
*/

#ifndef CLOX_CHUNK_H
#define CLOX_CHUNK_H

#include "common.h"
#include "value.h"

/**
 * \enum OpCode
 * \brief Enum type for all Opcodes available in clox's instruction set.
 */
typedef enum {
  OP_CONSTANT, /**< Loads a constant value with an 8-bit offset to the stack */
  OP_CONSTANT_LONG, /**< Loads a constant value with a 24-bit offset to the
                       stack */
  OP_NIL,           /**< Loads a nil value to the stack */
  OP_TRUE,          /**< Loads a true boolean value to the stack */
  OP_FALSE,         /**< Loads a false boolean value to the stack */
  OP_POP,           /**< Pops a value from the stack */
  OP_GET_LOCAL, /**< Loads a local variable to the stack with an 8-bit offset */
  OP_GET_LOCAL_LONG, /**< Loads a local variable to the stack with an 24-bit
                        offset */
  OP_SET_LOCAL,      /**< Updates the value of a defined local variable with an
                         8-bit offset */
  OP_SET_LOCAL_LONG, /**< Updates the value of a defined local variable with an
                    24-bit offset */
  OP_GET_GLOBAL, /**< Loads a global variable to the stack with an 8-bit offset
                  */
  OP_GET_GLOBAL_LONG, /**< Loads a global variable to the stack with a 24-bit
                         offset */
  OP_DEFINE_GLOBAL,   /**< Defines a new global variable variable with an 8-bit
                         offset */
  OP_DEFINE_GLOBAL_LONG, /**< Defines a new global variable variable with a
                            24-bit offset */
  OP_SET_GLOBAL, /**< Updates the value of a defined global variable with an
                    8-bit offset */
  OP_SET_GLOBAL_LONG, /**< Updates the value of a defined global variable with a
                         24-bit offset */
  OP_NOT,    /**< Applies logical negation to the value at the top of the stack
              */
  OP_NEGATE, /**< Negates the value at the top of the stack */
  OP_EQUAL,  /**< Compares the next two values in the stack using equality */
  OP_NOT_EQUAL, /**< Compares the next two values in the stack using inequality
                 */
  OP_GREATER,   /**< Compares the next two values in the stack using "greater
                   than" */
  OP_GTE,  /**< Compares the next two values in the stack using "greater than or
              equal to" */
  OP_LESS, /**< Compares the next two values in the stack using "less than" */
  OP_LTE,  /**< Compares the next two values in the stack using "less than or
              equal to" */
  OP_ADD,  /**< Adds the next two values in the stack */
  OP_SUBTRACT, /**< Subtracts the next two values in the stack */
  OP_MULTIPLY, /**< Multiplies the next two values in the stack */
  OP_DIVIDE,   /**< Divides the next two values in the stack */
  OP_PRINT,
  OP_JUMP,
  OP_JUMP_IF_FALSE,
  OP_JUMP_IF_FALSE_NP,
  OP_LOOP,
  OP_RETURN /**< Returns from the current function */
} OpCode;

/**
 * \struct LineInfo
 * \brief Strucure that contains line metadata for a given bytecode. This
 * strucure helps to save memory when many bytecodes are in the same line.
 */
typedef struct {
  size_t startOffset; /**< First index of a bytecode with this line in the
                         chunk's array */
  uint32_t line;      /**< Line number */
} LineInfo;

/**
 * \struct Chunk
 * \brief Structure that contains a chunk of bytecode and its metadata.
 */
typedef struct {
  size_t count;    /**< Number of bytecodes currently in the array */
  size_t capacity; /**< Current capacity of the bytecode array */
  uint8_t* code;   /**< Pointer to the bytecode array */

  size_t linesCount; /**< Number of line information variables currently in the
                        array */
  size_t linesCapacity; /**< Current capacity of the line information array */
  LineInfo* lines;      /**< Pointer to the line information array */

  ValueArray constants; /**< Array of constant values used as operands */
} Chunk;

/**
 * \brief Initializes a chunk.
 *
 * \param chunk Chunk to be initialized
 */
void initChunk(Chunk* chunk);

/**
 * \brief Frees the resources of a given chunk.
 *
 * \param chunk Chunk to be freed
 */
void freeChunk(Chunk* chunk);

/**
 * \brief Adds a bytecode to a given chunk.
 *
 * \param chunk Chunk where the bytecode will be inserted
 * \param byte Bytecode instruction that will be added to \p chunk
 * \param line Line where \p byte appears in the source code
 */
void writeChunk(Chunk* chunk, const uint8_t byte, const uint32_t line);

/**
 * \brief Gets the line number of a given bytecode based on its offset in the
 * bytecode array.
 *
 * \param chunk Chunk that contains the bytecode
 * \param instructionOffset Offset of the bytecode in \p chunk's bytecode array
 *
 * \return Line number of the bytecode
 */
uint32_t getLine(const Chunk* chunk, const size_t instructionOffset);

/**
 * \brief Adds a constant value to the constants array of a given chunk.
 *
 * \param chunk Chunk where the constant will be added
 * \param value The constant value that will be added
 *
 * \return The offset of the constant in the chunk's constants array
 */
uint32_t addConstant(Chunk* chunk, const Value value);

#endif
