/*! \file vm.h
    \brief Functions and data types associated with the clox virtual machine.
*/

#ifndef CLOX_VM_H
#define CLOX_VM_H

#include "chunk.h"
#include "value.h"

/**
 * \def STACK_MAX
 * \brief Maximum size of the value stack.
 */
#define STACK_MAX 256

/**
 * \struct VM
 * \brief Structure that represents the clox virtual machine and contains its
 * necessary attributes.
 */
typedef struct {
  Chunk* chunk; /**< Pointer to the chunk of bytecode instructions */
  uint8_t* ip; /**< Pointer to the next instruction to be executed */
  Value stack[STACK_MAX]; /**< Value stack used to manage temporary values */
  Value* stackTop; /**< Pointer to the top of the stack */
} VM;

/**
 * \enum InterpretResult
 * \brief Enum type for possible results of the interpreter.
 */
typedef enum {
  INTERPRET_OK,
  INTERPRET_COMPILE_ERROR,
  INTERPRET_RUNTIME_ERROR
} InterpretResult;

/**
 * \brief Initializes the clox virtual machine.
 */
void initVM();

/**
 * \brief Frees resources from the clox virtual machine.
 */
void freeVM();

/**
 * \brief Interprets the instructions in a chunk of bytecode.
 * 
 * \param chunk Chunk of bytecode instructions to be interpreted
 * 
 * \return The result of the interpretation process
 */
InterpretResult interpret(Chunk* chunk);

/**
 * \brief Pushes a value to the top of the stack.
 * 
 * \param value Constant value that will be added to the stack
 */
void push(const Value value);

/**
 * \brief Removes the value currently at the top of the stack.
 * 
 * \return Value that was removed
 */
Value pop();

#endif
