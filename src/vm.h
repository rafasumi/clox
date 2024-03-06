/*! \file vm.h
    \brief Functions and data types associated with the clox virtual machine.
*/

#ifndef CLOX_VM_H
#define CLOX_VM_H

#include "chunk.h"
#include "global.h"
#include "object.h"
#include "table.h"
#include "value.h"

/**
 * \def FRAMES_MAX
 * \brief Maximum size of the call stack.
 */
#define FRAMES_MAX 64

/**
 * \def STACK_MAX
 * \brief Maximum size of the value stack.
 */
#define STACK_MAX UINT16_COUNT

/**
 * \struct CallFrame
 * \brief Represents the call frame of a single ongoing function call
 */
typedef struct {
  ObjClosure* closure; /**< Pointer to the ObjClosure of the callee function */
  uint8_t* ip;         /**< Instruction pointer for the function's chunk */
  Value* slots; /**< Pointer to the first slot in the value stack that can be
                   used by the function */
} CallFrame;

/**
 * \struct VM
 * \brief Structure that represents the clox virtual machine and contains its
 * necessary attributes.
 */
typedef struct {
  CallFrame frames[FRAMES_MAX]; /**< Call stack used to manage function calls */
  size_t frameCount;            /**< Number of ongoing function calls */
  Value stack[STACK_MAX]; /**< Value stack used to manage temporary values */
  Value* stackTop;        /**< Pointer to the top of the stack */
  Table globalNames;      /**< Table of defined global identifiers */
  GlobalVarArray globalValues; /**< Array with values of global variables */
  Table strings; /**< Table of allocated strings, used for string interning */
  ObjUpvalue* openUpvalues; /**< Pointer to the head of the linked-list of open
                               upvalues */
  size_t bytesAllocated; /**< Number of bytes of managed memory allocated by the
                            VM */
  size_t nextGC;         /**< Threshold to trigger a garbage collection */
  Obj* objects; /**< Pointer to the head of the linked-list of heap-allocated
                   objects */
  size_t grayCount; /**< Number of objects in the gray stack */
  size_t grayCapacity; /**< Capacity of the gray stack */
  Obj** grayStack; /**< Stack of "gray" memory objects, meaning that they are
                      reachable in memory and may reference other objects */
} VM;

/**
 * \enum InterpretResult
 * \brief Enum type for possible results of the interpreter.
 */
typedef enum {
  INTERPRET_OK,            /**< Successful interpretation */
  INTERPRET_COMPILE_ERROR, /**< Compile-time error (either during scanning or
                              parsing) */
  INTERPRET_RUNTIME_ERROR  /**< Runtime error */
} InterpretResult;

// Exposing the "vm" variable, which is defined in vm.c
extern VM vm;

/**
 * \brief Initializes the clox virtual machine.
 */
void initVM();

/**
 * \brief Frees resources from the clox virtual machine.
 */
void freeVM();

/**
 * \brief Interprets a given Lox source file.
 *
 * \param source Source code to be interpreted
 *
 * \return The result of the interpretation process
 */
InterpretResult interpret(const char* source);

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
