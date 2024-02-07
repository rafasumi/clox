/*! \file compiler.h
    \brief Functions and datatypes used by clox's compiler module.
*/

#ifndef CLOX_COMPILER_H
#define CLOX_COMPILER_H

#include "vm.h"

/**
 * \brief Compiles a string of source code into a chunk of bytecode to be
 * executed by the clox virtual machine
 *
 * \param source Pointer to the source code
 * \param chunk Pointer to the Chunk where the bytecodes must be added
 *
 * \return Boolean value that indicates if the compiling process was successful
 */
ObjFunction* compile(const char* source);

#endif
