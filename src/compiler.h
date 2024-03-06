/*! \file compiler.h
    \brief Functions and datatypes used by clox's compiler module.
*/

#ifndef CLOX_COMPILER_H
#define CLOX_COMPILER_H

#include "vm.h"

/**
 * \brief Compiles a string of source code into a chunk of bytecode to be
 * executed by the clox virtual machine.
 *
 * The compiled chunk is part of a function.
 *
 * \param source Pointer to the source code
 *
 * \return Pointer to the function that was compiled
 */
ObjFunction* compile(const char* source);

/**
 * \brief Marks all function objects used by compilers as reachable for garbage
 * collection.
 * 
 */
void markCompilerRoots();

#endif
