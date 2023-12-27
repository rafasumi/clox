/*! \file compiler.h
    \brief Functions and datatypes used by clox's compiler module.
*/

#ifndef CLOX_COMPILER_H
#define CLOX_COMPILER_H

#include "vm.h"

bool compile(const char* source, Chunk* chunk);

#endif
