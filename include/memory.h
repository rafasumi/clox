/*! \file memory.h
    \brief Functions and macros for memory management
*/

#ifndef CLOX_MEMORY_H
#define CLOX_MEMORY_H

#include "common.h"
#include "object.h"

/**
 * \def ALLOCATE(type, count)
 * \brief Allocates an array of a given type with a specified size
 */
#define ALLOCATE(type, count) (type*)reallocate(NULL, 0, sizeof(type) * count)

/**
 * \def FREE(type, pointer)
 * \brief Frees an array of a given type
 */
#define FREE(type, pointer) reallocate(pointer, sizeof(type), 0);

/**
 * \def GROW_CAPACITY(capacity)
 * \brief Determines the next capacity for an array given its current capacity
 */
#define GROW_CAPACITY(capacity) ((capacity) < 8 ? 8 : (capacity)*2)

/**
 * \def GROW_ARRAY(type, pointer, oldCount, newCount)
 * \brief Wrapper to grow an array of a specified type to have a new size
 */
#define GROW_ARRAY(type, pointer, oldCount, newCount)                          \
  (type*)reallocate(pointer, sizeof(type) * (oldCount),                        \
                    sizeof(type) * (newCount))

/**
 * \def FREE_ARRAY(type, pointer, oldCount)
 * \brief Wrapper to free an existing an array
 */
#define FREE_ARRAY(type, pointer, oldCount)                                    \
  reallocate(pointer, sizeof(type) * (oldCount), 0)

/**
 * \brief Reallocates a given memory location to have a given size
 *
 * \param pointer Pointer to the memory location
 * \param oldSize Previous size of the memory location
 * \param newSize Size of the reallocated memory location
 *
 * \return Pointer to the newly reallocated memory location
 */
void* reallocate(void* pointer, const size_t oldSize, const size_t newSize);

/**
 * \brief Frees all allocated objects in the virtual machine
 * 
 */
void freeObjects();

#endif
