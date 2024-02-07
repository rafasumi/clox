/*! \file global.h
    \brief Functions and data types for handling the storage of global variables
*/

#ifndef CLOX_GLOBAL_H
#define CLOX_GLOBAL_H

#include "common.h"
#include "object.h"
#include "value.h"

/**
 * \struct GlobalVar
 * \brief Strucure that represents a global variable.
 */
typedef struct {
  ObjString* identifier; /**< The variable's identifier */
  Value value;           /**< The value of the variable */
  bool isConst; /**< Flag that indicates if it is a constant variable */
} GlobalVar;

/**
 * \struct GlobalVarArray
 * \brief Strucure that contains an array of global variable values and its
 * metadata.
 */
typedef struct {
  size_t capacity; /**< The number of values in the array */
  size_t count;    /**< The current capacity of the array */
  GlobalVar* vars; /**< Pointer to the global values in memory */
} GlobalVarArray;

/**
 * \def UNDEFINED_GLOBAL(identifier, isConst)
 * \brief Helper that creates an uninitialized global variable
 */
#define UNDEFINED_GLOBAL(identifier, isConst)                                  \
  ((GlobalVar){identifier, UNDEFINED_VAL, isConst})

#define NEW_GLOBAL(identifier, value, isConst)                                 \
  ((GlobalVar){identifier, value, isConst})

/**
 * \brief Initializes the resources of a given global values array.
 *
 * \param array Array to be initialized
 */
void initGlobalVarArray(GlobalVarArray* array);

/**
 * \brief Frees the resources of a given array.
 *
 * \param array Array to be freed
 */
void freeGlobalVarArray(GlobalVarArray* array);

/**
 * \brief Adds a value to a given array.
 *
 * \param array Array where the value will be inserted
 * \param var Global value that will be added to \p array
 */
void writeGlobalVarArray(GlobalVarArray* array, const GlobalVar var);

#endif
