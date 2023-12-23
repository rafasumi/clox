/*! \file value.h
    \brief Functions and data types for manipulating arrays of constant values.
*/

#ifndef CLOX_VALUE_H
#define CLOX_VALUE_H

#include "common.h"

/**
 * \var Value
 * \brief Type definition for a constant value.
 */
typedef double Value;

/**
 * \struct ValueArray
 * \brief  Strucure that contains a array of constant values and its metadata.
 */
typedef struct {
  size_t count;
  size_t capacity;
  Value* values;
} ValueArray;

/**
 * \brief Initializes the resources of a given value array.
 *
 * \param array Array to be initialized
 */
void initValueArray(ValueArray* array);

/**
 * \brief Adds a value to a given array.
 *
 * \param array Array where the value will be inserted
 * \param value Constant value that will be added to \p array
 */
void writeValueArray(ValueArray* array, const Value value);

/**
 * \brief Frees the resources of a given array.
 *
 * \param array Array to be freed
 */
void freeValueArray(ValueArray* array);

/**
 * \brief Prints a constant value.
 *
 * \param value Value to be printed
 */
void printValue(const Value value);

#endif
