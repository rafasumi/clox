/*! \file value.h
    \brief Functions and data types for manipulating arrays of constant values.
*/

#ifndef CLOX_VALUE_H
#define CLOX_VALUE_H

#include "common.h"

// ------- Forward declarations -------
typedef struct Obj Obj;
typedef struct ObjString ObjString;
// ------------------------------------

/**
 * \enum ValueType
 * \brief Enum for all possible value types in Lox
 */
typedef enum {
  VAL_BOOL,   /**< Boolean value */
  VAL_NIL,    /**< Nil value */
  VAL_NUMBER, /**< Numeric value */
  VAL_OBJ     /**< Heap-allocated value */
} ValueType;

/**
 * \struct Value
 * \brief Struct used to represent a value stored in the value stack
 */
typedef struct {
  ValueType type; /**< Type of the value */
  union {
    bool boolean;
    double number;
    Obj* obj;
  } as; /**< Tagged union used to store the actual value with the appropriate
           type */
} Value;

/**
 * \def IS_BOOL(value)
 * \brief Helper macro to determine if a Value is of type bool
 */
#define IS_BOOL(value) ((value).type == VAL_BOOL)

/**
 * \def IS_NIL(value)
 * \brief Helper macro to determine if a Value is of type nil
 */
#define IS_NIL(value) ((value).type == VAL_NIL)

/**
 * \def IS_NUMBER(value)
 * \brief Helper macro to determine if a Value is of type number
 */
#define IS_NUMBER(value) ((value).type == VAL_NUMBER)

/**
 * \def IS_OBJ(value)
 * \brief Helper macro to determine if a Value is of type object
 */
#define IS_OBJ(value) ((value).type == VAL_OBJ)

/**
 * \def AS_BOOL(value)
 * \brief Gets the stored Value as a boolean value
 */
#define AS_BOOL(value) ((value).as.boolean)

/**
 * \def AS_NUMBER(value)
 * \brief Gets the stored Value as a numeric value
 */
#define AS_NUMBER(value) ((value).as.number)

/**
 * \def AS_OBJ(value)
 * \brief Gets the stored Value as an object value
 */
#define AS_OBJ(value) ((value).as.obj)

/**
 * \def BOOL_VAL(value)
 * \brief Creates a Value of type bool with a specified value
 */
#define BOOL_VAL(value) ((Value){VAL_BOOL, {.boolean = value}})

/**
 * \def NIL_VAL
 * \brief Creates a Value of type nil
 */
#define NIL_VAL ((Value){VAL_NIL, {.number = 0}})

/**
 * \def NUMBER_VAL(value)
 * \brief Creates a Value of type number with a specified value
 */
#define NUMBER_VAL(value) ((Value){VAL_NUMBER, {.number = value}})

/**
 * \def NUMBER_VAL(value)
 * \brief Creates a Value of type object with a specified object pointer
 */
#define OBJ_VAL(object) ((Value){VAL_OBJ, {.obj = (Obj*)object}})

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
 * \brief Determines if two Values are equal
 *
 * \param a First Value to be compared
 * \param b Second Value to be compared
 *
 * \return Boolean value that indicates if the values are equal
 */
bool valuesEqual(const Value a, const Value b);

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
