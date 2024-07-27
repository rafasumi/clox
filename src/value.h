/*! \file value.h
    \brief Functions and data types for manipulating arrays of constant values.
*/

#ifndef CLOX_VALUE_H
#define CLOX_VALUE_H

#include "common.h"
#include <string.h>

// ------- Forward declarations -------
typedef struct Obj Obj;
typedef struct ObjString ObjString;
// ------------------------------------

#ifdef NAN_BOXING

/**
 * \def SIGN_BIT
 * \brief Bit mask for the IEEE 754 sign bit
 */
#define SIGN_BIT ((uint64_t)0x8000000000000000)

/**
 * \def QNAN
 * \brief Bit mask for a quiet NaN
 */
#define QNAN ((uint64_t)0x7ffc000000000000)

/**
 * \def TAG_UNDEF
 * \brief Bit tag for an undefined value
 */
#define TAG_UNDEF 0

/**
 * \def TAG_NIL
 * \brief Bit tag for a nil value
 */
#define TAG_NIL 1

/**
 * \def TAG_FALSE
 * \brief Bit tag for a false boolean value
 */
#define TAG_FALSE 2

/**
 * \def TAG_TRUE
 * \brief Bit tag for a true boolean value
 */
#define TAG_TRUE 3

/**
 * \var Value
 * \brief Type definition for a value in the value stack
 */
typedef uint64_t Value;

/**
 * \def IS_BOOL(value)
 * \brief Helper macro to determine if a Value is of type bool
 */
#define IS_BOOL(value) (((value) | 1) == TRUE_VAL)

/**
 * \def IS_NIL(value)
 * \brief Helper macro to determine if a Value is of type nil
 */
#define IS_NIL(value) ((value) == NIL_VAL)

/**
 * \def IS_NUMBER(value)
 * \brief Helper macro to determine if a Value is of type number
 */
#define IS_NUMBER(value) (((value)&QNAN) != QNAN)

/**
 * \def IS_OBJ(value)
 * \brief Helper macro to determine if a Value is of type object
 */
#define IS_OBJ(value) (((value) & (QNAN | SIGN_BIT)) == (QNAN | SIGN_BIT))

/**
 * \def IS_UNDEFINED(value)
 * \brief Helper macro to determine if the type of a Value variable is VAL_UNDEF
 */
#define IS_UNDEFINED(value) ((value) == UNDEFINED_VAL)

/**
 * \def AS_BOOL(value)
 * \brief Gets the stored Value as a boolean value
 */
#define AS_BOOL(value) ((value) == TRUE_VAL)

/**
 * \def AS_NUMBER(value)
 * \brief Gets the stored Value as a numeric value
 */
#define AS_NUMBER(value) valueToNum(value)

/**
 * \def AS_OBJ(value)
 * \brief Gets the stored Value as an object value
 */
#define AS_OBJ(value) ((Obj*)(uintptr_t)((value) & ~(SIGN_BIT | QNAN)))

/**
 * \def BOOL_VAL(value)
 * \brief Creates a Value of type bool with a specified value
 */
#define BOOL_VAL(value) ((value) ? TRUE_VAL : FALSE_VAL)

/**
 * \def FALSE_VAL
 * \brief Binary representation for a false boolean value
 */
#define FALSE_VAL ((Value)(uint64_t)(QNAN | TAG_FALSE))

/**
 * \def TRUE_VAL
 * \brief Binary representation for a true boolean value
 */
#define TRUE_VAL ((Value)(uint64_t)(QNAN | TAG_TRUE))

/**
 * \def NIL_VAL
 * \brief Creates a Value of type nil
 */
#define NIL_VAL ((Value)(uint64_t)(QNAN | TAG_NIL))

/**
 * \def NUMBER_VAL(value)
 * \brief Creates a Value of type number with a specified value
 */
#define NUMBER_VAL(num) numToValue(num)

/**
 * \def OBJ_VAL(value)
 * \brief Creates a Value of type object with a specified object pointer
 */
#define OBJ_VAL(obj) (Value)(SIGN_BIT | QNAN | (uint64_t)(uintptr_t)(obj))

/**
 * \def UNDEFINED_VAL(value)
 * \brief Creates a Value of type object with a specified object pointer
 */
#define UNDEFINED_VAL ((Value)(uint64_t)(QNAN | TAG_UNDEF))

/**
 * \brief Converts a Value to a double
 *
 * \param value Value to be converted
 *
 * \return Double representation of the value
 */
static inline double valueToNum(const Value value) {
  double num;
  memcpy(&num, &value, sizeof(Value));
  return num;
}

/**
 * \brief Converts a double to a Value
 *
 * \param num Number to be converted
 *
 * \return Value representation of the number
 */
static inline Value numToValue(const double num) {
  Value value;
  memcpy(&value, &num, sizeof(double));
  return value;
}

#else

/**
 * \enum ValueType
 * \brief Enum for all possible value types in Lox
 */
typedef enum {
  VAL_BOOL,     /**< Boolean value */
  VAL_NIL,      /**< Nil value */
  VAL_NUMBER,   /**< Numeric value */
  VAL_OBJ,      /**< Heap-allocated value */
  VAL_UNDEFINED /**< Internal use only. Represents a non-initialized variable */
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
 * \def IS_UNDEFINED(value)
 * \brief Helper macro to determine if the type of a Value variable is VAL_UNDEF
 */
#define IS_UNDEFINED(value) ((value).type == VAL_UNDEFINED)

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
 * \def OBJ_VAL(value)
 * \brief Creates a Value of type object with a specified object pointer
 */
#define OBJ_VAL(object) ((Value){VAL_OBJ, {.obj = (Obj*)object}})

/**
 * \def UNDEFINED_VAL(value)
 * \brief Creates a Value of type object with a specified object pointer
 */
#define UNDEFINED_VAL ((Value){VAL_UNDEFINED, {.number = 0}})

#endif

/**
 * \struct ValueArray
 * \brief  Strucure that contains an array of constant values and its metadata.
 */
typedef struct {
  size_t count;    /**< The number of values in the array */
  size_t capacity; /**< The current capacity of the array */
  Value* values;   /**< Pointer to the values in memory */
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
