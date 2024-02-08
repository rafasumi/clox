/*! \file object.h
    \brief Functions and macros for managing heap-allocated objects
*/

#ifndef CLOX_OBJECT_H
#define CLOX_OBJECT_H

#include "chunk.h"
#include "common.h"
#include "value.h"

/**
 * \def OBJ_TYPE(value)
 * \brief Returns the object type of a value
 */
#define OBJ_TYPE(value) (AS_OBJ(value)->type)

#define IS_FUNCTION(value) isObjType(value, OBJ_FUNCTION)
#define IS_NATIVE(value) isObjType(value, OBJ_NATIVE)

/**
 * \def IS_STRING(value)
 * \brief Helper macro used to determine if a given value is a string
 */
#define IS_STRING(value) isObjType(value, OBJ_STRING)

#define AS_FUNCTION(value) ((ObjFunction*)AS_OBJ(value))
#define AS_NATIVE(value) ((ObjNative*)AS_OBJ(value))

/**
 * \def AS_STRING(value)
 * \brief Helper macro used to get an object value as a ObjString
 */
#define AS_STRING(value) ((ObjString*)AS_OBJ(value))

/**
 * \def AS_STRING(value)
 * \brief Helper macro used to get the C string stored in an object value
 */
#define AS_CSTRING(value) (((ObjString*)AS_OBJ(value))->chars)

/**
 * \enum ObjType
 * \brief Enum for all types of object values
 */
typedef enum {
  OBJ_FUNCTION,
  OBJ_NATIVE,
  OBJ_STRING, /**< String object */
} ObjType;

/**
 * \struct Obj
 * \brief Base struct used to store common attributes to all object types
 */
struct Obj {
  ObjType type;     /**< The object type */
  struct Obj* next; /**< Next object in the linked-list of allocated objects */
};

typedef struct {
  Obj obj;
  uint32_t arity;
  Chunk chunk;
  ObjString* name;
} ObjFunction;

typedef bool (*NativeFn)(const uint8_t argCount, Value* args);

typedef struct {
  Obj obj;
  uint32_t arity;
  NativeFn function;
} ObjNative;

/**
 * \struct ObjString
 * \brief Struct used to represent a string object
 */
struct ObjString {
  Obj obj;       /**< Obj field needed for "struct inheritance" */
  size_t length; /**< The length of the allocated string */
  char* chars;   /**< Pointer to the string in the heap */
  uint32_t hash; /**< Hash code of the string, needed for use in hash tables */
};

ObjFunction* newFunction();
ObjNative* newNative(NativeFn function, const uint32_t arity);

/**
 * \brief Allocates an ObjString and takes ownership of an already allocated
 * string.
 *
 * Because of string interning, the allocation will only happen if there isn't
 * already an allocated ObjString with the string specified in \p chars.
 *
 * \param chars Pointer to the string whose ownership will be taken
 * \param length Length of the string that will taken
 *
 * \return Pointer to the allocated ObjString
 */
ObjString* takeString(char* chars, const size_t length);

/**
 * \brief Allocates an ObjString and a string, and copies an existing string
 * into it.
 *
 * Because of string interning, the allocation will only happen if there isn't
 * already an allocated ObjString with the string specified in \p chars.
 *
 * \param chars Pointer to the string that will be copied
 * \param length Length of the string that will be copied
 *
 * \return Pointer to the allocated ObjString
 */
ObjString* copyString(const char* chars, const size_t length);

/**
 * \brief Prints an object value.
 *
 * \param value Constant value to be printed. It is assumed that this value is
 * of type VAL_OBJ
 */
void printObject(const Value value);

/**
 * \brief Determines if a given value has a specified object type
 *
 * \param value Value whose type will be tested
 * \param type Target ObjType
 *
 * \return Boolean value that indicates if the value has the specified type
 */
static inline bool isObjType(const Value value, const ObjType type) {
  return IS_OBJ(value) && AS_OBJ(value)->type == type;
}

#endif
