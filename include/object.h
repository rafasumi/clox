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

#define IS_CLOSURE(value) isObjType(value, OBJ_CLOSURE)

/**
 * \def IS_FUNCTION(value)
 * \brief Helper macro used to determine if a given value is a Lox function
 */
#define IS_FUNCTION(value) isObjType(value, OBJ_FUNCTION)

/**
 * \def IS_NATIVE(value)
 * \brief Helper macro used to determine if a given value is a native function
 */
#define IS_NATIVE(value) isObjType(value, OBJ_NATIVE)

/**
 * \def IS_STRING(value)
 * \brief Helper macro used to determine if a given value is a string
 */
#define IS_STRING(value) isObjType(value, OBJ_STRING)

#define AS_CLOSURE(value) ((ObjClosure*)AS_OBJ(value))

/**
 * \def AS_FUNCTION(value)
 * \brief Helper macro used to get an object value as a ObjFunction
 */
#define AS_FUNCTION(value) ((ObjFunction*)AS_OBJ(value))

/**
 * \def AS_NATIVE(value)
 * \brief Helper macro used to get an object value as a ObjNative
 */
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
  OBJ_CLOSURE,
  OBJ_FUNCTION, /**< Lox function */
  OBJ_NATIVE,   /**< Native function */
  OBJ_STRING,   /**< String object */
  OBJ_UPVALUE
} ObjType;

/**
 * \struct Obj
 * \brief Base struct used to store common attributes to all object types
 */
struct Obj {
  ObjType type;     /**< The object type */
  struct Obj* next; /**< Next object in the linked-list of allocated objects */
};

/**
 * \struct ObjFunction
 * \brief Struct used to represent a Lox bytecode function
 */
typedef struct {
  Obj obj;         /**< Obj field needed for "struct inheritance" */
  uint32_t arity;  /**< Number of parameters that the function expects */
  uint16_t upvalueCount;
  Chunk chunk;     /**< Chunk of bytecode associated with the function */
  ObjString* name; /**< Name of the function, if there is any */
} ObjFunction;

/**
 * \var NativeFn
 * \brief Type definition for a native function that can be called in Lox
 */
typedef bool (*NativeFn)(const uint8_t argCount, Value* args);

/**
 * \struct ObjNative
 * \brief Struct used to represent a native function
 */
typedef struct {
  Obj obj;           /**< Obj field needed for "struct inheritance" */
  uint32_t arity;    /**< Number of parameters that the function expects */
  NativeFn function; /**< Pointer to the C function */
} ObjNative;

typedef struct ObjUpvalue {
  Obj obj;
  Value* location;
  Value closed;
  struct ObjUpvalue* next;
} ObjUpvalue;

typedef struct {
  Obj obj;
  ObjFunction* function;
  ObjUpvalue** upvalues;
  uint16_t upvalueCount;
} ObjClosure;

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

ObjClosure* newClosure(ObjFunction* function);

/**
 * \brief Creates an empty ObjFunction object.
 *
 * \return Pointer to the created ObjFunction
 */
ObjFunction* newFunction();

/**
 * \brief Creates a new ObjNative object.
 *
 * \param function Pointer to the C function
 * \param arity Expected number of parameters
 *
 * \return Pointer to the created ObjNative
 */
ObjNative* newNative(const NativeFn function, const uint32_t arity);

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

ObjUpvalue* newUpvalue(Value* slot);

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
