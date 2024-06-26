/*! \file object.h
    \brief Functions and macros for managing heap-allocated objects
*/

#ifndef CLOX_OBJECT_H
#define CLOX_OBJECT_H

#include "chunk.h"
#include "common.h"
#include "table.h"
#include "value.h"

/**
 * \def OBJ_TYPE(value)
 * \brief Returns the object type of a value
 */
#define OBJ_TYPE(value) (AS_OBJ(value)->type)

/**
 * \def IS_BOUND_METHOD(value)
 * \brief Helper macro used to determine if a given value is a bound method
 */
#define IS_BOUND_METHOD(value) isObjType(value, OBJ_BOUND_METHOD)

/**
 * \def IS_CLASS(value)
 * \brief Helper macro used to determine if a given value is a class object
 */
#define IS_CLASS(value) isObjType(value, OBJ_CLASS)

/**
 * \def IS_CLOSURE(value)
 * \brief Helper macro used to determine if a given value is a closure
 */
#define IS_CLOSURE(value) isObjType(value, OBJ_CLOSURE)

/**
 * \def IS_FUNCTION(value)
 * \brief Helper macro used to determine if a given value is a Lox function
 */
#define IS_FUNCTION(value) isObjType(value, OBJ_FUNCTION)

/**
 * \def IS_INSTANCE(value)
 * \brief Helper macro used to determine if a given value is a class instance
 */
#define IS_INSTANCE(value) isObjType(value, OBJ_INSTANCE)

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

/**
 * \def AS_BOUND_METHOD(value)
 * \brief Helper macro used to get an object value as a ObjBoundMethod
 */
#define AS_BOUND_METHOD(value) ((ObjBoundMethod*)AS_OBJ(value))

/**
 * \def AS_CLASS(value)
 * \brief Helper macro used to get an object value as a ObjClass
 */
#define AS_CLASS(value) ((ObjClass*)AS_OBJ(value))

/**
 * \def AS_CLOSURE(value)
 * \brief Helper macro used to get an object value as a ObjClosure
 */
#define AS_CLOSURE(value) ((ObjClosure*)AS_OBJ(value))

/**
 * \def AS_FUNCTION(value)
 * \brief Helper macro used to get an object value as a ObjFunction
 */
#define AS_FUNCTION(value) ((ObjFunction*)AS_OBJ(value))

/**
 * \def AS_INSTANCE(value)
 * \brief Helper macro used to get an object value as a ObjInstance
 */
#define AS_INSTANCE(value) ((ObjInstance*)AS_OBJ(value))

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
  OBJ_BOUND_METHOD, /**< "Bound method" function object */
  OBJ_CLASS,        /**< Class object */
  OBJ_CLOSURE,      /**< Closure object */
  OBJ_FUNCTION,     /**< Lox function */
  OBJ_INSTANCE,     /**< Class instance object */
  OBJ_NATIVE,       /**< Native function */
  OBJ_STRING,       /**< String object */
  OBJ_UPVALUE       /**< Upvalue object */
} ObjType;

/**
 * \struct Obj
 * \brief Base struct used to store common attributes to all object types
 */
struct Obj {
  ObjType type;     /**< The object type */
  bool isMarked;    /**< Boolean flag that indicates if the object currently
                       reachable in memory */
  struct Obj* next; /**< Next object in the linked-list of allocated objects */
};

/**
 * \struct ObjFunction
 * \brief Struct used to represent a Lox bytecode function
 */
typedef struct {
  Obj obj;               /**< Obj field needed for "struct inheritance" */
  uint32_t arity;        /**< Number of parameters that the function expects */
  uint16_t upvalueCount; /**< Number of upvalues captured by the function */
  Chunk chunk;           /**< Chunk of bytecode associated with the function */
  ObjString* name;       /**< Name of the function, if there is any */
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

/**
 * \struct ObjUpvalue
 * \brief Struct used to represent an upvalue. An upvalue refers to a local
 * variable in an enclosing function, that is captured by a closure.
 */
typedef struct ObjUpvalue {
  Obj obj;         /**< Obj field needed for "struct inheritance" */
  Value* location; /**< Location of the upvalue in memory. It may be in the
                      stack (if the upvalue is open) or in the upvalue itself
                      (if it is closed). */
  Value closed;    /**< Field that stores the upvalue if it is closed */
  struct ObjUpvalue* next; /**< Pointer to the next upvalue in the intrusive
                              linked-list of upvalues */
} ObjUpvalue;

/**
 * \struct ObjClosure
 * \brief Struct used to represent a closure.
 */
typedef struct {
  Obj obj;               /**< Obj field needed for "struct inheritance" */
  ObjFunction* function; /**< Pointer to the function object associated with the
                            closure */
  ObjUpvalue** upvalues; /**< Array of upvalues captured by the closure */
  uint16_t upvalueCount; /**< Number of upvalues captured by the closure */
} ObjClosure;

/**
 * \struct ObjClass
 * \brief Struct used to represent a class.
 */
typedef struct {
  Obj obj;           /**< Obj field needed for "struct inheritance" */
  ObjString* name;   /**< Name of the class */
  Table methods;     /**< Hash table of the class' methods */
  Value initializer; /**< Value used to cache the class' initializer */
} ObjClass;

/**
 * \struct ObjInstance
 * \brief Struct used to represent a class instance.
 */
typedef struct {
  Obj obj;          /**< Obj field needed for "struct inheritance" */
  ObjClass* class_; /**< Pointer to the instance's class */
  Table fields;     /**< Hash table of the instance's fields. Fields in Lox are
                       associated with the instance, not with the class */
} ObjInstance;

/**
 * \struct ObjBoundMethod
 * \brief Struct used to represent a "bound method" object. A bound method is
 * created when the user executes a method access and saves it in a variable.
 */
typedef struct {
  Obj obj;            /**< Obj field needed for "struct inheritance" */
  Value receiver;     /**< Class instance associated with this method */
  ObjClosure* method; /**< Pointer to the method's closure */
} ObjBoundMethod;

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

/**
 * \brief Creates an empty ObjBoundMethod object.
 *
 * \param receiver The method's receiver
 * \param method Pointer to the method's closure
 *
 * \return Pointer to the created ObjBoundMethod
 */
ObjBoundMethod* newBoundMethod(Value receiver, ObjClosure* method);

/**
 * \brief Creates an empty ObjClass object.
 *
 * \param name Name of the class
 *
 * \return Pointer to the created ObjClass
 */
ObjClass* newClass(ObjString* name);

/**
 * \brief Creates an empty ObjClosure object.
 *
 * \param function Pointer to the ObjFunction that will be wrapped in the
 * ObjClosure
 *
 * \return Pointer to the created ObjClosure
 */
ObjClosure* newClosure(ObjFunction* function);

/**
 * \brief Creates an empty ObjFunction object.
 *
 * \return Pointer to the created ObjFunction
 */
ObjFunction* newFunction();

/**
 * \brief Creates an empty ObjInstance object.
 *
 * \param class_ Pointer to the instance's class
 *
 * \return Pointer to the created ObjInstance
 */
ObjInstance* newInstance(ObjClass* class_);

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

/**
 * \brief Creates an empty ObjUpvalue object.
 *
 * \param slot Pointer to the slot in the stack where the upvalue resides.
 *
 * \return Pointer to the created ObjUpvalue
 */
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
