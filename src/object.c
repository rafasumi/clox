/*! \file object.c
    \brief Definitions of functions from object.h
*/

#include "object.h"
#include "memory.h"
#include "table.h"
#include "value.h"
#include "vm.h"

#include <string.h>

/**
 * \def ALLOCATE_OBJ(type, objectType)
 * \brief Helper macro to allocate an object of a given type
 */
#define ALLOCATE_OBJ(type, objectType)                                         \
  (type*)allocateObject(sizeof(type), objectType)

/**
 * \brief Allocates a generic Obj with a given size and type.
 *
 * This function also updated the linked-list of heap-allocated objects.
 *
 * \param size Size of the object that will be allocated
 * \param type Type of the object that will be allocated
 *
 * \return Pointer to the allocated Object
 *
 */
static Obj* allocateObject(const size_t size, const ObjType type) {
  Obj* object = (Obj*)reallocate(NULL, 0, size);
  object->type = type;

  object->next = vm.objects;
  vm.objects = object;
  return object;
}

ObjClosure* newClosure(ObjFunction* function) {
  ObjUpvalue** upvalues = ALLOCATE(ObjUpvalue*, function->upvalueCount);
  for (uint16_t i = 0; i < function->upvalueCount; ++i) {
    upvalues[i] = NULL;
  }
  
  ObjClosure* closure = ALLOCATE_OBJ(ObjClosure, OBJ_CLOSURE);
  closure->function = function;
  closure->upvalues = upvalues;
  closure->upvalueCount = function->upvalueCount;
  return closure;
}

ObjFunction* newFunction() {
  ObjFunction* function = ALLOCATE_OBJ(ObjFunction, OBJ_FUNCTION);
  function->arity = 0;
  function->upvalueCount = 0;
  function->name = NULL;
  initChunk(&function->chunk);
  return function;
}

ObjNative* newNative(const NativeFn function, const uint32_t arity) {
  ObjNative* native = ALLOCATE_OBJ(ObjNative, OBJ_NATIVE);
  native->function = function;
  native->arity = arity;

  return native;
}

/**
 * \brief Produces the hash code for a given string using the FNV-1a hash
 * function.
 *
 * \param key Pointer to the string
 * \param length Length of the string
 *
 * \return 32-bit integer hash code
 */
static uint32_t hashString(const char* key, const size_t length) {
  uint32_t hash = 2166136261u;
  for (size_t i = 0; i < length; i++) {
    hash ^= (uint8_t)key[i];
    hash *= 16777619;
  }
  return hash;
}

/**
 * \brief Allocates an ObjString with a given size.
 *
 * The function assumes that the string will be copied into the struct after
 * allocation.
 *
 * \param length Length of the ObjString to be allocated
 *
 * \return Pointer to the allocated ObjString
 */
static ObjString* allocateString(char* chars, const size_t length,
                                 const uint32_t hash) {
  ObjString* string = ALLOCATE_OBJ(ObjString, OBJ_STRING);
  string->chars = chars;
  string->length = length;
  string->hash = hash;

  tableSet(&vm.strings, string, NIL_VAL);
  return string;
}

ObjString* takeString(char* chars, const size_t length) {
  uint32_t hash = hashString(chars, length);

  ObjString* interned = tableFindString(&vm.strings, chars, length, hash);
  if (interned != NULL) {
    FREE_ARRAY(char, chars, length + 1);
    return interned;
  }

  return allocateString(chars, length, hash);
}

ObjString* copyString(const char* chars, const size_t length) {
  uint32_t hash = hashString(chars, length);

  ObjString* interned = tableFindString(&vm.strings, chars, length, hash);
  if (interned != NULL) {
    return interned;
  }

  char* heapChars = ALLOCATE(char, length + 1);
  memcpy(heapChars, chars, length);
  heapChars[length] = '\0';

  return allocateString(heapChars, length, hash);
}

ObjUpvalue* newUpvalue(Value* slot) {
  ObjUpvalue* upvalue = ALLOCATE_OBJ(ObjUpvalue, OBJ_UPVALUE);
  upvalue->location = slot;

  return upvalue;
}

/**
 * \brief Auxiliary function used to print a Lox function
 *
 * \param function Pointer to the function
 */
static void printFunction(ObjFunction* function) {
  if (function->name == NULL) {
    printf("<script>");
    return;
  }

  printf("<fn %s>", function->name->chars);
}

void printObject(const Value value) {
  switch (OBJ_TYPE(value)) {
  case OBJ_CLOSURE:
    printFunction(AS_CLOSURE(value)->function);
    break;
  case OBJ_FUNCTION:
    printFunction(AS_FUNCTION(value));
    break;
  case OBJ_NATIVE:
    printf("<native fn>");
    break;
  case OBJ_STRING:
    printf("%s", AS_CSTRING(value));
    break;
  case OBJ_UPVALUE:
    printf("upvalue");
    break;
  }
}
