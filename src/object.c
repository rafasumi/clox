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

void printObject(const Value value) {
  switch (OBJ_TYPE(value)) {
  case OBJ_STRING:
    printf("%s", AS_CSTRING(value));
    break;
  }
}