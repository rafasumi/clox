/*! \file object.c
    \brief Definitions of functions from object.h
*/

#include "object.h"
#include "memory.h"
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

uint32_t hashString(const char* key, const size_t length) {
  uint32_t hash = 2166136261u;
  for (size_t i = 0; i < length; i++) {
    hash ^= (uint8_t)key[i];
    hash *= 16777619;
  }
  return hash;
}

ObjString* allocateString(const size_t length) {
  ObjString* string =
      (ObjString*)allocateObject(sizeof(ObjString) + length + 1, OBJ_STRING);
  string->length = length;

  return string;
}

ObjString* copyString(const char* chars, const size_t length) {
  ObjString* string = allocateString(length);
  string->hash = hashString(chars, length);

  memcpy(string->chars, chars, length);
  string->chars[length] = '\0';

  return string;
}

void printObject(const Value value) {
  switch (OBJ_TYPE(value)) {
  case OBJ_STRING:
    printf("%s", AS_CSTRING(value));
    break;
  }
}
