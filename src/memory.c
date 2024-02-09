/*! \file memory.c
    \brief Definitions of functions from memory.h
*/

#include "memory.h"
#include "vm.h"

#include <stdlib.h>

void* reallocate(void* pointer, const size_t oldSize, const size_t newSize) {
  if (newSize == 0) {
    free(pointer);
    return NULL;
  }

  void* result = realloc(pointer, newSize);
  if (result == NULL) {
    exit(1);
  }

  return result;
}

/**
 * \brief Frees a single heap-allocated object based on its object type
 *
 * \param object Object to be freed
 */
static void freeObject(Obj* object) {
  switch (object->type) {
  case OBJ_FUNCTION: {
    ObjFunction* function = (ObjFunction*)object;
    freeChunk(&function->chunk);
    FREE(ObjFunction, object);
    break;
  }
  case OBJ_NATIVE:
    FREE(ObjNative, object);
    break;
  case OBJ_STRING:
    ObjString* string = (ObjString*)object;
    FREE_ARRAY(char, string->chars, string->length + 1);
    FREE(ObjString, object);
    break;
  }
}

void freeObjects() {
  Obj* object = vm.objects;
  while (object) {
    Obj* next = object->next;
    freeObject(object);
    object = next;
  }
}
