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
  case OBJ_STRING:
    ObjString* string = (ObjString*)object;
    // Can't use the free macros here because of the flexible array member
    reallocate(string, sizeof(ObjString) + string->length + 1, 0);
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
