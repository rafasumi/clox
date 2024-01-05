#include "object.h"
#include "memory.h"
#include "value.h"
#include "vm.h"

#include <string.h>

#define ALLOCATE_OBJ(type, objectType)                                         \
  (type*)allocateObject(sizeof(type), objectType)

static Obj* allocateObject(const size_t size, const ObjType type) {
  Obj* object = (Obj*)reallocate(NULL, 0, size);
  object->type = type;

  object->next = vm.objects;
  vm.objects = object;
  return object;
}

ObjString* allocateString(const size_t length) {
  ObjString* string =
      (ObjString*)allocateObject(sizeof(ObjString) + length + 1, OBJ_STRING);
  string->length = length;

  return string;
}

ObjString* copyString(const char* chars, const size_t length) {
  ObjString* string = allocateString(length);

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
