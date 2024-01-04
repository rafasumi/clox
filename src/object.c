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

  return object;
}

static ObjString* allocateString(char* chars, const size_t length) {
  ObjString* string = ALLOCATE_OBJ(ObjString, OBJ_STRING);
  string->length = length;
  string->chars = chars;

  return string;
}

ObjString* takeString(char* chars, const size_t length) {
  return allocateString(chars, length);
}

ObjString* copyString(const char* chars, const size_t length) {
  char* heapChars = ALLOCATE(char, length + 1);
  memcpy(heapChars, chars, length);
  heapChars[length] = '\0';

  return allocateString(heapChars, length);
}

void printObject(const Value value) {
  switch (OBJ_TYPE(value)) {
  case OBJ_STRING:
    printf("%s", AS_CSTRING(value));
    break;
  }
}
