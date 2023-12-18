#include "value.h"
#include "memory.h"
#include <stdio.h>

void initValueArray(ValueArray* array) {
  array->count = 0;
  array->capacity = 0;
  array->values = NULL;
}

void writeValueArray(ValueArray* array, const Value value) {
  if (array->capacity <= array->count) {
    uint32_t oldCapacity = array->capacity;
    array->capacity = GROW_CAPACITY(oldCapacity);
    array->values =
        GROW_ARRAY(Value, array->values, oldCapacity, array->capacity);
  }

  array->values[array->count] = value;
  ++array->count;
}

void freeValueArray(ValueArray* array) {
  FREE_ARRAY(Value, array->values, array->capacity);
  initValueArray(array);
}

void printValue(Value value) {
  printf("%g", value);
}