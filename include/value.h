#ifndef CLOX_VALUE_H
#define CLOX_VALUE_H

#include "common.h"

typedef double Value;

typedef struct {
  uint32_t count;
  uint32_t capacity;
  Value* values;
} ValueArray;

void initValueArray(ValueArray* array);
void writeValueArray(ValueArray* array, const Value value);
void freeValueArray(ValueArray* array);
void printValue(Value value);

#endif
