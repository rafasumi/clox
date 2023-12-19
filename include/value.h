#ifndef CLOX_VALUE_H
#define CLOX_VALUE_H

#include "common.h"

typedef double Value;

typedef struct {
  size_t count;
  size_t capacity;
  Value* values;
} ValueArray;

void initValueArray(ValueArray* array);
void writeValueArray(ValueArray* array, const Value value);
void freeValueArray(ValueArray* array);
void printValue(Value value);

#endif
