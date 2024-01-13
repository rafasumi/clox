#include "global.h"
#include "memory.h"

void initGlobalVarArray(GlobalVarArray* array) {
  array->count = 0;
  array->capacity = 0;
  array->vars = NULL;
}

void freeGlobalVarArray(GlobalVarArray* array) {
  FREE_ARRAY(GlobalVar, array->vars, array->capacity);
  initGlobalVarArray(array);
}

void writeGlobalVarArray(GlobalVarArray* array, const GlobalVar var) {
  if (array->capacity <= array->count) {
    size_t oldCapacity = array->capacity;
    array->capacity = GROW_CAPACITY(oldCapacity);
    array->vars =
        GROW_ARRAY(GlobalVar, array->vars, oldCapacity, array->capacity);
  }

  array->vars[array->count] = var;
  ++array->count;
}
