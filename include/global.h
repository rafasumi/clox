#ifndef CLOX_GLOBAL_H
#define CLOX_GLOBAL_H

#include "common.h"
#include "object.h"
#include "value.h"

typedef struct {
  ObjString* identifier;
  Value value;
} GlobalVar;

typedef struct {
  size_t capacity;
  size_t count;
  GlobalVar* vars;
} GlobalVarArray;

#define UNDEFINED_GLOBAL(identifier) ((GlobalVar){identifier, UNDEFINED_VAL})

void initGlobalVarArray(GlobalVarArray* array);
void freeGlobalVarArray(GlobalVarArray* array);
void writeGlobalVarArray(GlobalVarArray* array, const GlobalVar var);

#endif
