#ifndef CLOX_TABLE_H
#define CLOX_TABLE_H

#include "common.h"
#include "value.h"

typedef struct {
  ObjString* key;
  Value value;
} Entry;

typedef struct {
  size_t count;
  size_t capacity;
  Entry* entries;
} Table;

void initTable(Table* table);
void freeTable(Table* table);
bool tableGet(const Table* table, const ObjString* key, Value* value);
bool tableSet(Table* table, ObjString* key, const Value value);
bool tableDelete(Table* table, const ObjString* key);
void tableAddAll(const Table* from, Table* to);

#endif
