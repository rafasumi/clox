/*! \file table.c
    \brief Definitions of functions from table.h
*/

#include "table.h"
#include "memory.h"
#include "object.h"

#include <stdlib.h>
#include <string.h>

/**
 * \def TABLE_MAX_LOAD
 * \brief Maximum load factor that a table can have before having its capacity
 * adjusted
 */
#define TABLE_MAX_LOAD 0.75

void initTable(Table* table) {
  table->count = 0;
  table->capacity = 0;
  table->entries = NULL;
}

void freeTable(Table* table) {
  FREE_ARRAY(Entry, table->entries, table->capacity);
  initTable(table);
}

/**
 * \brief Searches for a key in a given entry array using ObjString* comparison.
 *
 * \param entries Pointer to the entries array
 * \param capacity Current capacity of the entries array
 * \param key Key that will be searched
 *
 * \return Pointer to the stored entry if its key is present. Otherwise, a
 * pointer to the first empty (or tombstone) position.
 */
static Entry* findEntry(Entry* entries, const size_t capacity,
                        const ObjString* key) {
  // Equivalent to hash % capacity
  uint32_t index = key->hash & (capacity - 1);
  Entry* tombstone = NULL;

  while (true) {
    Entry* entry = &entries[index];
    if (entry->key == NULL) {
      if (IS_NIL(entry->value)) {
        // Empty entry
        return tombstone != NULL ? tombstone : entry;
      } else {
        // We found a tombstone
        if (tombstone == NULL)
          tombstone = entry;
      }
    } else if (entry->key == key) {
      return entry;
    }

    index = (index + 1) & (capacity - 1);
  }
}

/**
 * \brief Adjusts the capacity of the entries array of a given hash table.
 *
 * The function also reinserts every entry from the previous array to the newly
 * allocated array so that they are placed in the appropriate position.
 *
 * \param table Pointer to the hash table
 * \param capacity Current capacity of the entries array
 */
static void adjustCapacity(Table* table, const size_t capacity) {
  Entry* entries = ALLOCATE(Entry, capacity);
  for (size_t i = 0; i < capacity; ++i) {
    entries[i].key = NULL;
    entries[i].value = NIL_VAL;
  }

  // Re-insert every entry into the new empty array
  // Tombstones aren't reinserted, thus the count is set to 0
  table->count = 0;
  for (size_t i = 0; i < table->capacity; ++i) {
    Entry* entry = &table->entries[i];
    if (entry->key == NULL)
      continue;

    Entry* dest = findEntry(entries, capacity, entry->key);
    dest->key = entry->key;
    dest->value = entry->value;
    table->count++;
  }

  FREE_ARRAY(Entry, table->entries, table->capacity);
  table->entries = entries;
  table->capacity = capacity;
}

bool tableGet(const Table* table, const ObjString* key, Value* value) {
  if (table->count == 0)
    return false;

  Entry* entry = findEntry(table->entries, table->capacity, key);
  if (entry->key == NULL)
    return false;

  *value = entry->value;
  return true;
}

bool tableSet(Table* table, ObjString* key, const Value value) {
  if (table->count + 1 > table->capacity * TABLE_MAX_LOAD) {
    size_t capacity = GROW_CAPACITY(table->capacity);
    adjustCapacity(table, capacity);
  }

  Entry* entry = findEntry(table->entries, table->capacity, key);
  bool isNewKey = entry->key == NULL;

  if (isNewKey && IS_NIL(entry->value))
    table->count++;

  entry->key = key;
  entry->value = value;

  return isNewKey;
}

bool tableDelete(Table* table, const ObjString* key) {
  if (table->count == 0)
    return false;

  Entry* entry = findEntry(table->entries, table->capacity, key);
  if (entry->key == NULL)
    return false;

  // Place a tombstone in the entry
  entry->key = NULL;
  entry->value = BOOL_VAL(true);

  return true;
}

void tableAddAll(const Table* src, Table* dest) {
  for (size_t i = 0; i < src->capacity; ++i) {
    Entry* entry = &src->entries[i];
    if (entry->key != NULL)
      tableSet(dest, entry->key, entry->value);
  }
}

ObjString* tableFindString(Table* table, const char* chars, const size_t length,
                           const uint32_t hash) {
  if (table->count == 0)
    return false;

  uint32_t index = hash % table->capacity;
  while (true) {
    Entry* entry = &table->entries[index];
    if (entry->key == NULL) {
      // Stop if we find an empty non-tombstone entry.
      if (IS_NIL(entry->value))
        return NULL;
    } else if (entry->key->length == length && entry->key->hash == hash &&
               memcmp(entry->key->chars, chars, length) == 0) {
      return entry->key;
    }

    index = (index + 1) % table->capacity;
  }
}

void tableRemoveWhite(Table* table) {
  for (size_t i = 0; i < table->capacity; ++i) {
    Entry* entry = &table->entries[i];
    if (entry->key != NULL && !entry->key->obj.isMarked)
      tableDelete(table, entry->key);
  }
}

void markTable(Table* table) {
  for (size_t i = 0; i < table->capacity; ++i) {
    Entry* entry = &table->entries[i];
    markObject((Obj*)entry->key);
    markValue(entry->value);
  }
}
