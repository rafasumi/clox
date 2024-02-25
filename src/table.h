/*! \file table.h
    \brief Functions and data types used for the implementation of a hash table
*/

#ifndef CLOX_TABLE_H
#define CLOX_TABLE_H

#include "common.h"
#include "value.h"

/**
 * \struct Entry
 * \brief Strucure used to store data for a hash table entry
 */
typedef struct {
  ObjString* key; /**< The key of the entry, which can only be a string */
  Value value;    /**< The value stored in the entry */
} Entry;

/**
 * \struct Entry
 * \brief Strucure used to represent a hash table
 */
typedef struct {
  size_t count; /**< The number of entries and tombstones in the hash table */
  size_t capacity; /**< The current capacity of the table's array of entries */
  Entry* entries; /**< Pointer to the array used to store the table's entires */
} Table;

/**
 * \brief Initializes a hash table.
 *
 * \param table Table to be initialized
 */
void initTable(Table* table);

/**
 * \brief Frees the resources of a given hash table.
 *
 * \param table Table to be freed
 */
void freeTable(Table* table);

/**
 * \brief Searches for a key in a given hash table and sets the value of \p
 * value to the stored value if the key is present.
 *
 * \param table Pointer to the hash table
 * \param key Key to be searched
 * \param value Value that will be set if the key is present
 *
 * \return Boolean value that indicates if the key is present
 */
bool tableGet(const Table* table, const ObjString* key, Value* value);

/**
 * \brief Sets the value of a key in a given hash table.
 *
 * If the key is already present in the hash table, it's value will be replaced.
 * If not, a new entry will be inserted.
 *
 * \param table Pointer to the hash table
 * \param key Key that will be inserted/updated in the hash table
 * \param value Value that will be associated with the key
 *
 * \return Boolean value that indicates if a new entry was added
 */
bool tableSet(Table* table, ObjString* key, const Value value);

/**
 * \brief Removes a given key from a given hash table if it is present.
 *
 * \param table Pointer to the hash table
 * \param key Key that should be deleted
 *
 * \return Boolean value that indicates if an entry was deleted
 */
bool tableDelete(Table* table, const ObjString* key);

/**
 * \brief Copies all entries in a hash table to another hash table.
 *
 * \param src Pointer to the source hash table
 * \param dest Pointer to the target hash table
 */
void tableAddAll(const Table* src, Table* dest);

/**
 * \brief Searches for a key in a given hash table using C string comparison.
 *
 * This function is important in the context of string interning.
 *
 * \param table Pointer to the hash table
 * \param chars String key that will be searched
 * \param length Length of the string key
 * \param hash Hash code of the string key
 *
 * \return Pointer to the stored value if the key is present. NULL pointer,
 * otherwise.
 */
ObjString* tableFindString(Table* table, const char* chars, const size_t length,
                           const uint32_t hash);

void tableRemoveWhite(Table* table);
void markTable(Table* table);

#endif
