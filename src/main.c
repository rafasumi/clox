/*! \file main.c
    \brief File with the interpreter's main function and its auxiliary functions
*/

#include "chunk.h"
#include "common.h"
#include "debug.h"
#include "vm.h"
#include <stdlib.h>
#include <string.h>

/**
 * \brief Creates a REPL session
 */
static void repl() {
  char line[1024];

  while (true) {
    printf("> ");

    if (!fgets(line, sizeof(line), stdin)) {
      printf("\n");
      break;
    }

    interpret(line);
  }
}

/**
 * \brief Reads the file with Lox source code at a given path and copies it to
 * memory.
 *
 * \param path Path to the file
 *
 * \return Pointer to the string with the source code.
 */
static char* readFile(const char* path) {
  FILE* file = fopen(path, "rb");
  if (file == NULL) {
    eprintf("Could not open file \"%s\".\n", path);
    exit(74);
  }

  fseek(file, 0L, SEEK_END);
  size_t fileSize = ftell(file);
  rewind(file);

  char* buffer = (char*)malloc(fileSize + 1);
  if (buffer == NULL) {
    eprintf("Not enough memory to read \"%s\".\n", path);
    exit(74);
  }

  size_t bytesRead = fread(buffer, sizeof(char), fileSize, file);
  if (bytesRead < fileSize) {
    eprintf("Could not read file \"%s\".\n", path);
    exit(74);
  }

  buffer[bytesRead] = '\0';

  fclose(file);

  return buffer;
}

/**
 * \brief Runs the interpreter for the source code at a given file.
 *
 * \param path Path to the file
 */
static void runFile(const char* path) {
  char* source = readFile(path);
  InterpretResult result = interpret(source);
  free(source);

  if (result == INTERPRET_COMPILE_ERROR)
    exit(65);

  if (result == INTERPRET_RUNTIME_ERROR)
    exit(70);
}

int main(int argc, char const* argv[]) {
  initVM();

  if (argc == 1) {
    repl();
  } else if (argc == 2) {
    runFile(argv[1]);
  } else {
    eprintf("Usage: clox [path]\n");
    exit(64);
  }

  freeVM();

  return 0;
}
