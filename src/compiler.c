#include "compiler.h"
#include "common.h"
#include "scanner.h"

void compile(const char* source) {
  initScanner(source);

  uint32_t line = -1;
  while (true) {
    Token token = scanToken();

    if (token.line != line) {
      printf("%4d ", token.line);
      line = token.line;
    } else {
      printf("   | ");
    }

    printf("%2d '%.*s'\n", token.type, (int)token.length, token.start);

    if (token.type == TOKEN_EOF)
      break;
  }
}
