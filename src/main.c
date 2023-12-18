#include "common.h"
#include "chunk.h"
#include "debug.h"

int main(int argc, char const* argv[]) {
  Chunk chunk;
  initChunk(&chunk);
  writeChunk(&chunk, OP_RETURN);

  uint32_t constantOffset = addConstant(&chunk, 1.2);
  writeChunk(&chunk, OP_CONSTANT);
  writeChunk(&chunk, constantOffset);
  
  disassembleChunk(&chunk, "test chunk");
  freeChunk(&chunk);
  return 0;
}
