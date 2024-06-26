/*! \file debug.c
    \brief Definitions of functions from debug.h
*/

#include "debug.h"
#include "object.h"
#include "value.h"
#include "vm.h"

void disassembleChunk(const Chunk* chunk, const char* name) {
  printf("=== %s ===\n", name);

  for (size_t offset = 0; offset < chunk->count;) {
    offset = disassembleInstruction(chunk, offset);
  }
}

/**
 * \brief Display a simple instruction at a given offset. The instruction must
 * have no operands.
 *
 * \param name Name of the instruction
 * \param offset Offset of the instruction within the bytecode array
 *
 * \return Offset of the next instruction
 */
static size_t simpleInstruction(const char* name, const size_t offset) {
  printf("%s\n", name);

  return offset + 1;
}

/**
 * \brief Display an instruction at a given offset along with the instruction's
 * 8-bit operand.
 *
 * \param name Name of the instruction
 * \param chunk Pointer to the chunk of bytecode that contains the instruction
 * \param offset Offset of the instruction within the bytecode array
 *
 * \return Offset of the next instruction
 */
static size_t byteInstruction(const char* name, const Chunk* chunk,
                              const size_t offset) {
  uint8_t slot = chunk->code[offset + 1];
  printf("%-21s %4d\n", name, slot);

  return offset + 2;
}

/**
 * \brief Display an instruction at a given offset along with the instruction's
 * 16-bit operand.
 *
 * \param name Name of the instruction
 * \param chunk Pointer to the chunk of bytecode that contains the instruction
 * \param offset Offset of the instruction within the bytecode array
 *
 * \return Offset of the next instruction
 */
static size_t byteShortInstruction(const char* name, const Chunk* chunk,
                                   const size_t offset) {
  uint32_t slot = (chunk->code[offset + 2] << 8) | chunk->code[offset + 1];
  printf("%-21s %4d\n", name, slot);

  return offset + 3;
}

/**
 * \brief Display an instruction that applies a jump. Instruction like these
 * take a 16-bit operand.
 *
 * \param name Name of the instruction
 * \param sign Integer value indicating the sign of the jump. If it is positive,
 * then it is a forward jump. Otherwise, it is backwards.
 * \param chunk Pointer to the chunk of bytecode that contains the instruction
 * \param offset Offset of the instruction within the bytecode array
 *
 * \return Offset of the next instruction
 */
static size_t jumpInstruction(const char* name, const int8_t sign,
                              const Chunk* chunk, const size_t offset) {
  uint16_t jump =
      (uint16_t)((chunk->code[offset + 2] << 8) | chunk->code[offset + 1]);
  printf("%-21s %4ld -> %ld\n", name, offset, offset + 3 + sign * jump);

  return offset + 3;
}

/**
 * \brief Display an instruction which use the constants array. This instruction
 * has one 8-bit operand.
 *
 * \param name Name of the instruction
 * \param chunk Pointer to the chunk of bytecode that contains the instruction
 * \param offset Offset of the instruction within the bytecode array
 *
 * \return Offset of the next instruction
 */
static size_t constantInstruction(const char* name, const Chunk* chunk,
                                  const size_t offset) {
  uint8_t constantOffset = chunk->code[offset + 1];
  printf("%-21s %4d '", name, constantOffset);

  printValue(chunk->constants.values[constantOffset]);
  printf("'\n");

  return offset + 2;
}

/**
 * \brief Display an instruction which use the constants array. This instruction
 * has three 8-bit operands.
 *
 * \param name Name of the instruction
 * \param chunk Pointer to the chunk of bytecode that contains the instruction
 * \param offset Offset of the instruction within the bytecode array
 *
 * \return Offset of the next instruction
 */
static size_t constantLongInstruction(const char* name, const Chunk* chunk,
                                      const size_t offset) {
  uint32_t constantOffset = (chunk->code[offset + 3] << 16) |
                            (chunk->code[offset + 2] << 8) |
                            chunk->code[offset + 1];

  printf("%-21s %4d '", name, constantOffset);

  printValue(chunk->constants.values[constantOffset]);
  printf("'\n");

  return offset + 4;
}

/**
 * \brief Display a OP_INVOKE or OP_SUPER_INVOKE instruction at a given offset.
 * This instruction has one 8-bit operand.
 *
 * \param name Name of the instruction
 * \param chunk Pointer to the chunk of bytecode that contains the instruction
 * \param offset Offset of the instruction within the bytecode array
 *
 * \return Offset of the next instruction
 */
static size_t invokeInstruction(const char* name, const Chunk* chunk,
                                const size_t offset) {
  uint8_t nameOffset = chunk->code[offset + 1];
  uint8_t argCount = chunk->code[offset + 2];
  printf("%-21s (%d args) %4d '%s'\n", name, argCount, nameOffset,
         vm.globalValues.vars[nameOffset].identifier->chars);

  return offset + 3;
}

/**
 * \brief Display a OP_INVOKE_LONG or OP_SUPER_INVOKE_LONG instruction at a
 * given offset. This instruction has three 8-bit operands.
 *
 * \param name Name of the instruction
 * \param chunk Pointer to the chunk of bytecode that contains the instruction
 * \param offset Offset of the instruction within the bytecode array
 *
 * \return Offset of the next instruction
 */
static size_t invokeLongInstruction(const char* name, const Chunk* chunk,
                                    const size_t offset) {
  uint32_t nameOffset = (chunk->code[offset + 3] << 16) |
                        (chunk->code[offset + 2] << 8) |
                        chunk->code[offset + 1];
  uint8_t argCount = chunk->code[offset + 4];
  printf("%-21s (%d args) %4d '%s'\n", name, argCount, nameOffset,
         vm.globalValues.vars[nameOffset].identifier->chars);

  return offset + 5;
}

/**
 * \brief Display an instruction which handles global variables. This
 * instruction has one 8-bit operand.
 *
 * \param name Name of the instruction
 * \param chunk Pointer to the chunk of bytecode that contains the instruction
 * \param offset Offset of the instruction within the bytecode array
 *
 * \return Offset of the next instruction
 */
static size_t globalInstruction(const char* name, const Chunk* chunk,
                                const size_t offset) {
  uint8_t globalOffset = chunk->code[offset + 1];
  printf("%-21s %4d '%s'\n", name, globalOffset,
         vm.globalValues.vars[globalOffset].identifier->chars);

  return offset + 2;
}

/**
 * \brief Display an instruction which handles global variables. This
 * instruction has three 8-bit operands.
 *
 * \param name Name of the instruction
 * \param chunk Pointer to the chunk of bytecode that contains the instruction
 * \param offset Offset of the instruction within the bytecode array
 *
 * \return Offset of the next instruction
 */
static size_t globalLongInstruction(const char* name, const Chunk* chunk,
                                    const size_t offset) {
  uint32_t globalOffset = (chunk->code[offset + 3] << 16) |
                          (chunk->code[offset + 2] << 8) |
                          chunk->code[offset + 1];

  printf("%-21s %4d '%s'\n", name, globalOffset,
         vm.globalValues.vars[globalOffset].identifier->chars);

  return offset + 4;
}

/**
 * \brief Display an instruction that initializes a closure.
 *
 * \param name Name of the instruction
 * \param chunk Pointer to the chunk of bytecode that contains the instruction
 * \param closureOffset The offset of the closure in the chunk's constants array
 * \param offset Offset of the instruction within the bytecode array
 *
 * \return Offset of the next instruction
 */
static size_t closureInstruction(const char* name, const Chunk* chunk,
                                 const uint32_t closureOffset, size_t offset) {
  printf("%-21s %4d ", name, closureOffset);
  printValue(chunk->constants.values[closureOffset]);
  printf("\n");

  ObjFunction* function = AS_FUNCTION(chunk->constants.values[closureOffset]);
  for (uint16_t j = 0; j < function->upvalueCount; j++) {
    uint8_t isLocal = chunk->code[offset++];
    uint8_t index = chunk->code[offset++];
    printf("%04ld    |                          %s %d\n", offset - 2,
           isLocal ? "local" : "upvalue", index);
  }

  return offset;
}

size_t disassembleInstruction(const Chunk* chunk, const size_t offset) {
  printf("%04ld ", offset);

  uint32_t line = getLine(chunk, offset);
  if (offset > 0 && line == getLine(chunk, offset - 1)) {
    printf("   | ");
  } else {
    printf("%4d ", line);
  }

  uint8_t instruction = chunk->code[offset];

  switch (instruction) {
  case OP_CONSTANT:
    return constantInstruction("OP_CONSTANT", chunk, offset);
  case OP_CONSTANT_LONG:
    return constantLongInstruction("OP_CONSTANT_LONG", chunk, offset);
  case OP_NIL:
    return simpleInstruction("OP_NIL", offset);
  case OP_TRUE:
    return simpleInstruction("OP_TRUE", offset);
  case OP_FALSE:
    return simpleInstruction("OP_FALSE", offset);
  case OP_POP:
    return simpleInstruction("OP_POP", offset);
  case OP_GET_LOCAL:
    return byteInstruction("OP_GET_LOCAL", chunk, offset);
  case OP_GET_LOCAL_SHORT:
    return byteShortInstruction("OP_GET_LOCAL_SHORT", chunk, offset);
  case OP_SET_LOCAL:
    return byteInstruction("OP_GET_LOCAL", chunk, offset);
  case OP_SET_LOCAL_SHORT:
    return byteShortInstruction("OP_SET_LOCAL_SHORT", chunk, offset);
  case OP_GET_GLOBAL:
    return globalInstruction("OP_GET_GLOBAL", chunk, offset);
  case OP_GET_GLOBAL_LONG:
    return globalLongInstruction("OP_GET_GLOBAL_LONG", chunk, offset);
  case OP_DEFINE_GLOBAL:
    return globalInstruction("OP_DEFINE_GLOBAL", chunk, offset);
  case OP_DEFINE_GLOBAL_LONG:
    return globalLongInstruction("OP_DEFINE_GLOBAL_LONG", chunk, offset);
  case OP_SET_GLOBAL:
    return globalInstruction("OP_SET_GLOBAL", chunk, offset);
  case OP_SET_GLOBAL_LONG:
    return globalLongInstruction("OP_SET_GLOBAL_LONG", chunk, offset);
  case OP_GET_UPVALUE:
    return byteInstruction("OP_GET_UPVALUE", chunk, offset);
  case OP_SET_UPVALUE:
    return byteInstruction("OP_SET_UPVALUE", chunk, offset);
  case OP_GET_PROPERTY:
    return globalInstruction("OP_GET_PROPERTY", chunk, offset);
  case OP_GET_PROPERTY_LONG:
    return globalLongInstruction("OP_GET_PROPERTY_LONG", chunk, offset);
  case OP_SET_PROPERTY:
    return globalInstruction("OP_SET_PROPERTY", chunk, offset);
  case OP_SET_PROPERTY_LONG:
    return globalLongInstruction("OP_SET_PROPERTY_LONG", chunk, offset);
  case OP_GET_SUPER:
    return globalInstruction("OP_GET_SUPER", chunk, offset);
  case OP_GET_SUPER_LONG:
    return globalLongInstruction("OP_GET_SUPER_LONG", chunk, offset);
  case OP_NOT:
    return simpleInstruction("OP_NOT", offset);
  case OP_NEGATE:
    return simpleInstruction("OP_NEGATE", offset);
  case OP_EQUAL:
    return simpleInstruction("OP_EQUAL", offset);
  case OP_NOT_EQUAL:
    return simpleInstruction("OP_NOT_EQUAL", offset);
  case OP_GREATER:
    return simpleInstruction("OP_GREATER", offset);
  case OP_GTE:
    return simpleInstruction("OP_GTE", offset);
  case OP_LESS:
    return simpleInstruction("OP_LESS", offset);
  case OP_LTE:
    return simpleInstruction("OP_LTE", offset);
  case OP_ADD:
    return simpleInstruction("OP_ADD", offset);
  case OP_SUBTRACT:
    return simpleInstruction("OP_SUBTRACT", offset);
  case OP_MULTIPLY:
    return simpleInstruction("OP_MULTIPLY", offset);
  case OP_DIVIDE:
    return simpleInstruction("OP_DIVIDE", offset);
  case OP_PRINT:
    return simpleInstruction("OP_PRINT", offset);
  case OP_JUMP:
    return jumpInstruction("OP_JUMP", 1, chunk, offset);
  case OP_JUMP_IF_FALSE:
    return jumpInstruction("OP_JUMP_IF_FALSE", 1, chunk, offset);
  case OP_JUMP_IF_FALSE_NP:
    return jumpInstruction("OP_JUMP_IF_FALSE_NP", 1, chunk, offset);
  case OP_LOOP:
    return jumpInstruction("OP_LOOP", -1, chunk, offset);
  case OP_CALL:
    return byteInstruction("OP_CALL", chunk, offset);
  case OP_INVOKE:
    return invokeInstruction("OP_INVOKE", chunk, offset);
  case OP_INVOKE_LONG:
    return invokeLongInstruction("OP_INVOKE_LONG", chunk, offset);
  case OP_SUPER_INVOKE:
    return invokeInstruction("OP_SUPER_INVOKE", chunk, offset);
  case OP_SUPER_INVOKE_LONG:
    return invokeLongInstruction("OP_SUPER_INVOKE_LONG", chunk, offset);
  case OP_CLOSURE: {
    uint8_t closureOffset = chunk->code[offset + 1];
    return closureInstruction("OP_CLOSURE", chunk, (uint32_t)closureOffset,
                              offset + 2);
  }
  case OP_CLOSURE_LONG: {
    uint32_t closureOffset = (chunk->code[offset + 3] << 16) |
                             (chunk->code[offset + 2] << 8) |
                             chunk->code[offset + 1];
    return closureInstruction("OP_CLOSURE_LONG", chunk, closureOffset,
                              offset + 4);
  }
  case OP_CLOSE_UPVALUE:
    return simpleInstruction("OP_CLOSE_UPVALUE", offset);
  case OP_RETURN:
    return simpleInstruction("OP_RETURN", offset);
  case OP_CLASS:
    return globalInstruction("OP_CLASS", chunk, offset);
  case OP_CLASS_LONG:
    return globalLongInstruction("OP_CLASS", chunk, offset);
  case OP_INHERIT:
    return simpleInstruction("OP_INHERIT", offset);
  case OP_METHOD:
    return globalInstruction("OP_METHOD", chunk, offset);
  case OP_METHOD_LONG:
    return globalLongInstruction("OP_METHOD_LONG", chunk, offset);
  default:
    printf("Unknown opcode %d\n", instruction);
    return offset + 1;
  }
}
