/*! \file vm.c
    \brief Definitions of functions from vm.h
*/

#include "vm.h"
#include "common.h"
#include "compiler.h"
#include "memory.h"
#include "object.h"

#ifdef DEBUG_TRACE_EXECUTION
#include "debug.h"
#endif

#include <math.h>
#include <stdarg.h>
#include <string.h>
#include <time.h>

VM vm;

/**
 * \brief Implementation of the "clock" native function.
 *
 * This function returns the elapsed time since the program started running, in
 * seconds.
 *
 * \param argCount Number of arguments that were passed to the function
 * \param args Pointer to the value stack, where the arguments reside. The value
 * of args[-1] is set to the return value of the function, or to an error
 * message if there were any errors.
 *
 * \return Boolean values that indicates if the function was successful.
 */
static bool clockNative(const uint8_t argCount, Value* args) {
  args[-1] = NUMBER_VAL((double)clock() / CLOCKS_PER_SEC);
  return true;
}

/**
 * \brief Implementation of the "sqrt" native function.
 *
 * This function takes one numeric argument and returns its square root.
 *
 * \param argCount Number of arguments that were passed to the function
 * \param args Pointer to the value stack, where the arguments reside. The value
 * of args[-1] is set to the return value of the function, or to an error
 * message if there were any errors.
 *
 * \return Boolean values that indicates if the function was successful.
 */
static bool sqrtNative(const uint8_t argCount, Value* args) {
  if (!IS_NUMBER(args[0])) {
    args[-1] = OBJ_VAL(copyString("Argument must be a number!", 26));
    return false;
  }

  args[-1] = NUMBER_VAL(sqrt(AS_NUMBER(args[0])));
  return true;
}

static bool hasPropertyNative(const uint8_t argCount, Value* args) {
  if (!IS_INSTANCE(args[0])) {
    args[-1] = OBJ_VAL(copyString("First argument must be an instance.", 35));
    return false;
  }

  if (!IS_STRING(args[1])) {
    args[-1] = OBJ_VAL(copyString("Second argument must be a string.", 33));
    return false;
  }

  ObjInstance* instance = AS_INSTANCE(args[0]);
  Value value;
  args[-1] = BOOL_VAL(tableGet(&instance->fields, AS_STRING(args[1]), &value));
  return true;
}

/**
 * \brief Resets the value stack by moving the stack pointer to its start.
 */
static void resetStack() {
  vm.stackTop = vm.stack;
  vm.frameCount = 0;
  vm.openUpvalues = NULL;
}

/**
 * \brief Variadic function used to report a runtime error in the appropriate
 * line
 *
 * \param format Format string for the error message
 * \param ... Arguments for format specification
 */
static void runtimeError(const char* format, ...) {
  va_list args;
  va_start(args, format);
  vfprintf(stderr, format, args);
  va_end(args);
  fputs("\n", stderr);

  for (int32_t i = vm.frameCount - 1; i >= 0; --i) {
    CallFrame* frame = &vm.frames[i];
    ObjFunction* function = frame->closure->function;
    size_t instruction = (frame->ip - 1) - function->chunk.code;
    eprintf("[line %d] in ",
            getLine(&frame->closure->function->chunk, instruction));

    if (function->name == NULL) {
      eprintf("script\n");
    } else {
      eprintf("%s()\n", function->name->chars);
    }
  }

  resetStack();
}

/**
 * \brief Helper function used to define a new native function.
 *
 * \param name Name of the native function
 * \param function Pointer to the C function that implements it
 * \param arity Number of expected parameters
 *
 */
static void defineNative(const char* name, const NativeFn function,
                         const uint32_t arity) {
  push(OBJ_VAL(copyString(name, strlen(name))));
  push(OBJ_VAL(newNative(function, arity)));

  ObjString* identifier = AS_STRING(vm.stack[0]);
  writeGlobalVarArray(&vm.globalValues,
                      NEW_GLOBAL(identifier, vm.stack[1], true));
  tableSet(&vm.globalNames, identifier,
           NUMBER_VAL((double)vm.globalValues.count - 1));

  pop();
  pop();
}

void initVM() {
  resetStack();
  vm.objects = NULL;
  vm.bytesAllocated = 0;
  vm.nextGC = 1024 * 1024;

  vm.grayCount = 0;
  vm.grayCapacity = 0;
  vm.grayStack = NULL;

  initTable(&vm.globalNames);
  initGlobalVarArray(&vm.globalValues);
  initTable(&vm.strings);

  vm.initString = NULL;
  vm.initString = copyString("init", 4);

  defineNative("clock", clockNative, 0);
  defineNative("sqrt", sqrtNative, 1);
  defineNative("hasProperty", hasPropertyNative, 2);
}

void freeVM() {
  freeTable(&vm.globalNames);
  freeGlobalVarArray(&vm.globalValues);
  freeTable(&vm.strings);
  vm.initString = NULL;
  freeObjects();
}

void push(const Value value) {
  *vm.stackTop = value;
  vm.stackTop++;
}

Value pop() {
  vm.stackTop--;
  return *vm.stackTop;
}

/**
 * \brief Returns the value at a certain distance in the stack without popping
 * it
 *
 * \param distance How far down the stack the target the desired value is
 *
 * \return The peeked Value
 */
static Value peek(const int32_t distance) {
  return vm.stackTop[-1 - distance];
}

/**
 * \brief Function used to prepare the VM to execute a Lox function.
 *
 * This function adds a new CallFrame to the call stack with the appropriate
 * attributes.
 *
 * \param function Pointer to the function's closure object
 * \param argCount The number of arguments that were passed to the function
 *
 * \return Boolean value that indicates if there were any errors.
 */
static bool call(ObjClosure* closure, const uint8_t argCount) {
  if (argCount != closure->function->arity) {
    runtimeError("Expected %d arguments but got %d", closure->function->arity,
                 argCount);
    return false;
  }

  if (vm.frameCount == FRAMES_MAX) {
    runtimeError("Stack overflow");
    return false;
  }

  CallFrame* frame = &vm.frames[vm.frameCount++];
  frame->closure = closure;
  frame->ip = closure->function->chunk.code;
  frame->slots = vm.stackTop - argCount - 1;

  return true;
}

/**
 * \brief Function used to execute a call to a native function.
 *
 * \param native Pointer to the native's function object
 * \param argCount The number of arguments that were passed to the function
 *
 * \return Boolean value that indicates if the call was successful.
 */
static bool callNative(const ObjNative* native, const uint8_t argCount) {
  if (argCount != native->arity) {
    runtimeError("Expected %d arguments but got %d", native->arity, argCount);
    return false;
  }

  if (native->function(argCount, vm.stackTop - argCount)) {
    vm.stackTop -= argCount;
    return true;
  } else {
    runtimeError(AS_CSTRING(vm.stackTop[-argCount - 1]));
    return false;
  }
}

/**
 * \brief Function used to execute a function call.
 *
 * The callee can only be called if it is a callable object type.
 *
 * \param callee The callee's value in the value stack.
 * \param argCount The number of arguments that were passed to the function
 *
 * \return Boolean value that indicates if the call was successful.
 */
static bool callValue(const Value callee, const uint8_t argCount) {
  if (IS_OBJ(callee)) {
    switch (OBJ_TYPE(callee)) {
    case OBJ_BOUND_METHOD: {
      ObjBoundMethod* bound = AS_BOUND_METHOD(callee);
      // Stores the receiver at the first slot of call frame, to be used as
      // "this"
      vm.stackTop[-argCount - 1] = bound->receiver;
      return call(bound->method, argCount);
    }
    case OBJ_CLASS: {
      ObjClass* class_ = AS_CLASS(callee);
      vm.stackTop[-argCount - 1] = OBJ_VAL(newInstance(class_));
      Value initializer;
      if (tableGet(&class_->methods, vm.initString, &initializer)) {
        return call(AS_CLOSURE(initializer), argCount);
      } else if (argCount != 0) {
        runtimeError("Expected 0 arguments but got %d.", argCount);
        return false;
      }
      return true;
    }
    case OBJ_CLOSURE:
      return call(AS_CLOSURE(callee), argCount);
    case OBJ_NATIVE:
      return callNative(AS_NATIVE(callee), argCount);
    default:
      break; // Non-callable object type
    }
  }

  runtimeError("Can only call functions and classes");
  return false;
}

static bool invokeFromClass(ObjClass* class_, ObjString* name, const uint8_t argCount) {
  Value method;
  if (!tableGet(&class_->methods, name, &method)) {
    runtimeError("Undefined property '%s'.", name->chars);
    return false;
  }

  return call(AS_CLOSURE(method), argCount);
}

static bool invoke(ObjString* name, const uint8_t argCount) {
  Value receiver = peek(argCount);
  if (!IS_INSTANCE(receiver)) {
    runtimeError("Only instances have methods.");
    return false;
  }

  ObjInstance* instance = AS_INSTANCE(receiver);

  Value value;
  if (tableGet(&instance->fields, name, &value)) {
    vm.stackTop[-argCount - 1] = value;
    return callValue(value, argCount);
  }

  return invokeFromClass(instance->class_, name, argCount);
}

static bool bindMethod(const ObjClass* class_, const ObjString* name) {
  Value method;
  if (!tableGet(&class_->methods, name, &method)) {
    runtimeError("Undefined property '%s'.", name->chars);
    return false;
  }

  ObjBoundMethod* bound = newBoundMethod(peek(0), AS_CLOSURE(method));

  pop(); // Pops the instance
  push(OBJ_VAL(bound));

  return true;
}

/**
 * \brief Capture a given local variable as an upvalue.
 *
 * The function will create a new ObjUpvalue for this new upvalue if it hasn't
 * been created already. However, if there's already an upvalue associated with
 * this local, it will be reused.
 *
 * If a new upvalue is created, it is added to the VM's linked-list of open
 * upvalues.
 *
 * \param local Pointer to the local's location in the stack.
 *
 * \return Pointer to an ObjUpvalue that is associated with \p local
 */
static ObjUpvalue* captureUpvalue(Value* local) {
  ObjUpvalue* prevUpvalue = NULL;
  ObjUpvalue* upvalue = vm.openUpvalues;
  while (upvalue != NULL && upvalue->location > local) {
    prevUpvalue = upvalue;
    upvalue = upvalue->next;
  }

  if (upvalue != NULL && upvalue->location == local)
    return upvalue;

  ObjUpvalue* createdUpvalue = newUpvalue(local);
  createdUpvalue->next = upvalue;

  if (prevUpvalue == NULL) {
    vm.openUpvalues = createdUpvalue;
  } else {
    prevUpvalue->next = createdUpvalue;
  }

  return createdUpvalue;
}

/**
 * \brief Closes all upvalues that appear in the stack after or in the same
 * position as \p last.
 *
 * In order to close an upvalue, the local's value is copied to the ObjUpvalue
 * instance that is associated with it. The ObjUpvalue's location field is
 * updated to point to this new location.
 *
 * \param last Pointer to the last local variable whose upvalue must be closed.
 *
 */
static void closeUpvalues(const Value* last) {
  while (vm.openUpvalues != NULL && vm.openUpvalues->location >= last) {
    ObjUpvalue* upvalue = vm.openUpvalues;
    upvalue->closed = *upvalue->location;
    upvalue->location = &upvalue->closed;
    vm.openUpvalues = upvalue->next;
  }
}

static void defineMethod(ObjString* name) {
  Value method = peek(0);
  ObjClass* class_ = AS_CLASS(peek(1));
  tableSet(&class_->methods, name, method);
  pop();
}

/**
 * \brief Returns whether a Value is falsey or not
 *
 * \param value The Value in consideration
 *
 * \return Boolean value that indicates if the parameter is falsey
 */
static bool isFalsey(const Value value) {
  return IS_NIL(value) || (IS_BOOL(value) && AS_BOOL(value) == false);
}

/**
 * \brief Concatenates the next two value at the value stack, assuming that they
 * are strings.
 *
 * This function has to allocate a new string for the concatenated string.
 *
 */
static void concatenate() {
  ObjString* right = AS_STRING(peek(0));
  ObjString* left = AS_STRING(peek(1));

  size_t length = left->length + right->length;
  char* chars = ALLOCATE(char, length + 1);
  memcpy(chars, left->chars, left->length);
  memcpy(chars + left->length, right->chars, right->length);
  chars[length] = '\0';

  ObjString* concatenatedString = takeString(chars, length);
  pop();
  pop();

  push(OBJ_VAL(concatenatedString));
}

/**
 * \brief Auxiliary function that gets the value of global variable based on its
 * offset in the globalValues array. The value is pushed to the stack.
 *
 * \param offset The offset of the variable in the globalValues array
 * \param frame Pointer to the call frame of the current function
 * \param ip Instruction pointer
 *
 * \return Boolean value that indicates if there were any errors when fetching
 * the variable
 *
 */
static bool getGlobal(const uint32_t offset, CallFrame* frame, uint8_t* ip) {
  GlobalVar var = vm.globalValues.vars[offset];

  if (IS_UNDEFINED(var.value)) {
    frame->ip = ip;
    runtimeError("Undefined variable '%s'.", var.identifier->chars);
    return false;
  }

  push(var.value);
  return true;
}

/**
 * \brief Auxiliary function that defines a global variable by setting its
 * value.
 *
 * \param offset The offset of the variable in the globalValues array
 */
static void defineGlobal(const uint32_t offset) {
  vm.globalValues.vars[offset].value = pop();
}

/**
 * \brief Auxiliary function that sets the value of a global variable only if it
 * has already been defined.
 *
 * \param offset The offset of the variable in the globalValues array
 * \param frame Pointer to the call frame of the current function
 * \param ip Instruction pointer
 *
 * \return Boolean value that indicates if there were any errors
 */
static bool setGlobal(const uint32_t offset, CallFrame* frame, uint8_t* ip) {
  GlobalVar* var = vm.globalValues.vars + offset;

  if (IS_UNDEFINED(var->value)) {
    frame->ip = ip;
    runtimeError("Undefined variable '%s'.", var->identifier->chars);
    return false;
  }

  var->value = peek(0);
  return true;
}

static bool getProperty(const uint32_t nameOffset) {
  if (!IS_INSTANCE(peek(0))) {
    runtimeError("Only instances have properties");
    return false;
  }

  ObjInstance* instance = AS_INSTANCE(peek(0));
  ObjString* name = vm.globalValues.vars[nameOffset].identifier;

  Value value;
  if (tableGet(&instance->fields, name, &value)) {
    pop(); // Instance
    push(value);
    return true;
  }

  if (!bindMethod(instance->class_, name)) {
    return false;
  }

  return true;
}

static bool setProperty(const uint32_t nameOffset) {
  if (!IS_INSTANCE(peek(1))) {
    runtimeError("Only instances have fields.");
    return false;
  }

  ObjInstance* instance = AS_INSTANCE(peek(1));
  ObjString* name = vm.globalValues.vars[nameOffset].identifier;
  tableSet(&instance->fields, name, peek(0));
  Value value = pop();
  pop(); // Instance
  push(value);

  return true;
}

/**
 * \brief Auxiliary function that defines a new closure.
 *
 * The function reads all of the closure's upvalues from the stack and updates
 * the ObjClosure's upvalues array.
 *
 * \param function Pointer to the ObjFunction that will be associated with the
 * closure
 * \param frame Pointer to the call frame of the current function
 * \param ip Instruction pointer
 *
 * \return Next position of the instruction pointer
 */
static uint8_t* defineClosure(ObjFunction* function, const CallFrame* frame,
                              uint8_t* ip) {
  ObjClosure* closure = newClosure(function);
  push(OBJ_VAL(closure));

  for (uint16_t i = 0; i < closure->upvalueCount; ++i) {
    uint8_t isLocal = (*(ip++));
    uint8_t index = (*(ip++));
    if (isLocal) {
      closure->upvalues[i] = captureUpvalue(frame->slots + index);
    } else {
      closure->upvalues[i] = frame->closure->upvalues[index];
    }
  }

  return ip;
}

/**
 * \brief Helper function that runs the instructions in the VM.
 *
 * \return Result of the interpretation process
 */
static InterpretResult run() {
  CallFrame* frame = &vm.frames[vm.frameCount - 1];
  register uint8_t* ip = frame->ip;

// Read a single bytecode from the chunk and updates the instruction pointer
#define READ_BYTE() (*(ip++))

// Read a 24-bit (or 3 bytes) operand from the chunk
#define READ_LONG_OPERAND()                                                    \
  (ip += 3, (uint32_t)(ip[-3] | (ip[-2] << 8) | (ip[-1] << 16)))

// Read a 16-bit (or 2 bytes) operand from the chunk
#define READ_SHORT() (ip += 2, (uint16_t)(ip[-2] | (ip[-1] << 8)))

// Read a constant from the chunk based on the 8-bit offset at the bytecode
// array
#define READ_CONSTANT()                                                        \
  (frame->closure->function->chunk.constants.values[READ_BYTE()])

// Read a constant from the chunk based on the 24-bit offset at the bytecode
// array
#define READ_CONSTANT_LONG()                                                   \
  (frame->closure->function->chunk.constants.values[READ_LONG_OPERAND()])

// Read a string from the constants array with an 8-bit offset
#define READ_STRING() AS_STRING(READ_CONSTANT())

// Read a string from the constants array with a 24-bit offset
#define READ_STRING_LONG() AS_STRING(READ_CONSTANT_LONG())

// Apply a binary operation based on the two next values at stack and on the
// type of the operands
#define BINARY_OP(valueType, op)                                               \
  do {                                                                         \
    if (!IS_NUMBER(peek(0)) || !IS_NUMBER(peek(1))) {                          \
      frame->ip = ip;                                                          \
      runtimeError("Operands must be numbers.");                               \
      return INTERPRET_RUNTIME_ERROR;                                          \
    }                                                                          \
    double right = AS_NUMBER(pop());                                           \
    double left = AS_NUMBER(pop());                                            \
    push(valueType(left op right));                                            \
  } while (false)

#ifdef DEBUG_TRACE_EXECUTION
  printf("\n=== execution trace ===");
#endif

  while (true) {

#ifdef DEBUG_TRACE_EXECUTION
    printf("          ");
    for (Value* slot = vm.stack; slot < vm.stackTop; ++slot) {
      printf("[");
      printValue(*slot);
      printf("]");
    }
    printf("\n");

    disassembleInstruction(&frame->closure->function->chunk,
                           (size_t)(ip - frame->closure->function->chunk.code));
#endif

    uint8_t instruction;
    switch (instruction = READ_BYTE()) {
    case OP_CONSTANT: {
      Value constant = READ_CONSTANT();
      push(constant);
      break;
    }
    case OP_CONSTANT_LONG: {
      Value constant = READ_CONSTANT_LONG();
      push(constant);
      break;
    }
    case OP_NIL:
      push(NIL_VAL);
      break;
    case OP_TRUE:
      push(BOOL_VAL(true));
      break;
    case OP_FALSE:
      push(BOOL_VAL(false));
      break;
    case OP_POP:
      pop();
      break;
    case OP_GET_LOCAL: {
      uint8_t slot = READ_BYTE();
      push(frame->slots[slot]);
      break;
    }
    case OP_GET_LOCAL_SHORT: {
      uint16_t slot = READ_SHORT();
      push(frame->slots[slot]);
      break;
    }
    case OP_SET_LOCAL: {
      uint8_t slot = READ_BYTE();
      frame->slots[slot] = peek(0);
      break;
    }
    case OP_SET_LOCAL_SHORT: {
      uint16_t slot = READ_SHORT();
      frame->slots[slot] = peek(0);
      break;
    }
    case OP_GET_GLOBAL:
      if (!getGlobal(READ_BYTE(), frame, ip))
        return INTERPRET_RUNTIME_ERROR;
      break;
    case OP_GET_GLOBAL_LONG:
      if (!getGlobal(READ_LONG_OPERAND(), frame, ip))
        return INTERPRET_RUNTIME_ERROR;
      break;
    case OP_DEFINE_GLOBAL:
      defineGlobal(READ_BYTE());
      break;
    case OP_DEFINE_GLOBAL_LONG:
      defineGlobal(READ_LONG_OPERAND());
      break;
    case OP_SET_GLOBAL:
      if (!setGlobal(READ_BYTE(), frame, ip))
        return INTERPRET_RUNTIME_ERROR;
      break;
    case OP_SET_GLOBAL_LONG:
      if (!setGlobal(READ_LONG_OPERAND(), frame, ip))
        return INTERPRET_RUNTIME_ERROR;
      break;
    case OP_GET_UPVALUE: {
      uint8_t slot = READ_BYTE();
      push(*frame->closure->upvalues[slot]->location);
      break;
    }
    case OP_SET_UPVALUE: {
      uint8_t slot = READ_BYTE();
      *frame->closure->upvalues[slot]->location = peek(0);
      break;
    }
    case OP_GET_PROPERTY:
      if (!getProperty(READ_BYTE()))
        return INTERPRET_RUNTIME_ERROR;
      break;
    case OP_GET_PROPERTY_LONG:
      if (!getProperty(READ_LONG_OPERAND()))
        return INTERPRET_RUNTIME_ERROR;
      break;
    case OP_SET_PROPERTY:
      if (!setProperty(READ_BYTE()))
        return INTERPRET_RUNTIME_ERROR;
      break;
    case OP_SET_PROPERTY_LONG:
      if (!setProperty(READ_LONG_OPERAND()))
        return INTERPRET_RUNTIME_ERROR;
      break;
    case OP_NOT:
      vm.stackTop[-1] = BOOL_VAL(isFalsey(vm.stackTop[-1]));
      break;
    case OP_NEGATE:
      if (!IS_NUMBER(peek(0))) {
        frame->ip = ip;
        runtimeError("Operand must be a number.");
        return INTERPRET_RUNTIME_ERROR;
      }

      vm.stackTop[-1] = NUMBER_VAL(-AS_NUMBER(vm.stackTop[-1]));
      break;
    case OP_EQUAL: {
      Value left = pop();
      Value right = pop();
      push(BOOL_VAL(valuesEqual(left, right)));
      break;
    }
    case OP_NOT_EQUAL: {
      Value left = pop();
      Value right = pop();
      push(BOOL_VAL(!valuesEqual(left, right)));
      break;
    }
    case OP_GREATER:
      BINARY_OP(BOOL_VAL, >);
      break;
    case OP_GTE:
      BINARY_OP(BOOL_VAL, >=);
      break;
    case OP_LESS:
      BINARY_OP(BOOL_VAL, <);
      break;
    case OP_LTE:
      BINARY_OP(BOOL_VAL, <=);
      break;
    case OP_ADD:
      if (IS_NUMBER(peek(0)) && IS_NUMBER(peek(1))) {
        double right = AS_NUMBER(pop());
        double left = AS_NUMBER(pop());
        push(NUMBER_VAL(left + right));
      } else if (IS_STRING(peek(0)) && IS_STRING(peek(1))) {
        concatenate();
      } else {
        frame->ip = ip;
        runtimeError("Operands must be two numbers or two strings.");
        return INTERPRET_RUNTIME_ERROR;
      }

      break;
    case OP_SUBTRACT:
      BINARY_OP(NUMBER_VAL, -);
      break;
    case OP_MULTIPLY:
      BINARY_OP(NUMBER_VAL, *);
      break;
    case OP_DIVIDE:
      BINARY_OP(NUMBER_VAL, /);
      break;
    case OP_PRINT: {
      printValue(pop());
      printf("\n");
      break;
    }
    case OP_JUMP: {
      uint16_t offset = READ_SHORT();
      ip += offset;
      break;
    }
    case OP_JUMP_IF_FALSE: {
      uint16_t offset = READ_SHORT();
      if (isFalsey(pop()))
        ip += offset;
      break;
    }
    case OP_JUMP_IF_FALSE_NP: {
      uint16_t offset = READ_SHORT();
      if (isFalsey(peek(0)))
        ip += offset;
      break;
    }
    case OP_LOOP: {
      uint16_t offset = READ_SHORT();
      ip -= offset;
      break;
    }
    case OP_CALL: {
      uint8_t argCount = READ_BYTE();
      frame->ip = ip;
      if (!callValue(peek(argCount), argCount))
        return INTERPRET_RUNTIME_ERROR;
      frame = &vm.frames[vm.frameCount - 1];
      ip = frame->ip;
      break;
    }
    case OP_INVOKE: {
      ObjString* method = vm.globalValues.vars[READ_BYTE()].identifier;
      uint8_t argCount = READ_BYTE();

      frame->ip = ip;
      if (!invoke(method, argCount))
        return INTERPRET_RUNTIME_ERROR;
      frame = &vm.frames[vm.frameCount - 1];
      ip = frame->ip;
      break;
    }
    case OP_INVOKE_LONG: {
      ObjString* method = vm.globalValues.vars[READ_LONG_OPERAND()].identifier;
      uint8_t argCount = READ_BYTE();

      frame->ip = ip;
      if (!invoke(method, argCount))
        return INTERPRET_RUNTIME_ERROR;
      frame = &vm.frames[vm.frameCount - 1];
      ip = frame->ip;
      break;
    }
    case OP_CLOSURE:
      ip = defineClosure(AS_FUNCTION(READ_CONSTANT()), frame, ip);
      break;
    case OP_CLOSURE_LONG:
      ip = defineClosure(AS_FUNCTION(READ_CONSTANT_LONG()), frame, ip);
      break;
    case OP_CLOSE_UPVALUE:
      closeUpvalues(vm.stackTop - 1);
      pop();
      break;
    case OP_RETURN: {
      Value result = pop();

      closeUpvalues(frame->slots);

      vm.frameCount--;
      if (vm.frameCount == 0) {
        pop();
        return INTERPRET_OK;
      }

      vm.stackTop = frame->slots;
      push(result);
      frame = &vm.frames[vm.frameCount - 1];
      ip = frame->ip;

      break;
    }
    case OP_CLASS:
      push(OBJ_VAL(newClass(vm.globalValues.vars[READ_BYTE()].identifier)));
      break;
    case OP_CLASS_LONG:
      push(OBJ_VAL(
          newClass(vm.globalValues.vars[READ_LONG_OPERAND()].identifier)));
      break;
    case OP_METHOD:
      defineMethod(vm.globalValues.vars[READ_BYTE()].identifier);
      break;
    case OP_METHOD_LONG:
      defineMethod(vm.globalValues.vars[READ_LONG_OPERAND()].identifier);
      break;
    default:
      break;
    }
  }

#undef READ_BYTE
#undef READ_LONG_OPERAND
#undef READ_SHORT
#undef READ_CONSTANT
#undef READ_CONSTANT_LONG
#undef READ_STRING
#undef READ_STRING_LONG
#undef BINARY_OP
}

InterpretResult interpret(const char* source) {
  ObjFunction* function = compile(source);
  if (function == NULL)
    return INTERPRET_COMPILE_ERROR;

  push(OBJ_VAL(function));

  ObjClosure* closure = newClosure(function);
  // We must pop the ObjFunction and push the ObjClosure in order to keep the
  // GC aware of this heap-allocated object
  pop();
  push(OBJ_VAL(closure));
  call(closure, 0);

  InterpretResult result = run();

  return result;
}
