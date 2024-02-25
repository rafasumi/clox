# Makefile for building a single configuration of the clox interpreter. It expects
# variables to be passed in for:
#
# MODE         "debug" or "release".

SOURCE_DIR := ./src
NAME := clox

CC := gcc
CFLAGS := -std=c99 -Wall -Wextra -Werror -Wno-unused-parameter -Wno-sequence-point

# If we're building at a point in the middle of a chapter, don't fail if there
# are functions that aren't used yet.
ifeq ($(SNIPPET),true)
	CFLAGS += -Wno-unused-function
endif

# Mode configuration.
ifeq ($(MODE),debug)
	CFLAGS += -O0 -DDEBUG_BYTECODE -DDEBUG_TRACE_EXECUTION -g
	BUILD_DIR := build/debug
else ifeq ($(MODE),debug_gc)
	CFLAGS += -O0 -DDEBUG_STRESS_GC -DDEBUG_LOG_GC -g
	BUILD_DIR := build/debug
else
	CFLAGS += -O3 -flto
	BUILD_DIR := build/release
endif

HEADERS := $(wildcard $(SOURCE_DIR)/*.h)
SOURCES := $(wildcard $(SOURCE_DIR)/*.c)
OBJECTS := $(addprefix $(BUILD_DIR)/$(NAME)/, $(notdir $(SOURCES:.c=.o)))

# Link the interpreter.
build/$(NAME): $(OBJECTS)
	@ printf "%8s %-40s %s\n" $(CC) $@ "$(CFLAGS)"
	@ mkdir -p build
	@ $(CC) $(CFLAGS) $^ -o $@ -lm

# Compile object files.
$(BUILD_DIR)/$(NAME)/%.o: $(SOURCE_DIR)/%.c $(HEADERS)
	@ printf "%8s %-40s %s\n" $(CC) $< "$(CFLAGS)"
	@ mkdir -p $(BUILD_DIR)/$(NAME)
	@ $(CC) -c $(C_LANG) $(CFLAGS) -o $@ $<

.PHONY: default

default: build/$(NAME)

clean:
	@ rm -rf build
