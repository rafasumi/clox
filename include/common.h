/*! \file common.h
    \brief Common definitions and libraries
*/

#ifndef CLOX_COMMON_H
#define CLOX_COMMON_H

#include <stdbool.h>
#include <stddef.h>
#include <stdint.h>
#include <stdio.h>

#define UINT24_MAX (UINT32_MAX >> 8)

/**
 * \def eprintf(...)
 * \brief Wrapper macro to print something to stderr.
 */
#define eprintf(...) fprintf(stderr, __VA_ARGS__)

#endif
