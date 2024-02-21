/*! \file common.h
    \brief Common definitions and libraries
*/

#ifndef CLOX_COMMON_H
#define CLOX_COMMON_H

#include <stdbool.h>
#include <stddef.h>
#include <stdint.h>
#include <stdio.h>

/**
 * \def UINT24_MAX
 * \brief Maximum value for a 24-bit unsigned integer
 */
#define UINT24_MAX (UINT32_MAX >> 8)

/**
 * \def UINT10_COUNT
 * \brief Maximum size for an array with 10-bit indexes
 */
#define UINT10_COUNT ((UINT16_MAX >> 6) + 1)

/**
 * \def UINT10_COUNT
 * \brief Maximum size for an array with 10-bit indexes
 */
#define UINT8_COUNT (UINT8_MAX + 1)

/**
 * \def UINT16_COUNT
 * \brief Maximum size for an array with 16-bit indexes
 */
#define UINT16_COUNT (UINT16_MAX + 1)

/**
 * \def eprintf(...)
 * \brief Wrapper macro to print something to stderr.
 */
#define eprintf(...) fprintf(stderr, __VA_ARGS__)

#endif
