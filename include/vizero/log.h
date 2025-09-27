#ifndef VIZERO_LOG_H
#define VIZERO_LOG_H

#include <stdio.h>
#include <stdarg.h>

/*
 * Simple logging header for Vizero.
 * Control verbosity with VIZERO_LOG_LEVEL (default: 1 = info).
 * Levels: 0=errors only, 1=info, 2=debug
 */
#ifndef VIZERO_LOG_LEVEL
#define VIZERO_LOG_LEVEL 1
#endif

#define VIZERO_ERR(fmt, ...) \
    do { fprintf(stderr, "[VIZERO][ERR] " fmt "\n", ##__VA_ARGS__); } while(0)

#if VIZERO_LOG_LEVEL >= 1
#define VIZERO_INFO(fmt, ...) \
    do { fprintf(stderr, "[VIZERO][INFO] " fmt "\n", ##__VA_ARGS__); } while(0)
#else
#define VIZERO_INFO(fmt, ...) ((void)0)
#endif

#if VIZERO_LOG_LEVEL >= 2
#define VIZERO_DBG(fmt, ...) \
    do { fprintf(stderr, "[VIZERO][DBG] " fmt "\n", ##__VA_ARGS__); } while(0)
#else
#define VIZERO_DBG(fmt, ...) ((void)0)
#endif

#endif /* VIZERO_LOG_H */
