#ifndef VIZERO_TEST_H
#define VIZERO_TEST_H

#include <stdio.h>
#include <stdlib.h>
#include <string.h>

/* Test macros */
#define TEST_ASSERT(condition, message) \
    do { \
        if (!(condition)) { \
            printf("FAIL: %s - %s (line %d)\n", __func__, message, __LINE__); \
            return 0; \
        } \
    } while (0)

#define TEST_ASSERT_EQUAL(expected, actual, message) \
    do { \
        if ((expected) != (actual)) { \
            printf("FAIL: %s - %s (line %d): expected %d, got %d\n", \
                   __func__, message, __LINE__, (int)(expected), (int)(actual)); \
            return 0; \
        } \
    } while (0)

#define TEST_ASSERT_STR_EQUAL(expected, actual, message) \
    do { \
        if (strcmp((expected), (actual)) != 0) { \
            printf("FAIL: %s - %s (line %d): expected '%s', got '%s'\n", \
                   __func__, message, __LINE__, (expected), (actual)); \
            return 0; \
        } \
    } while (0)

#define TEST_ASSERT_NULL(ptr, message) \
    do { \
        if ((ptr) != NULL) { \
            printf("FAIL: %s - %s (line %d): expected NULL\n", \
                   __func__, message, __LINE__); \
            return 0; \
        } \
    } while (0)

#define TEST_ASSERT_NOT_NULL(ptr, message) \
    do { \
        if ((ptr) == NULL) { \
            printf("FAIL: %s - %s (line %d): expected non-NULL\n", \
                   __func__, message, __LINE__); \
            return 0; \
        } \
    } while (0)

/* Test runner */
typedef int (*test_func_t)(void);

typedef struct {
    const char* name;
    test_func_t func;
} test_case_t;

int run_test_suite(const char* suite_name, test_case_t* tests, size_t count);

#endif /* VIZERO_TEST_H */