#include "test_framework.h"

int run_test_suite(const char* suite_name, test_case_t* tests, size_t count) {
    printf("Running test suite: %s\n", suite_name);
    printf("=====================%s\n", "=================");
    
    size_t passed = 0;
    size_t failed = 0;
    
    for (size_t i = 0; i < count; i++) {
        printf("Running test: %s... ", tests[i].name);
        fflush(stdout);
        
        if (tests[i].func()) {
            printf("PASS\n");
            passed++;
        } else {
            failed++;
        }
    }
    
    printf("\nResults: %zu passed, %zu failed\n", passed, failed);
    
    if (failed == 0) {
        printf("All tests passed!\n");
        return 0;
    } else {
        printf("Some tests failed.\n");
        return 1;
    }
}