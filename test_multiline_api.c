// Test file to verify the new insert_text_multiline API
// This file will be used to test proper newline handling in the REPL buffer

#include <stdio.h>

int main() {
    printf("Testing multiline API functionality\n");
    printf("Line 1\n");
    printf("Line 2\n");
    printf("Line 3\n");
    return 0;
}

/* This file contains multiple lines to test:
 * 1. That the REPL can handle multiline expressions
 * 2. That the new insert_text_multiline API properly splits on \n
 * 3. That the buffer display shows proper line breaks
 */