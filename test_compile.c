#include <stdio.h>
#include "nonexistent.h"

int main() {
    undeclared_variable = 5;
    another_undefined_function();
    printf("This will have errors\n");
    return 0;
}