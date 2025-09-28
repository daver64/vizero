#include <stdio.h>

int main() {
    printf("Hello, World!\n");
    
    if (1) {
        printf("Inside if block\n");
        printf("Line 2 of if block\n");
        printf("Line 3 of if block\n");
    }
    
    for (int i = 0; i < 5; i++) {
        printf("Loop iteration %d\n", i);
        printf("More loop content\n");
    }
    
    return 0;
}