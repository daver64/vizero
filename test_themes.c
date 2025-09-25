/* 
 * Vizero Colour Theme Showcase
 * Test file to demonstrate the new colour themes
 */

#include <stdio.h>
#include <stdlib.h>
#include <string.h>

/* Constants and defines */
#define MAX_BUFFER_SIZE 1024
#define PI 3.14159265359

/* Type definitions */
typedef struct {
    int id;
    char* name;
    float value;
} demo_struct_t;

/* Global variables */
static int g_counter = 0;
const char* g_message = "Hello, Vizero!";

/* Function prototypes */
int calculate_sum(int a, int b);
void print_demo_info(const demo_struct_t* data);
char* allocate_string(size_t length);

/*
 * Main function - entry point
 * 
 * Available colour themes:
 * :colourscheme Default       - Original dark theme
 * :colourscheme Monokai       - Popular Sublime Text inspired
 * :colourscheme "Solarized Dark" - Ethan Schoonover's theme
 * :colourscheme "MSVC Light"     - Visual Studio light theme
 * :colourscheme "MSVC Blue"      - Visual Studio blue theme  
 * :colourscheme "GVim Default"   - Classic GVim colours
 * :colourscheme "GVim Desert"    - Popular desert scheme
 * :colourscheme "GVim Evening"   - Evening colour scheme
 * :colourscheme "GitHub Light"   - GitHub inspired light theme
 * :colourscheme "Gruvbox Dark"   - Retro groove colours
 *
 * Type :colourscheme without arguments to list all themes
 */
int main(int argc, char** argv) {
    printf("=== Vizero Colour Theme Showcase ===\n");
    
    // String literals and escape sequences
    char* test_string = "This is a \"quoted string\" with \n escape sequences";
    
    // Numbers in different formats
    int decimal = 42;
    float floating = 3.14f;
    int hex = 0xDEADBEEF;
    int octal = 0755;
    
    // Control structures
    for (int i = 0; i < 10; i++) {
        if (i % 2 == 0) {
            printf("Even: %d\n", i);
        } else {
            printf("Odd: %d\n", i);
        }
    }
    
    // Function calls and operators
    int result = calculate_sum(decimal, hex);
    demo_struct_t demo = {
        .id = 100,
        .name = allocate_string(64),
        .value = PI * floating
    };
    
    // Conditional compilation
    #ifdef DEBUG
        printf("Debug mode enabled\n");
    #endif
    
    // Switch statement
    switch (result % 3) {
        case 0:
            printf("Divisible by 3\n");
            break;
        case 1:
            printf("Remainder 1\n");
            break;
        default:
            printf("Remainder 2\n");
            break;
    }
    
    // Pointer operations
    char* buffer = malloc(MAX_BUFFER_SIZE);
    if (buffer != NULL) {
        strcpy(buffer, g_message);
        print_demo_info(&demo);
        free(buffer);
    }
    
    return 0;
}

/* Helper functions */
int calculate_sum(int a, int b) {
    return a + b; // Simple arithmetic
}

void print_demo_info(const demo_struct_t* data) {
    if (!data) return;
    
    printf("Demo struct info:\n");
    printf("  ID: %d\n", data->id);
    printf("  Name: %s\n", data->name ? data->name : "NULL");
    printf("  Value: %.2f\n", data->value);
}

char* allocate_string(size_t length) {
    char* str = (char*)calloc(length, sizeof(char));
    if (str) {
        snprintf(str, length, "Allocated string #%d", ++g_counter);
    }
    return str;
}