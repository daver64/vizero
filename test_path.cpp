#include "vizero/file_utils.h"
#include <stdio.h>
#include <stdlib.h>

int main() {
    printf("Testing executable path resolution...\n\n");
    
    char* exe_path = vizero_get_executable_path();
    if (exe_path) {
        printf("Executable path: %s\n", exe_path);
        free(exe_path);
    } else {
        printf("Failed to get executable path\n");
    }
    
    char* exe_dir = vizero_get_executable_directory();
    if (exe_dir) {
        printf("Executable directory: %s\n", exe_dir);
        free(exe_dir);
    } else {
        printf("Failed to get executable directory\n");
    }
    
    char* font_path = vizero_get_resource_path("fonts/whitefont.bmp");
    if (font_path) {
        printf("Font resource path: %s\n", font_path);
        free(font_path);
    } else {
        printf("Failed to get font resource path\n");
    }
    
    char* logo_path = vizero_get_resource_path("images/logo.bmp");
    if (logo_path) {
        printf("Logo resource path: %s\n", logo_path);
        free(logo_path);
    } else {
        printf("Failed to get logo resource path\n");
    }
    
    return 0;
}