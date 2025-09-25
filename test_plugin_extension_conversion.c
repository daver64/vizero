/*
 * Test program to demonstrate cross-platform plugin extension conversion
 * This shows how the plugin manager converts .dll paths to .so on Unix platforms
 */
#include <stdio.h>
#include <string.h>

void test_extension_conversion(const char* dll_path) {
    char plugin_filename[512];
    strncpy(plugin_filename, dll_path, sizeof(plugin_filename) - 1);
    plugin_filename[sizeof(plugin_filename) - 1] = '\0';
    
    printf("Original path: %s\n", dll_path);
    
#ifdef _WIN32
    printf("Windows: Using original path\n");
#else
    /* Convert .dll extension to .so on Unix platforms */
    char* ext = strrchr(plugin_filename, '.');
    if (ext && strcmp(ext, ".dll") == 0) {
        strcpy(ext, ".so");
        printf("Unix: Converted to %s\n", plugin_filename);
    } else {
        printf("Unix: No conversion needed\n");
    }
#endif
    printf("---\n");
}

int main() {
    printf("Plugin Extension Conversion Test\n");
    printf("================================\n\n");
    
    // Test cases from the manifest
    test_extension_conversion("example_plugin.dll");
    test_extension_conversion("file_browser.dll");
    test_extension_conversion("syntax_c.dll");
    test_extension_conversion("syntax_python.dll");
    test_extension_conversion("clangd.dll");
    
    // Edge cases
    test_extension_conversion("some_plugin.so");  // Already .so
    test_extension_conversion("no_extension");    // No extension
    test_extension_conversion("multiple.dot.dll"); // Multiple dots
    
    return 0;
}