/* C syntax highlighting implementation */
#include "vizero/plugin_interface.h"
#include <string.h>

/* C syntax keywords */
static const char* c_keywords[] = {
    "auto", "break", "case", "char", "const", "continue", "default", "do",
    "double", "else", "enum", "extern", "float", "for", "goto", "if",
    "inline", "int", "long", "register", "restrict", "return", "short",
    "signed", "sizeof", "static", "struct", "switch", "typedef", "union",
    "unsigned", "void", "volatile", "while", "_Bool", "_Complex", "_Imaginary",
    NULL
};

/* Check if a word is a C keyword */
int is_c_keyword(const char* word, size_t length) {
    for (int i = 0; c_keywords[i]; i++) {
        if (strlen(c_keywords[i]) == length && 
            strncmp(c_keywords[i], word, length) == 0) {
            return 1;
        }
    }
    return 0;
}