/* C++ syntax highlighting implementation */
#include "vizero/plugin_interface.h"
#include <string.h>

/* C++ syntax keywords (includes C keywords + C++ specific ones) */
static const char* cpp_keywords[] = {
    "auto", "break", "case", "char", "const", "continue", "default", "do",
    "double", "else", "enum", "extern", "float", "for", "goto", "if",
    "inline", "int", "long", "register", "restrict", "return", "short",
    "signed", "sizeof", "static", "struct", "switch", "typedef", "union",
    "unsigned", "void", "volatile", "while", "_Bool", "_Complex", "_Imaginary",
    /* C++ specific keywords */
    "class", "namespace", "template", "typename", "bool", "true", "false",
    "public", "private", "protected", "virtual", "override", "final",
    "new", "delete", "this", "friend", "operator", "try", "catch", "throw",
    "using", "explicit", "mutable", "constexpr", "nullptr", "decltype",
    NULL
};

/* Check if a word is a C++ keyword */
int is_cpp_keyword(const char* word, size_t length) {
    for (int i = 0; cpp_keywords[i]; i++) {
        if (strlen(cpp_keywords[i]) == length && 
            strncmp(cpp_keywords[i], word, length) == 0) {
            return 1;
        }
    }
    return 0;
}