/* Stub implementations */
#include "vizero/string_utils.h"
#include <stdlib.h>
#include <string.h>

char* vizero_string_duplicate(const char* str) {
    return str ? _strdup(str) : NULL;
}

char* vizero_string_substring(const char* str, size_t start, size_t length) {
    (void)str; (void)start; (void)length; return NULL;
}

int vizero_string_compare_case_insensitive(const char* str1, const char* str2) {
    (void)str1; (void)str2; return 0;
}

int vizero_string_starts_with(const char* str, const char* prefix) {
    (void)str; (void)prefix; return 0;
}

int vizero_string_ends_with(const char* str, const char* suffix) {
    (void)str; (void)suffix; return 0;
}

char** vizero_string_split(const char* str, const char* delimiter, size_t* count) {
    (void)str; (void)delimiter; if (count) *count = 0; return NULL;
}

void vizero_string_array_free(char** array, size_t count) {
    (void)array; (void)count;
}

char* vizero_string_join(char** array, size_t count, const char* separator) {
    (void)array; (void)count; (void)separator; return NULL;
}

char* vizero_string_trim(const char* str) {
    (void)str; return NULL;
}

char* vizero_string_trim_left(const char* str) {
    (void)str; return NULL;
}

char* vizero_string_trim_right(const char* str) {
    (void)str; return NULL;
}

int vizero_string_visual_column(const char* str, int logical_column, int tab_width) {
    if (!str || logical_column < 0) return 0;
    
    int visual_col = 0;
    for (int i = 0; i < logical_column && str[i] != '\0'; i++) {
        if (str[i] == '\t') {
            /* Advance to next tab stop */
            visual_col = ((visual_col / tab_width) + 1) * tab_width;
        } else {
            visual_col++;
        }
    }
    return visual_col;
}