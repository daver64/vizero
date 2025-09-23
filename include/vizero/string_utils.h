#ifndef VIZERO_STRING_UTILS_H
#define VIZERO_STRING_UTILS_H

#ifdef __cplusplus
extern "C" {
#endif

#include <stddef.h>

/* String operations */
char* vizero_string_duplicate(const char* str);
char* vizero_string_substring(const char* str, size_t start, size_t length);
int vizero_string_compare_case_insensitive(const char* str1, const char* str2);
int vizero_string_starts_with(const char* str, const char* prefix);
int vizero_string_ends_with(const char* str, const char* suffix);

/* String arrays */
char** vizero_string_split(const char* str, const char* delimiter, size_t* count);
void vizero_string_array_free(char** array, size_t count);
char* vizero_string_join(char** array, size_t count, const char* separator);

/* String trimming */
char* vizero_string_trim(const char* str);
char* vizero_string_trim_left(const char* str);
char* vizero_string_trim_right(const char* str);

/* Visual column calculations */
int vizero_string_visual_column(const char* str, int logical_column, int tab_width);

#ifdef __cplusplus
}
#endif

#endif /* VIZERO_STRING_UTILS_H */