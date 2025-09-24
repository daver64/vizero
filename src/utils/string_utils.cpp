/* Complete string utilities implementation */
#include "vizero/string_utils.h"
#include <stdlib.h>
#include <string.h>
#include <ctype.h>

/* Windows compatibility */
#ifdef _WIN32
#define strdup _strdup
#define strcasecmp _stricmp
#else
#include <strings.h>
#endif

char* vizero_string_duplicate(const char* str) {
    return str ? strdup(str) : NULL;
}

char* vizero_string_substring(const char* str, size_t start, size_t length) {
    if (!str) return NULL;
    
    size_t str_len = strlen(str);
    if (start >= str_len) return NULL;
    
    /* Adjust length if it goes beyond string end */
    if (start + length > str_len) {
        length = str_len - start;
    }
    
    char* substr = (char*)malloc(length + 1);
    if (!substr) return NULL;
    
    strncpy(substr, str + start, length);
    substr[length] = '\0';
    
    return substr;
}

int vizero_string_compare_case_insensitive(const char* str1, const char* str2) {
    if (!str1 && !str2) return 0;
    if (!str1) return -1;
    if (!str2) return 1;
    
    return strcasecmp(str1, str2);
}

int vizero_string_starts_with(const char* str, const char* prefix) {
    if (!str || !prefix) return 0;
    
    size_t prefix_len = strlen(prefix);
    size_t str_len = strlen(str);
    
    if (prefix_len > str_len) return 0;
    
    return strncmp(str, prefix, prefix_len) == 0;
}

int vizero_string_ends_with(const char* str, const char* suffix) {
    if (!str || !suffix) return 0;
    
    size_t str_len = strlen(str);
    size_t suffix_len = strlen(suffix);
    
    if (suffix_len > str_len) return 0;
    
    return strcmp(str + str_len - suffix_len, suffix) == 0;
}

char** vizero_string_split(const char* str, const char* delimiter, size_t* count) {
    if (!str || !delimiter || !count) {
        if (count) *count = 0;
        return NULL;
    }
    
    *count = 0;
    
    /* Make a copy of the string to work with */
    char* str_copy = vizero_string_duplicate(str);
    if (!str_copy) return NULL;
    
    /* Count tokens first */
    char* temp_copy = vizero_string_duplicate(str);
    if (!temp_copy) {
        free(str_copy);
        return NULL;
    }
    
    char* token = strtok(temp_copy, delimiter);
    size_t token_count = 0;
    while (token) {
        token_count++;
        token = strtok(NULL, delimiter);
    }
    free(temp_copy);
    
    if (token_count == 0) {
        free(str_copy);
        return NULL;
    }
    
    /* Allocate array for tokens */
    char** tokens = (char**)malloc(token_count * sizeof(char*));
    if (!tokens) {
        free(str_copy);
        return NULL;
    }
    
    /* Extract tokens */
    token = strtok(str_copy, delimiter);
    size_t i = 0;
    while (token && i < token_count) {
        tokens[i] = vizero_string_duplicate(token);
        if (!tokens[i]) {
            /* Cleanup on failure */
            for (size_t j = 0; j < i; j++) {
                free(tokens[j]);
            }
            free(tokens);
            free(str_copy);
            return NULL;
        }
        i++;
        token = strtok(NULL, delimiter);
    }
    
    free(str_copy);
    *count = token_count;
    return tokens;
}

void vizero_string_array_free(char** array, size_t count) {
    if (!array) return;
    
    for (size_t i = 0; i < count; i++) {
        free(array[i]);
    }
    free(array);
}

char* vizero_string_join(char** array, size_t count, const char* separator) {
    if (!array || count == 0) return NULL;
    
    const char* sep = separator ? separator : "";
    size_t sep_len = strlen(sep);
    
    /* Calculate total length needed */
    size_t total_len = 0;
    for (size_t i = 0; i < count; i++) {
        if (array[i]) {
            total_len += strlen(array[i]);
            if (i < count - 1) {
                total_len += sep_len;
            }
        }
    }
    
    char* result = (char*)malloc(total_len + 1);
    if (!result) return NULL;
    
    result[0] = '\0';
    
    for (size_t i = 0; i < count; i++) {
        if (array[i]) {
            strcat(result, array[i]);
            if (i < count - 1 && sep_len > 0) {
                strcat(result, sep);
            }
        }
    }
    
    return result;
}

char* vizero_string_trim(const char* str) {
    if (!str) return NULL;
    
    /* Find start of non-whitespace */
    const char* start = str;
    while (*start && isspace(*start)) {
        start++;
    }
    
    /* Find end of non-whitespace */
    const char* end = str + strlen(str) - 1;
    while (end > start && isspace(*end)) {
        end--;
    }
    
    /* Calculate length */
    size_t len = end - start + 1;
    
    char* trimmed = (char*)malloc(len + 1);
    if (!trimmed) return NULL;
    
    strncpy(trimmed, start, len);
    trimmed[len] = '\0';
    
    return trimmed;
}

char* vizero_string_trim_left(const char* str) {
    if (!str) return NULL;
    
    /* Find start of non-whitespace */
    const char* start = str;
    while (*start && isspace(*start)) {
        start++;
    }
    
    return vizero_string_duplicate(start);
}

char* vizero_string_trim_right(const char* str) {
    if (!str) return NULL;
    
    size_t len = strlen(str);
    if (len == 0) return vizero_string_duplicate("");
    
    /* Find end of non-whitespace */
    const char* end = str + len - 1;
    while (end >= str && isspace(*end)) {
        end--;
    }
    
    /* Calculate length */
    size_t trimmed_len = end - str + 1;
    
    char* trimmed = (char*)malloc(trimmed_len + 1);
    if (!trimmed) return NULL;
    
    strncpy(trimmed, str, trimmed_len);
    trimmed[trimmed_len] = '\0';
    
    return trimmed;
}

int vizero_string_visual_column(const char* str, int logical_column, int tab_width) {
    if (!str || logical_column < 0 || tab_width <= 0) return 0;
    
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
