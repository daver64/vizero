/* Line operations implementation */
#include "vizero/line.h"
#include <stdlib.h>
#include <string.h>

vizero_line_t* vizero_line_create(const char* text) {
    vizero_line_t* line = (vizero_line_t*)calloc(1, sizeof(vizero_line_t));
    if (!line) return NULL;
    
    if (text) {
        line->length = strlen(text);
        line->capacity = line->length + 1;
        line->text = (char*)malloc(line->capacity);
        if (line->text) {
            strcpy(line->text, text);
        } else {
            free(line);
            return NULL;
        }
    } else {
        line->length = 0;
        line->capacity = 1;
        line->text = (char*)malloc(1);
        if (line->text) {
            line->text[0] = '\0';
        } else {
            free(line);
            return NULL;
        }
    }
    
    line->dirty = 0;
    return line;
}

void vizero_line_destroy(vizero_line_t* line) {
    if (line) {
        free(line->text);
        free(line);
    }
}

int vizero_line_insert(vizero_line_t* line, size_t pos, const char* text) {
    if (!line || !text || pos > line->length) return -1;
    
    size_t insert_len = strlen(text);
    size_t new_len = line->length + insert_len;
    
    if (new_len + 1 > line->capacity) {
        size_t new_capacity = (new_len + 1) * 2;
        char* new_text = (char*)realloc(line->text, new_capacity);
        if (!new_text) return -1;
        line->text = new_text;
        line->capacity = new_capacity;
    }
    
    memmove(line->text + pos + insert_len, line->text + pos, line->length - pos + 1);
    memcpy(line->text + pos, text, insert_len);
    line->length = new_len;
    line->dirty = 1;
    
    return 0;
}

int vizero_line_delete(vizero_line_t* line, size_t start, size_t end) {
    if (!line || start >= line->length || end <= start || end > line->length) return -1;
    
    size_t delete_len = end - start;
    memmove(line->text + start, line->text + end, line->length - end + 1);
    line->length -= delete_len;
    line->dirty = 1;
    
    return 0;
}

const char* vizero_line_get_text(vizero_line_t* line) {
    return line ? line->text : NULL;
}

size_t vizero_line_get_length(vizero_line_t* line) {
    return line ? line->length : 0;
}