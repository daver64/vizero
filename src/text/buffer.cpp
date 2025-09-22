/* Stub implementations for basic compilation */
#include "vizero/buffer.h"
#include <stdlib.h>
#include <string.h>
#include <stdio.h>

/* Windows compatibility for strdup */
#ifdef _WIN32
#define strdup _strdup
#endif

struct vizero_buffer_t { 
    char* filename;
    char** lines;
    size_t line_count;
    int modified;
    int readonly;
};

struct vizero_line_t { int dummy; };

vizero_buffer_t* vizero_buffer_create(void) {
    vizero_buffer_t* buffer = (vizero_buffer_t*)calloc(1, sizeof(vizero_buffer_t));
    if (buffer) {
        buffer->lines = (char**)calloc(1, sizeof(char*));
        buffer->lines[0] = _strdup("");
        buffer->line_count = 1;
    }
    return buffer;
}

vizero_buffer_t* vizero_buffer_create_from_file(const char* filename) {
    vizero_buffer_t* buffer = vizero_buffer_create();
    if (buffer && filename) {
        buffer->filename = _strdup(filename);
        
        /* Load file content */
        FILE* file = fopen(filename, "r");
        if (file) {
            /* Clear the initial empty line since we're loading from file */
            if (buffer->line_count == 1 && buffer->lines[0] && strlen(buffer->lines[0]) == 0) {
                free(buffer->lines[0]);
                buffer->line_count = 0;
            }
            
            char line_buffer[1024];
            while (fgets(line_buffer, sizeof(line_buffer), file)) {
                /* Remove newline if present */
                size_t len = strlen(line_buffer);
                if (len > 0 && line_buffer[len - 1] == '\n') {
                    line_buffer[len - 1] = '\0';
                }
                
                /* Add line to buffer */
                vizero_buffer_insert_line(buffer, buffer->line_count, line_buffer);
            }
            fclose(file);
            
            /* If file was empty, ensure we have at least one empty line */
            if (buffer->line_count == 0) {
                vizero_buffer_insert_line(buffer, 0, "");
            }
        } else {
            /* File doesn't exist or can't be opened - start with empty buffer */
            printf("Note: File '%s' not found, creating new buffer\n", filename);
        }
    }
    return buffer;
}

void vizero_buffer_destroy(vizero_buffer_t* buffer) {
    if (!buffer) return;
    
    if (buffer->filename) free(buffer->filename);
    if (buffer->lines) {
        for (size_t i = 0; i < buffer->line_count; i++) {
            free(buffer->lines[i]);
        }
        free(buffer->lines);
    }
    free(buffer);
}

const char* vizero_buffer_get_filename(vizero_buffer_t* buffer) {
    return buffer ? buffer->filename : NULL;
}

int vizero_buffer_set_filename(vizero_buffer_t* buffer, const char* filename) {
    if (!buffer) return -1;
    if (buffer->filename) free(buffer->filename);
    buffer->filename = filename ? _strdup(filename) : NULL;
    return 0;
}

int vizero_buffer_is_modified(vizero_buffer_t* buffer) {
    return buffer ? buffer->modified : 0;
}

int vizero_buffer_is_readonly(vizero_buffer_t* buffer) {
    return buffer ? buffer->readonly : 0;
}

void vizero_buffer_set_readonly(vizero_buffer_t* buffer, int readonly) {
    if (buffer) buffer->readonly = readonly;
}

size_t vizero_buffer_get_line_count(vizero_buffer_t* buffer) {
    return buffer ? buffer->line_count : 0;
}

const char* vizero_buffer_get_text(vizero_buffer_t* buffer) {
    if (!buffer || !buffer->lines || buffer->line_count == 0) {
        return "";
    }
    
    /* Calculate total length needed */
    size_t total_len = 0;
    for (size_t i = 0; i < buffer->line_count; i++) {
        if (buffer->lines[i]) {
            total_len += strlen(buffer->lines[i]);
        }
        if (i < buffer->line_count - 1) total_len++; /* For newline */
    }
    total_len++; /* For null terminator */
    
    /* Allocate buffer for combined text */
    static char* combined_text = NULL;
    static size_t combined_capacity = 0;
    
    if (total_len > combined_capacity) {
        free(combined_text);
        combined_text = (char*)malloc(total_len);
        combined_capacity = total_len;
    }
    
    if (!combined_text) return "";
    
    /* Combine all lines */
    combined_text[0] = '\0';
    for (size_t i = 0; i < buffer->line_count; i++) {
        if (buffer->lines[i]) {
            strcat(combined_text, buffer->lines[i]);
        }
        if (i < buffer->line_count - 1) {
            strcat(combined_text, "\n");
        }
    }
    
    return combined_text;
}

vizero_line_t* vizero_buffer_get_line(vizero_buffer_t* buffer, size_t line_num) {
    (void)buffer; (void)line_num;
    return NULL;
}

const char* vizero_buffer_get_line_text(vizero_buffer_t* buffer, size_t line_num) {
    if (!buffer || line_num >= buffer->line_count) return NULL;
    return buffer->lines[line_num];
}

size_t vizero_buffer_get_line_length(vizero_buffer_t* buffer, size_t line_num) {
    const char* line = vizero_buffer_get_line_text(buffer, line_num);
    return line ? strlen(line) : 0;
}

/* Basic buffer text operations */
int vizero_buffer_insert_char(vizero_buffer_t* buffer, size_t line, size_t col, char c) {
    if (!buffer || line >= buffer->line_count) return -1;
    
    char* current_line = buffer->lines[line];
    size_t line_len = strlen(current_line);
    
    /* Clamp column to line length */
    if (col > line_len) col = line_len;
    
    /* Allocate new line with room for one more character */
    char* new_line = (char*)malloc(line_len + 2);
    if (!new_line) return -1;
    
    /* Copy before insertion point */
    strncpy(new_line, current_line, col);
    /* Insert character */
    new_line[col] = c;
    /* Copy after insertion point */
    strcpy(new_line + col + 1, current_line + col);
    
    /* Replace line */
    free(buffer->lines[line]);
    buffer->lines[line] = new_line;
    buffer->modified = 1;
    
    return 0;
}

int vizero_buffer_insert_text(vizero_buffer_t* buffer, size_t line, size_t col, const char* text) {
    if (!buffer || !text) return -1;
    
    /* Insert each character */
    for (const char* c = text; *c; c++) {
        if (vizero_buffer_insert_char(buffer, line, col, *c) != 0) {
            return -1;
        }
        col++;
    }
    return 0;
}

int vizero_buffer_delete_char(vizero_buffer_t* buffer, size_t line, size_t col) {
    if (!buffer || line >= buffer->line_count) return -1;
    
    char* current_line = buffer->lines[line];
    size_t line_len = strlen(current_line);
    
    if (col >= line_len) return -1;
    
    /* Allocate new line with one less character */
    char* new_line = (char*)malloc(line_len);
    if (!new_line) return -1;
    
    /* Copy before deletion point */
    strncpy(new_line, current_line, col);
    new_line[col] = '\0';
    /* Copy after deletion point */
    strcat(new_line, current_line + col + 1);
    
    /* Replace line */
    free(buffer->lines[line]);
    buffer->lines[line] = new_line;
    buffer->modified = 1;
    
    return 0;
}

int vizero_buffer_delete_range(vizero_buffer_t* buffer, size_t start_line, size_t start_col, size_t end_line, size_t end_col) {
    (void)buffer; (void)start_line; (void)start_col; (void)end_line; (void)end_col; return 0;
}

int vizero_buffer_insert_line(vizero_buffer_t* buffer, size_t line_num, const char* text) {
    if (!buffer || line_num > buffer->line_count) return -1;
    
    /* Expand lines array */
    char** new_lines = (char**)realloc(buffer->lines, (buffer->line_count + 1) * sizeof(char*));
    if (!new_lines) return -1;
    buffer->lines = new_lines;
    
    /* Shift lines down */
    for (size_t i = buffer->line_count; i > line_num; i--) {
        buffer->lines[i] = buffer->lines[i - 1];
    }
    
    /* Insert new line */
    buffer->lines[line_num] = text ? _strdup(text) : _strdup("");
    if (!buffer->lines[line_num]) return -1;
    
    buffer->line_count++;
    buffer->modified = 1;
    return 0;
}

int vizero_buffer_delete_line(vizero_buffer_t* buffer, size_t line_num) {
    if (!buffer || line_num >= buffer->line_count || buffer->line_count <= 1) return -1;
    
    /* Free the line */
    free(buffer->lines[line_num]);
    
    /* Shift lines up */
    for (size_t i = line_num; i < buffer->line_count - 1; i++) {
        buffer->lines[i] = buffer->lines[i + 1];
    }
    
    buffer->line_count--;
    buffer->modified = 1;
    return 0;
}

int vizero_buffer_split_line(vizero_buffer_t* buffer, size_t line_num, size_t col) {
    if (!buffer || line_num >= buffer->line_count) return -1;
    
    char* current_line = buffer->lines[line_num];
    size_t line_len = strlen(current_line);
    
    /* Clamp column to line length */
    if (col > line_len) col = line_len;
    
    /* Create first part (up to split point) */
    char* first_part = (char*)malloc(col + 1);
    if (!first_part) return -1;
    strncpy(first_part, current_line, col);
    first_part[col] = '\0';
    
    /* Create second part (after split point) */
    char* second_part = _strdup(current_line + col);
    if (!second_part) {
        free(first_part);
        return -1;
    }
    
    /* Expand lines array */
    char** new_lines = (char**)realloc(buffer->lines, (buffer->line_count + 1) * sizeof(char*));
    if (!new_lines) {
        free(first_part);
        free(second_part);
        return -1;
    }
    buffer->lines = new_lines;
    
    /* Shift lines down */
    for (size_t i = buffer->line_count; i > line_num + 1; i--) {
        buffer->lines[i] = buffer->lines[i - 1];
    }
    
    /* Set new lines */
    free(buffer->lines[line_num]);
    buffer->lines[line_num] = first_part;
    buffer->lines[line_num + 1] = second_part;
    buffer->line_count++;
    buffer->modified = 1;
    
    return 0;
}

int vizero_buffer_join_lines(vizero_buffer_t* buffer, size_t line_num) {
    if (!buffer || line_num >= buffer->line_count - 1) return -1;
    
    char* first_line = buffer->lines[line_num];
    char* second_line = buffer->lines[line_num + 1];
    
    /* Create combined line */
    size_t new_len = strlen(first_line) + strlen(second_line) + 1;
    char* combined = (char*)malloc(new_len);
    if (!combined) return -1;
    
    strcpy(combined, first_line);
    strcat(combined, second_line);
    
    /* Free old lines */
    free(buffer->lines[line_num]);
    free(buffer->lines[line_num + 1]);
    
    /* Shift lines up */
    buffer->lines[line_num] = combined;
    for (size_t i = line_num + 1; i < buffer->line_count - 1; i++) {
        buffer->lines[i] = buffer->lines[i + 1];
    }
    
    buffer->line_count--;
    buffer->modified = 1;
    
    return 0;
}

int vizero_buffer_load_from_file(vizero_buffer_t* buffer, const char* filename) {
    (void)buffer; (void)filename; return 0;
}

int vizero_buffer_save_to_file(vizero_buffer_t* buffer, const char* filename) {
    if (!buffer || !filename) return -1;
    
    FILE* file = fopen(filename, "w");
    if (!file) {
        return -1;
    }
    
    /* Write each line to the file */
    for (size_t i = 0; i < buffer->line_count; i++) {
        if (buffer->lines[i]) {
            fprintf(file, "%s", buffer->lines[i]);
        }
        /* Add newline except for the last line if it doesn't have one */
        if (i < buffer->line_count - 1) {
            fprintf(file, "\n");
        }
    }
    
    fclose(file);
    
    /* Mark buffer as saved */
    buffer->modified = 0;
    
    /* Update filename if it was different */
    if (buffer->filename && strcmp(buffer->filename, filename) != 0) {
        free(buffer->filename);
        buffer->filename = strdup(filename);
    } else if (!buffer->filename) {
        buffer->filename = strdup(filename);
    }
    
    return 0;
}

int vizero_buffer_save(vizero_buffer_t* buffer) {
    if (!buffer) {
        return -1;
    }
    
    /* If buffer has no filename, we can't save it */
    if (!buffer->filename) {
        return -1; /* Should prompt for filename in the future */
    }
    
    return vizero_buffer_save_to_file(buffer, buffer->filename);
}

int vizero_buffer_search(vizero_buffer_t* buffer, const char* pattern, int use_regex, vizero_search_result_t* results, size_t max_results, size_t* result_count) {
    (void)buffer; (void)pattern; (void)use_regex; (void)results; (void)max_results;
    if (result_count) *result_count = 0;
    return 0;
}

int vizero_buffer_undo(vizero_buffer_t* buffer) {
    (void)buffer; return 0;
}

int vizero_buffer_redo(vizero_buffer_t* buffer) {
    (void)buffer; return 0;
}

int vizero_buffer_can_undo(vizero_buffer_t* buffer) {
    (void)buffer; return 0;
}

int vizero_buffer_can_redo(vizero_buffer_t* buffer) {
    (void)buffer; return 0;
}

void vizero_buffer_get_stats(vizero_buffer_t* buffer, vizero_buffer_stats_t* stats) {
    if (!stats) return;
    
    stats->line_count = buffer ? buffer->line_count : 0;
    stats->character_count = 0;
    stats->word_count = 0;
    stats->byte_count = 0;
}