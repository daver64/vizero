/* Complete buffer implementation with undo/redo support */
#include "vizero/buffer.h"
#include <stdlib.h>
#include <string.h>
#include "vizero/filewatch_poll.h"
#include <stdio.h>

/* Windows compatibility for strdup */
#ifdef _WIN32
#define strdup _strdup
#endif

/* Undo/Redo operation types */
typedef enum {
    UNDO_OP_INSERT_LINE,
    UNDO_OP_DELETE_LINE,
    UNDO_OP_MODIFY_LINE,
    UNDO_OP_SPLIT_LINE,
    UNDO_OP_JOIN_LINES
} undo_op_type_t;

/* Undo/Redo operation structure */
typedef struct undo_op_t {
    undo_op_type_t type;
    size_t line_num;
    char* old_text;      /* Original line content */
    char* new_text;      /* New line content */
    size_t split_pos;    /* For split operations */
    struct undo_op_t* next;
} undo_op_t;

/* Undo/Redo stack */
typedef struct {
    undo_op_t* operations;
    size_t max_operations;
    size_t count;
} undo_stack_t;

struct vizero_buffer_t { 
    char* filename;
    char** lines;
    size_t line_count;
    int modified;
    int readonly;
    int in_undo_redo;     /* Flag to prevent undo tracking during undo/redo */
    undo_stack_t undo_stack;
    undo_stack_t redo_stack;
    uint64_t last_disk_mtime; /* Last known modification time on disk (for auto-reload) */
};

struct vizero_line_t { int dummy; };

/* Undo/Redo helper functions */
static undo_op_t* undo_op_create(undo_op_type_t type, size_t line_num, const char* old_text, const char* new_text, size_t split_pos) {
    undo_op_t* op = (undo_op_t*)malloc(sizeof(undo_op_t));
    if (!op) return NULL;
    
    op->type = type;
    op->line_num = line_num;
    op->old_text = old_text ? strdup(old_text) : NULL;
    op->new_text = new_text ? strdup(new_text) : NULL;
    op->split_pos = split_pos;
    op->next = NULL;
    
    return op;
}

static void undo_op_destroy(undo_op_t* op) {
    if (!op) return;
    
    free(op->old_text);
    free(op->new_text);
    free(op);
}

static void undo_stack_clear(undo_stack_t* stack) {
    if (!stack) return;
    
    undo_op_t* current = stack->operations;
    while (current) {
        undo_op_t* next = current->next;
        undo_op_destroy(current);
        current = next;
    }
    
    stack->operations = NULL;
    stack->count = 0;
}

static void undo_stack_push(undo_stack_t* stack, undo_op_t* op) {
    if (!stack || !op) return;
    
    /* If stack is full, remove oldest operation */
    if (stack->count >= stack->max_operations && stack->operations) {
        undo_op_t* oldest = stack->operations;
        undo_op_t* prev = NULL;
        
        /* Find the last operation (oldest) */
        while (oldest->next) {
            prev = oldest;
            oldest = oldest->next;
        }
        
        if (prev) {
            prev->next = NULL;
        } else {
            stack->operations = NULL;
        }
        
        undo_op_destroy(oldest);
        stack->count--;
    }
    
    /* Add new operation to front */
    op->next = stack->operations;
    stack->operations = op;
    stack->count++;
}

static undo_op_t* undo_stack_pop(undo_stack_t* stack) {
    if (!stack || !stack->operations) return NULL;
    
    undo_op_t* op = stack->operations;
    stack->operations = op->next;
    op->next = NULL;
    stack->count--;
    
    return op;
}

static void buffer_push_undo(vizero_buffer_t* buffer, undo_op_type_t type, size_t line_num, const char* old_text, const char* new_text, size_t split_pos) {
    if (!buffer || buffer->in_undo_redo) return;
    
    undo_op_t* op = undo_op_create(type, line_num, old_text, new_text, split_pos);
    if (op) {
        undo_stack_push(&buffer->undo_stack, op);
        /* Clear redo stack when new operation is performed */
        undo_stack_clear(&buffer->redo_stack);
    }
}

vizero_buffer_t* vizero_buffer_create(void) {
    vizero_buffer_t* buffer = (vizero_buffer_t*)calloc(1, sizeof(vizero_buffer_t));
    if (buffer) {
        buffer->lines = (char**)calloc(1, sizeof(char*));
        buffer->lines[0] = strdup("");
        buffer->line_count = 1;
        
        /* Initialize undo/redo stacks */
        buffer->undo_stack.max_operations = 100; /* Configurable limit */
        buffer->undo_stack.operations = NULL;
        buffer->undo_stack.count = 0;
        
        buffer->redo_stack.max_operations = 100;
        buffer->redo_stack.operations = NULL;
        buffer->redo_stack.count = 0;
    }
    return buffer;
}

vizero_buffer_t* vizero_buffer_create_from_file(const char* filename) {
    vizero_buffer_t* buffer = vizero_buffer_create();
    if (buffer && filename) {
        buffer->filename = strdup(filename);
        
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
            
            /* Mark buffer as unmodified since we just loaded it from disk */
            buffer->modified = 0;
            /* Set last_disk_mtime to current file mtime */
            buffer->last_disk_mtime = vizero_get_file_mtime(filename);
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
    
    /* Clean up undo/redo stacks */
    undo_stack_clear(&buffer->undo_stack);
    undo_stack_clear(&buffer->redo_stack);
    
    free(buffer);
}

const char* vizero_buffer_get_filename(vizero_buffer_t* buffer) {
    return buffer ? buffer->filename : NULL;
}

int vizero_buffer_set_filename(vizero_buffer_t* buffer, const char* filename) {
    if (!buffer) return -1;
    if (buffer->filename) free(buffer->filename);
    buffer->filename = filename ? strdup(filename) : NULL;
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
    
    /* Record undo operation (old line content) */
    buffer_push_undo(buffer, UNDO_OP_MODIFY_LINE, line, current_line, NULL, 0);
    
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
    
    /* Record undo operation (old line content) */
    buffer_push_undo(buffer, UNDO_OP_MODIFY_LINE, line, current_line, NULL, 0);
    
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
    if (!buffer || start_line >= buffer->line_count || end_line >= buffer->line_count) return -1;
    
    /* Handle single line deletion */
    if (start_line == end_line) {
        const char* line_text = vizero_buffer_get_line_text(buffer, start_line);
        if (!line_text) return -1;
        
        size_t line_len = strlen(line_text);
        if (start_col > line_len || end_col > line_len || start_col > end_col) return -1;
        
        /* Create new line without the deleted range */
        size_t new_len = line_len - (end_col - start_col);
        char* new_line = (char*)malloc(new_len + 1);
        if (!new_line) return -1;
        
        /* Copy part before deletion */
        strncpy(new_line, line_text, start_col);
        /* Copy part after deletion */
        strcpy(new_line + start_col, line_text + end_col);
        
        /* Replace the line */
        free(buffer->lines[start_line]);
        buffer->lines[start_line] = new_line;
        buffer->modified = 1;
        return 0;
    }
    
    /* Handle multi-line deletion */
    const char* start_line_text = vizero_buffer_get_line_text(buffer, start_line);
    const char* end_line_text = vizero_buffer_get_line_text(buffer, end_line);
    if (!start_line_text || !end_line_text) return -1;
    
    /* Create new line combining start of first line with end of last line */
    size_t start_line_len = strlen(start_line_text);
    size_t end_line_len = strlen(end_line_text);
    
    if (start_col > start_line_len || end_col > end_line_len) return -1;
    
    char* new_line = (char*)malloc(start_col + (end_line_len - end_col) + 1);
    if (!new_line) return -1;
    
    /* Copy start of first line */
    strncpy(new_line, start_line_text, start_col);
    /* Copy end of last line */
    strcpy(new_line + start_col, end_line_text + end_col);
    
    /* Replace first line with combined line */
    free(buffer->lines[start_line]);
    buffer->lines[start_line] = new_line;
    
    /* Delete intermediate lines */
    for (size_t i = start_line + 1; i <= end_line; i++) {
        free(buffer->lines[i]);
    }
    
    /* Shift remaining lines up */
    size_t lines_deleted = end_line - start_line;
    for (size_t i = start_line + 1; i < buffer->line_count - lines_deleted; i++) {
        buffer->lines[i] = buffer->lines[i + lines_deleted];
    }
    
    buffer->line_count -= lines_deleted;
    buffer->modified = 1;
    return 0;
}

int vizero_buffer_insert_line(vizero_buffer_t* buffer, size_t line_num, const char* text) {
    if (!buffer || line_num > buffer->line_count) return -1;
    
    /* Record undo operation (delete line at insertion point) */
    buffer_push_undo(buffer, UNDO_OP_DELETE_LINE, line_num, NULL, text, 0);
    
    /* Expand lines array */
    char** new_lines = (char**)realloc(buffer->lines, (buffer->line_count + 1) * sizeof(char*));
    if (!new_lines) return -1;
    buffer->lines = new_lines;
    
    /* Shift lines down */
    for (size_t i = buffer->line_count; i > line_num; i--) {
        buffer->lines[i] = buffer->lines[i - 1];
    }
    
    /* Insert new line */
    buffer->lines[line_num] = text ? strdup(text) : strdup("");
    if (!buffer->lines[line_num]) return -1;
    
    buffer->line_count++;
    buffer->modified = 1;
    return 0;
}

int vizero_buffer_delete_line(vizero_buffer_t* buffer, size_t line_num) {
    if (!buffer || line_num >= buffer->line_count || buffer->line_count <= 1) return -1;
    
    /* Record undo operation (insert the deleted line back) */
    buffer_push_undo(buffer, UNDO_OP_INSERT_LINE, line_num, buffer->lines[line_num], NULL, 0);
    
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
    
    /* Record undo operation (join the lines that will be split) */
    buffer_push_undo(buffer, UNDO_OP_JOIN_LINES, line_num, current_line, NULL, col);
    
    /* Create first part (up to split point) */
    char* first_part = (char*)malloc(col + 1);
    if (!first_part) return -1;
    strncpy(first_part, current_line, col);
    first_part[col] = '\0';
    
    /* Create second part (after split point) */
    char* second_part = strdup(current_line + col);
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
    
    /* Record undo operation (split the line that will be joined) */
    buffer_push_undo(buffer, UNDO_OP_SPLIT_LINE, line_num, first_line, second_line, strlen(first_line));
    
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
    if (!buffer || !filename) return -1;
    
    FILE* file = fopen(filename, "r");
    if (!file) {
        return -1;
    }
    
    /* Clear existing lines */
    for (size_t i = 0; i < buffer->line_count; i++) {
        free(buffer->lines[i]);
    }
    free(buffer->lines);
    
    /* Read file into memory first */
    fseek(file, 0, SEEK_END);
    long file_size = ftell(file);
    fseek(file, 0, SEEK_SET);
    
    char* file_content = (char*)malloc(file_size + 1);
    if (!file_content) {
        fclose(file);
        return -1;
    }
    
    size_t bytes_read = fread(file_content, 1, file_size, file);
    file_content[bytes_read] = '\0';
    fclose(file);
    
    /* Count lines first */
    size_t line_count = 1;
    for (size_t i = 0; i < bytes_read; i++) {
        if (file_content[i] == '\n') {
            line_count++;
        }
    }
    
    /* Allocate line array */
    buffer->lines = (char**)calloc(line_count, sizeof(char*));
    if (!buffer->lines) {
        free(file_content);
        return -1;
    }
    
    /* Split content into lines */
    buffer->line_count = 0;
    char* line_start = file_content;
    char* pos = file_content;
    
    while (*pos) {
        if (*pos == '\n') {
            /* Create line (without the newline) */
            size_t line_len = pos - line_start;
            buffer->lines[buffer->line_count] = (char*)malloc(line_len + 1);
            if (buffer->lines[buffer->line_count]) {
                strncpy(buffer->lines[buffer->line_count], line_start, line_len);
                buffer->lines[buffer->line_count][line_len] = '\0';
                buffer->line_count++;
            }
            line_start = pos + 1;
        } else if (*pos == '\r') {
            /* Skip carriage returns */
            *pos = ' '; /* Replace with space to avoid issues */
        }
        pos++;
    }
    
    /* Handle last line if file doesn't end with newline */
    if (line_start < file_content + bytes_read) {
        size_t line_len = (file_content + bytes_read) - line_start;
        buffer->lines[buffer->line_count] = (char*)malloc(line_len + 1);
        if (buffer->lines[buffer->line_count]) {
            strncpy(buffer->lines[buffer->line_count], line_start, line_len);
            buffer->lines[buffer->line_count][line_len] = '\0';
            buffer->line_count++;
        }
    }
    
    /* Ensure we have at least one line */
    if (buffer->line_count == 0) {
        buffer->lines[0] = strdup("");
        buffer->line_count = 1;
    }
    
    free(file_content);
    buffer->modified = 0;
    
    /* Set filename */
    if (buffer->filename) {
        free(buffer->filename);
    }
    buffer->filename = strdup(filename);
    
    return 0;
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
    if (!buffer || !pattern || !result_count) return -1;
    
    *result_count = 0;
    
    /* Use the existing search system from search.cpp - but we need to implement a buffer-local version */
    size_t line_count = vizero_buffer_get_line_count(buffer);
    size_t found_count = 0;
    
    if (use_regex) {
        /* For regex search, we'd need to include the C++ regex functionality here */
        /* For now, implement simple string search */
        use_regex = 0; /* Fall back to simple search */
    }
    
    /* Simple string search implementation */
    for (size_t line = 0; line < line_count && found_count < max_results; line++) {
        const char* line_text = vizero_buffer_get_line_text(buffer, line);
        if (!line_text) continue;
        
        const char* pos = line_text;
        while ((pos = strstr(pos, pattern)) != NULL && found_count < max_results) {
            if (results) {
                results[found_count].line = line;
                results[found_count].column = pos - line_text;
                results[found_count].length = strlen(pattern);
            }
            found_count++;
            pos++; /* Move past this match to find overlapping matches */
        }
    }
    
    *result_count = found_count;
    return 0;
}

int vizero_buffer_undo(vizero_buffer_t* buffer) {
    if (!buffer || !buffer->undo_stack.operations) return -1;
    
    buffer->in_undo_redo = 1; /* Prevent recursive undo tracking */
    
    undo_op_t* op = undo_stack_pop(&buffer->undo_stack);
    if (!op) {
        buffer->in_undo_redo = 0;
        return -1;
    }
    
    /* Push to redo stack before applying undo */
    undo_op_t* redo_op = NULL;
    
    switch (op->type) {
        case UNDO_OP_INSERT_LINE:
            /* Undo: delete the line that was inserted */
            if (op->line_num < buffer->line_count) {
                redo_op = undo_op_create(UNDO_OP_DELETE_LINE, op->line_num, buffer->lines[op->line_num], NULL, 0);
                
                free(buffer->lines[op->line_num]);
                for (size_t i = op->line_num; i < buffer->line_count - 1; i++) {
                    buffer->lines[i] = buffer->lines[i + 1];
                }
                buffer->line_count--;
            }
            break;
            
        case UNDO_OP_DELETE_LINE:
            /* Undo: insert the line that was deleted */
            if (op->old_text && op->line_num <= buffer->line_count) {
                redo_op = undo_op_create(UNDO_OP_INSERT_LINE, op->line_num, NULL, op->old_text, 0);
                
                char** new_lines = (char**)realloc(buffer->lines, (buffer->line_count + 1) * sizeof(char*));
                if (new_lines) {
                    buffer->lines = new_lines;
                    for (size_t i = buffer->line_count; i > op->line_num; i--) {
                        buffer->lines[i] = buffer->lines[i - 1];
                    }
                    buffer->lines[op->line_num] = strdup(op->old_text);
                    buffer->line_count++;
                }
            }
            break;
            
        case UNDO_OP_MODIFY_LINE:
            /* Undo: restore the original line content */
            if (op->old_text && op->line_num < buffer->line_count) {
                redo_op = undo_op_create(UNDO_OP_MODIFY_LINE, op->line_num, buffer->lines[op->line_num], op->old_text, 0);
                
                free(buffer->lines[op->line_num]);
                buffer->lines[op->line_num] = strdup(op->old_text);
            }
            break;
            
        case UNDO_OP_SPLIT_LINE:
            /* Undo: join the lines that were split */
            if (op->line_num < buffer->line_count - 1) {
                char* first_line = buffer->lines[op->line_num];
                char* second_line = buffer->lines[op->line_num + 1];
                
                redo_op = undo_op_create(UNDO_OP_JOIN_LINES, op->line_num, first_line, second_line, op->split_pos);
                
                size_t new_len = strlen(first_line) + strlen(second_line) + 1;
                char* combined = (char*)malloc(new_len);
                if (combined) {
                    strcpy(combined, first_line);
                    strcat(combined, second_line);
                    
                    free(buffer->lines[op->line_num]);
                    free(buffer->lines[op->line_num + 1]);
                    
                    buffer->lines[op->line_num] = combined;
                    for (size_t i = op->line_num + 1; i < buffer->line_count - 1; i++) {
                        buffer->lines[i] = buffer->lines[i + 1];
                    }
                    buffer->line_count--;
                }
            }
            break;
            
        case UNDO_OP_JOIN_LINES:
            /* Undo: split the line that was joined */
            if (op->old_text && op->new_text && op->line_num < buffer->line_count) {
                redo_op = undo_op_create(UNDO_OP_SPLIT_LINE, op->line_num, op->old_text, NULL, op->split_pos);
                
                char** new_lines = (char**)realloc(buffer->lines, (buffer->line_count + 1) * sizeof(char*));
                if (new_lines) {
                    buffer->lines = new_lines;
                    for (size_t i = buffer->line_count; i > op->line_num + 1; i--) {
                        buffer->lines[i] = buffer->lines[i - 1];
                    }
                    
                    free(buffer->lines[op->line_num]);
                    buffer->lines[op->line_num] = strdup(op->old_text);
                    buffer->lines[op->line_num + 1] = strdup(op->new_text);
                    buffer->line_count++;
                }
            }
            break;
    }
    
    /* Add redo operation if created */
    if (redo_op) {
        undo_stack_push(&buffer->redo_stack, redo_op);
    }
    
    undo_op_destroy(op);
    buffer->modified = 1;
    buffer->in_undo_redo = 0; /* Re-enable undo tracking */
    return 0;
}

int vizero_buffer_redo(vizero_buffer_t* buffer) {
    if (!buffer || !buffer->redo_stack.operations) return -1;
    
    buffer->in_undo_redo = 1; /* Prevent recursive undo tracking */
    
    undo_op_t* op = undo_stack_pop(&buffer->redo_stack);
    if (!op) {
        buffer->in_undo_redo = 0;
        return -1;
    }
    
    /* Push to undo stack before applying redo */
    undo_op_t* undo_op = NULL;
    
    switch (op->type) {
        case UNDO_OP_INSERT_LINE:
            /* Redo: delete the line */
            if (op->line_num < buffer->line_count) {
                undo_op = undo_op_create(UNDO_OP_DELETE_LINE, op->line_num, buffer->lines[op->line_num], NULL, 0);
                
                free(buffer->lines[op->line_num]);
                for (size_t i = op->line_num; i < buffer->line_count - 1; i++) {
                    buffer->lines[i] = buffer->lines[i + 1];
                }
                buffer->line_count--;
            }
            break;
            
        case UNDO_OP_DELETE_LINE:
            /* Redo: insert the line */
            if (op->old_text && op->line_num <= buffer->line_count) {
                undo_op = undo_op_create(UNDO_OP_INSERT_LINE, op->line_num, NULL, op->old_text, 0);
                
                char** new_lines = (char**)realloc(buffer->lines, (buffer->line_count + 1) * sizeof(char*));
                if (new_lines) {
                    buffer->lines = new_lines;
                    for (size_t i = buffer->line_count; i > op->line_num; i--) {
                        buffer->lines[i] = buffer->lines[i - 1];
                    }
                    buffer->lines[op->line_num] = strdup(op->old_text);
                    buffer->line_count++;
                }
            }
            break;
            
        case UNDO_OP_MODIFY_LINE:
            /* Redo: restore the modified line content */
            if (op->new_text && op->line_num < buffer->line_count) {
                undo_op = undo_op_create(UNDO_OP_MODIFY_LINE, op->line_num, buffer->lines[op->line_num], op->new_text, 0);
                
                free(buffer->lines[op->line_num]);
                buffer->lines[op->line_num] = strdup(op->new_text);
            }
            break;
            
        case UNDO_OP_SPLIT_LINE:
            /* Redo: join the lines */
            if (op->line_num < buffer->line_count - 1) {
                char* first_line = buffer->lines[op->line_num];
                char* second_line = buffer->lines[op->line_num + 1];
                
                undo_op = undo_op_create(UNDO_OP_JOIN_LINES, op->line_num, first_line, second_line, op->split_pos);
                
                size_t new_len = strlen(first_line) + strlen(second_line) + 1;
                char* combined = (char*)malloc(new_len);
                if (combined) {
                    strcpy(combined, first_line);
                    strcat(combined, second_line);
                    
                    free(buffer->lines[op->line_num]);
                    free(buffer->lines[op->line_num + 1]);
                    
                    buffer->lines[op->line_num] = combined;
                    for (size_t i = op->line_num + 1; i < buffer->line_count - 1; i++) {
                        buffer->lines[i] = buffer->lines[i + 1];
                    }
                    buffer->line_count--;
                }
            }
            break;
            
        case UNDO_OP_JOIN_LINES:
            /* Redo: split the line */
            if (op->old_text && op->new_text && op->line_num < buffer->line_count) {
                undo_op = undo_op_create(UNDO_OP_SPLIT_LINE, op->line_num, op->old_text, NULL, op->split_pos);
                
                char** new_lines = (char**)realloc(buffer->lines, (buffer->line_count + 1) * sizeof(char*));
                if (new_lines) {
                    buffer->lines = new_lines;
                    for (size_t i = buffer->line_count; i > op->line_num + 1; i--) {
                        buffer->lines[i] = buffer->lines[i - 1];
                    }
                    
                    free(buffer->lines[op->line_num]);
                    buffer->lines[op->line_num] = strdup(op->old_text);
                    buffer->lines[op->line_num + 1] = strdup(op->new_text);
                    buffer->line_count++;
                }
            }
            break;
    }
    
    /* Add undo operation if created */
    if (undo_op) {
        undo_stack_push(&buffer->undo_stack, undo_op);
    }
    
    undo_op_destroy(op);
    buffer->modified = 1;
    buffer->in_undo_redo = 0; /* Re-enable undo tracking */
    return 0;
}

int vizero_buffer_can_undo(vizero_buffer_t* buffer) {
    return buffer && buffer->undo_stack.operations ? 1 : 0;
}

int vizero_buffer_can_redo(vizero_buffer_t* buffer) {
    return buffer && buffer->redo_stack.operations ? 1 : 0;
}

void vizero_buffer_get_stats(vizero_buffer_t* buffer, vizero_buffer_stats_t* stats) {
    if (!stats) return;
    
    stats->line_count = buffer ? buffer->line_count : 0;
    stats->character_count = 0;
    stats->word_count = 0;
    stats->byte_count = 0;
}

uint64_t vizero_buffer_get_last_disk_mtime(vizero_buffer_t* buffer) {
    if (!buffer) return 0;
    return buffer->last_disk_mtime;
}

void vizero_buffer_set_last_disk_mtime(vizero_buffer_t* buffer, uint64_t mtime) {
    if (buffer) buffer->last_disk_mtime = mtime;
}