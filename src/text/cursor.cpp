/* Complete cursor implementation */
#include "vizero/cursor.h"
#include "vizero/buffer.h"
#include <stdlib.h>
#include <string.h>
#include <ctype.h>

struct vizero_cursor_t { 
    vizero_buffer_t* buffer;
    size_t line;
    size_t column;
};

vizero_cursor_t* vizero_cursor_create(vizero_buffer_t* buffer) {
    vizero_cursor_t* cursor = (vizero_cursor_t*)calloc(1, sizeof(vizero_cursor_t));
    if (cursor) cursor->buffer = buffer;
    return cursor;
}

void vizero_cursor_destroy(vizero_cursor_t* cursor) { free(cursor); }
size_t vizero_cursor_get_line(vizero_cursor_t* cursor) { return cursor ? cursor->line : 0; }
size_t vizero_cursor_get_column(vizero_cursor_t* cursor) { return cursor ? cursor->column : 0; }
vizero_position_t vizero_cursor_get_position(vizero_cursor_t* cursor) { 
    vizero_position_t pos = {0, 0};
    if (cursor) {
        pos.line = cursor->line;
        pos.column = cursor->column;
    }
    return pos;
}
void vizero_cursor_set_position(vizero_cursor_t* cursor, size_t line, size_t column) { 
    if (cursor) { cursor->line = line; cursor->column = column; }
}
void vizero_cursor_move_up(vizero_cursor_t* cursor) { 
    if (cursor && cursor->line > 0) {
        cursor->line--;
        /* Clamp column to new line length */
        if (cursor->buffer) {
            size_t line_len = vizero_buffer_get_line_length(cursor->buffer, cursor->line);
            if (cursor->column > line_len) {
                cursor->column = line_len;
            }
        }
    }
}
void vizero_cursor_move_down(vizero_cursor_t* cursor) { 
    if (cursor && cursor->buffer) {
        size_t line_count = vizero_buffer_get_line_count(cursor->buffer);
        if (cursor->line < line_count - 1) {
            cursor->line++;
            /* Clamp column to new line length */
            size_t line_len = vizero_buffer_get_line_length(cursor->buffer, cursor->line);
            if (cursor->column > line_len) {
                cursor->column = line_len;
            }
        }
    }
}
void vizero_cursor_move_left(vizero_cursor_t* cursor) { 
    if (cursor && cursor->column > 0) {
        cursor->column--;
    }
}
void vizero_cursor_move_right(vizero_cursor_t* cursor) { 
    if (cursor && cursor->buffer) {
        size_t line_len = vizero_buffer_get_line_length(cursor->buffer, cursor->line);
        if (cursor->column < line_len) {
            cursor->column++;
        }
    }
}
void vizero_cursor_move_to_line_start(vizero_cursor_t* cursor) { 
    if (cursor) {
        cursor->column = 0;
    }
}

void vizero_cursor_move_to_line_end(vizero_cursor_t* cursor) { 
    if (cursor && cursor->buffer) {
        size_t line_len = vizero_buffer_get_line_length(cursor->buffer, cursor->line);
        cursor->column = line_len;
    }
}
void vizero_cursor_move_word_forward(vizero_cursor_t* cursor) { 
    if (!cursor || !cursor->buffer) return;
    
    const char* line_text = vizero_buffer_get_line_text(cursor->buffer, cursor->line);
    if (!line_text) return;
    
    size_t line_len = strlen(line_text);
    size_t col = cursor->column;
    
    /* Skip current word */
    while (col < line_len && (isalnum(line_text[col]) || line_text[col] == '_')) {
        col++;
    }
    
    /* Skip whitespace */
    while (col < line_len && isspace(line_text[col])) {
        col++;
    }
    
    /* If at end of line, move to next line */
    if (col >= line_len) {
        size_t line_count = vizero_buffer_get_line_count(cursor->buffer);
        if (cursor->line < line_count - 1) {
            cursor->line++;
            cursor->column = 0;
        } else {
            cursor->column = line_len;
        }
    } else {
        cursor->column = col;
    }
}

void vizero_cursor_move_word_backward(vizero_cursor_t* cursor) { 
    if (!cursor || !cursor->buffer) return;
    
    const char* line_text = vizero_buffer_get_line_text(cursor->buffer, cursor->line);
    if (!line_text) return;
    
    size_t col = cursor->column;
    
    /* If at beginning of line, move to previous line end */
    if (col == 0) {
        if (cursor->line > 0) {
            cursor->line--;
            size_t prev_line_len = vizero_buffer_get_line_length(cursor->buffer, cursor->line);
            cursor->column = prev_line_len;
        }
        return;
    }
    
    /* Move back one character */
    col--;
    
    /* Skip whitespace */
    while (col > 0 && isspace(line_text[col])) {
        col--;
    }
    
    /* Skip to beginning of current word */
    while (col > 0 && (isalnum(line_text[col - 1]) || line_text[col - 1] == '_')) {
        col--;
    }
    
    cursor->column = col;
}
void vizero_cursor_move_to_buffer_start(vizero_cursor_t* cursor) { 
    if (cursor) {
        cursor->line = 0;
        cursor->column = 0;
    }
}

void vizero_cursor_move_to_buffer_end(vizero_cursor_t* cursor) { 
    if (cursor && cursor->buffer) {
        size_t line_count = vizero_buffer_get_line_count(cursor->buffer);
        if (line_count > 0) {
            cursor->line = line_count - 1;
            cursor->column = vizero_buffer_get_line_length(cursor->buffer, cursor->line);
        } else {
            cursor->line = 0;
            cursor->column = 0;
        }
    }
}
vizero_buffer_t* vizero_cursor_get_buffer(vizero_cursor_t* cursor) { return cursor ? cursor->buffer : NULL; }
int vizero_cursor_is_at_line_start(vizero_cursor_t* cursor) { 
    return cursor ? (cursor->column == 0) : 0;
}

int vizero_cursor_is_at_line_end(vizero_cursor_t* cursor) { 
    if (!cursor || !cursor->buffer) return 0;
    
    size_t line_len = vizero_buffer_get_line_length(cursor->buffer, cursor->line);
    return cursor->column >= line_len;
}

void vizero_cursor_move_page_up(vizero_cursor_t* cursor) {
    if (!cursor || !cursor->buffer) return;
    
    /* Move up by approximately one screen height (20 lines) */
    const size_t page_size = 20;
    
    if (cursor->line >= page_size) {
        cursor->line -= page_size;
    } else {
        cursor->line = 0;
    }
    
    /* Clamp column to new line length */
    size_t line_len = vizero_buffer_get_line_length(cursor->buffer, cursor->line);
    if (cursor->column > line_len) {
        cursor->column = line_len;
    }
}

void vizero_cursor_move_page_down(vizero_cursor_t* cursor) {
    if (!cursor || !cursor->buffer) return;
    
    /* Move down by approximately one screen height (20 lines) */
    const size_t page_size = 20;
    size_t line_count = vizero_buffer_get_line_count(cursor->buffer);
    
    if (cursor->line + page_size < line_count) {
        cursor->line += page_size;
    } else {
        cursor->line = line_count > 0 ? line_count - 1 : 0;
    }
    
    /* Clamp column to new line length */
    size_t line_len = vizero_buffer_get_line_length(cursor->buffer, cursor->line);
    if (cursor->column > line_len) {
        cursor->column = line_len;
    }
}
