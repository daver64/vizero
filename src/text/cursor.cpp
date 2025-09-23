/* Stub implementations */
#include "vizero/cursor.h"
#include "vizero/buffer.h"
#include <stdlib.h>

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
void vizero_cursor_move_to_line_start(vizero_cursor_t* cursor) { (void)cursor; }
void vizero_cursor_move_to_line_end(vizero_cursor_t* cursor) { (void)cursor; }
void vizero_cursor_move_word_forward(vizero_cursor_t* cursor) { (void)cursor; }
void vizero_cursor_move_word_backward(vizero_cursor_t* cursor) { (void)cursor; }
void vizero_cursor_move_to_buffer_start(vizero_cursor_t* cursor) { (void)cursor; }
void vizero_cursor_move_to_buffer_end(vizero_cursor_t* cursor) { (void)cursor; }
vizero_buffer_t* vizero_cursor_get_buffer(vizero_cursor_t* cursor) { return cursor ? cursor->buffer : NULL; }
int vizero_cursor_is_at_line_start(vizero_cursor_t* cursor) { (void)cursor; return 0; }
int vizero_cursor_is_at_line_end(vizero_cursor_t* cursor) { (void)cursor; return 0; }

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