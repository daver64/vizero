#ifndef VIZERO_CURSOR_H
#define VIZERO_CURSOR_H

#ifdef __cplusplus
extern "C" {
#endif

#include <stddef.h>
#include "plugin_interface.h"  /* For vizero_position_t */

/* Forward declarations */
typedef struct vizero_cursor_t vizero_cursor_t;
typedef struct vizero_buffer_t vizero_buffer_t;

/* Cursor creation and destruction */
vizero_cursor_t* vizero_cursor_create(vizero_buffer_t* buffer);
void vizero_cursor_destroy(vizero_cursor_t* cursor);

/* Cursor position */
size_t vizero_cursor_get_line(vizero_cursor_t* cursor);
size_t vizero_cursor_get_column(vizero_cursor_t* cursor);
vizero_position_t vizero_cursor_get_position(vizero_cursor_t* cursor);
void vizero_cursor_set_position(vizero_cursor_t* cursor, size_t line, size_t column);

/* Cursor movement */
void vizero_cursor_move_up(vizero_cursor_t* cursor);
void vizero_cursor_move_down(vizero_cursor_t* cursor);
void vizero_cursor_move_left(vizero_cursor_t* cursor);
void vizero_cursor_move_right(vizero_cursor_t* cursor);
void vizero_cursor_move_to_line_start(vizero_cursor_t* cursor);
void vizero_cursor_move_to_line_end(vizero_cursor_t* cursor);

/* Word movement */
void vizero_cursor_move_word_forward(vizero_cursor_t* cursor);
void vizero_cursor_move_word_backward(vizero_cursor_t* cursor);

/* Buffer navigation */
void vizero_cursor_move_to_buffer_start(vizero_cursor_t* cursor);
void vizero_cursor_move_to_buffer_end(vizero_cursor_t* cursor);

/* Page movement */
void vizero_cursor_move_page_up(vizero_cursor_t* cursor);
void vizero_cursor_move_page_down(vizero_cursor_t* cursor);

/* Cursor properties */
vizero_buffer_t* vizero_cursor_get_buffer(vizero_cursor_t* cursor);
int vizero_cursor_is_at_line_start(vizero_cursor_t* cursor);
int vizero_cursor_is_at_line_end(vizero_cursor_t* cursor);

#ifdef __cplusplus
}
#endif

#endif /* VIZERO_CURSOR_H */