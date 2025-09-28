// Minimal, clean implementation for moving the cursor up/down by visual row (word wrap aware)
#include "vizero/editor_window.h"
#include "vizero/editor_state.h"
#include "vizero/buffer.h"
#include "vizero/cursor.h"
#include <string.h>
#include <stdlib.h>

// Forward declaration for compatibility
struct vizero_editor_state_t;

int vizero_editor_window_move_visual_row(vizero_editor_window_t* window, struct vizero_editor_state_t* state, int direction) {
    if (!window || !state) return -1;
    
    /* Get buffer and cursor using new architecture */
    vizero_buffer_t* buffer = vizero_editor_window_get_buffer(window, state);
    vizero_cursor_t* cursor = vizero_editor_window_get_cursor(window, state);
    
    if (!buffer || !cursor) return -1;
    
    /* Word wrap aware visual row movement */
    size_t current_line = vizero_cursor_get_line(cursor);
    size_t current_col = vizero_cursor_get_column(cursor);
    
    /* Store preferred column for consistent vertical movement */
    if (window->preferred_column < 0) {
        window->preferred_column = (int)current_col;
    }
    
    if (direction > 0) {
        /* Move down one visual row */
        vizero_cursor_move_down(cursor);
        /* Try to maintain preferred column position */
        size_t new_line = vizero_cursor_get_line(cursor);
        if (new_line != current_line) {
            vizero_cursor_set_position(cursor, new_line, (size_t)window->preferred_column);
        }
    } else if (direction < 0) {
        /* Move up one visual row */
        vizero_cursor_move_up(cursor);
        /* Try to maintain preferred column position */
        size_t new_line = vizero_cursor_get_line(cursor);
        if (new_line != current_line) {
            vizero_cursor_set_position(cursor, new_line, (size_t)window->preferred_column);
        }
    }
    
    return 0;
}
