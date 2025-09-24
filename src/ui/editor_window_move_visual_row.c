// Minimal, clean implementation for moving the cursor up/down by visual row (word wrap aware)
#include "vizero/editor_window.h"
#include "vizero/editor_state.h"
#include "vizero/buffer.h"
#include "vizero/cursor.h"
#include <string.h>
#include <stdlib.h>

int vizero_editor_window_move_visual_row(struct vizero_editor_window_t* window, struct vizero_editor_state_t* state, int direction) {
    if (!window || !state) return -1;
    
    /* Get buffer and cursor using new architecture */
    vizero_buffer_t* buffer = vizero_editor_window_get_buffer(window, state);
    vizero_cursor_t* cursor = vizero_editor_window_get_cursor(window, state);
    
    if (!buffer || !cursor) return -1;
    
    /* Simple implementation: use regular line-based movement for now
     * TODO: Implement full visual row movement with word wrap awareness */
    if (direction > 0) {
        vizero_cursor_move_down(cursor);
    } else if (direction < 0) {
        vizero_cursor_move_up(cursor);
    }
    
    return 0;
}
