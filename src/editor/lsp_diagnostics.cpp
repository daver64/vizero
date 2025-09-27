#include "editor_state_internal.h"
#include "vizero/plugin_manager.h"
#include <string.h>
#include <stdlib.h>

/* LSP Diagnostics Management Functions */

void vizero_editor_update_diagnostics(vizero_editor_state_t* state, vizero_buffer_t* buffer) {
    if (!state || !buffer) return;
    
    /* Clear existing diagnostics */
    if (state->diagnostics) {
        for (size_t i = 0; i < state->diagnostic_count; i++) {
            free(state->diagnostics[i].message);
            free(state->diagnostics[i].source);
        }
        free(state->diagnostics);
        state->diagnostics = NULL;
    }
    state->diagnostic_count = 0;
    state->diagnostic_buffer = NULL;
    
    /* Get diagnostics from LSP plugins */
    vizero_diagnostic_t* diagnostics = NULL;
    size_t diagnostic_count = 0;
    
    /* TODO: Get plugin manager from global state or parameter */
    /* For now, we'll implement this when the plugin manager is accessible */
    
    /* Store new diagnostics */
    if (diagnostics && diagnostic_count > 0) {
        state->diagnostics = diagnostics;
        state->diagnostic_count = diagnostic_count;
        state->diagnostic_buffer = buffer;
    }
}

void vizero_editor_show_hover(vizero_editor_state_t* state, const char* text, 
                             vizero_position_t position, int screen_x, int screen_y) {
    if (!state || !text) return;
    
    /* Clear existing hover */
    vizero_editor_hide_hover(state);
    
    /* Set new hover */
    state->hover_text = strdup(text);
    state->hover_position = position;
    state->hover_x = screen_x;
    state->hover_y = screen_y;
    state->hover_visible = 1;
}

void vizero_editor_hide_hover(vizero_editor_state_t* state) {
    if (!state) return;
    
    if (state->hover_text) {
        free(state->hover_text);
        state->hover_text = NULL;
    }
    state->hover_visible = 0;
}

int vizero_editor_is_hover_visible(vizero_editor_state_t* state) {
    return state ? state->hover_visible : 0;
}

vizero_diagnostic_t* vizero_editor_get_diagnostics(vizero_editor_state_t* state, 
                                                  vizero_buffer_t* buffer, 
                                                  size_t* count) {
    if (!state || !buffer || !count) {
        if (count) *count = 0;
        return NULL;
    }
    
    /* Check if diagnostics are for this buffer */
    if (state->diagnostic_buffer != buffer) {
        *count = 0;
        return NULL;
    }
    
    *count = state->diagnostic_count;
    return state->diagnostics;
}