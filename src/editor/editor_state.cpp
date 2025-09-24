/* Enhanced multi-buffer implementation */
#include "vizero/editor_state.h"
#include "editor_state_internal.h"
#include "vizero/buffer.h"
#include "vizero/cursor.h"
#include "vizero/project.h"
#include "vizero/settings.h"
#include "vizero/search.h"
#include "vizero/file_utils.h"
#include "vizero/editor_window.h"
#include "vizero/mode_manager.h"
#include <SDL.h>
#include <stdlib.h>
#include <string.h>
#include <stdio.h>


#ifdef _WIN32
#include <windows.h>
#include <io.h>
#define popen _popen
#define pclose _pclose
#else
#include <unistd.h>
#include <sys/wait.h>
#include <dirent.h>
#include <sys/types.h>
#endif



vizero_editor_state_t* vizero_editor_state_create(void) {
    vizero_editor_state_t* state = (vizero_editor_state_t*)calloc(1, sizeof(vizero_editor_state_t));
    if (state) {
        state->mode = VIZERO_MODE_NORMAL;
        
        /* Initialize window manager */
        state->window_manager = vizero_window_manager_create();
        if (!state->window_manager) {
            free(state);
            return NULL;
        }
        
        /* Initialize mode manager */
        state->mode_manager = vizero_mode_manager_create(state);
        if (!state->mode_manager) {
            vizero_window_manager_destroy(state->window_manager);
            free(state);
            return NULL;
        }
        
        /* Initialize legacy buffer management */
        state->buffer_count = 0;
        state->current_buffer_index = 0;
        state->current_project = NULL;
        state->command_length = 0;
        state->command_buffer[0] = '\0';
        state->should_quit = 0;
        
        /* Initialize clipboard */
        state->clipboard_content = NULL;
        state->clipboard_size = 0;
        
        /* Initialize selection */
        state->has_selection = 0;
        state->selection_start.line = 0;
        state->selection_start.column = 0;
        state->selection_end.line = 0;
        state->selection_end.column = 0;
        
        /* Initialize undo stack */
        state->undo_stack = (vizero_undo_stack_t*)malloc(sizeof(vizero_undo_stack_t));
        if (state->undo_stack) {
            state->undo_stack->operations = (vizero_undo_operation_t*)malloc(MAX_UNDO_OPERATIONS * sizeof(vizero_undo_operation_t));
            state->undo_stack->count = 0;
            state->undo_stack->capacity = MAX_UNDO_OPERATIONS;
            state->undo_stack->current_index = 0;
        }
        
        /* Create settings */
        state->settings = vizero_settings_create();
        
        /* Load settings from file */
        if (state->settings) {
            vizero_settings_load_from_file(state->settings);
        }
        
        /* Initialize compilation output storage */
        state->last_compile_output = NULL;
        
        /* Initialize popup system */
        state->popup_visible = 0;
        state->popup_content = NULL;
        state->popup_start_time = 0;
        state->popup_duration_ms = 5000; /* 5 seconds default */
        
        /* Initialize help system */
        state->help_mode_active = 0;
        state->help_original_buffer = NULL;
        state->help_original_cursor = NULL;
        state->help_original_buffer_index = 0;
        
        /* Create initial empty buffer */
        state->buffers[0] = vizero_buffer_create();
        state->cursors[0] = vizero_cursor_create(state->buffers[0]);
        if (state->buffers[0] && state->cursors[0]) {
            state->buffer_count = 1;
        }
    }
    return state;
}

vizero_editor_state_t* vizero_editor_state_create_with_settings(vizero_settings_t* settings) {
    vizero_editor_state_t* state = (vizero_editor_state_t*)calloc(1, sizeof(vizero_editor_state_t));
    if (state) {
        state->mode = VIZERO_MODE_NORMAL;
        
        /* Initialize window manager */
        state->window_manager = vizero_window_manager_create();
        if (!state->window_manager) {
            free(state);
            return NULL;
        }
        
        /* Initialize mode manager */
        state->mode_manager = vizero_mode_manager_create(state);
        if (!state->mode_manager) {
            vizero_window_manager_destroy(state->window_manager);
            free(state);
            return NULL;
        }
        
        state->buffer_count = 0;
        state->current_buffer_index = 0;
        state->current_project = NULL;
        state->command_length = 0;
        state->command_buffer[0] = '\0';
        state->should_quit = 0;
        
        /* Initialize clipboard */
        state->clipboard_content = NULL;
        state->clipboard_size = 0;
        
        /* Initialize selection */
        state->has_selection = 0;
        state->selection_start.line = 0;
        state->selection_start.column = 0;
        state->selection_end.line = 0;
        state->selection_end.column = 0;
        
        /* Initialize undo stack */
        state->undo_stack = (vizero_undo_stack_t*)malloc(sizeof(vizero_undo_stack_t));
        if (state->undo_stack) {
            state->undo_stack->operations = (vizero_undo_operation_t*)malloc(MAX_UNDO_OPERATIONS * sizeof(vizero_undo_operation_t));
            state->undo_stack->count = 0;
            state->undo_stack->capacity = MAX_UNDO_OPERATIONS;
            state->undo_stack->current_index = 0;
        }
        
        /* Use provided settings instead of creating new ones */
        state->settings = settings;
        
        /* Initialize compilation output storage */
        state->last_compile_output = NULL;
        
        /* Initialize popup system */
        state->popup_visible = 0;
        state->popup_content = NULL;
        state->popup_start_time = 0;
        state->popup_duration_ms = 5000; /* 5 seconds default */
        
        /* Create initial empty buffer and window, but if a file is opened, replace them safely */
        state->buffers[0] = vizero_buffer_create();
        state->cursors[0] = vizero_cursor_create(state->buffers[0]);
        if (state->buffers[0] && state->cursors[0]) {
            state->buffer_count = 1;
            if (vizero_editor_create_window_for_buffer(state, state->buffers[0], 100, 100) != 0) {
                /* Cleanup on failure */
                if (state->buffers[0]) vizero_buffer_destroy(state->buffers[0]);
                if (state->cursors[0]) vizero_cursor_destroy(state->cursors[0]);
                if (state->window_manager) vizero_window_manager_destroy(state->window_manager);
                if (state->undo_stack) {
                    if (state->undo_stack->operations) free(state->undo_stack->operations);
                    free(state->undo_stack);
                }
                free(state);
                return NULL;
            }
        } else {
            /* Cleanup on failure */
            if (state->buffers[0]) vizero_buffer_destroy(state->buffers[0]);
            if (state->cursors[0]) vizero_cursor_destroy(state->cursors[0]);
            if (state->window_manager) vizero_window_manager_destroy(state->window_manager);
            if (state->undo_stack) {
                if (state->undo_stack->operations) free(state->undo_stack->operations);
                free(state->undo_stack);
            }
            free(state);
            return NULL;
        }

        /* If a filename was provided at startup, open it and replace the initial buffer/window */
        // ...existing code...
    }
    return state;
}

void vizero_editor_state_destroy(vizero_editor_state_t* state) {
    if (!state) return;
    
    /* With new architecture, windows no longer own cursors - they're all managed by editor state */
    if (state->window_manager) {
        vizero_window_manager_destroy(state->window_manager);
    }
    
    /* Clean up mode manager */
    if (state->mode_manager) {
        vizero_mode_manager_destroy(state->mode_manager);
    }
    
    /* Clean up all buffers and cursors */
    for (size_t i = 0; i < state->buffer_count; i++) {
        if (state->cursors[i]) {
            vizero_cursor_destroy(state->cursors[i]);
        }
        if (state->buffers[i]) {
            vizero_buffer_destroy(state->buffers[i]);
        }
    }
    
    if (state->current_project) vizero_project_destroy(state->current_project);
    if (state->status_message) {
        size_t len = strlen(state->status_message);
        memset(state->status_message, 0, len);
        free(state->status_message);
        state->status_message = NULL;
    }
    
    /* Save and destroy settings */
    if (state->settings) {
        vizero_settings_save_to_file(state->settings);
        vizero_settings_destroy(state->settings);
    }
    
    if (state->clipboard_content) free(state->clipboard_content);
    
    /* Clean up compilation output */
    if (state->last_compile_output) free(state->last_compile_output);
    
    /* Clean up popup content */
    if (state->popup_content) free(state->popup_content);
    
    /* Clean up undo stack */
    if (state->undo_stack) {
        if (state->undo_stack->operations) {
            /* Free all operation text data */
            for (size_t i = 0; i < state->undo_stack->count; i++) {
                if (state->undo_stack->operations[i].text) {
                    free(state->undo_stack->operations[i].text);
                }
            }
            free(state->undo_stack->operations);
        }
        free(state->undo_stack);
    }
    
    free(state);
}

vizero_editor_mode_t vizero_editor_get_mode(vizero_editor_state_t* state) {
    return state ? state->mode : VIZERO_MODE_NORMAL;
}

void vizero_editor_set_mode(vizero_editor_state_t* state, vizero_editor_mode_t mode) {
    if (state) state->mode = mode;
}

vizero_buffer_t* vizero_editor_get_current_buffer(vizero_editor_state_t* state) {
    if (!state) return NULL;
    /* If we have a window manager, get buffer from the focused window */
    if (state->window_manager) {
        vizero_editor_window_t* focused_window = vizero_window_manager_get_focused_window(state->window_manager);
        if (focused_window) {
            return vizero_editor_window_get_buffer(focused_window, state);
        }
    }
    /* Fallback to old system */
    if (state->current_buffer_index >= state->buffer_count) return NULL;
    return state->buffers[state->current_buffer_index];
}

vizero_cursor_t* vizero_editor_get_current_cursor(vizero_editor_state_t* state) {
    if (!state) return NULL;
    
    /* If we have a window manager, get cursor from the focused window */
    if (state->window_manager) {
        vizero_editor_window_t* focused_window = vizero_window_manager_get_focused_window(state->window_manager);
        if (focused_window) {
            return vizero_editor_window_get_cursor(focused_window, state);
        }
    }
    
    /* Fallback to old system */
    if (state->current_buffer_index >= state->buffer_count) return NULL;
    return state->cursors[state->current_buffer_index];
}

size_t vizero_editor_get_current_buffer_index(vizero_editor_state_t* state) {
    return state ? state->current_buffer_index : 0;
}

size_t vizero_editor_get_buffer_count(vizero_editor_state_t* state) {
    return state ? state->buffer_count : 0;
}

vizero_buffer_t* vizero_editor_get_buffer(vizero_editor_state_t* state, size_t index) {
    if (!state || index >= state->buffer_count) return NULL;
    return state->buffers[index];
}

vizero_cursor_t* vizero_editor_get_cursor(vizero_editor_state_t* state, size_t index) {
    if (!state || index >= state->buffer_count) return NULL;
    return state->cursors[index];
}

int vizero_editor_open_buffer(vizero_editor_state_t* state, const char* filename) {
    if (!state || !filename || state->buffer_count >= MAX_BUFFERS) return -1;
    

    /* Check if buffer is already open */
    for (size_t i = 0; i < state->buffer_count; i++) {
        const char* buffer_filename = vizero_buffer_get_filename(state->buffers[i]);
        if (buffer_filename && strcmp(buffer_filename, filename) == 0) {
            /* Switch to existing buffer */
            state->current_buffer_index = i;
            return 0;
        }
    }

    /* Create new buffer from file */
    vizero_buffer_t* buffer = vizero_buffer_create_from_file(filename);
    if (!buffer) return -1;

    vizero_buffer_set_filename(buffer, filename);
    vizero_cursor_t* cursor = vizero_cursor_create(buffer);
    if (!cursor) {
        vizero_buffer_destroy(buffer);
        return -1;
    }

    /* If the only buffer is empty and unnamed, replace it */
    if (state->buffer_count == 1 && state->buffers[0]) {
        const char* first_filename = vizero_buffer_get_filename(state->buffers[0]);
        const char* first_text = vizero_buffer_get_text(state->buffers[0]);
        if ((!first_filename || strlen(first_filename) == 0) && (!first_text || strlen(first_text) == 0)) {
            /* Store old buffer/cursor pointers before destroying them */
            vizero_buffer_t* old_buffer = state->buffers[0];
            vizero_cursor_t* old_cursor = state->cursors[0];
            
            /* Update state arrays first */
            state->buffers[0] = buffer;
            state->cursors[0] = cursor;
            state->current_buffer_index = 0;
            
            /* With new architecture: windows already point to buffer index 0, 
               and we've updated state->buffers[0] and state->cursors[0] above,
               so no window updates needed - they'll automatically see the new buffer/cursor */
            
            /* Now destroy the old buffer/cursor */
            if (old_buffer) vizero_buffer_destroy(old_buffer);
            if (old_cursor) vizero_cursor_destroy(old_cursor);
            
            /* buffer_count remains 1 */
            /* Notify plugins of buffer open (for syntax highlighting, etc.) */
            if (state->plugin_manager) {
                vizero_plugin_manager_on_buffer_open(state->plugin_manager, buffer, filename);
            }
            return 0;
        }
    }

    // Always add the buffer/cursor to the global list if not present
    int found = 0;
    for (size_t i = 0; i < state->buffer_count; ++i) {
        if (state->buffers[i] == buffer) {
            found = 1;
            break;
        }
    }
    if (!found && state->buffer_count < MAX_BUFFERS) {
        state->buffers[state->buffer_count] = buffer;
        state->cursors[state->buffer_count] = cursor;
        state->current_buffer_index = state->buffer_count;
        state->buffer_count++;
    }

    /* Notify plugins of buffer open (for syntax highlighting, etc.) */
    if (state->plugin_manager) {
        vizero_plugin_manager_on_buffer_open(state->plugin_manager, buffer, filename);
    }
    
    // Add to buffer list and handle window creation/reuse robustly
    int window_handled = 0;
    if (state->window_manager) {
        // Special case: replacing initial empty buffer
        if (state->buffer_count == 1 && state->buffers[0]) {
            const char* first_filename = vizero_buffer_get_filename(state->buffers[0]);
            const char* first_text = vizero_buffer_get_text(state->buffers[0]);
            if (!first_filename && (!first_text || strlen(first_text) == 0)) {
                vizero_editor_window_t* existing_window = vizero_window_manager_get_focused_window(state->window_manager);
                if (existing_window) {
                    // With new architecture: window already points to buffer index 0,
                    // and we're replacing state->buffers[0] and state->cursors[0],
                    // so window will automatically see the new buffer/cursor
                    vizero_editor_window_set_title(existing_window, filename);
                    // Make sure buffer/cursor are in the global list
                    int buffer_found = 0;
                    for (size_t i = 0; i < state->buffer_count; ++i) {
                        if (state->buffers[i] == buffer) {
                            buffer_found = 1;
                            break;
                        }
                    }
                    if (!buffer_found && state->buffer_count < MAX_BUFFERS) {
                        state->buffers[state->buffer_count] = buffer;
                        state->cursors[state->buffer_count] = cursor;
                        state->current_buffer_index = state->buffer_count;
                        state->buffer_count++;
                    }
                    // Notify plugins again for this window (in case of split)
                    if (state->plugin_manager) {
                        vizero_plugin_manager_on_buffer_open(state->plugin_manager, buffer, filename);
                    }
                }
                // With new architecture: check if any window points to buffer index 0
                int still_referenced = 0;
                size_t win_count = vizero_window_manager_get_window_count_raw(state->window_manager);
                for (size_t w = 0; w < win_count; ++w) {
                    vizero_editor_window_t* win = vizero_window_manager_get_window_raw(state->window_manager, w);
                    if (win && win->buffer_index == 0) {
                        still_referenced = 1;
                        break;
                    }
                }
                if (!still_referenced) {
                    /* Notify plugins of buffer close */
                    if (state->plugin_manager) {
                        vizero_plugin_manager_on_buffer_close(state->plugin_manager, state->buffers[0]);
                    }
                    printf("[DEBUG] vizero_editor_open_buffer: destroying buffer %p (index 0)\n", (void*)state->buffers[0]);
                    vizero_buffer_destroy(state->buffers[0]);
                    printf("[DEBUG] vizero_editor_open_buffer: destroyed buffer %p (index 0)\n", (void*)state->buffers[0]);
                    if (state->cursors[0]) {
                        printf("[DEBUG] vizero_editor_open_buffer: destroying cursor %p (index 0)\n", (void*)state->cursors[0]);
                        vizero_cursor_destroy(state->cursors[0]);
                        printf("[DEBUG] vizero_editor_open_buffer: destroyed cursor %p (index 0)\n", (void*)state->cursors[0]);
                    }
                    state->buffers[0] = NULL;
                    state->cursors[0] = NULL;
                }
                state->buffers[0] = buffer;
                state->cursors[0] = cursor;
                state->current_buffer_index = 0;
                window_handled = 1;
                // buffer_count remains 1
            }
        }
        if (!window_handled) {
            vizero_editor_window_t* focused_window = vizero_window_manager_get_focused_window(state->window_manager);
            if (focused_window) {
                // With new architecture: focused window points to a buffer index
                size_t focused_buffer_index = focused_window->buffer_index;
                
                // Check if any other windows reference this buffer index
                int refcount = 0;
                size_t win_count = vizero_window_manager_get_window_count_raw(state->window_manager);
                for (size_t w = 0; w < win_count; ++w) {
                    vizero_editor_window_t* win = vizero_window_manager_get_window_raw(state->window_manager, w);
                    if (win && win != focused_window && win->buffer_index == focused_buffer_index) {
                        refcount++;
                    }
                }
                
                // If no other windows use this buffer index, destroy the old buffer/cursor
                if (refcount == 0 && focused_buffer_index < state->buffer_count) {
                    if (state->buffers[focused_buffer_index] && state->buffers[focused_buffer_index] != buffer) {
                        /* Notify plugins of buffer close */
                        if (state->plugin_manager) {
                            vizero_plugin_manager_on_buffer_close(state->plugin_manager, state->buffers[focused_buffer_index]);
                        }
                        vizero_buffer_destroy(state->buffers[focused_buffer_index]);
                    }
                    if (state->cursors[focused_buffer_index] && state->cursors[focused_buffer_index] != cursor) {
                        vizero_cursor_destroy(state->cursors[focused_buffer_index]);
                    }
                    
                    // Replace the buffer/cursor at this index
                    state->buffers[focused_buffer_index] = buffer;
                    state->cursors[focused_buffer_index] = cursor;
                    state->current_buffer_index = focused_buffer_index;
                } else {
                    // Other windows use this index, so add buffer to a new slot
                    if (state->buffer_count < MAX_BUFFERS) {
                        state->buffers[state->buffer_count] = buffer;
                        state->cursors[state->buffer_count] = cursor;
                        focused_window->buffer_index = state->buffer_count;
                        state->current_buffer_index = state->buffer_count;
                        state->buffer_count++;
                    }
                }
                
                vizero_editor_window_set_title(focused_window, filename);
                window_handled = 1;
            }
        }
        if (!window_handled) {
            // No focused window, fallback: create a new window and add buffer/cursor to arrays
            if (state->buffer_count < MAX_BUFFERS) {
                state->buffers[state->buffer_count] = buffer;
                state->cursors[state->buffer_count] = cursor;
                state->current_buffer_index = state->buffer_count;
                state->buffer_count++;
                if (vizero_editor_create_window_for_buffer(state, buffer, 100, 100) != 0) {
                    printf("Warning: Could not create window for buffer\n");
                }
            } else {
                vizero_buffer_destroy(buffer);
                vizero_cursor_destroy(cursor);
                return -1;
            }
        }
    } else {
        // No window manager: legacy mode, just add to arrays
        if (state->buffer_count < MAX_BUFFERS) {
            state->buffers[state->buffer_count] = buffer;
            state->cursors[state->buffer_count] = cursor;
            state->current_buffer_index = state->buffer_count;
            state->buffer_count++;
        } else {
            vizero_buffer_destroy(buffer);
            vizero_cursor_destroy(cursor);
            return -1;
        }
    }
    return 0;
}

int vizero_editor_close_buffer(vizero_editor_state_t* state, vizero_buffer_t* buffer) {
    if (!state || !buffer || state->buffer_count == 0) return -1;
    
    /* Find buffer index */
    size_t buffer_index = SIZE_MAX;
    for (size_t i = 0; i < state->buffer_count; i++) {
        if (state->buffers[i] == buffer) {
            buffer_index = i;
            break;
        }
    }
    
    if (buffer_index == SIZE_MAX) return -1;
    
    /* Check if we're closing a help buffer */
    if (state->help_mode_active && buffer_index == state->current_buffer_index) {
        /* This is a help buffer being closed, exit help mode instead */
        return vizero_editor_exit_help_mode(state);
    }
    
    /* Don't close the last buffer */
    if (state->buffer_count == 1) return -1;

    /* --- PATCH: Update windows referencing this buffer index --- */
    if (state->window_manager) {
        size_t win_count = vizero_window_manager_get_window_count_raw(state->window_manager);
        for (size_t w = 0; w < win_count; ++w) {
            vizero_editor_window_t* win = vizero_window_manager_get_window_raw(state->window_manager, w);
            if (win && win->buffer_index == buffer_index) {
                /* Point window to another buffer index (fallback to 0) */
                win->buffer_index = (buffer_index > 0) ? 0 : ((state->buffer_count > 1) ? 1 : 0);
            }
        }
    }
    /* --- END PATCH --- */
    
    /* Clean up buffer and cursor */
    /* Notify plugins of buffer close */
    if (state->plugin_manager) {
        vizero_plugin_manager_on_buffer_close(state->plugin_manager, state->buffers[buffer_index]);
    }
    if (state->cursors[buffer_index]) {
        printf("[DEBUG] vizero_editor_close_buffer: destroying cursor %p (index %zu)\n", (void*)state->cursors[buffer_index], buffer_index);
        vizero_cursor_destroy(state->cursors[buffer_index]);
        printf("[DEBUG] vizero_editor_close_buffer: destroyed cursor %p (index %zu)\n", (void*)state->cursors[buffer_index], buffer_index);
    }
    state->cursors[buffer_index] = NULL;
    printf("[DEBUG] vizero_editor_close_buffer: destroying buffer %p (index %zu)\n", (void*)state->buffers[buffer_index], buffer_index);
    vizero_buffer_destroy(state->buffers[buffer_index]);
    printf("[DEBUG] vizero_editor_close_buffer: destroyed buffer %p (index %zu)\n", (void*)state->buffers[buffer_index], buffer_index);
    state->buffers[buffer_index] = NULL;
    
    /* Shift remaining buffers down */
    for (size_t i = buffer_index; i < state->buffer_count - 1; i++) {
        state->buffers[i] = state->buffers[i + 1];
        state->cursors[i] = state->cursors[i + 1];
    }
    
    state->buffer_count--;
    
    /* Adjust current buffer index if necessary */
    if (state->current_buffer_index >= buffer_index) {
        if (state->current_buffer_index > 0) {
            state->current_buffer_index--;
        }
    }
    
    return 0;
}

int vizero_editor_switch_buffer(vizero_editor_state_t* state, size_t buffer_index) {
    if (!state || buffer_index >= state->buffer_count) return -1;
    size_t old_index = state->current_buffer_index;
    state->current_buffer_index = buffer_index;
    /* Notify plugins of cursor moved if buffer/cursor changed */
    if (state->plugin_manager && state->cursors[old_index] && state->cursors[buffer_index] && old_index != buffer_index) {
        vizero_position_t old_pos = vizero_cursor_get_position(state->cursors[old_index]);
        vizero_position_t new_pos = vizero_cursor_get_position(state->cursors[buffer_index]);
        vizero_plugin_manager_on_cursor_moved(state->plugin_manager, state->cursors[buffer_index], old_pos, new_pos);
    }
    return 0;
}

int vizero_editor_next_buffer(vizero_editor_state_t* state) {
    if (!state || state->buffer_count <= 1) return -1;
    state->current_buffer_index = (state->current_buffer_index + 1) % state->buffer_count;
    return 0;
}

int vizero_editor_previous_buffer(vizero_editor_state_t* state) {
    if (!state || state->buffer_count <= 1) return -1;
    if (state->current_buffer_index == 0) {
        state->current_buffer_index = state->buffer_count - 1;
    } else {
        state->current_buffer_index--;
    }
    return 0;
}

int vizero_editor_create_new_buffer(vizero_editor_state_t* state, const char* name) {
    if (!state || state->buffer_count >= MAX_BUFFERS) return -1;
    
    vizero_buffer_t* buffer = vizero_buffer_create();
    if (!buffer) return -1;
    
    if (name) {
        vizero_buffer_set_filename(buffer, name);
    }
    
    vizero_cursor_t* cursor = vizero_cursor_create(buffer);
    if (!cursor) {
        vizero_buffer_destroy(buffer);
        return -1;
    }
    
    /* Add to buffer list */
    state->buffers[state->buffer_count] = buffer;
    state->cursors[state->buffer_count] = cursor;
    state->current_buffer_index = state->buffer_count;
    state->buffer_count++;
    
    return 0;
}

int vizero_editor_close_current_buffer(vizero_editor_state_t* state) {
    if (!state || state->buffer_count == 0) return -1;
    
    /* Don't close the last buffer */
    if (state->buffer_count == 1) {
        vizero_editor_set_status_message(state, "Cannot close last buffer");
        return -1;
    }
    
    vizero_buffer_t* current_buffer = vizero_editor_get_current_buffer(state);
    if (!current_buffer) return -1;
    
    /* Check if buffer has unsaved changes */
    if (vizero_buffer_is_modified(current_buffer)) {
        const char* filename = vizero_buffer_get_filename(current_buffer);
        char msg[512];
        snprintf(msg, sizeof(msg), "No write since last change for \"%s\" (add ! to override)", 
                 filename ? filename : "[No Name]");
        vizero_editor_set_status_message(state, msg);
        return -1;
    }
    
    int result = vizero_editor_close_buffer(state, current_buffer);
    if (result == 0) {
        const char* filename = vizero_buffer_get_filename(current_buffer);
        char msg[256];
        snprintf(msg, sizeof(msg), "Buffer \"%s\" closed", filename ? filename : "[No Name]");
        vizero_editor_set_status_message(state, msg);
    }
    return result;
}

int vizero_editor_close_buffer_by_index(vizero_editor_state_t* state, size_t buffer_index) {
    if (!state || buffer_index >= state->buffer_count) return -1;
    
    /* Don't close the last buffer */
    if (state->buffer_count == 1) {
        vizero_editor_set_status_message(state, "Cannot close last buffer");
        return -1;
    }
    
    vizero_buffer_t* buffer = vizero_editor_get_buffer(state, buffer_index);
    if (!buffer) return -1;
    
    /* Check if buffer has unsaved changes */
    if (vizero_buffer_is_modified(buffer)) {
        const char* filename = vizero_buffer_get_filename(buffer);
        char msg[512];
        snprintf(msg, sizeof(msg), "No write since last change for \"%s\" (add ! to override)", 
                 filename ? filename : "[No Name]");
        vizero_editor_set_status_message(state, msg);
        return -1;
    }
    
    int result = vizero_editor_close_buffer(state, buffer);
    if (result == 0) {
        const char* filename = vizero_buffer_get_filename(buffer);
        char msg[256];
        snprintf(msg, sizeof(msg), "Buffer \"%s\" closed", filename ? filename : "[No Name]");
        vizero_editor_set_status_message(state, msg);
    }
    return result;
}

/* Popup system functions */
void vizero_editor_show_popup(vizero_editor_state_t* state, const char* content, uint32_t duration_ms) {
    if (!state || !content) return;
    
    /* Clean up existing popup */
    if (state->popup_content) {
        free(state->popup_content);
    }
    
    /* Set new popup */
    state->popup_content = strdup(content);
    state->popup_visible = 1;
    state->popup_start_time = SDL_GetTicks();
    state->popup_duration_ms = duration_ms;
    state->popup_scroll_offset = 0;  /* Reset scroll to top */
}

void vizero_editor_hide_popup(vizero_editor_state_t* state) {
    if (!state) return;
    
    state->popup_visible = 0;
    if (state->popup_content) {
        free(state->popup_content);
        state->popup_content = NULL;
    }
}

int vizero_editor_is_popup_visible(vizero_editor_state_t* state) {
    if (!state || !state->popup_visible) return 0;
    
    /* Check if popup has timed out */
    if (state->popup_duration_ms > 0) {  /* Only check timeout if duration is set */
        uint32_t current_time = SDL_GetTicks();
        if (current_time - state->popup_start_time >= state->popup_duration_ms) {
            vizero_editor_hide_popup(state);
            return 0;
        }
    }
    
    return 1;
}

const char* vizero_editor_get_popup_content(vizero_editor_state_t* state) {
    if (!state || !vizero_editor_is_popup_visible(state)) return NULL;
    return state->popup_content;
}

uint32_t vizero_editor_get_popup_duration(vizero_editor_state_t* state) {
    if (!state || !vizero_editor_is_popup_visible(state)) return 0;
    return state->popup_duration_ms;
}

void vizero_editor_scroll_popup(vizero_editor_state_t* state, int lines) {
    if (!state || !vizero_editor_is_popup_visible(state) || !state->popup_content) return;
    
    /* Count total lines in popup content */
    int total_lines = 1;  /* At least one line */
    const char* p = state->popup_content;
    while (*p) {
        if (*p == '\n') total_lines++;
        p++;
    }
    
    /* Update scroll offset */
    state->popup_scroll_offset += lines;
    
    /* Clamp scroll offset */
    if (state->popup_scroll_offset < 0) {
        state->popup_scroll_offset = 0;
    }
    
    /* Don't scroll past the end (leave some lines visible) */
    int max_scroll = total_lines - 10;  /* Keep at least 10 lines visible */
    if (max_scroll < 0) max_scroll = 0;
    if (state->popup_scroll_offset > max_scroll) {
        state->popup_scroll_offset = max_scroll;
    }
}

int vizero_editor_get_popup_scroll_offset(vizero_editor_state_t* state) {
    if (!state || !vizero_editor_is_popup_visible(state)) return 0;
    return state->popup_scroll_offset;
}

/* Window management functions */
vizero_window_manager_t* vizero_editor_get_window_manager(vizero_editor_state_t* state) {
    return state ? state->window_manager : NULL;
}

vizero_editor_window_t* vizero_editor_get_active_window(vizero_editor_state_t* state) {
    if (!state || !state->window_manager) return NULL;
    return vizero_window_manager_get_focused_window(state->window_manager);
}

int vizero_editor_create_window_for_buffer(vizero_editor_state_t* state, vizero_buffer_t* buffer,
                                          int window_width, int window_height) {
    if (!state || !state->window_manager || !buffer) return -1;
    
    /* Find buffer index for this buffer */
    size_t buffer_index = 0;
    for (size_t i = 0; i < state->buffer_count; i++) {
        if (state->buffers[i] == buffer) {
            buffer_index = i;
            break;
        }
    }
    
    /* Create window with full dimensions minus status bar */
    int height_minus_status = window_height - 24; /* 24px for status bar */
    
    vizero_editor_window_t* window = vizero_window_manager_create_window(
        state->window_manager, buffer_index, 0, 0, window_width, height_minus_status);
    
    return window ? 0 : -1;
}

/* Helper function to execute command and capture output */
static int execute_command_with_output(const char* command, char* output_buffer, size_t buffer_size) {
    if (!command || !output_buffer || buffer_size == 0) return -1;
    
    output_buffer[0] = '\0';
    
#ifdef _WIN32
    /* On Windows, redirect both stdout and stderr to capture all output */
    char full_command[2048];
    snprintf(full_command, sizeof(full_command), "%s 2>&1", command);
    
    FILE* pipe = popen(full_command, "r");
    if (!pipe) return -1;
    
    char line[512];
    while (fgets(line, sizeof(line), pipe)) {
        size_t current_len = strlen(output_buffer);
        size_t line_len = strlen(line);
        if (current_len + line_len < buffer_size - 1) {
            strcat(output_buffer, line);
        }
    }
    
    int exit_code = pclose(pipe);
    return exit_code;
#else
    /* Unix version using popen */
    char full_command[2048];
    snprintf(full_command, sizeof(full_command), "%s 2>&1", command);
    
    FILE* pipe = popen(full_command, "r");
    if (!pipe) return -1;
    
    char line[512];
    while (fgets(line, sizeof(line), pipe)) {
        size_t current_len = strlen(output_buffer);
        size_t line_len = strlen(line);
        if (current_len + line_len < buffer_size - 1) {
            strcat(output_buffer, line);
        }
    }
    
    int exit_code = pclose(pipe);
    return WEXITSTATUS(exit_code);
#endif
}

/* Compilation helper function */
static int vizero_editor_compile_file(vizero_editor_state_t* state, const char* args, const char* language) {
    if (!state || !args || !language) return -1;
    
    /* Parse arguments (file.c -o output.exe) */
    char input_file[512] = {0};
    char output_file[512] = {0};
    char extra_args[1024] = {0};
    
    /* Simple argument parsing */
    const char* ptr = args;
    
    /* Skip leading whitespace */
    while (*ptr && (*ptr == ' ' || *ptr == '\t')) ptr++;
    
    /* Get input file */
    const char* start = ptr;
    while (*ptr && *ptr != ' ' && *ptr != '\t') ptr++;
    if (ptr > start) {
        size_t len = ptr - start;
        if (len < sizeof(input_file) - 1) {
            strncpy(input_file, start, len);
            input_file[len] = '\0';
        }
    }
    
    /* Parse remaining arguments */
    while (*ptr) {
        /* Skip whitespace */
        while (*ptr && (*ptr == ' ' || *ptr == '\t')) ptr++;
        if (!*ptr) break;
        
        if (strncmp(ptr, "-o", 2) == 0) {
            ptr += 2;
            /* Skip whitespace after -o */
            while (*ptr && (*ptr == ' ' || *ptr == '\t')) ptr++;
            /* Get output file */
            start = ptr;
            while (*ptr && *ptr != ' ' && *ptr != '\t') ptr++;
            if (ptr > start) {
                size_t len = ptr - start;
                if (len < sizeof(output_file) - 1) {
                    strncpy(output_file, start, len);
                    output_file[len] = '\0';
                }
            }
        } else {
            /* Other arguments - add to extra_args */
            start = ptr;
            while (*ptr && *ptr != ' ' && *ptr != '\t') ptr++;
            if (ptr > start) {
                size_t len = ptr - start;
                if (strlen(extra_args) + len + 2 < sizeof(extra_args)) {
                    if (extra_args[0]) strcat(extra_args, " ");
                    strncat(extra_args, start, len);
                }
            }
        }
    }
    
    /* If no input file specified, use current buffer filename */
    if (input_file[0] == '\0') {
        vizero_buffer_t* buffer = vizero_editor_get_current_buffer(state);
        if (buffer) {
            const char* filename = vizero_buffer_get_filename(buffer);
            if (filename) {
                strncpy(input_file, filename, sizeof(input_file) - 1);
            } else {
                vizero_editor_set_status_message(state, "No file to compile");
                return -1;
            }
        } else {
            vizero_editor_set_status_message(state, "No buffer available");
            return -1;
        }
    }
    
    /* If no output file specified, generate one */
    if (output_file[0] == '\0') {
        const char* dot = strrchr(input_file, '.');
        if (dot) {
            size_t base_len = dot - input_file;
            strncpy(output_file, input_file, base_len);
            output_file[base_len] = '\0';
#ifdef _WIN32
            strcat(output_file, ".exe");
#endif
        } else {
            strcpy(output_file, input_file);
#ifdef _WIN32
            strcat(output_file, ".exe");
#endif
        }
    }
    
    /* Get compiler from settings */
    const char* compiler = NULL;
    const char* compiler_path = NULL;
    
    if (strcmp(language, "c") == 0) {
        compiler = vizero_settings_get_string(state->settings, VIZERO_SETTING_C_COMPILER);
        compiler_path = vizero_settings_get_string(state->settings, VIZERO_SETTING_C_COMPILER_PATH);
        if (!compiler) compiler = "gcc"; /* Default */
    } else if (strcmp(language, "cpp") == 0) {
        compiler = vizero_settings_get_string(state->settings, VIZERO_SETTING_CPP_COMPILER);
        compiler_path = vizero_settings_get_string(state->settings, VIZERO_SETTING_CPP_COMPILER_PATH);
        if (!compiler) compiler = "g++"; /* Default */
    } else if (strcmp(language, "asm") == 0) {
        compiler = vizero_settings_get_string(state->settings, VIZERO_SETTING_ASSEMBLER);
        compiler_path = vizero_settings_get_string(state->settings, VIZERO_SETTING_ASSEMBLER_PATH);
        if (!compiler) compiler = "nasm"; /* Default */
    }
    
    /* Build command line */
    char command_line[2048];
    const char* actual_compiler = compiler;
    
    /* Map setting values to actual executable names */
    if (strcmp(compiler, "msvc") == 0) {
        actual_compiler = "cl";
    }
    
    if (compiler_path && compiler_path[0]) {
        snprintf(command_line, sizeof(command_line), "\"%s\"", compiler_path);
    } else {
        snprintf(command_line, sizeof(command_line), "%s", actual_compiler);
    }
    
    /* Add compiler-specific arguments */
    if (strcmp(compiler, "gcc") == 0 || strcmp(compiler, "g++") == 0) {
        char temp[1024];
        snprintf(temp, sizeof(temp), " \"%s\" -o \"%s\"", input_file, output_file);
        strcat(command_line, temp);
        if (extra_args[0]) {
            strcat(command_line, " ");
            strcat(command_line, extra_args);
        }
    } else if (strcmp(compiler, "msvc") == 0) {
        char temp[1024];
        snprintf(temp, sizeof(temp), " \"%s\" /Fe:\"%s\"", input_file, output_file);
        strcat(command_line, temp);
        if (extra_args[0]) {
            strcat(command_line, " ");
            strcat(command_line, extra_args);
        }
    } else if (strcmp(compiler, "nasm") == 0) {
        char temp[1024];
        snprintf(temp, sizeof(temp), " -f win64 \"%s\" -o \"%s\"", input_file, output_file);
        strcat(command_line, temp);
        if (extra_args[0]) {
            strcat(command_line, " ");
            strcat(command_line, extra_args);
        }
    } else if (strcmp(compiler, "fasm") == 0) {
        char temp[1024];
        snprintf(temp, sizeof(temp), " \"%s\" \"%s\"", input_file, output_file);
        strcat(command_line, temp);
        if (extra_args[0]) {
            strcat(command_line, " ");
            strcat(command_line, extra_args);
        }
    }
    
    /* Execute compilation */
    char msg[512];
    snprintf(msg, sizeof(msg), "Compiling with: %s", command_line);
    vizero_editor_set_status_message(state, msg);
    
    /* Capture compiler output */
    char output[4096] = {0};
    int result = execute_command_with_output(command_line, output, sizeof(output));
    /* Create compilation result message */
    char result_msg[5120];
    if (result == 0) {
        snprintf(result_msg, sizeof(result_msg), 
                "Compilation successful: %s\n\nOutput:\n%s", 
                output_file, output[0] ? output : "(no output)");
    } else {
        snprintf(result_msg, sizeof(result_msg), 
                "Compilation failed (exit code %d)\n\nOutput:\n%s", 
                result, output[0] ? output : "(no output)");
    }
    
    /* For now, we'll show a truncated version in status message */
    /* TODO: Implement popup window for full output */
    char short_msg[256];
    if (result == 0) {
        snprintf(short_msg, sizeof(short_msg), "SUCCESS: %s", output_file);
    } else {
        /* Show first line of error */
        char* first_line = strtok(output, "\n");
        if (first_line) {
            snprintf(short_msg, sizeof(short_msg), "ERROR: %.180s", first_line);
        } else {
            snprintf(short_msg, sizeof(short_msg), "ERROR: Compilation failed (exit code %d)", result);
        }
    }
    
    vizero_editor_set_status_message(state, short_msg);
    
    /* Store full compilation result */
    if (state->last_compile_output) {
        free(state->last_compile_output);
    }
    
    /* Create full result with command and output */
    char full_result[6144];
    snprintf(full_result, sizeof(full_result),
            "=== COMPILATION RESULT ===\n"
            "Command: %s\n"
            "Exit Code: %d\n"
            "Status: %s\n\n"
            "Output:\n%s\n"
            "========================\n",
            command_line, result,
            (result == 0) ? "SUCCESS" : "FAILED",
            output[0] ? output : "(no output)");
    
    state->last_compile_output = strdup(full_result);
    
    /* Show popup with compilation result */
    vizero_editor_show_popup(state, full_result, 5000); /* 5 seconds */
    
    /* Also print to console for debugging */
    printf("\n%s\n", full_result);
    
    return result;
}

/* Helper functions for line range operations */
static int vizero_parse_line_range(vizero_editor_state_t* state, const char* range_str, size_t* start_line, size_t* end_line) {
    if (!state || !range_str || !start_line || !end_line) return -1;
    
    vizero_buffer_t* buffer = vizero_editor_get_current_buffer(state);
    vizero_cursor_t* cursor = vizero_editor_get_current_cursor(state);
    if (!buffer || !cursor) return -1;
    
    size_t line_count = vizero_buffer_get_line_count(buffer);
    size_t current_line = vizero_cursor_get_line(cursor) + 1; /* Convert to 1-based */
    
    char* range_copy = strdup(range_str);
    char* comma = strchr(range_copy, ',');
    if (!comma) {
        free(range_copy);
        return -1;
    }
    
    *comma = '\0';
    char* start_str = range_copy;
    char* end_str = comma + 1;
    
    /* Parse start line */
    if (strcmp(start_str, ".") == 0) {
        *start_line = current_line;
    } else if (strcmp(start_str, "$") == 0) {
        *start_line = line_count;
    } else {
        *start_line = strtoul(start_str, NULL, 10);
    }
    
    /* Parse end line */
    if (strcmp(end_str, ".") == 0) {
        *end_line = current_line;
    } else if (strcmp(end_str, "$") == 0) {
        *end_line = line_count;
    } else if (end_str[0] == '+') {
        *end_line = current_line + strtoul(end_str + 1, NULL, 10);
    } else if (end_str[0] == '-') {
        *end_line = current_line - strtoul(end_str + 1, NULL, 10);
    } else {
        *end_line = strtoul(end_str, NULL, 10);
    }
    
    /* Clamp to valid range */
    if (*start_line < 1) *start_line = 1;
    if (*start_line > line_count) *start_line = line_count;
    if (*end_line < 1) *end_line = 1;
    if (*end_line > line_count) *end_line = line_count;
    
    /* Ensure start <= end */
    if (*start_line > *end_line) {
        size_t temp = *start_line;
        *start_line = *end_line;
        *end_line = temp;
    }
    
    free(range_copy);
    return 0;
}

static int vizero_execute_line_range_command(vizero_editor_state_t* state, const char* command) {
    if (!state || !command) return -1;
    
    /* Find the command part (after range) */
    char* cmd_copy = strdup(command);
    char* cmd_part = NULL;
    
    if (strstr(cmd_copy, "d")) {
        cmd_part = strstr(cmd_copy, "d");
    } else if (strstr(cmd_copy, "y")) {
        cmd_part = strstr(cmd_copy, "y");
    } else if (strstr(cmd_copy, "s/")) {
        cmd_part = strstr(cmd_copy, "s/");
    }
    
    if (!cmd_part) {
        free(cmd_copy);
        return -1;
    }
    
    /* Extract range part */
    *cmd_part = '\0';
    char* range_str = cmd_copy;
    
    size_t start_line, end_line;
    if (vizero_parse_line_range(state, range_str, &start_line, &end_line) != 0) {
        free(cmd_copy);
        vizero_editor_set_status_message(state, "Invalid line range");
        return -1;
    }
    
    vizero_buffer_t* buffer = vizero_editor_get_current_buffer(state);
    vizero_cursor_t* cursor = vizero_editor_get_current_cursor(state);
    if (!buffer || !cursor) {
        free(cmd_copy);
        return -1;
    }
    
    /* Execute the command */
    if (*cmd_part == 'd') {
        /* Delete lines in range */
        char lines_text[8192] = "";
        for (size_t line = start_line; line <= end_line; line++) {
            const char* line_text = vizero_buffer_get_line_text(buffer, line - 1);
            if (line_text) {
                strcat(lines_text, line_text);
                strcat(lines_text, "\n");
            }
        }
        
        /* Store in clipboard for potential paste */
        if (state->clipboard_content) free(state->clipboard_content);
        state->clipboard_content = strdup(lines_text);
        
        /* Delete lines from end to start to maintain line numbers */
        for (size_t line = end_line; line >= start_line; line--) {
            vizero_buffer_delete_line(buffer, line - 1);
        }
        
        /* Position cursor at start of range */
        size_t new_line = (start_line > 1) ? start_line - 1 : 0;
        if (new_line >= vizero_buffer_get_line_count(buffer)) {
            new_line = vizero_buffer_get_line_count(buffer) - 1;
        }
        vizero_cursor_set_position(cursor, new_line, 0);
        
        char msg[64];
        sprintf(msg, "%zu lines deleted", end_line - start_line + 1);
        vizero_editor_set_status_message(state, msg);
        
    } else if (*cmd_part == 'y') {
        /* Yank (copy) lines in range */
        char lines_text[8192] = "";
        for (size_t line = start_line; line <= end_line; line++) {
            const char* line_text = vizero_buffer_get_line_text(buffer, line - 1);
            if (line_text) {
                strcat(lines_text, line_text);
                strcat(lines_text, "\n");
            }
        }
        
        if (state->clipboard_content) free(state->clipboard_content);
        state->clipboard_content = strdup(lines_text);
        
        char msg[64];
        sprintf(msg, "%zu lines yanked", end_line - start_line + 1);
        vizero_editor_set_status_message(state, msg);
        
    } else if (cmd_part && cmd_part[0] == 's' && cmd_part[1] == '/') {
        /* Substitute in range */
        char pattern[512] = {0};
        char replacement[512] = {0};
        char flags[16] = {0};
        
        const char* ptr = cmd_part + 2; /* Skip 's/' */
        char* dest = pattern;
        size_t dest_size = sizeof(pattern) - 1;
        
        /* Extract pattern */
        while (*ptr && *ptr != '/' && dest_size > 0) {
            *dest++ = *ptr++;
            dest_size--;
        }
        *dest = '\0';
        
        if (*ptr == '/') ptr++;
        
        /* Extract replacement */
        dest = replacement;
        dest_size = sizeof(replacement) - 1;
        while (*ptr && *ptr != '/' && dest_size > 0) {
            *dest++ = *ptr++;
            dest_size--;
        }
        *dest = '\0';
        
        if (*ptr == '/') ptr++;
        
        /* Extract flags */
        dest = flags;
        dest_size = sizeof(flags) - 1;
        while (*ptr && dest_size > 0) {
            *dest++ = *ptr++;
            dest_size--;
        }
        *dest = '\0';
        
        int global = (strchr(flags, 'g') != NULL);
        
        /* Apply substitution to each line in range */
        int substitutions = 0;
        for (size_t line = start_line; line <= end_line; line++) {
            int result = vizero_substitute_line(state, pattern, replacement, (int)(line - 1), global);
            if (result > 0) substitutions += result;
        }
        
        char msg[128];
        sprintf(msg, "%d substitutions on %zu lines", substitutions, end_line - start_line + 1);
        vizero_editor_set_status_message(state, msg);
    }
    
    free(cmd_copy);
    return 0;
}

static int vizero_execute_global_command(vizero_editor_state_t* state, const char* command) {
    if (!state || !command) return -1;
    
    /* Parse g/pattern/command */
    const char* cmd = command + 2; /* Skip 'g/' */
    char pattern[512] = {0};
    char action[64] = {0};
    
    const char* ptr = cmd;
    char* dest = pattern;
    size_t dest_size = sizeof(pattern) - 1;
    
    /* Extract pattern */
    while (*ptr && *ptr != '/' && dest_size > 0) {
        *dest++ = *ptr++;
        dest_size--;
    }
    *dest = '\0';
    
    if (*ptr == '/') ptr++;
    
    /* Extract action */
    dest = action;
    dest_size = sizeof(action) - 1;
    while (*ptr && dest_size > 0) {
        *dest++ = *ptr++;
        dest_size--;
    }
    *dest = '\0';
    
    if (strlen(pattern) == 0) {
        vizero_editor_set_status_message(state, "Empty pattern in global command");
        return -1;
    }
    
    vizero_buffer_t* buffer = vizero_editor_get_current_buffer(state);
    if (!buffer) return -1;
    
    size_t line_count = vizero_buffer_get_line_count(buffer);
    int operations = 0;
    
    if (strcmp(action, "d") == 0) {
        /* Delete all lines matching pattern */
        for (size_t line = line_count; line > 0; line--) {
            const char* line_text = vizero_buffer_get_line_text(buffer, line - 1);
            if (line_text && strstr(line_text, pattern)) {
                vizero_buffer_delete_line(buffer, line - 1);
                operations++;
            }
        }
        char msg[64];
        sprintf(msg, "%d lines deleted", operations);
        vizero_editor_set_status_message(state, msg);
        
    } else if (strcmp(action, "p") == 0) {
        /* Print (show in popup) all lines matching pattern */
        char popup[8192] = "";
        snprintf(popup, sizeof(popup), "Lines matching '%s':\n\n", pattern);
        
        for (size_t line = 1; line <= line_count; line++) {
            const char* line_text = vizero_buffer_get_line_text(buffer, line - 1);
            if (line_text && strstr(line_text, pattern)) {
                char line_info[512];
                snprintf(line_info, sizeof(line_info), "%zu: %s\n", line, line_text);
                strcat(popup, line_info);
                operations++;
            }
        }
        
        if (operations > 0) {
            vizero_editor_show_popup(state, popup, 0); /* No timeout */
        } else {
            vizero_editor_set_status_message(state, "No matches found");
        }
        
    } else if (strncmp(action, "s//", 3) == 0) {
        /* Global substitute with pattern */
        char replacement[256];
        strncpy(replacement, action + 3, sizeof(replacement) - 1);
        replacement[sizeof(replacement) - 1] = '\0';
        char* end_marker = strstr(replacement, "/g");
        if (end_marker) *end_marker = '\0';
        
        for (size_t line = 0; line < line_count; line++) {
            const char* line_text = vizero_buffer_get_line_text(buffer, line);
            if (line_text && strstr(line_text, pattern)) {
                int result = vizero_substitute_line(state, pattern, replacement, (int)line, 1);
                if (result > 0) operations += result;
            }
        }
        
        char msg[64];
        sprintf(msg, "%d substitutions", operations);
        vizero_editor_set_status_message(state, msg);
        
    } else {
        vizero_editor_set_status_message(state, "Unknown global command action");
        return -1;
    }
    
    return 0;
}

static int vizero_execute_inverse_global_command(vizero_editor_state_t* state, const char* command) {
    if (!state || !command) return -1;
    
    /* Parse v/pattern/command (inverse of g/pattern/command) */
    const char* cmd = command + 2; /* Skip 'v/' */
    char pattern[512] = {0};
    char action[64] = {0};
    
    const char* ptr = cmd;
    char* dest = pattern;
    size_t dest_size = sizeof(pattern) - 1;
    
    /* Extract pattern */
    while (*ptr && *ptr != '/' && dest_size > 0) {
        *dest++ = *ptr++;
        dest_size--;
    }
    *dest = '\0';
    
    if (*ptr == '/') ptr++;
    
    /* Extract action */
    dest = action;
    dest_size = sizeof(action) - 1;
    while (*ptr && dest_size > 0) {
        *dest++ = *ptr++;
        dest_size--;
    }
    *dest = '\0';
    
    if (strlen(pattern) == 0) {
        vizero_editor_set_status_message(state, "Empty pattern in inverse global command");
        return -1;
    }
    
    vizero_buffer_t* buffer = vizero_editor_get_current_buffer(state);
    if (!buffer) return -1;
    
    size_t line_count = vizero_buffer_get_line_count(buffer);
    int operations = 0;
    
    if (strcmp(action, "d") == 0) {
        /* Delete all lines NOT matching pattern */
        for (size_t line = line_count; line > 0; line--) {
            const char* line_text = vizero_buffer_get_line_text(buffer, line - 1);
            if (line_text && !strstr(line_text, pattern)) {
                vizero_buffer_delete_line(buffer, line - 1);
                operations++;
            }
        }
        char msg[64];
        sprintf(msg, "%d lines deleted", operations);
        vizero_editor_set_status_message(state, msg);
        
    } else {
        vizero_editor_set_status_message(state, "Only 'd' action supported for inverse global");
        return -1;
    }
    
    return 0;
}

/* Placeholder marks and navigation functions */
static int vizero_show_marks(vizero_editor_state_t* state) {
    if (!state) return -1;
    
    /* For now, show a simple message since marks aren't fully implemented */
    const char* marks_info = 
        "Marks:\n\n"
        "Marks system not yet fully implemented.\n"
        "In vi, marks allow you to:\n"
        "- Set marks with 'm<letter>' in normal mode\n"
        "- Jump to marks with '`<letter>' or ''<letter>'\n"
        "- Use automatic marks like '.' (last change), ''' (last jump)\n\n"
        "This feature is planned for future implementation.";
    
    vizero_editor_show_popup(state, marks_info, 0);
    return 0;
}

static int vizero_show_jumps(vizero_editor_state_t* state) {
    if (!state) return -1;
    
    /* For now, show a simple message since jump history isn't fully implemented */
    const char* jumps_info = 
        "Jump History:\n\n"
        "Jump history not yet fully implemented.\n"
        "In vi, jump history tracks:\n"
        "- Line jumps with G, gg, :line\n"
        "- Search jumps with /, ?, n, N\n"
        "- File jumps between buffers\n\n"
        "Use Ctrl+O (older) and Ctrl+I (newer) to navigate.\n"
        "This feature is planned for future implementation.";
    
    vizero_editor_show_popup(state, jumps_info, 0);
    return 0;
}

static int vizero_show_changes(vizero_editor_state_t* state) {
    if (!state) return -1;
    
    /* For now, show a simple message since change history isn't fully implemented */
    const char* changes_info = 
        "Change History:\n\n"
        "Change history not yet fully implemented.\n"
        "In vi, change history tracks:\n"
        "- Text insertions, deletions, modifications\n"
        "- Position of each change\n"
        "- Timestamp of changes\n\n"
        "Use g; (older change) and g, (newer change) to navigate.\n"
        "This feature is planned for future implementation.";
    
    vizero_editor_show_popup(state, changes_info, 0);
    return 0;
}

/* Helper functions for file operations and window management */
static int vizero_edit_next_file(vizero_editor_state_t* state) {
    if (!state) return -1;
    
    /* For now, treat as next buffer since we don't maintain separate argument list */
    return vizero_editor_next_buffer(state);
}

static int vizero_edit_previous_file(vizero_editor_state_t* state) {
    if (!state) return -1;
    
    /* For now, treat as previous buffer since we don't maintain separate argument list */
    return vizero_editor_previous_buffer(state);
}

static int vizero_print_working_directory(vizero_editor_state_t* state) {
    if (!state) return -1;
    
    char cwd[1024];
    
#ifdef _WIN32
    DWORD result = GetCurrentDirectoryA(sizeof(cwd), cwd);
    if (result > 0 && result < sizeof(cwd)) {
        char msg[1200];
        snprintf(msg, sizeof(msg), "Current directory: %s", cwd);
        vizero_editor_set_status_message(state, msg);
        return 0;
    } else {
        vizero_editor_set_status_message(state, "Failed to get current directory");
        return -1;
    }
#else
    if (getcwd(cwd, sizeof(cwd)) != NULL) {
        char msg[1200];
        snprintf(msg, sizeof(msg), "Current directory: %s", cwd);
        vizero_editor_set_status_message(state, msg);
        return 0;
    } else {
        vizero_editor_set_status_message(state, "Failed to get current directory");
        return -1;
    }
#endif
}

static int vizero_close_all_except_current_window(vizero_editor_state_t* state) {
    if (!state || !state->window_manager) {
        vizero_editor_set_status_message(state, "No window manager available");
        return -1;
    }
    
    /* Get the currently focused window */
    vizero_editor_window_t* focused_window = vizero_window_manager_get_focused_window(state->window_manager);
    if (!focused_window) {
        vizero_editor_set_status_message(state, "No focused window");
        return -1;
    }
    
    /* Get window count before closing */
    int window_count = vizero_window_manager_get_window_count(state->window_manager);
    if (window_count <= 1) {
        vizero_editor_set_status_message(state, "Only one window open");
        return 0;
    }
    
    /* Close all windows except the focused one */
    int closed_count = 0;
    for (int i = 0; i < window_count; i++) {
        vizero_editor_window_t* window = vizero_window_manager_get_window(state->window_manager, (size_t)i);
        if (window && window != focused_window) {
            if (vizero_window_manager_close_window(state->window_manager, window->window_id, state) == 0) {
                closed_count++;
            }
        }
    }
    
    char msg[64];
    snprintf(msg, sizeof(msg), "Closed %d windows", closed_count);
    vizero_editor_set_status_message(state, msg);
    
    return 0;
}

static int vizero_edit_new_unnamed_buffer(vizero_editor_state_t* state) {
    if (!state) return -1;
    
    /* Create a new unnamed buffer */
    int result = vizero_editor_create_new_buffer(state, NULL);
    if (result == 0) {
        /* Switch to the new buffer */
        size_t new_buffer_index = vizero_editor_get_buffer_count(state) - 1;
        vizero_editor_switch_buffer(state, new_buffer_index);
        
        vizero_editor_set_status_message(state, "New unnamed buffer created");
        return 0;
    } else {
        vizero_editor_set_status_message(state, "Failed to create new buffer");
        return -1;
    }
}

/* External command execution functions */
static int vizero_execute_shell_command(vizero_editor_state_t* state, const char* command) {
    if (!state || !command || strlen(command) == 0) {
        vizero_editor_set_status_message(state, "Empty command");
        return -1;
    }
    
    /* Trim leading whitespace */
    while (*command && (*command == ' ' || *command == '\t')) command++;
    
    if (strlen(command) == 0) {
        vizero_editor_set_status_message(state, "Empty command");
        return -1;
    }
    
#ifdef _WIN32
    /* Windows: Execute command in new console window */
    char cmd_line[1024];
    snprintf(cmd_line, sizeof(cmd_line), "cmd.exe /c start \"Vizero Shell Command\" cmd.exe /k \"%s & echo. & echo Command completed. Press any key to close... & pause >nul\"", command);
    
    STARTUPINFOA si;
    PROCESS_INFORMATION pi;
    memset(&si, 0, sizeof(si));
    memset(&pi, 0, sizeof(pi));
    si.cb = sizeof(si);
    
    if (CreateProcessA(NULL, cmd_line, NULL, NULL, FALSE, 0, NULL, NULL, &si, &pi)) {
        CloseHandle(pi.hProcess);
        CloseHandle(pi.hThread);
        
        char msg[256];
        snprintf(msg, sizeof(msg), "Executed: %s", command);
        vizero_editor_set_status_message(state, msg);
        return 0;
    } else {
        char msg[256];
        snprintf(msg, sizeof(msg), "Failed to execute: %s", command);
        vizero_editor_set_status_message(state, msg);
        return -1;
    }
#else
    /* Unix: Execute command in new terminal */
    char cmd_line[1024];
    
    /* Try gnome-terminal first, then xterm */
    snprintf(cmd_line, sizeof(cmd_line), "gnome-terminal -- bash -c '%s; echo; echo \"Command completed. Press any key to close...\"; read -n 1'", command);
    int result = system(cmd_line);
    
    if (result != 0) {
        /* Try xterm if gnome-terminal failed */
        snprintf(cmd_line, sizeof(cmd_line), "xterm -e bash -c '%s; echo; echo \"Command completed. Press any key to close...\"; read -n 1'", command);
        result = system(cmd_line);
    }
    
    if (result == 0) {
        char msg[256];
        snprintf(msg, sizeof(msg), "Executed: %s", command);
        vizero_editor_set_status_message(state, msg);
        return 0;
    } else {
        char msg[256];
        snprintf(msg, sizeof(msg), "Failed to execute: %s", command);
        vizero_editor_set_status_message(state, msg);
        return -1;
    }
#endif
}

static int vizero_read_shell_command_output(vizero_editor_state_t* state, const char* command) {
    if (!state || !command || strlen(command) == 0) {
        vizero_editor_set_status_message(state, "Empty command");
        return -1;
    }
    
    /* Trim leading whitespace */
    while (*command && (*command == ' ' || *command == '\t')) command++;
    
    if (strlen(command) == 0) {
        vizero_editor_set_status_message(state, "Empty command");
        return -1;
    }
    
    /* Execute command and capture output */
    FILE* pipe = popen(command, "r");
    if (!pipe) {
        char msg[256];
        snprintf(msg, sizeof(msg), "Failed to execute: %s", command);
        vizero_editor_set_status_message(state, msg);
        return -1;
    }
    
    /* Read command output */
    char buffer[8192] = "";
    char line[512];
    
    while (fgets(line, sizeof(line), pipe) != NULL) {
        if (strlen(buffer) + strlen(line) < sizeof(buffer) - 1) {
            strcat(buffer, line);
        } else {
            /* Buffer getting full, stop reading */
            strcat(buffer, "... (output truncated)\n");
            break;
        }
    }
    
    int exit_code = pclose(pipe);
    
    if (strlen(buffer) > 0) {
        /* Insert the output at cursor position */
        vizero_buffer_t* buf = vizero_editor_get_current_buffer(state);
        vizero_cursor_t* cursor = vizero_editor_get_current_cursor(state);
        
        if (buf && cursor) {
            size_t cursor_line = vizero_cursor_get_line(cursor);
            size_t cursor_col = vizero_cursor_get_column(cursor);
            
            /* Split output into lines and insert */
            char* output_copy = strdup(buffer);
            char* line_start = output_copy;
            char* line_end;
            size_t insert_line = cursor_line;
            
            while ((line_end = strchr(line_start, '\n')) != NULL) {
                *line_end = '\0';
                
                if (insert_line == cursor_line && cursor_col > 0) {
                    /* Insert into current line at cursor position */
                    vizero_buffer_insert_text(buf, cursor_line, cursor_col, line_start);
                    insert_line++;
                } else {
                    /* Insert as new line */
                    vizero_buffer_insert_line(buf, insert_line, line_start);
                    insert_line++;
                }
                
                line_start = line_end + 1;
            }
            
            /* Handle last line if it doesn't end with newline */
            if (*line_start) {
                if (insert_line == cursor_line && cursor_col > 0) {
                    vizero_buffer_insert_text(buf, cursor_line, cursor_col, line_start);
                } else {
                    vizero_buffer_insert_line(buf, insert_line, line_start);
                }
            }
            
            free(output_copy);
            
            char msg[256];
            snprintf(msg, sizeof(msg), "Read output from: %s (exit code: %d)", command, exit_code);
            vizero_editor_set_status_message(state, msg);
            return 0;
        }
    } else {
        char msg[256];
        snprintf(msg, sizeof(msg), "No output from: %s (exit code: %d)", command, exit_code);
        vizero_editor_set_status_message(state, msg);
        return 0;
    }
    
    return -1;
}

static int vizero_run_make_command(vizero_editor_state_t* state) {
    if (!state) return -1;
    
    /* Check if Makefile exists */
    FILE* makefile = fopen("Makefile", "r");
    if (!makefile) {
        makefile = fopen("makefile", "r");
    }
    
    if (!makefile) {
        vizero_editor_set_status_message(state, "No Makefile found in current directory");
        return -1;
    }
    
    fclose(makefile);
    
    /* Execute make command */
#ifdef _WIN32
    /* On Windows, try different make commands */
    const char* make_commands[] = {"make", "mingw32-make", "nmake", NULL};
    
    for (int i = 0; make_commands[i]; i++) {
        char test_cmd[64];
        snprintf(test_cmd, sizeof(test_cmd), "%s --version >nul 2>&1", make_commands[i]);
        
        if (system(test_cmd) == 0) {
            /* This make command exists, use it */
            char cmd_line[256];
            snprintf(cmd_line, sizeof(cmd_line), "cmd.exe /c start \"Vizero Make\" cmd.exe /k \"%s & echo. & echo Build completed. Press any key to close... & pause >nul\"", make_commands[i]);
            
            STARTUPINFOA si;
            PROCESS_INFORMATION pi;
            memset(&si, 0, sizeof(si));
            memset(&pi, 0, sizeof(pi));
            si.cb = sizeof(si);
            
            if (CreateProcessA(NULL, cmd_line, NULL, NULL, FALSE, 0, NULL, NULL, &si, &pi)) {
                CloseHandle(pi.hProcess);
                CloseHandle(pi.hThread);
                
                char msg[128];
                snprintf(msg, sizeof(msg), "Running %s in new window", make_commands[i]);
                vizero_editor_set_status_message(state, msg);
                return 0;
            }
        }
    }
    
    vizero_editor_set_status_message(state, "No make command found (tried: make, mingw32-make, nmake)");
    return -1;
#else
    /* Unix: Use make directly */
    char cmd_line[256];
    snprintf(cmd_line, sizeof(cmd_line), "gnome-terminal -- bash -c 'make; echo; echo \"Build completed. Press any key to close...\"; read -n 1'");
    int result = system(cmd_line);
    
    if (result != 0) {
        /* Try xterm if gnome-terminal failed */
        snprintf(cmd_line, sizeof(cmd_line), "xterm -e bash -c 'make; echo; echo \"Build completed. Press any key to close...\"; read -n 1'");
        result = system(cmd_line);
    }
    
    if (result == 0) {
        vizero_editor_set_status_message(state, "Running make in new terminal");
        return 0;
    } else {
        vizero_editor_set_status_message(state, "Failed to run make command");
        return -1;
    }
#endif
}

static int vizero_show_version_info(vizero_editor_state_t* state) {
    if (!state) return -1;
    
    const char* version_info = 
        "Vizero - Vi Clone v1.0.0\n\n"
        "Build Information:\n"
        "- Built with SDL2 and OpenGL\n"
        "- Cross-platform plugin support\n"
        "- Hardware-accelerated rendering\n"
        "- Comprehensive vi command compatibility\n\n"
        "Features:\n"
        "- 60+ vi commands implemented\n"
        "- Multi-buffer and multi-window support\n"
        "- Advanced search/replace with regex\n"
        "- Line range and global operations\n"
        "- Integrated C/C++/Assembly compilation\n"
        "- Syntax highlighting and word wrap\n"
        "- Persistent settings system\n"
        "- Plugin architecture\n\n"
        "Copyright (c) 2025 Vizero Team\n"
        "Licensed under the MIT License\n\n"
        "For help, use :help or visit the project documentation.";
    
    vizero_editor_show_popup(state, version_info, 0); /* No timeout */
    return 0;
}

int vizero_editor_execute_command(vizero_editor_state_t* state, const char* command) {
 
    if (!state || !command) return -1;
    
    /* Skip leading colon if present */
    if (command[0] == ':') command++;
    
    /* Parse and execute commands */
    if (strcmp(command, "ls") == 0) {
        /* List files in current directory in a popup */
        #ifdef _WIN32
        WIN32_FIND_DATAA findFileData;
        HANDLE hFind = FindFirstFileA("*", &findFileData);
        if (hFind == INVALID_HANDLE_VALUE) {
            vizero_editor_set_status_message(state, "No files found");
            return -1;
        }
        char popup[8192] = "";
        strcat(popup, "Files in current directory:\n\n");
        do {
            const char* name = findFileData.cFileName;
            if (strcmp(name, ".") == 0 || strcmp(name, "..") == 0) continue;
            int is_dir = (findFileData.dwFileAttributes & FILE_ATTRIBUTE_DIRECTORY) != 0;
            int is_exe = 0;
            if (!is_dir) {
                const char* ext = strrchr(name, '.');
                if (ext && (_stricmp(ext, ".exe") == 0 || _stricmp(ext, ".bat") == 0 || _stricmp(ext, ".com") == 0)) {
                    is_exe = 1;
                }
            }
            char line[512];
            if (is_dir) {
                snprintf(line, sizeof(line), "\033[38;2;128;192;255m%-40s   [DIR]\033[0m\n", name); // pale blue
            } else if (is_exe) {
                unsigned long long size = ((unsigned long long)findFileData.nFileSizeHigh << 32) | findFileData.nFileSizeLow;
                snprintf(line, sizeof(line), "\033[38;2;255;128;128m%-40s   [EXE]  %10llu bytes\033[0m\n", name, size); // pale red
            } else {
                unsigned long long size = ((unsigned long long)findFileData.nFileSizeHigh << 32) | findFileData.nFileSizeLow;
                snprintf(line, sizeof(line), "\033[38;2;255;255;192m%-40s   [FILE] %10llu bytes\033[0m\n", name, size); // pale yellow
            }
            strcat(popup, line);
        } while (FindNextFileA(hFind, &findFileData));
        FindClose(hFind);
        vizero_editor_show_popup(state, popup, 0); /* No timeout, stays until ESC */
        return 0;
        #else
        DIR* dir = opendir(".");
        if (!dir) {
            vizero_editor_set_status_message(state, "No files found");
            return -1;
        }
        char popup[8192] = "";
        strcat(popup, "Files in current directory:\n\n");
        struct dirent* entry;
        while ((entry = readdir(dir)) != NULL) {
            const char* name = entry->d_name;
            if (strcmp(name, ".") == 0 || strcmp(name, "..") == 0) continue;
            int is_dir = (entry->d_type == DT_DIR);
            int is_exe = 0;
            if (!is_dir) {
                const char* ext = strrchr(name, '.');
                if (ext && (strcasecmp(ext, ".exe") == 0 || strcasecmp(ext, ".sh") == 0)) {
                    is_exe = 1;
                }
            }
            char line[512];
            if (is_dir) {
                snprintf(line, sizeof(line), "\033[38;2;128;192;255m%-40s   [DIR]\033[0m\n", name); // pale blue
            } else if (is_exe) {
                snprintf(line, sizeof(line), "\033[38;2;255;128;128m%-40s   [EXE]\033[0m\n", name); // pale red
            } else {
                snprintf(line, sizeof(line), "\033[38;2;255;255;192m%-40s   [FILE]\033[0m\n", name); // pale yellow
            }
            strcat(popup, line);
        }
        closedir(dir);
        vizero_editor_show_popup(state, popup, 0);
        return 0;
        #endif
    }
    else if (strncmp(command, "chdir ", 6) == 0) {
        /* Change working directory */
        const char* path = command + 6;
        while (*path == ' ' || *path == '\t') path++;
        if (strlen(path) == 0) {
            vizero_editor_set_status_message(state, "Usage: :chdir <path>");
            return -1;
        }
#ifdef _WIN32
        int result = SetCurrentDirectoryA(path);
        if (result) {
            char msg[512];
            snprintf(msg, sizeof(msg), "Changed directory to: %s", path);
            vizero_editor_set_status_message(state, msg);
            return 0;
        } else {
            char msg[512];
            snprintf(msg, sizeof(msg), "Failed to change directory: %s", path);
            vizero_editor_set_status_message(state, msg);
            return -1;
        }
#else
        int result = chdir(path);
        if (result == 0) {
            char msg[512];
            snprintf(msg, sizeof(msg), "Changed directory to: %s", path);
            vizero_editor_set_status_message(state, msg);
            return 0;
        } else {
            char msg[512];
            snprintf(msg, sizeof(msg), "Failed to change directory: %s", path);
            vizero_editor_set_status_message(state, msg);
            return -1;
        }
#endif
    } else if (strcmp(command, "q") == 0 || strcmp(command, "quit") == 0) {
        /* Check if we're in help mode first */
        if (state->help_mode_active) {
            /* Exit help mode instead of quitting */
            return vizero_editor_exit_help_mode(state);
        }
        
        /* Quit command - check for unsaved changes */
        int has_unsaved = 0;
        
        /* Check project level changes */
        if (state->current_project && vizero_project_has_unsaved_changes(state->current_project)) {
            has_unsaved = 1;
        }
        
        /* Check buffer level changes */
        if (!has_unsaved) {
            for (size_t i = 0; i < state->buffer_count; i++) {
                if (vizero_buffer_is_modified(state->buffers[i])) {
                    has_unsaved = 1;
                    break;
                }
            }
        }
        
        if (has_unsaved) {
            vizero_editor_set_status_message_with_timeout(state, "No write since last change (add ! to override)", 3000);
            return -1;
        }
        
        /* Signal application to quit */
        vizero_editor_set_quit_flag(state);
        return 0;
        
    } else if (strcmp(command, "q!") == 0 || strcmp(command, "quit!") == 0) {
        /* Force quit without saving */
        vizero_editor_set_quit_flag(state);
        return 0;
        
    } else if (strcmp(command, "w") == 0 || strcmp(command, "write") == 0) {
        /* Save current buffer */
        vizero_buffer_t* buffer = vizero_editor_get_current_buffer(state);
        if (buffer) {
            if (vizero_buffer_save(buffer) == 0) {
                vizero_editor_set_status_message(state, "File written");
                return 0;
            } else {
                vizero_editor_set_status_message(state, "Error writing file");
                return -1;
            }
        }
        
    } else if (strcmp(command, "wq") == 0 || strcmp(command, "x") == 0) {
        /* Save and quit */
        vizero_buffer_t* buffer = vizero_editor_get_current_buffer(state);
        if (buffer && vizero_buffer_save(buffer) == 0) {
            exit(0); /* TODO: Better quit mechanism */
        } else {
            vizero_editor_set_status_message(state, "Error writing file");
            return -1;
        }
        
    } else if (strcmp(command, "wa") == 0) {
        /* Save all buffers */
        if (state->current_project) {
            if (vizero_project_save_all_buffers(state->current_project) == 0) {
                vizero_editor_set_status_message(state, "All files written");
                return 0;
            } else {
                vizero_editor_set_status_message(state, "Error writing some files");
                return -1;
            }
        } else {
            /* Save all buffers in editor */
            int errors = 0;
            for (size_t i = 0; i < state->buffer_count; i++) {
                if (vizero_buffer_is_modified(state->buffers[i])) {
                    if (vizero_buffer_save(state->buffers[i]) != 0) {
                        errors++;
                    }
                }
            }
            if (errors == 0) {
                vizero_editor_set_status_message(state, "All files written");
                return 0;
            } else {
                vizero_editor_set_status_message(state, "Error writing some files");
                return -1;
            }
        }
        
    } else if (strncmp(command, "e ", 2) == 0 || strncmp(command, "edit ", 5) == 0) {
        /* Open file */
        const char* filename = strchr(command, ' ');
        if (filename) {
            filename++; /* Skip space */
            
            /* Check if file exists */
            int file_exists = vizero_file_exists(filename);
            
            if (vizero_editor_open_buffer(state, filename) == 0) {
                char msg[512];
                if (file_exists) {
                    sprintf(msg, "\"%s\" opened", filename);
                } else {
                    sprintf(msg, "\"%s\" [New File]", filename);
                }
                // Show status message for 1.5 seconds, then revert to default
                vizero_editor_set_status_message_with_timeout(state, msg, 1500);
                return 0;
            } else {
                vizero_editor_set_status_message(state, "Error opening file");
                return -1;
            }
        }

    } else if (strncmp(command, "set ", 4) == 0) {
        /* Set setting command, with special handling for linewrap */
        const char* args = command + 4;
        const char* space = strchr(args, ' ');
        if (space) {
            size_t key_len = space - args;
            char key[128];
            if (key_len < sizeof(key)) {
                strncpy(key, args, key_len);
                key[key_len] = '\0';
                const char* value = space + 1;
                if (strcmp(key, "linewrap") == 0) {
                    if (strcmp(value, "on") == 0) {
                        vizero_settings_set_bool(state->settings, VIZERO_SETTING_WORD_WRAP, 1);
                        vizero_editor_set_status_message(state, "Line wrap enabled");
                        return 0;
                    } else if (strcmp(value, "off") == 0) {
                        vizero_settings_set_bool(state->settings, VIZERO_SETTING_WORD_WRAP, 0);
                        vizero_editor_set_status_message(state, "Line wrap disabled");
                        return 0;
                    } else {
                        vizero_editor_set_status_message(state, "Usage: :set linewrap on|off");
                        return -1;
                    }
                } else if (strcmp(key, "readonly") == 0 || strcmp(key, "ro") == 0) {
                    vizero_buffer_t* buffer = vizero_editor_get_current_buffer(state);
                    if (buffer) {
                        if (strcmp(value, "on") == 0) {
                            vizero_buffer_set_readonly(buffer, 1);
                            /* If currently in insert mode, switch to normal mode */
                            if (vizero_editor_get_mode(state) == VIZERO_MODE_INSERT) {
                                vizero_editor_set_mode(state, VIZERO_MODE_NORMAL);
                            }
                            vizero_editor_set_status_message(state, "Buffer is now read-only");
                            return 0;
                        } else if (strcmp(value, "off") == 0) {
                            vizero_buffer_set_readonly(buffer, 0);
                            vizero_editor_set_status_message(state, "Buffer is now writable");
                            return 0;
                        } else {
                            vizero_editor_set_status_message(state, "Usage: :set readonly on|off");
                            return -1;
                        }
                    } else {
                        vizero_editor_set_status_message(state, "No current buffer");
                        return -1;
                    }
                }
                /* Set the setting (fallback for other keys) */
                vizero_settings_set_string(state->settings, key, value);
                char msg[256];
                snprintf(msg, sizeof(msg), "Set %s = %s", key, value);
                vizero_editor_set_status_message(state, msg);
                return 0;
            }
        }
        /* Handle single-word set commands (like :set readonly, :set noreadonly) */
        if (strcmp(args, "readonly") == 0 || strcmp(args, "ro") == 0) {
            vizero_buffer_t* buffer = vizero_editor_get_current_buffer(state);
            if (buffer) {
                vizero_buffer_set_readonly(buffer, 1);
                /* If currently in insert mode, switch to normal mode */
                if (vizero_editor_get_mode(state) == VIZERO_MODE_INSERT) {
                    vizero_editor_set_mode(state, VIZERO_MODE_NORMAL);
                }
                vizero_editor_set_status_message(state, "Buffer is now read-only");
                return 0;
            } else {
                vizero_editor_set_status_message(state, "No current buffer");
                return -1;
            }
        } else if (strcmp(args, "noreadonly") == 0 || strcmp(args, "noro") == 0) {
            vizero_buffer_t* buffer = vizero_editor_get_current_buffer(state);
            if (buffer) {
                vizero_buffer_set_readonly(buffer, 0);
                vizero_editor_set_status_message(state, "Buffer is now writable");
                return 0;
            } else {
                vizero_editor_set_status_message(state, "No current buffer");
                return -1;
            }
        }
        vizero_editor_set_status_message(state, "Usage: :set key value");
        return -1;
    } else if (strcmp(command, "bn") == 0 || strcmp(command, "bnext") == 0) {
        /* Next buffer */
        if (vizero_editor_next_buffer(state) == 0) {
            char msg[256];
            vizero_buffer_t* buffer = vizero_editor_get_current_buffer(state);
            const char* filename = buffer ? vizero_buffer_get_filename(buffer) : NULL;
            sprintf(msg, "Buffer: %s", filename ? filename : "[No Name]");
            vizero_editor_set_status_message(state, msg);
            return 0;
        }
        
    } else if (strcmp(command, "bp") == 0 || strcmp(command, "bprev") == 0) {
        /* Previous buffer */
        if (vizero_editor_previous_buffer(state) == 0) {
            char msg[256];
            vizero_buffer_t* buffer = vizero_editor_get_current_buffer(state);
            const char* filename = buffer ? vizero_buffer_get_filename(buffer) : NULL;
            sprintf(msg, "Buffer: %s", filename ? filename : "[No Name]");
            vizero_editor_set_status_message(state, msg);
            return 0;
        }
        
    } else if (strcmp(command, "ls") == 0 || strcmp(command, "buffers") == 0) {
        /* List buffers */
        char msg[512] = "Buffers: ";
        for (size_t i = 0; i < state->buffer_count; i++) {
            const char* filename = vizero_buffer_get_filename(state->buffers[i]);
            char buffer_info[64];
            sprintf(buffer_info, "%zu:%s%s ", 
                    i + 1, 
                    filename ? filename : "[No Name]",
                    (i == state->current_buffer_index) ? "*" : "");
            strncat(msg, buffer_info, sizeof(msg) - strlen(msg) - 1);
        }
        vizero_editor_set_status_message(state, msg);
        return 0;
        
    } else if (strcmp(command, "ro") == 0 || strcmp(command, "readonly") == 0) {
        /* Set buffer read-only */
        vizero_buffer_t* buffer = vizero_editor_get_current_buffer(state);
        if (buffer) {
            vizero_buffer_set_readonly(buffer, 1);
            /* If currently in insert mode, switch to normal mode */
            if (vizero_editor_get_mode(state) == VIZERO_MODE_INSERT) {
                vizero_editor_set_mode(state, VIZERO_MODE_NORMAL);
            }
            vizero_editor_set_status_message(state, "Buffer is now read-only");
            return 0;
        } else {
            vizero_editor_set_status_message(state, "No current buffer");
            return -1;
        }
        
    } else if (strcmp(command, "noro") == 0 || strcmp(command, "noreadonly") == 0) {
        /* Set buffer writable */
        vizero_buffer_t* buffer = vizero_editor_get_current_buffer(state);
        if (buffer) {
            vizero_buffer_set_readonly(buffer, 0);
            vizero_editor_set_status_message(state, "Buffer is now writable");
            return 0;
        } else {
            vizero_editor_set_status_message(state, "No current buffer");
            return -1;
        }
        
    } else if (strcmp(command, "bd") == 0 || strcmp(command, "bdelete") == 0) {
        /* Delete/close current buffer */
        return vizero_editor_close_current_buffer(state);
        
    } else if (strncmp(command, "bd ", 3) == 0 || strncmp(command, "bdelete ", 8) == 0) {
        /* Delete specific buffer by number */
        const char* number_str = strchr(command, ' ');
        if (number_str) {
            number_str++; /* Skip space */
            char* endptr;
            long buffer_num = strtol(number_str, &endptr, 10);
            
            if (*endptr == '\0' && buffer_num > 0 && buffer_num <= (long)state->buffer_count) {
                size_t buffer_index = buffer_num - 1;
                return vizero_editor_close_buffer_by_index(state, buffer_index);
            } else {
                char msg[256];
                sprintf(msg, "Invalid buffer number: %s (valid range: 1-%zu)", number_str, state->buffer_count);
                vizero_editor_set_status_message(state, msg);
                return -1;
            }
        }
        
    } else if (strncmp(command, "b", 1) == 0 && strlen(command) > 1) {
        /* Switch to buffer by number (:b1, :b2, etc.) */
        const char* number_str = command + 1;
        char* endptr;
        long buffer_num = strtol(number_str, &endptr, 10);
        
        if (*endptr == '\0' && buffer_num >= 1 && buffer_num <= (long)state->buffer_count) {
            size_t buffer_index = (size_t)(buffer_num - 1); /* Convert to 0-based index */
            if (vizero_editor_switch_buffer(state, buffer_index) == 0) {
                char msg[256];
                vizero_buffer_t* buffer = vizero_editor_get_current_buffer(state);
                const char* filename = buffer ? vizero_buffer_get_filename(buffer) : NULL;
                sprintf(msg, "Buffer %ld: %s", buffer_num, filename ? filename : "[No Name]");
                vizero_editor_set_status_message(state, msg);
                return 0;
            }
        } else {
            char msg[256];
            sprintf(msg, "Invalid buffer number: %s (valid range: 1-%zu)", number_str, state->buffer_count);
            vizero_editor_set_status_message(state, msg);
            return -1;
        }
        
    } else if (strcmp(command, "split") == 0 || strcmp(command, "sp") == 0) {
        /* Split window horizontally */
        if (state->window_manager) {
            vizero_editor_window_t* current_window = vizero_window_manager_get_focused_window(state->window_manager);
            if (current_window) {
                if (vizero_window_manager_split_horizontal(state->window_manager, current_window->window_id) == 0) {
                    vizero_editor_set_status_message(state, "Window split horizontally");
                    return 0;
                } else {
                    vizero_editor_set_status_message(state, "Failed to split window");
                    return -1;
                }
            }
        }
        vizero_editor_set_status_message(state, "No active window to split");
        return -1;
        
    } else if (strcmp(command, "vsplit") == 0 || strcmp(command, "vsp") == 0) {
        /* Split window vertically */
        if (state->window_manager) {
            vizero_editor_window_t* current_window = vizero_window_manager_get_focused_window(state->window_manager);
            if (current_window) {
                if (vizero_window_manager_split_vertical(state->window_manager, current_window->window_id) == 0) {
                    vizero_editor_set_status_message(state, "Window split vertically");
                    return 0;
                } else {
                    vizero_editor_set_status_message(state, "Failed to split window");
                    return -1;
                }
            }
        }
        vizero_editor_set_status_message(state, "No active window to split");
        return -1;
        
    } else if (strcmp(command, "close") == 0 || strcmp(command, "clo") == 0) {
        /* Close current window (only works in split mode) */
        if (state->window_manager) {
            vizero_layout_type_t layout = vizero_window_manager_get_layout_type(state->window_manager);
            if (layout != VIZERO_LAYOUT_SINGLE) {
                vizero_editor_window_t* current_window = vizero_window_manager_get_focused_window(state->window_manager);
                if (current_window) {
                    if (vizero_window_manager_close_split(state->window_manager, current_window->window_id) == 0) {
                        vizero_editor_set_status_message(state, "Window closed");
                        return 0;
                    } else {
                        vizero_editor_set_status_message(state, "Failed to close window");
                        return -1;
                    }
                }
            } else {
                vizero_editor_set_status_message(state, "Cannot close window in single layout");
                return -1;
            }
        }
        vizero_editor_set_status_message(state, "No window manager available");
        return -1;
        
    } else if (strcmp(command, "new") == 0) {
        /* Create new empty buffer */
        if (vizero_editor_create_new_buffer(state, NULL) == 0) {
            vizero_editor_set_status_message(state, "DEBUG: New buffer created successfully!");
            return 0;
        } else {
            vizero_editor_set_status_message(state, "DEBUG: Failed to create new buffer!");
            return -1;
        }
        
    } else if (strcmp(command, "help") == 0 || strcmp(command, "h") == 0) {
        /* Load manual.md into current buffer */
        if (vizero_editor_enter_help_mode(state) == 0) {
            return 0;
        } else {
            /* Fallback to popup if help mode fails */
            const char* help_content = 
                "VIZERO EDITOR - HELP\n"
                "====================\n\n"
                "MOVEMENT COMMANDS:\n"
                "  h, j, k, l        - Left, Down, Up, Right\n"
                "  w, b              - Next/Previous word\n"
                "  0, $              - Start/End of line\n"
                "  gg, G             - First/Last line\n"
                "  Ctrl+U, Ctrl+D    - Page Up/Down\n\n"
                "EDITING COMMANDS:\n"
                "  i, a              - Insert before/after cursor\n"
                "  I, A              - Insert at start/end of line\n"
                "  o, O              - Open line below/above\n"
                "  x, X              - Delete char after/before cursor\n"
                "  dd                - Delete current line\n"
                "  yy                - Copy current line\n"
                "  p, P              - Paste after/before cursor\n"
                "  u, Ctrl+R         - Undo/Redo\n\n"
                "BUFFER COMMANDS:\n"
                "  :e filename       - Open file in new buffer\n"
                "  :w                - Write (save) current buffer\n"
                "  :q                - Quit (if no unsaved changes)\n"
                "  :wq               - Write and quit\n"
                "  :bn, :bp          - Next/Previous buffer\n"
                "  :b1, :b2, :b3     - Switch to buffer number\n"
                "  :ls               - List all buffers\n"
                "  :bd               - Delete current buffer\n"
                "  :new              - Create new empty buffer\n\n"
                "WINDOW COMMANDS:\n"
                "  :split (:sp)      - Split window horizontally\n"
                "  :vsplit (:vsp)    - Split window vertically\n"
                "  :close (:clo)     - Close current window\n\n"
                "OTHER COMMANDS:\n"
                "  :10               - Go to line 10\n"
                "  :syntax           - Toggle syntax highlighting\n"
                "  :compile          - Compile current file\n"
                "  :help             - Show this help\n\n"
                "Press ESC to close this help window.";
            
            vizero_editor_show_popup(state, help_content, 0); /* No timeout - stays until ESC */
            return 0;
        }
        
    } else if (strcmp(command, "$") == 0) {
        /* Go to last line */
        vizero_buffer_t* buffer = vizero_editor_get_current_buffer(state);
        vizero_cursor_t* cursor = vizero_editor_get_current_cursor(state);
        if (buffer && cursor) {
            size_t line_count = vizero_buffer_get_line_count(buffer);
            if (line_count > 0) {
                vizero_cursor_set_position(cursor, line_count - 1, 0);
                char msg[64];
                sprintf(msg, "Line %zu", line_count);
                vizero_editor_set_status_message(state, msg);
                return 0;
            }
        }
        return -1;
        
    } else if (strncmp(command, "linenum ", 8) == 0) {
        /* Line number display setting */
        const char* value = command + 8;
        if (strcmp(value, "on") == 0) {
            vizero_settings_set_bool(state->settings, VIZERO_SETTING_LINE_NUMBERS, true);
            vizero_editor_set_status_message(state, "Line numbers enabled");
        } else if (strcmp(value, "off") == 0) {
            vizero_settings_set_bool(state->settings, VIZERO_SETTING_LINE_NUMBERS, false);
            vizero_editor_set_status_message(state, "Line numbers disabled");
        } else {
            char msg[256];
            sprintf(msg, "Invalid linenum value: %s (use 'on' or 'off')", value);
            vizero_editor_set_status_message(state, msg);
            return -1;
        }
        return 0;
        
    } else if (strncmp(command, "syntax ", 7) == 0) {
        /* Syntax highlighting setting */
        const char* value = command + 7;
        if (strcmp(value, "on") == 0) {
            vizero_settings_set_bool(state->settings, VIZERO_SETTING_SYNTAX_HIGHLIGHTING, true);
            vizero_editor_set_status_message(state, "Syntax highlighting enabled");
        } else if (strcmp(value, "off") == 0) {
            vizero_settings_set_bool(state->settings, VIZERO_SETTING_SYNTAX_HIGHLIGHTING, false);
            vizero_editor_set_status_message(state, "Syntax highlighting disabled");
        } else {
            char msg[256];
            sprintf(msg, "Invalid syntax value: %s (use 'on' or 'off')", value);
            vizero_editor_set_status_message(state, msg);
            return -1;
        }
        return 0;
        
    } else if (strncmp(command, "tabs ", 5) == 0) {
        /* Tab size setting */
        const char* value = command + 5;
        char* endptr;
        long tab_size = strtol(value, &endptr, 10);
        if (*endptr != '\0' || tab_size < 1 || tab_size > 16) {
            char msg[256];
            sprintf(msg, "Invalid tab size: %s (must be 1-16)", value);
            vizero_editor_set_status_message(state, msg);
            return -1;
        }
        vizero_settings_set_int(state->settings, VIZERO_SETTING_TAB_SIZE, (int)tab_size);
        char msg[256];
        sprintf(msg, "Tab size set to %d", (int)tab_size);
        vizero_editor_set_status_message(state, msg);
        return 0;
        
    } else if (strncmp(command, "set ", 4) == 0) {
        /* Set setting command */
        const char* args = command + 4;
        const char* space = strchr(args, ' ');
        if (space) {
            /* Extract key and value */
            size_t key_len = space - args;
            char key[128];
            if (key_len < sizeof(key)) {
                strncpy(key, args, key_len);
                key[key_len] = '\0';
                const char* value = space + 1;
                
                /* Set the setting */
                vizero_settings_set_string(state->settings, key, value);
                char msg[256];
                snprintf(msg, sizeof(msg), "Set %s = %s", key, value);
                vizero_editor_set_status_message(state, msg);
                return 0;
            }
        }
        vizero_editor_set_status_message(state, "Usage: :set key value");
        return -1;
        
    } else if (strcmp(command, "show") == 0) {
        /* Show all settings in popup */
        char* settings_text = vizero_settings_get_all_as_string(state->settings);
        if (settings_text) {
            vizero_editor_show_popup(state, settings_text, 5000); /* 5 seconds */
            free(settings_text);
        } else {
            vizero_editor_set_status_message(state, "Error: Could not retrieve settings");
        }
        return 0;
        
    } else if (strncmp(command, "show ", 5) == 0) {
        /* Show setting value */
        const char* key = command + 5;
        const char* value = vizero_settings_get_string(state->settings, key);
        if (value) {
            char msg[256];
            snprintf(msg, sizeof(msg), "%s = %s", key, value);
            vizero_editor_set_status_message(state, msg);
        } else {
            char msg[256];
            snprintf(msg, sizeof(msg), "Setting '%s' not found", key);
            vizero_editor_set_status_message(state, msg);
        }
        return 0;
        
    } else if (strcmp(command, "result") == 0 || strcmp(command, "compileresult") == 0) {
        /* Show last compilation result */
        if (state->last_compile_output) {
            /* For now, create a temporary buffer to display the result */
            /* Later we can implement a proper popup window */
            
            /* Create a new buffer with the compilation results */
            vizero_buffer_t* result_buffer = vizero_buffer_create();
            if (result_buffer) {
                /* Set the buffer content to the compilation result */
                const char* lines = state->last_compile_output;
                const char* line_start = lines;
                
                while (*lines) {
                    if (*lines == '\n') {
                        /* Add line to buffer */
                        char line_text[1024];
                        size_t line_len = lines - line_start;
                        if (line_len < sizeof(line_text) - 1) {
                            strncpy(line_text, line_start, line_len);
                            line_text[line_len] = '\0';
                            vizero_buffer_insert_line(result_buffer, vizero_buffer_get_line_count(result_buffer), line_text);
                        }
                        line_start = lines + 1;
                    }
                    lines++;
                }
                
                /* Add the last line if it doesn't end with newline */
                if (line_start < lines) {
                    char line_text[1024];
                    size_t line_len = lines - line_start;
                    if (line_len < sizeof(line_text) - 1) {
                        strncpy(line_text, line_start, line_len);
                        line_text[line_len] = '\0';
                        vizero_buffer_insert_line(result_buffer, vizero_buffer_get_line_count(result_buffer), line_text);
                    }
                }
                
                /* Set buffer filename to indicate it's a result */
                vizero_buffer_set_filename(result_buffer, "[Compilation Result]");
                
                /* Add to editor buffers if there's space */
                if (state->buffer_count < MAX_BUFFERS) {
                    state->buffers[state->buffer_count] = result_buffer;
                    state->cursors[state->buffer_count] = vizero_cursor_create(result_buffer);
                    state->current_buffer_index = state->buffer_count;
                    state->buffer_count++;
                    
                    vizero_editor_set_status_message(state, "Compilation result opened in new buffer");
                } else {
                    vizero_buffer_destroy(result_buffer);
                    vizero_editor_set_status_message(state, "Too many buffers open - compilation result printed to console");
                }
            } else {
                vizero_editor_set_status_message(state, "Could not create result buffer");
            }
        } else {
            vizero_editor_set_status_message(state, "No compilation result available");
        }
        return 0;
        
    } else if (strncmp(command, "cc ", 3) == 0) {
        /* Compile C file */
        const char* args = command + 3;
        return vizero_editor_compile_file(state, args, "c");
        
    } else if (strncmp(command, "cpp ", 4) == 0) {
        /* Compile C++ file */
        const char* args = command + 4;
        return vizero_editor_compile_file(state, args, "cpp");
        
    } else if (strncmp(command, "asm ", 4) == 0) {
        /* Assemble file */
        const char* args = command + 4;
        return vizero_editor_compile_file(state, args, "asm");
        
    } else if (command[0] == '/') {
        /* Forward search */
        const char* pattern = command + 1;
        if (strlen(pattern) > 0) {
            return vizero_search_forward(state, pattern);
        } else {
            vizero_editor_set_status_message(state, "Empty search pattern");
            return -1;
        }
        
    } else if (command[0] == '?') {
        /* Backward search */
        const char* pattern = command + 1;
        if (strlen(pattern) > 0) {
            return vizero_search_backward(state, pattern);
        } else {
            vizero_editor_set_status_message(state, "Empty search pattern");
            return -1;
        }
        
    } else if (strncmp(command, "s/", 2) == 0) {
        /* Substitute command: s/pattern/replacement/[flags] */
        const char* cmd = command + 2; /* Skip 's/' */
        
        /* Parse s/pattern/replacement/flags */
        char pattern[512] = {0};
        char replacement[512] = {0};
        char flags[16] = {0};
        
        const char* ptr = cmd;
        char* dest = pattern;
        size_t dest_size = sizeof(pattern) - 1;
        
        /* Extract pattern (until next /) */
        while (*ptr && *ptr != '/' && dest_size > 0) {
            *dest++ = *ptr++;
            dest_size--;
        }
        *dest = '\0';
        
        if (*ptr == '/') ptr++; /* Skip separator */
        
        /* Extract replacement (until next /) */
        dest = replacement;
        dest_size = sizeof(replacement) - 1;
        while (*ptr && *ptr != '/' && dest_size > 0) {
            *dest++ = *ptr++;
            dest_size--;
        }
        *dest = '\0';
        
        if (*ptr == '/') ptr++; /* Skip separator */
        
        /* Extract flags */
        dest = flags;
        dest_size = sizeof(flags) - 1;
        while (*ptr && dest_size > 0) {
            *dest++ = *ptr++;
            dest_size--;
        }
        *dest = '\0';
        
        /* Determine scope and global flag */
        int global = (strchr(flags, 'g') != NULL);
        
        if (strlen(pattern) == 0) {
            vizero_editor_set_status_message(state, "Empty search pattern");
            return -1;
        }
        
        /* For now, substitute on current line only */
        vizero_cursor_t* cursor = vizero_editor_get_current_cursor(state);
        if (cursor) {
            size_t current_line = vizero_cursor_get_line(cursor);
            return vizero_substitute_line(state, pattern, replacement, (int)current_line, global);
        }
        
        return -1;
        
    } else if (strncmp(command, "%s/", 3) == 0) {
        /* Global substitute command: %s/pattern/replacement/[flags] */
        const char* cmd = command + 3; /* Skip '%s/' */
        
        /* Parse like regular substitute but apply to all lines */
        char pattern[512] = {0};
        char replacement[512] = {0};
        char flags[16] = {0};
        
        const char* ptr = cmd;
        char* dest = pattern;
        size_t dest_size = sizeof(pattern) - 1;
        
        /* Extract pattern */
        while (*ptr && *ptr != '/' && dest_size > 0) {
            *dest++ = *ptr++;
            dest_size--;
        }
        *dest = '\0';
        
        if (*ptr == '/') ptr++;
        
        /* Extract replacement */
        dest = replacement;
        dest_size = sizeof(replacement) - 1;
        while (*ptr && *ptr != '/' && dest_size > 0) {
            *dest++ = *ptr++;
            dest_size--;
        }
        *dest = '\0';
        
        if (*ptr == '/') ptr++;
        
        /* Extract flags */
        dest = flags;
        dest_size = sizeof(flags) - 1;
        while (*ptr && dest_size > 0) {
            *dest++ = *ptr++;
            dest_size--;
        }
        *dest = '\0';
        
        int global = (strchr(flags, 'g') != NULL);
        
        if (strlen(pattern) == 0) {
            vizero_editor_set_status_message(state, "Empty search pattern");
            return -1;
        }
        
        return vizero_substitute_all(state, pattern, replacement, global);
        
    } else if (strncmp(command, "run ", 4) == 0) {
        /* Run command: execute program in new command window */
        const char* program = command + 4;
        if (strlen(program) == 0) {
            vizero_editor_set_status_message(state, "Usage: :run <program> [args]");
            return -1;
        }
        
#ifdef _WIN32
        /* Windows: Use cmd.exe to run the program in a new window */
        char cmd_line[1024];
        snprintf(cmd_line, sizeof(cmd_line), "cmd.exe /c start \"Vizero Run\" cmd.exe /k \"%s\"", program);
        
        STARTUPINFOA si;
        PROCESS_INFORMATION pi;
        memset(&si, 0, sizeof(si));
        memset(&pi, 0, sizeof(pi));
        si.cb = sizeof(si);
        
        if (CreateProcessA(NULL, cmd_line, NULL, NULL, FALSE, 0, NULL, NULL, &si, &pi)) {
            CloseHandle(pi.hProcess);
            CloseHandle(pi.hThread);
            vizero_editor_set_status_message(state, "Program launched in new window");
            return 0;
        } else {
            char msg[256];
            snprintf(msg, sizeof(msg), "Failed to launch: %s", program);
            vizero_editor_set_status_message(state, msg);
            return -1;
        }
#else
        /* Unix: Use xterm or gnome-terminal to run in new window */
        char cmd_line[1024];
        
        /* Try gnome-terminal first, then xterm */
        snprintf(cmd_line, sizeof(cmd_line), "gnome-terminal -- bash -c '%s; echo; echo Press any key to continue...; read -n 1'", program);
        int result = system(cmd_line);
        
        if (result != 0) {
            /* Try xterm if gnome-terminal failed */
            snprintf(cmd_line, sizeof(cmd_line), "xterm -e bash -c '%s; echo; echo Press any key to continue...; read -n 1'", program);
            result = system(cmd_line);
        }
        
        if (result == 0) {
            vizero_editor_set_status_message(state, "Program launched in new window");
            return 0;
        } else {
            char msg[256];
            snprintf(msg, sizeof(msg), "Failed to launch: %s", program);
            vizero_editor_set_status_message(state, msg);
            return -1;
        }
#endif
        
    } else if (strcmp(command, "run") == 0) {
        /* Run without arguments - try to run the last compiled executable */
        const char* last_exe = NULL;
        
        /* Try common executable names based on current file */
        vizero_buffer_t* buffer = vizero_editor_get_current_buffer(state);
        if (buffer) {
            const char* filename = vizero_buffer_get_filename(buffer);
            if (filename) {
                const char* dot = strrchr(filename, '.');
                if (dot) {
                    char exe_name[256];
                    size_t name_len = dot - filename;
                    strncpy(exe_name, filename, name_len);
                    exe_name[name_len] = '\0';
                    
#ifdef _WIN32
                    strcat(exe_name, ".exe");
#endif
                    
                    /* Check if the executable exists */
                    FILE* test = fopen(exe_name, "rb");
                    if (test) {
                        fclose(test);
                        last_exe = exe_name;
                    }
                }
            }
        }
        
        if (last_exe) {
            /* Recursively call run with the executable name */
            char run_cmd[300];
            snprintf(run_cmd, sizeof(run_cmd), "run %s", last_exe);
            return vizero_editor_execute_command(state, run_cmd);
        } else {
            vizero_editor_set_status_message(state, "No executable found. Use :run <program>");
            return -1;
        }
        
    } else if (strcmp(command, "r") == 0) {
        /* Read command without filename - show usage */
        vizero_editor_set_status_message(state, "Usage: :r filename");
        return -1;
        
    } else if (strncmp(command, "r ", 2) == 0) {
        /* Read file into buffer at cursor position */
        const char* filename = command + 2;
        
        /* Skip leading whitespace */
        while (*filename && (*filename == ' ' || *filename == '\t')) filename++;
        
        if (strlen(filename) == 0) {
            vizero_editor_set_status_message(state, "Usage: :r filename");
            return -1;
        }
        
        /* Read the file */
        size_t file_size = 0;
        char* file_content = vizero_file_read_all(filename, &file_size);
        
        if (file_content != NULL) {
            /* Insert file content at cursor position */
            vizero_buffer_t* buffer = vizero_editor_get_current_buffer(state);
            vizero_cursor_t* cursor = vizero_editor_get_current_cursor(state);
            
            if (buffer && cursor) {
                size_t cursor_line = vizero_cursor_get_line(cursor);
                size_t cursor_col = vizero_cursor_get_column(cursor);
                
                /* Split file content into lines and insert */
                const char* line_start = file_content;
                const char* line_end;
                size_t insert_line = cursor_line;
                bool first_line = true;
                
                while ((line_end = strchr(line_start, '\n')) != NULL) {
                    /* Create line text */
                    size_t line_len = line_end - line_start;
                    char* line_text = (char*)malloc(line_len + 1);
                    if (line_text) {
                        strncpy(line_text, line_start, line_len);
                        line_text[line_len] = '\0';
                        
                        if (first_line && cursor_col > 0) {
                            /* Insert into current line at cursor position */
                            vizero_buffer_insert_text(buffer, cursor_line, cursor_col, line_text);
                            first_line = false;
                            insert_line++;
                        } else {
                            /* Insert as new line */
                            vizero_buffer_insert_line(buffer, insert_line, line_text);
                            insert_line++;
                        }
                        
                        free(line_text);
                    }
                    line_start = line_end + 1;
                }
                
                /* Handle last line if it doesn't end with newline */
                if (*line_start) {
                    if (first_line && cursor_col > 0) {
                        vizero_buffer_insert_text(buffer, cursor_line, cursor_col, line_start);
                    } else {
                        vizero_buffer_insert_line(buffer, insert_line, line_start);
                    }
                }
                
                char msg[512];
                snprintf(msg, sizeof(msg), "Read %zu bytes from %s", file_size, filename);
                vizero_editor_set_status_message(state, msg);
            } else {
                vizero_editor_set_status_message(state, "No current buffer");
            }
            
            free(file_content);
        } else {
            char msg[512];
            snprintf(msg, sizeof(msg), "Cannot read file: %s", filename);
            vizero_editor_set_status_message(state, msg);
            return -1;
        }
        
        return 0;
        
    } else if (strcmp(command, "file") == 0 || strcmp(command, "f") == 0) {
        /* Show current filename and status */
        vizero_buffer_t* buffer = vizero_editor_get_current_buffer(state);
        if (buffer) {
            const char* filename = vizero_buffer_get_filename(buffer);
            size_t line_count = vizero_buffer_get_line_count(buffer);
            int is_modified = vizero_buffer_is_modified(buffer);
            
            char msg[512];
            snprintf(msg, sizeof(msg), "\"%s\" %zu lines %s", 
                    filename ? filename : "[No Name]", 
                    line_count,
                    is_modified ? "[Modified]" : "");
            vizero_editor_set_status_message(state, msg);
        } else {
            vizero_editor_set_status_message(state, "No current buffer");
        }
        return 0;
        
    } else if (strncmp(command, "file ", 5) == 0 || strncmp(command, "f ", 2) == 0) {
        /* Set filename for current buffer */
        const char* filename = strchr(command, ' ');
        if (filename) {
            filename++; /* Skip space */
            while (*filename && (*filename == ' ' || *filename == '\t')) filename++; /* Skip whitespace */
            
            if (strlen(filename) > 0) {
                vizero_buffer_t* buffer = vizero_editor_get_current_buffer(state);
                if (buffer) {
                    vizero_buffer_set_filename(buffer, filename);
                    char msg[512];
                    snprintf(msg, sizeof(msg), "Set filename to: %s", filename);
                    vizero_editor_set_status_message(state, msg);
                    return 0;
                } else {
                    vizero_editor_set_status_message(state, "No current buffer");
                    return -1;
                }
            }
        }
        vizero_editor_set_status_message(state, "Usage: :file filename");
        return -1;
        
    } else if (strcmp(command, "d") == 0) {
        /* Delete current line */
        return vizero_editor_cut_current_line(state);
        
    } else if (strcmp(command, "y") == 0) {
        /* Yank (copy) current line */
        return vizero_editor_copy_current_line(state);
        
    } else if (strcmp(command, "p") == 0) {
        /* Put (paste) after cursor */
        return vizero_editor_paste_at_cursor(state);
        
    } else if (strcmp(command, "P") == 0) {
        /* Put (paste) before cursor */
        vizero_cursor_t* cursor = vizero_editor_get_current_cursor(state);
        if (cursor) {
            /* Move cursor to beginning of line, paste, then adjust position */
            size_t original_line = vizero_cursor_get_line(cursor);
            size_t original_col = vizero_cursor_get_column(cursor);
            
            /* Move to beginning of line */
            vizero_cursor_set_position(cursor, original_line, 0);
            
            /* Paste */
            int result = vizero_editor_paste_at_cursor(state);
            
            /* If paste was successful and added a line, move cursor to original position + 1 line */
            if (result == 0) {
                const char* clipboard = vizero_editor_get_clipboard_content(state);
                if (clipboard && strchr(clipboard, '\n')) {
                    /* Pasted content has newlines, cursor should be on next line */
                    vizero_cursor_set_position(cursor, original_line + 1, original_col);
                } else {
                    /* Pasted content is on same line, adjust column */
                    size_t clipboard_len = clipboard ? strlen(clipboard) : 0;
                    vizero_cursor_set_position(cursor, original_line, original_col + clipboard_len);
                }
            }
            
            return result;
        }
        return -1;
        
    } else if (strcmp(command, "j") == 0) {
        /* Join current line with next */
        vizero_buffer_t* buffer = vizero_editor_get_current_buffer(state);
        vizero_cursor_t* cursor = vizero_editor_get_current_cursor(state);
        if (buffer && cursor) {
            size_t current_line = vizero_cursor_get_line(cursor);
            size_t line_count = vizero_buffer_get_line_count(buffer);
            
            if (current_line + 1 < line_count) {
                /* Get current line length before joining */
                size_t current_line_len = vizero_buffer_get_line_length(buffer, current_line);
                
                if (vizero_buffer_join_lines(buffer, current_line) == 0) {
                    /* Position cursor at the join point */
                    vizero_cursor_set_position(cursor, current_line, current_line_len);
                    vizero_editor_set_status_message_with_timeout(state, "Lines joined", 2000);
                    return 0;
                } else {
                    vizero_editor_set_status_message(state, "Error joining lines");
                    return -1;
                }
            } else {
                vizero_editor_set_status_message(state, "No next line to join");
                return -1;
            }
        }
        return -1;
        
    } else if (strcmp(command, "u") == 0) {
        /* Undo last change */
        return vizero_editor_undo(state);
        
    } else if (strcmp(command, "redo") == 0) {
        /* Redo last undone change */
        return vizero_editor_redo(state);
        
    } else if (strcmp(command, "n") == 0 || strcmp(command, "next") == 0) {
        /* Edit next file in argument list */
        return vizero_edit_next_file(state);
        
    } else if (strcmp(command, "prev") == 0 || strcmp(command, "previous") == 0) {
        /* Edit previous file in argument list */
        return vizero_edit_previous_file(state);
        
    } else if (strcmp(command, "pwd") == 0) {
        /* Print working directory */
        return vizero_print_working_directory(state);
        
    } else if (strcmp(command, "only") == 0) {
        /* Close all windows except current */
        return vizero_close_all_except_current_window(state);
        
    } else if (strcmp(command, "enew") == 0) {
        /* Edit new unnamed buffer */
        return vizero_edit_new_unnamed_buffer(state);
        
    } else if (command[0] == '!' && command[1] != '\0') {
        /* Execute shell command */
        return vizero_execute_shell_command(state, command + 1);
        
    } else if (strncmp(command, "r !", 3) == 0) {
        /* Read output of shell command */
        return vizero_read_shell_command_output(state, command + 3);
        
    } else if (strcmp(command, "make") == 0) {
        /* Run make command */
        return vizero_run_make_command(state);
        
    } else if (strcmp(command, "version") == 0) {
        /* Show version information */
        return vizero_show_version_info(state);
        
    } else {
        /* Check if command is a line number */
        char* endptr;
        /* Check for line range operations like 1,5d or .,+5d */
        if (strchr(command, ',') && (strstr(command, "d") || strstr(command, "y") || strstr(command, "s/"))) {
            return vizero_execute_line_range_command(state, command);
        }
        
        /* Check for global commands like g/pattern/d */
        if (command[0] == 'g' && command[1] == '/') {
            return vizero_execute_global_command(state, command);
        }
        
        /* Check for inverse global commands like v/pattern/d */
        if (command[0] == 'v' && command[1] == '/') {
            return vizero_execute_inverse_global_command(state, command);
        }
        
        /* Handle marks and navigation commands */
        if (strcmp(command, "marks") == 0) {
            return vizero_show_marks(state);
        } else if (strcmp(command, "jumps") == 0) {
            return vizero_show_jumps(state);
        } else if (strcmp(command, "changes") == 0) {
            return vizero_show_changes(state);
        }
        
        long line_num = strtol(command, &endptr, 10);
        
        if (*endptr == '\0' && line_num > 0) {
            /* Pure numeric command - go to line */
            vizero_buffer_t* buffer = vizero_editor_get_current_buffer(state);
            vizero_cursor_t* cursor = vizero_editor_get_current_cursor(state);
            if (buffer && cursor) {
                size_t line_count = vizero_buffer_get_line_count(buffer);
                size_t target_line = (size_t)(line_num - 1); /* Convert to 0-based */
                
                if (target_line < line_count) {
                    vizero_cursor_set_position(cursor, target_line, 0);
                    char msg[64];
                    sprintf(msg, "Line %ld", line_num);
                    vizero_editor_set_status_message(state, msg);
                    return 0;
                } else {
                    char msg[128];
                    sprintf(msg, "Line %ld out of range (1-%zu)", line_num, line_count);
                    vizero_editor_set_status_message(state, msg);
                    return -1;
                }
            }
            return -1;
        }
        
        /* Unknown command */
        char msg[256];
        sprintf(msg, "Not an editor command: %s", command);
        vizero_editor_set_status_message_with_timeout(state, msg, 3000); /* 3 second timeout */
        return -1;
    }
    
    return 0;
}

void vizero_editor_set_status_message_with_timeout(vizero_editor_state_t* state, const char* message, unsigned int timeout_ms) {
    if (!state) return;
    if (state->status_message) {
        size_t len = strlen(state->status_message);
        memset(state->status_message, 0, len); /* Defensive: clear memory before free */
        free(state->status_message);
        state->status_message = NULL;
    }
    if (message) {
        state->status_message = strdup(message);
        state->status_message_set_time = SDL_GetTicks();
        state->status_message_timeout_ms = timeout_ms;
    } else {
        state->status_message_set_time = 0;
        state->status_message_timeout_ms = 0;
    }
}

void vizero_editor_set_status_message(vizero_editor_state_t* state, const char* message) {
    vizero_editor_set_status_message_with_timeout(state, message, 0);
}

const char* vizero_editor_get_status_message(vizero_editor_state_t* state) {
    return state ? state->status_message : NULL;
}

void vizero_editor_set_plugin_manager(vizero_editor_state_t* state, vizero_plugin_manager_t* manager) {
    if (state) state->plugin_manager = manager;
}

vizero_plugin_manager_t* vizero_editor_get_plugin_manager(vizero_editor_state_t* state) {
    return state ? state->plugin_manager : NULL;
}

/* Mode Manager */
vizero_mode_manager_t* vizero_editor_get_mode_manager(vizero_editor_state_t* state) {
    return state ? state->mode_manager : NULL;
}

/* Project management functions */
vizero_project_t* vizero_editor_get_current_project(vizero_editor_state_t* state) {
    return state ? state->current_project : NULL;
}

int vizero_editor_open_project(vizero_editor_state_t* state, const char* root_directory) {
    if (!state || !root_directory) return -1;
    
    /* Close existing project */
    if (state->current_project) {
        vizero_project_destroy(state->current_project);
    }
    
    /* Create new project */
    state->current_project = vizero_project_create(root_directory);
    return state->current_project ? 0 : -1;
}

int vizero_editor_close_project(vizero_editor_state_t* state) {
    if (!state) return -1;
    
    if (state->current_project) {
        vizero_project_destroy(state->current_project);
        state->current_project = NULL;
    }
    
    return 0;
}

int vizero_editor_save_project_workspace(vizero_editor_state_t* state, const char* filename) {
    if (!state || !state->current_project || !filename) return -1;
    return vizero_project_save_workspace(state->current_project, filename);
}

/* Command mode functions */
const char* vizero_editor_get_command_buffer(vizero_editor_state_t* state) {
    return state ? state->command_buffer : NULL;
}

void vizero_editor_clear_command_buffer(vizero_editor_state_t* state) {
    if (state) {
        state->command_length = 0;
        state->command_buffer[0] = '\0';
    }
}

int vizero_editor_append_to_command(vizero_editor_state_t* state, char c) {
    if (!state || state->command_length >= MAX_COMMAND_LENGTH - 1) return -1;
    
    state->command_buffer[state->command_length] = c;
    state->command_length++;
    state->command_buffer[state->command_length] = '\0';
    return 0;
}

int vizero_editor_backspace_command(vizero_editor_state_t* state) {
    if (!state || state->command_length == 0) return -1;
    
    state->command_length--;
    state->command_buffer[state->command_length] = '\0';
    return 0;
}

int vizero_editor_execute_current_command(vizero_editor_state_t* state) {
    if (!state) return -1;
    
    int result = vizero_editor_execute_command(state, state->command_buffer);
    vizero_editor_clear_command_buffer(state);
    return result;
}

int vizero_editor_should_quit(vizero_editor_state_t* state) {
    return state ? state->should_quit : 0;
}

void vizero_editor_set_quit_flag(vizero_editor_state_t* state) {
    if (state) {
        state->should_quit = 1;
    }
}

vizero_settings_t* vizero_editor_get_settings(vizero_editor_state_t* state) {
    return state ? state->settings : NULL;
}

/* Helper function to set clipboard content */
static int vizero_editor_set_clipboard(vizero_editor_state_t* state, const char* content) {
    if (!state || !content) return -1;
    
    /* Set system clipboard using SDL2 */
    if (SDL_SetClipboardText(content) != 0) {
        return -1;
    }
    
    /* Also store in internal clipboard as backup */
    if (state->clipboard_content) {
        free(state->clipboard_content);
        state->clipboard_content = NULL;
        state->clipboard_size = 0;
    }
    
    size_t content_len = strlen(content);
    state->clipboard_content = (char*)malloc(content_len + 1);
    if (!state->clipboard_content) return -1;
    
    strcpy(state->clipboard_content, content);
    state->clipboard_size = content_len;
    
    return 0;
}

int vizero_editor_copy_current_line(vizero_editor_state_t* state) {
    if (!state) return -1;
    
    vizero_buffer_t* buffer = vizero_editor_get_current_buffer(state);
    vizero_cursor_t* cursor = vizero_editor_get_current_cursor(state);
    if (!buffer || !cursor) return -1;
    
    vizero_position_t pos = vizero_cursor_get_position(cursor);
    const char* line_text = vizero_buffer_get_line_text(buffer, pos.line);
    if (!line_text) return -1;
    
    /* Create a copy with newline for proper pasting */
    size_t line_len = strlen(line_text);
    char* line_with_newline = (char*)malloc(line_len + 2);
    if (!line_with_newline) return -1;
    
    strcpy(line_with_newline, line_text);
    strcat(line_with_newline, "\n");
    
    int result = vizero_editor_set_clipboard(state, line_with_newline);
    free(line_with_newline);
    
    if (result == 0) {
        vizero_editor_set_status_message(state, "Line copied");
    }
    
    return result;
}

int vizero_editor_copy_selection(vizero_editor_state_t* state) {
    if (!state) return -1;
    
    /* If we have a selection, copy the selected text */
    if (state->has_selection) {
        char* selected_text = vizero_editor_get_selected_text(state);
        if (selected_text) {
            int result = vizero_editor_set_clipboard(state, selected_text);
            free(selected_text);
            if (result == 0) {
                vizero_editor_set_status_message(state, "Selection copied");
            }
            return result;
        }
    }
    
    /* No selection, copy current line */
    return vizero_editor_copy_current_line(state);
}

int vizero_editor_cut_current_line(vizero_editor_state_t* state) {
    if (!state) return -1;
    
    /* First copy the line */
    if (vizero_editor_copy_current_line(state) != 0) return -1;
    
    /* Then delete the line */
    vizero_buffer_t* buffer = vizero_editor_get_current_buffer(state);
    vizero_cursor_t* cursor = vizero_editor_get_current_cursor(state);
    if (!buffer || !cursor) return -1;
    
    vizero_position_t pos = vizero_cursor_get_position(cursor);
    
    /* Delete the entire line */
    if (vizero_buffer_delete_line(buffer, pos.line) == 0) {
        /* Adjust cursor position if we deleted the last line */
        size_t line_count = vizero_buffer_get_line_count(buffer);
        if (pos.line >= line_count && line_count > 0) {
            vizero_cursor_set_position(cursor, line_count - 1, 0);
        } else if (line_count == 0) {
            /* If no lines left, ensure we have at least one empty line */
            vizero_buffer_insert_line(buffer, 0, "");
            vizero_cursor_set_position(cursor, 0, 0);
        }
        
        vizero_editor_set_status_message(state, "Line cut");
        return 0;
    }
    
    return -1;
}

int vizero_editor_cut_selection(vizero_editor_state_t* state) {
    if (!state) return -1;
    
    /* If we have a selection, cut the selected text */
    if (state->has_selection) {
        char* selected_text = vizero_editor_get_selected_text(state);
        if (selected_text) {
            /* Copy to clipboard first */
            int result = vizero_editor_set_clipboard(state, selected_text);
            free(selected_text);
            
            if (result == 0) {
                /* Delete the selected text */
                vizero_buffer_t* buffer = vizero_editor_get_current_buffer(state);
                vizero_cursor_t* cursor = vizero_editor_get_current_cursor(state);
                if (buffer && cursor) {
                    vizero_position_t start = {0, 0}, end = {0, 0};
                    vizero_editor_get_selection_range(state, &start, &end);
                    
                    /* Delete the selection */
                    if (vizero_buffer_delete_range(buffer, start.line, start.column, end.line, end.column) == 0) {
                        /* Move cursor to start of deleted selection */
                        vizero_cursor_set_position(cursor, start.line, start.column);
                        vizero_editor_clear_selection(state);
                        vizero_editor_set_status_message(state, "Selection cut");
                        return 0;
                    }
                }
            }
            return result;
        }
    }
    
    /* No selection, cut current line */
    return vizero_editor_cut_current_line(state);
}

int vizero_editor_paste_at_cursor(vizero_editor_state_t* state) {
    if (!state) return -1;
    
    vizero_buffer_t* buffer = vizero_editor_get_current_buffer(state);
    vizero_cursor_t* cursor = vizero_editor_get_current_cursor(state);
    if (!buffer || !cursor) return -1;
    
    /* Get text from system clipboard first */
    char* clipboard_text = SDL_GetClipboardText();
    const char* content_to_paste = NULL;
    
    if (clipboard_text && strlen(clipboard_text) > 0) {
        content_to_paste = clipboard_text;
    } else if (state->clipboard_content) {
        content_to_paste = state->clipboard_content;
    } else {
        if (clipboard_text) SDL_free(clipboard_text);
        return -1;
    }
    
    vizero_position_t pos = vizero_cursor_get_position(cursor);
    
    /* Check if clipboard content contains newlines (multi-line paste) */
    const char* newline_pos = strchr(content_to_paste, '\n');
    const char* carriage_return_pos = strchr(content_to_paste, '\r');
    
    /* Handle multi-line paste if we find any line ending characters */
    if (newline_pos != NULL || carriage_return_pos != NULL) {
        /* Multi-line paste - normalize line endings and handle line by line */
        char* content_copy = (char*)malloc(strlen(content_to_paste) + 1);
        if (!content_copy) {
            if (clipboard_text) SDL_free(clipboard_text);
            return -1;
        }
        
        /* Normalize line endings: convert \r\n and \r to \n */
        const char* src = content_to_paste;
        char* dst = content_copy;
        while (*src) {
            if (*src == '\r') {
                if (*(src + 1) == '\n') {
                    /* \r\n -> \n */
                    *dst++ = '\n';
                    src += 2;
                } else {
                    /* \r -> \n */
                    *dst++ = '\n';
                    src++;
                }
            } else {
                *dst++ = *src++;
            }
        }
        *dst = '\0';
        
        /* Split the current line at cursor position */
        const char* current_line = vizero_buffer_get_line_text(buffer, pos.line);
        if (current_line) {
            size_t current_line_len = strlen(current_line);
            
            /* Split current line: part before cursor + part after cursor */
            char* line_before = (char*)malloc(pos.column + 1);
            char* line_after = (char*)malloc(current_line_len - pos.column + 1);
            
            if (line_before && line_after) {
                strncpy(line_before, current_line, pos.column);
                line_before[pos.column] = '\0';
                strcpy(line_after, current_line + pos.column);
                
                /* Replace current line with the part before cursor + first paste line */
                char* first_line_end = strchr(content_copy, '\n');
                if (first_line_end) {
                    *first_line_end = '\0';
                    
                    /* Create new first line: line_before + first_paste_line */
                    char* new_first_line = (char*)malloc(strlen(line_before) + strlen(content_copy) + 1);
                    if (new_first_line) {
                        strcpy(new_first_line, line_before);
                        strcat(new_first_line, content_copy);
                        
                        /* Delete the current line */
                        if (vizero_buffer_delete_line(buffer, pos.line) == 0) {
                            /* Insert the new first line */
                            if (vizero_buffer_insert_line(buffer, pos.line, new_first_line) == 0) {
                                /* Insert middle lines */
                                char* line_start = first_line_end + 1;
                                size_t insert_line = pos.line + 1;
                                
                                while (line_start < content_copy + strlen(content_copy)) {
                                    char* line_end = strchr(line_start, '\n');
                                    if (line_end) {
                                        *line_end = '\0';
                                        vizero_buffer_insert_line(buffer, insert_line, line_start);
                                        insert_line++;
                                        line_start = line_end + 1;
                                    } else {
                                        /* Last line - combine with remaining part */
                                        char* final_line = (char*)malloc(strlen(line_start) + strlen(line_after) + 1);
                                        if (final_line) {
                                            strcpy(final_line, line_start);
                                            strcat(final_line, line_after);
                                            vizero_buffer_insert_line(buffer, insert_line, final_line);
                                            free(final_line);
                                            
                                            /* Move cursor to end of pasted content */
                                            vizero_cursor_set_position(cursor, insert_line, strlen(line_start));
                                        }
                                        break;
                                    }
                                }
                            }
                        }
                        free(new_first_line);
                        
                        vizero_editor_set_status_message(state, "Multi-line text pasted");
                        free(line_before);
                        free(line_after);
                        free(content_copy);
                        if (clipboard_text) SDL_free(clipboard_text);
                        return 0;
                    }
                } /* End if (first_line_end) */
            } /* End if (line_before && line_after) */
            
            if (line_before) free(line_before);
            if (line_after) free(line_after);
        } /* End if (current_line) */
        
        free(content_copy);
    } else {
        /* Single-line paste - insert at current position */
        if (vizero_buffer_insert_text(buffer, pos.line, pos.column, content_to_paste) == 0) {
            /* Move cursor to end of pasted text */
            vizero_cursor_set_position(cursor, pos.line, pos.column + strlen(content_to_paste));
            vizero_editor_set_status_message(state, "Text pasted");
            if (clipboard_text) SDL_free(clipboard_text);
            return 0;
        }
    }
    
    if (clipboard_text) SDL_free(clipboard_text);
    return -1;
}

const char* vizero_editor_get_clipboard_content(vizero_editor_state_t* state) {
    return state ? state->clipboard_content : NULL;
}

/* Text selection functions */
int vizero_editor_has_selection(vizero_editor_state_t* state) {
    return state ? state->has_selection : 0;
}

void vizero_editor_start_selection(vizero_editor_state_t* state) {
    if (!state) return;
    
    vizero_cursor_t* cursor = vizero_editor_get_current_cursor(state);
    if (!cursor) return;
    
    vizero_position_t pos = vizero_cursor_get_position(cursor);
    state->has_selection = 1;
    state->selection_start = pos;
    state->selection_end = pos;
}

void vizero_editor_start_selection_at(vizero_editor_state_t* state, size_t line, size_t column) {
    if (!state) return;
    
    state->has_selection = 1;
    state->selection_start.line = line;
    state->selection_start.column = column;
    state->selection_end.line = line;
    state->selection_end.column = column;
}

void vizero_editor_update_selection(vizero_editor_state_t* state) {
    if (!state || !state->has_selection) return;
    
    vizero_cursor_t* cursor = vizero_editor_get_current_cursor(state);
    if (!cursor) return;
    
    state->selection_end = vizero_cursor_get_position(cursor);
}

void vizero_editor_clear_selection(vizero_editor_state_t* state) {
    if (state) {
        state->has_selection = 0;
    }
}

void vizero_editor_get_selection_range(vizero_editor_state_t* state, vizero_position_t* start, vizero_position_t* end) {
    if (!state || !start || !end || !state->has_selection) return;
    
    /* Ensure start is before end */
    if (state->selection_start.line < state->selection_end.line ||
        (state->selection_start.line == state->selection_end.line && 
         state->selection_start.column < state->selection_end.column)) {
        *start = state->selection_start;
        *end = state->selection_end;
    } else {
        *start = state->selection_end;
        *end = state->selection_start;
    }
}

char* vizero_editor_get_selected_text(vizero_editor_state_t* state) {
    if (!state || !state->has_selection) return NULL;
    
    vizero_buffer_t* buffer = vizero_editor_get_current_buffer(state);
    if (!buffer) return NULL;
    
    vizero_position_t start = {0, 0}, end = {0, 0};
    vizero_editor_get_selection_range(state, &start, &end);
    
    /* Handle single line selection */
    if (start.line == end.line) {
        const char* line_text = vizero_buffer_get_line_text(buffer, start.line);
        if (!line_text) return NULL;
        
        size_t line_len = strlen(line_text);
        if (start.column >= line_len) return NULL;
        
        size_t selection_len = (end.column <= line_len) ? (end.column - start.column) : (line_len - start.column);
        char* selected = (char*)malloc(selection_len + 1);
        if (!selected) return NULL;
        
        strncpy(selected, line_text + start.column, selection_len);
        selected[selection_len] = '\0';
        return selected;
    }
    
    /* Handle multi-line selection */
    size_t total_len = 0;
    
    /* Calculate total length needed */
    for (size_t line = start.line; line <= end.line; line++) {
        const char* line_text = vizero_buffer_get_line_text(buffer, line);
        if (!line_text) continue;
        
        if (line == start.line) {
            total_len += strlen(line_text) - start.column + 1; /* +1 for newline */
        } else if (line == end.line) {
            total_len += end.column;
        } else {
            total_len += strlen(line_text) + 1; /* +1 for newline */
        }
    }
    
    char* selected = (char*)malloc(total_len + 1);
    if (!selected) return NULL;
    
    selected[0] = '\0';
    
    /* Copy the selected text */
    for (size_t line = start.line; line <= end.line; line++) {
        const char* line_text = vizero_buffer_get_line_text(buffer, line);
        if (!line_text) continue;
        
        if (line == start.line) {
            strncat(selected, line_text + start.column, strlen(line_text) - start.column);
            if (line != end.line) strcat(selected, "\n");
        } else if (line == end.line) {
            strncat(selected, line_text, end.column);
        } else {
            strcat(selected, line_text);
            strcat(selected, "\n");
        }
    }
    
    return selected;
}

/* Undo/Redo system implementation */

void vizero_editor_push_undo_operation(vizero_editor_state_t* state, vizero_undo_type_t type, 
                                       vizero_position_t position, vizero_position_t end_position, 
                                       const char* text) {
    if (!state || !state->undo_stack) return;
    
    vizero_undo_stack_t* stack = state->undo_stack;
    
    /* If we're at capacity, remove the oldest operation */
    if (stack->count >= stack->capacity) {
        /* Free the text of the oldest operation */
        if (stack->operations[0].text) {
            free(stack->operations[0].text);
        }
        
        /* Shift all operations down */
        for (size_t i = 0; i < stack->count - 1; i++) {
            stack->operations[i] = stack->operations[i + 1];
        }
        stack->count--;
    }
    
    /* Add new operation */
    vizero_undo_operation_t* op = &stack->operations[stack->count];
    op->type = type;
    op->position = position;
    op->end_position = end_position;
    op->text = text ? strdup(text) : NULL;
    op->text_length = text ? strlen(text) : 0;
    
    stack->count++;
    stack->current_index = stack->count;
}

int vizero_editor_undo(vizero_editor_state_t* state) {
    if (!state || !state->undo_stack || state->undo_stack->current_index == 0) {
        vizero_editor_set_status_message(state, "Nothing to undo");
        return -1;
    }
    
    vizero_undo_stack_t* stack = state->undo_stack;
    stack->current_index--;
    
    vizero_undo_operation_t* op = &stack->operations[stack->current_index];
    vizero_buffer_t* buffer = vizero_editor_get_current_buffer(state);
    vizero_cursor_t* cursor = vizero_editor_get_current_cursor(state);
    
    if (!buffer || !cursor) return -1;
    
    /* Apply the reverse operation */
    switch (op->type) {
        case VIZERO_UNDO_INSERT_CHAR:
            /* Undo insert char by deleting */
            vizero_buffer_delete_char(buffer, op->position.line, op->position.column);
            vizero_cursor_set_position(cursor, op->position.line, op->position.column);
            break;
            
        case VIZERO_UNDO_DELETE_CHAR:
            /* Undo delete char by inserting */
            if (op->text && op->text_length > 0) {
                vizero_buffer_insert_char(buffer, op->position.line, op->position.column, op->text[0]);
                vizero_cursor_set_position(cursor, op->position.line, op->position.column + 1);
            }
            break;
            
        case VIZERO_UNDO_INSERT_LINE:
            /* Undo insert line by deleting */
            vizero_buffer_delete_line(buffer, op->position.line);
            vizero_cursor_set_position(cursor, op->position.line, op->position.column);
            break;
            
        case VIZERO_UNDO_DELETE_LINE:
            /* Undo delete line by inserting */
            if (op->text) {
                vizero_buffer_insert_line(buffer, op->position.line, op->text);
                vizero_cursor_set_position(cursor, op->position.line, op->position.column);
            }
            break;
            
        case VIZERO_UNDO_INSERT_TEXT:
            /* Undo insert text by deleting range */
            if (op->text_length > 0) {
                vizero_buffer_delete_range(buffer, op->position.line, op->position.column, 
                                         op->end_position.line, op->end_position.column);
                vizero_cursor_set_position(cursor, op->position.line, op->position.column);
            }
            break;
            
        case VIZERO_UNDO_DELETE_RANGE:
            /* Undo delete range by inserting text */
            if (op->text) {
                vizero_buffer_insert_text(buffer, op->position.line, op->position.column, op->text);
                vizero_cursor_set_position(cursor, op->end_position.line, op->end_position.column);
            }
            break;
    }
    
    vizero_editor_set_status_message(state, "Undo successful");
    return 0;
}

int vizero_editor_redo(vizero_editor_state_t* state) {
    if (!state || !state->undo_stack || state->undo_stack->current_index >= state->undo_stack->count) {
        vizero_editor_set_status_message(state, "Nothing to redo");
        return -1;
    }
    
    vizero_undo_stack_t* stack = state->undo_stack;
    vizero_undo_operation_t* op = &stack->operations[stack->current_index];
    vizero_buffer_t* buffer = vizero_editor_get_current_buffer(state);
    vizero_cursor_t* cursor = vizero_editor_get_current_cursor(state);
    
    if (!buffer || !cursor) return -1;
    
    /* Re-apply the original operation */
    switch (op->type) {
        case VIZERO_UNDO_INSERT_CHAR:
            if (op->text && op->text_length > 0) {
                vizero_buffer_insert_char(buffer, op->position.line, op->position.column, op->text[0]);
                vizero_cursor_set_position(cursor, op->position.line, op->position.column + 1);
            }
            break;
            
        case VIZERO_UNDO_DELETE_CHAR:
            vizero_buffer_delete_char(buffer, op->position.line, op->position.column);
            vizero_cursor_set_position(cursor, op->position.line, op->position.column);
            break;
            
        case VIZERO_UNDO_INSERT_LINE:
            if (op->text) {
                vizero_buffer_insert_line(buffer, op->position.line, op->text);
                vizero_cursor_set_position(cursor, op->position.line + 1, 0);
            }
            break;
            
        case VIZERO_UNDO_DELETE_LINE:
            vizero_buffer_delete_line(buffer, op->position.line);
            vizero_cursor_set_position(cursor, op->position.line, op->position.column);
            break;
            
        case VIZERO_UNDO_INSERT_TEXT:
            if (op->text) {
                vizero_buffer_insert_text(buffer, op->position.line, op->position.column, op->text);
                vizero_cursor_set_position(cursor, op->end_position.line, op->end_position.column);
            }
            break;
            
        case VIZERO_UNDO_DELETE_RANGE:
            vizero_buffer_delete_range(buffer, op->position.line, op->position.column, 
                                     op->end_position.line, op->end_position.column);
            vizero_cursor_set_position(cursor, op->position.line, op->position.column);
            break;
    }
    
    stack->current_index++;
    vizero_editor_set_status_message(state, "Redo successful");
    return 0;
}

/* Navigation functions */
int vizero_editor_go_to_line(vizero_editor_state_t* state, size_t line_number) {
    if (!state) return -1;
    
    vizero_buffer_t* buffer = vizero_editor_get_current_buffer(state);
    vizero_cursor_t* cursor = vizero_editor_get_current_cursor(state);
    
    if (!buffer || !cursor) return -1;
    
    size_t total_lines = vizero_buffer_get_line_count(buffer);
    if (line_number < 1) line_number = 1;
    if (line_number > total_lines) line_number = total_lines;
    
    /* Convert to 0-based line number and go to start of line */
    vizero_cursor_set_position(cursor, line_number - 1, 0);
    
    char msg[64];
    sprintf(msg, "Line %zu", line_number);
    vizero_editor_set_status_message_with_timeout(state, msg, 2000);
    
    return 0;
}

int vizero_editor_go_to_end(vizero_editor_state_t* state) {
    if (!state) return -1;
    
    vizero_buffer_t* buffer = vizero_editor_get_current_buffer(state);
    vizero_cursor_t* cursor = vizero_editor_get_current_cursor(state);
    
    if (!buffer || !cursor) return -1;
    
    size_t total_lines = vizero_buffer_get_line_count(buffer);
    if (total_lines == 0) total_lines = 1;
    
    /* Go to last line, start of line */
    vizero_cursor_set_position(cursor, total_lines - 1, 0);
    
    char msg[64];
    sprintf(msg, "Line %zu", total_lines);
    vizero_editor_set_status_message_with_timeout(state, msg, 2000);
    
    return 0;
}

/* Insert new line commands */
int vizero_editor_open_line_below(vizero_editor_state_t* state) {
    if (!state) return -1;
    
    vizero_buffer_t* buffer = vizero_editor_get_current_buffer(state);
    vizero_cursor_t* cursor = vizero_editor_get_current_cursor(state);
    
    if (!buffer || !cursor) return -1;
    
    size_t current_line = vizero_cursor_get_line(cursor);
    
    /* Insert empty line after current line */
    if (vizero_buffer_insert_line(buffer, current_line + 1, "") == 0) {
        /* Move cursor to the new line */
        vizero_cursor_set_position(cursor, current_line + 1, 0);
        return 0;
    }
    
    return -1;
}

int vizero_editor_open_line_above(vizero_editor_state_t* state) {
    if (!state) return -1;
    
    vizero_buffer_t* buffer = vizero_editor_get_current_buffer(state);
    vizero_cursor_t* cursor = vizero_editor_get_current_cursor(state);
    
    if (!buffer || !cursor) return -1;
    
    size_t current_line = vizero_cursor_get_line(cursor);
    
    /* Insert empty line at current line (pushing current line down) */
    if (vizero_buffer_insert_line(buffer, current_line, "") == 0) {
        /* Move cursor to the new line (which is at current_line position) */
        vizero_cursor_set_position(cursor, current_line, 0);
        return 0;
    }
    
    return -1;
}

/* Help system functions */
int vizero_editor_enter_help_mode(vizero_editor_state_t* state) {
    if (!state) return -1;
    
    /* If already in help mode, don't enter again */
    if (state->help_mode_active) {
        return 0;
    }
    
    /* Check if we have space for another buffer */
    if (state->buffer_count >= MAX_BUFFERS) {
        vizero_editor_set_status_message(state, "Error: Cannot open help - too many buffers");
        return -1;
    }
    
    /* Store the current buffer index for restoration */
    state->help_original_buffer_index = state->current_buffer_index;
    
    /* Create help buffer and load manual.md */
    vizero_buffer_t* help_buffer = vizero_buffer_create_from_file("manual.md");
    if (!help_buffer) {
        vizero_editor_set_status_message(state, "Error: Could not load manual.md");
        return -1;
    }
    
    /* Ensure the filename is set for syntax highlighting */
    if (vizero_buffer_set_filename(help_buffer, "manual.md") != 0) {
        vizero_editor_set_status_message(state, "Warning: Could not set filename for help buffer");
    }
    
    /* Make help buffer read-only */
    vizero_buffer_set_readonly(help_buffer, 1);
    
    /* Create cursor for help buffer */
    vizero_cursor_t* help_cursor = vizero_cursor_create(help_buffer);
    if (!help_cursor) {
        vizero_buffer_destroy(help_buffer);
        vizero_editor_set_status_message(state, "Error: Could not create help cursor");
        return -1;
    }
    
    /* Add help buffer to the buffer array at the end */
    size_t help_buffer_index = state->buffer_count;
    state->buffers[help_buffer_index] = help_buffer;
    state->cursors[help_buffer_index] = help_cursor;
    state->buffer_count++;
    
    /* Switch to the help buffer */
    state->current_buffer_index = help_buffer_index;
    
    /* Notify plugins of buffer open (for syntax highlighting, etc.) */
    if (state->plugin_manager) {
        vizero_plugin_manager_on_buffer_open(state->plugin_manager, help_buffer, "manual.md");
        
        /* Force syntax highlighting update for the help buffer */
        vizero_syntax_token_t tokens[256];
        size_t token_count = 0;
        size_t line_count = vizero_buffer_get_line_count(help_buffer);
        if (line_count > 0) {
            vizero_plugin_manager_highlight_syntax(state->plugin_manager, help_buffer, 0, 
                                                 (line_count > 10) ? 10 : line_count, 
                                                 tokens, 256, &token_count);
        }
    }
    
    /* Update focused window to use the help buffer */
    vizero_editor_window_t* window = vizero_window_manager_get_focused_window(state->window_manager);
    if (window) {
        vizero_editor_window_set_buffer_index(window, help_buffer_index);
    }
    
    /* Mark help mode as active */
    state->help_mode_active = 1;
    
    vizero_editor_set_status_message(state, "Help loaded - Close buffer (:bd or :q) to return to previous buffer");
    return 0;
}

int vizero_editor_exit_help_mode(vizero_editor_state_t* state) {
    if (!state || !state->help_mode_active) return -1;
    
    /* Get the help buffer index (should be the current one) */
    size_t help_buffer_index = state->current_buffer_index;
    
    /* Clean up help buffer and cursor */
    if (state->buffers[help_buffer_index]) {
        vizero_buffer_destroy(state->buffers[help_buffer_index]);
        state->buffers[help_buffer_index] = NULL;
    }
    if (state->cursors[help_buffer_index]) {
        vizero_cursor_destroy(state->cursors[help_buffer_index]);
        state->cursors[help_buffer_index] = NULL;
    }
    
    /* Remove the help buffer from the array by shifting remaining buffers */
    for (size_t i = help_buffer_index; i < state->buffer_count - 1; i++) {
        state->buffers[i] = state->buffers[i + 1];
        state->cursors[i] = state->cursors[i + 1];
    }
    state->buffer_count--;
    
    /* Restore the original buffer index, adjusting if necessary */
    if (state->help_original_buffer_index >= help_buffer_index) {
        /* Original buffer was after help buffer, so its index shifted down */
        if (state->help_original_buffer_index > 0) {
            state->current_buffer_index = state->help_original_buffer_index - 1;
        } else {
            state->current_buffer_index = 0;
        }
    } else {
        /* Original buffer was before help buffer, index unchanged */
        state->current_buffer_index = state->help_original_buffer_index;
    }
    
    /* Ensure we don't go out of bounds */
    if (state->current_buffer_index >= state->buffer_count && state->buffer_count > 0) {
        state->current_buffer_index = state->buffer_count - 1;
    }
    
    /* Update focused window to use the restored buffer */
    vizero_editor_window_t* window = vizero_window_manager_get_focused_window(state->window_manager);
    if (window) {
        vizero_editor_window_set_buffer_index(window, state->current_buffer_index);
    }
    
    /* Clear help mode */
    state->help_mode_active = 0;
    state->help_original_buffer_index = 0;
    
    vizero_editor_set_status_message(state, "Returned to previous buffer");
    return 0;
}

int vizero_editor_is_help_mode_active(vizero_editor_state_t* state) {
    return state ? state->help_mode_active : 0;
}