/* Enhanced multi-buffer implementation */
#include "vizero/editor_state.h"
#include "vizero/buffer.h"
#include "vizero/cursor.h"
#include "vizero/project.h"
#include <stdlib.h>
#include <string.h>
#include <stdio.h>

#define MAX_BUFFERS 128
#define MAX_COMMAND_LENGTH 256

struct vizero_editor_state_t {
    vizero_editor_mode_t mode;
    
    /* Buffer management */
    vizero_buffer_t* buffers[MAX_BUFFERS];
    vizero_cursor_t* cursors[MAX_BUFFERS];
    size_t buffer_count;
    size_t current_buffer_index;
    
    /* Project management */
    vizero_project_t* current_project;
    
    /* Command mode */
    char command_buffer[MAX_COMMAND_LENGTH];
    size_t command_length;
    
    /* Plugin integration */
    vizero_plugin_manager_t* plugin_manager;
    char* status_message;
    
    /* Application control */
    int should_quit;
};

vizero_editor_state_t* vizero_editor_state_create(void) {
    vizero_editor_state_t* state = (vizero_editor_state_t*)calloc(1, sizeof(vizero_editor_state_t));
    if (state) {
        state->mode = VIZERO_MODE_NORMAL;
        state->buffer_count = 0;
        state->current_buffer_index = 0;
        state->current_project = NULL;
        state->command_length = 0;
        state->command_buffer[0] = '\0';
        state->should_quit = 0;
        
        /* Create initial empty buffer */
        state->buffers[0] = vizero_buffer_create();
        state->cursors[0] = vizero_cursor_create(state->buffers[0]);
        if (state->buffers[0] && state->cursors[0]) {
            state->buffer_count = 1;
        }
    }
    return state;
}

void vizero_editor_state_destroy(vizero_editor_state_t* state) {
    if (!state) return;
    
    /* Clean up all buffers and cursors */
    for (size_t i = 0; i < state->buffer_count; i++) {
        if (state->cursors[i]) vizero_cursor_destroy(state->cursors[i]);
        if (state->buffers[i]) vizero_buffer_destroy(state->buffers[i]);
    }
    
    if (state->current_project) vizero_project_destroy(state->current_project);
    if (state->status_message) free(state->status_message);
    free(state);
}

vizero_editor_mode_t vizero_editor_get_mode(vizero_editor_state_t* state) {
    return state ? state->mode : VIZERO_MODE_NORMAL;
}

void vizero_editor_set_mode(vizero_editor_state_t* state, vizero_editor_mode_t mode) {
    if (state) state->mode = mode;
}

vizero_buffer_t* vizero_editor_get_current_buffer(vizero_editor_state_t* state) {
    if (!state || state->current_buffer_index >= state->buffer_count) return NULL;
    return state->buffers[state->current_buffer_index];
}

vizero_cursor_t* vizero_editor_get_current_cursor(vizero_editor_state_t* state) {
    if (!state || state->current_buffer_index >= state->buffer_count) return NULL;
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
    
    /* Don't close the last buffer */
    if (state->buffer_count == 1) return -1;
    
    /* Clean up buffer and cursor */
    vizero_cursor_destroy(state->cursors[buffer_index]);
    vizero_buffer_destroy(state->buffers[buffer_index]);
    
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
    state->current_buffer_index = buffer_index;
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

int vizero_editor_execute_command(vizero_editor_state_t* state, const char* command) {
    if (!state || !command) return -1;
    
    /* Skip leading colon if present */
    if (command[0] == ':') command++;
    
    /* Parse and execute commands */
    if (strcmp(command, "q") == 0 || strcmp(command, "quit") == 0) {
        /* Quit command - check for unsaved changes */
        if (state->current_project && vizero_project_has_unsaved_changes(state->current_project)) {
            vizero_editor_set_status_message(state, "No write since last change (add ! to override)");
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
            if (vizero_editor_open_buffer(state, filename) == 0) {
                char msg[512];
                sprintf(msg, "Opened: %s", filename);
                vizero_editor_set_status_message(state, msg);
                return 0;
            } else {
                vizero_editor_set_status_message(state, "Error opening file");
                return -1;
            }
        }
        
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
        
    } else {
        /* Unknown command */
        char msg[256];
        sprintf(msg, "Not an editor command: %s", command);
        vizero_editor_set_status_message(state, msg);
        return -1;
    }
    
    return 0;
}

void vizero_editor_set_status_message(vizero_editor_state_t* state, const char* message) {
    if (!state) return;
    if (state->status_message) free(state->status_message);
    state->status_message = message ? _strdup(message) : NULL;
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