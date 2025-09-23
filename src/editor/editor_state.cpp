/* Enhanced multi-buffer implementation */
#include "vizero/editor_state.h"
#include "vizero/buffer.h"
#include "vizero/cursor.h"
#include "vizero/project.h"
#include "vizero/settings.h"
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
#endif

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
    
    /* Compilation results */
    char* last_compile_output;
    
    /* Popup system */
    int popup_visible;
    char* popup_content;
    uint32_t popup_start_time;
    uint32_t popup_duration_ms;
    
    /* Settings */
    vizero_settings_t* settings;
    
    /* Text selection */
    int has_selection;
    vizero_position_t selection_start;
    vizero_position_t selection_end;
    
    /* Clipboard */
    char* clipboard_content;
    size_t clipboard_size;
    
    /* Undo system */
    vizero_undo_stack_t* undo_stack;
    
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

/* Popup system functions */
void vizero_editor_show_popup(vizero_editor_state_t* state, const char* content, uint32_t duration_ms) {
    if (!state || !content) return;
    
    /* Clean up existing popup */
    if (state->popup_content) {
        free(state->popup_content);
    }
    
    /* Set new popup */
    state->popup_content = _strdup(content);
    state->popup_visible = 1;
    state->popup_start_time = SDL_GetTicks();
    state->popup_duration_ms = duration_ms;
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
    uint32_t current_time = SDL_GetTicks();
    if (current_time - state->popup_start_time >= state->popup_duration_ms) {
        vizero_editor_hide_popup(state);
        return 0;
    }
    
    return 1;
}

const char* vizero_editor_get_popup_content(vizero_editor_state_t* state) {
    if (!state || !vizero_editor_is_popup_visible(state)) return NULL;
    return state->popup_content;
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
    
    state->last_compile_output = _strdup(full_result);
    
    /* Show popup with compilation result */
    vizero_editor_show_popup(state, full_result, 5000); /* 5 seconds */
    
    /* Also print to console for debugging */
    printf("\n%s\n", full_result);
    
    return result;
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
        
    } else if (strncmp(command, "linenum ", 8) == 0) {
        /* Line number display setting */
        const char* value = command + 8;
        if (strcmp(value, "on") == 0) {
            vizero_settings_set_bool(state->settings, VIZERO_SETTING_SHOW_LINE_NUMBERS, true);
            vizero_editor_set_status_message(state, "Line numbers enabled");
        } else if (strcmp(value, "off") == 0) {
            vizero_settings_set_bool(state->settings, VIZERO_SETTING_SHOW_LINE_NUMBERS, false);
            vizero_editor_set_status_message(state, "Line numbers disabled");
        } else {
            char msg[256];
            sprintf(msg, "Invalid linenum value: %s (use 'on' or 'off')", value);
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
                    vizero_position_t start, end;
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
    size_t content_len = strlen(content_to_paste);
    const char* newline_pos = strchr(content_to_paste, '\n');
    const char* carriage_return_pos = strchr(content_to_paste, '\r');
    
    /* Handle multi-line paste if we find any line ending characters */
    if (newline_pos != NULL || carriage_return_pos != NULL) {
        /* Multi-line paste - normalize line endings and handle line by line */
        char* content_copy = (char*)malloc(content_len + 1);
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
                                
                                while (line_start < content_copy + content_len) {
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
            vizero_cursor_set_position(cursor, pos.line, pos.column + content_len);
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
    
    vizero_position_t start, end;
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
    op->text = text ? _strdup(text) : NULL;
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