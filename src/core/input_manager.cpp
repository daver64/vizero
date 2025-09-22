/* Stub implementations for basic compilation */
#include "vizero/input_manager.h"
#include "vizero/application.h"
#include "vizero/editor_state.h"
#include "vizero/cursor.h"
#include "vizero/buffer.h"
#include <SDL.h>
#include <stdlib.h>
#include <stdio.h>
#include <string.h>

struct vizero_input_manager_t { 
    vizero_application_t* app;
    int mode_changed_this_frame;
};

vizero_input_manager_t* vizero_input_manager_create(vizero_application_t* app) {
    vizero_input_manager_t* input = (vizero_input_manager_t*)calloc(1, sizeof(vizero_input_manager_t));
    if (input) {
        input->app = app;
        input->mode_changed_this_frame = 0;
    }
    return input;
}

void vizero_input_manager_destroy(vizero_input_manager_t* input) {
    free(input);
}

void vizero_input_manager_process_events(vizero_input_manager_t* input) {
    if (!input) return;
    
    /* Reset mode change flag at start of frame */
    input->mode_changed_this_frame = 0;
    
    SDL_Event event;
    while (SDL_PollEvent(&event)) {
        switch (event.type) {
            case SDL_QUIT:
                /* Signal application to quit */
                if (input->app) {
                    vizero_application_quit(input->app);
                }
                break;
            case SDL_KEYDOWN:
                /* Handle key press */
                if (input->app) {
                    /* Get editor state for key handling */
                    vizero_editor_state_t* editor = vizero_application_get_editor(input->app);
                    if (editor && vizero_editor_get_mode(editor) == VIZERO_MODE_NORMAL) {
                        vizero_cursor_t* cursor = vizero_editor_get_current_cursor(editor);
                        if (cursor) {
                            switch (event.key.keysym.sym) {
                                case SDLK_h:
                                case SDLK_LEFT:
                                    vizero_cursor_move_left(cursor);
                                    break;
                                case SDLK_j:
                                case SDLK_DOWN:
                                    vizero_cursor_move_down(cursor);
                                    break;
                                case SDLK_k:
                                case SDLK_UP:
                                    vizero_cursor_move_up(cursor);
                                    break;
                                case SDLK_l:
                                case SDLK_RIGHT:
                                    vizero_cursor_move_right(cursor);
                                    break;
                                case SDLK_i:
                                    /* Enter insert mode */
                                    vizero_editor_set_mode(editor, VIZERO_MODE_INSERT);
                                    input->mode_changed_this_frame = 1;
                                    break;
                                case SDLK_SEMICOLON:
                                    /* Check if shift is held for colon */
                                    if (event.key.keysym.mod & KMOD_SHIFT) {
                                        /* Enter command mode */
                                        vizero_editor_set_mode(editor, VIZERO_MODE_COMMAND);
                                        vizero_editor_clear_command_buffer(editor);
                                        vizero_editor_append_to_command(editor, ':');
                                        input->mode_changed_this_frame = 1;
                                    }
                                    break;
                                case SDLK_ESCAPE:
                                    /* ESC in normal mode does nothing (vi behavior) */
                                    break;
                                default:
                                    break;
                            }
                        }
                    } else if (editor && vizero_editor_get_mode(editor) == VIZERO_MODE_INSERT) {
                        /* In insert mode, handle special keys */
                        vizero_buffer_t* buffer = vizero_editor_get_current_buffer(editor);
                        vizero_cursor_t* cursor = vizero_editor_get_current_cursor(editor);
                        
                        if (event.key.keysym.sym == SDLK_ESCAPE) {
                            /* ESC returns to normal mode */
                            vizero_editor_set_mode(editor, VIZERO_MODE_NORMAL);
                            input->mode_changed_this_frame = 1;
                        } else if (buffer && cursor) {
                            size_t line = vizero_cursor_get_line(cursor);
                            size_t col = vizero_cursor_get_column(cursor);
                            
                            switch (event.key.keysym.sym) {
                                case SDLK_RETURN:
                                    /* Enter key - split line */
                                    if (vizero_buffer_split_line(buffer, line, col) == 0) {
                                        vizero_cursor_move_down(cursor);
                                        vizero_cursor_set_position(cursor, line + 1, 0);
                                    }
                                    break;
                                case SDLK_BACKSPACE:
                                    /* Backspace - delete character before cursor */
                                    if (col > 0) {
                                        if (vizero_buffer_delete_char(buffer, line, col - 1) == 0) {
                                            vizero_cursor_move_left(cursor);
                                        }
                                    } else if (line > 0) {
                                        /* At start of line, join with previous line */
                                        size_t prev_line_len = vizero_buffer_get_line_length(buffer, line - 1);
                                        if (vizero_buffer_join_lines(buffer, line - 1) == 0) {
                                            vizero_cursor_set_position(cursor, line - 1, prev_line_len);
                                        }
                                    }
                                    break;
                                case SDLK_DELETE:
                                    /* Delete key - delete character at cursor */
                                    vizero_buffer_delete_char(buffer, line, col);
                                    break;
                                case SDLK_TAB:
                                    /* Tab - insert spaces or tab character */
                                    if (vizero_buffer_insert_char(buffer, line, col, '\t') == 0) {
                                        vizero_cursor_move_right(cursor);
                                    }
                                    break;
                                default:
                                    /* Arrow keys work in insert mode too */
                                    switch (event.key.keysym.sym) {
                                        case SDLK_LEFT:
                                            vizero_cursor_move_left(cursor);
                                            break;
                                        case SDLK_RIGHT:
                                            vizero_cursor_move_right(cursor);
                                            break;
                                        case SDLK_UP:
                                            vizero_cursor_move_up(cursor);
                                            break;
                                        case SDLK_DOWN:
                                            vizero_cursor_move_down(cursor);
                                            break;
                                        default:
                                            break;
                                    }
                                    break;
                            }
                        }
                    } else if (editor && vizero_editor_get_mode(editor) == VIZERO_MODE_COMMAND) {
                        /* In command mode, handle command input */
                        if (event.key.keysym.sym == SDLK_ESCAPE) {
                            /* ESC cancels command mode */
                            vizero_editor_set_mode(editor, VIZERO_MODE_NORMAL);
                            input->mode_changed_this_frame = 1;
                            vizero_editor_clear_command_buffer(editor);
                            vizero_editor_set_status_message(editor, "");
                        } else if (event.key.keysym.sym == SDLK_RETURN) {
                            /* Enter executes command */
                            vizero_editor_execute_current_command(editor);
                            vizero_editor_set_mode(editor, VIZERO_MODE_NORMAL);
                            input->mode_changed_this_frame = 1;
                        } else if (event.key.keysym.sym == SDLK_BACKSPACE) {
                            /* Backspace removes last character */
                            if (vizero_editor_backspace_command(editor) == 0) {
                                /* Update status message */
                                const char* cmd = vizero_editor_get_command_buffer(editor);
                                char msg[512];
                                sprintf(msg, ":%s", cmd ? cmd : "");
                                vizero_editor_set_status_message(editor, msg);
                            } else {
                                /* If command buffer is empty, cancel command mode */
                                vizero_editor_set_mode(editor, VIZERO_MODE_NORMAL);
                                vizero_editor_set_status_message(editor, "");
                            }
                        }
                    }
                }
                break;
            case SDL_TEXTINPUT:
                /* Handle text input */
                if (input->app) {
                    vizero_editor_state_t* editor = vizero_application_get_editor(input->app);
                    if (editor && vizero_editor_get_mode(editor) == VIZERO_MODE_INSERT) {
                        /* Skip mode-switching characters that just switched modes */
                        const char* text = event.text.text;
                        if (input->mode_changed_this_frame && strlen(text) == 1 && 
                            (text[0] == 'i' || text[0] == ':')) {
                            break;
                        }
                        
                        vizero_buffer_t* buffer = vizero_editor_get_current_buffer(editor);
                        vizero_cursor_t* cursor = vizero_editor_get_current_cursor(editor);
                        if (buffer && cursor) {
                            /* Insert each character from the text input */
                            for (const char* c = text; *c; c++) {
                                size_t line = vizero_cursor_get_line(cursor);
                                size_t col = vizero_cursor_get_column(cursor);
                                
                                if (vizero_buffer_insert_char(buffer, line, col, *c) == 0) {
                                    /* Move cursor forward after successful insertion */
                                    vizero_cursor_move_right(cursor);
                                }
                            }
                        }
                    } else if (editor && vizero_editor_get_mode(editor) == VIZERO_MODE_COMMAND) {
                        /* In command mode, append text to command buffer */
                        const char* text = event.text.text;
                        
                        /* Skip the : character if we just entered command mode */
                        if (input->mode_changed_this_frame && strlen(text) == 1 && text[0] == ':') {
                            break;
                        }
                        
                        for (const char* c = text; *c; c++) {
                            vizero_editor_append_to_command(editor, *c);
                        }
                    }
                }
                break;
            default:
                break;
        }
    }
}

int vizero_input_manager_is_key_pressed(vizero_input_manager_t* input, uint32_t key) {
    (void)input; (void)key;
    return 0;
}

int vizero_input_manager_was_key_pressed(vizero_input_manager_t* input, uint32_t key) {
    (void)input; (void)key;
    return 0;
}

uint32_t vizero_input_manager_get_modifiers(vizero_input_manager_t* input) {
    (void)input;
    return 0;
}

void vizero_input_manager_get_mouse_position(vizero_input_manager_t* input, int* x, int* y) {
    (void)input;
    if (x) *x = 0;
    if (y) *y = 0;
}

int vizero_input_manager_is_mouse_button_pressed(vizero_input_manager_t* input, int button) {
    (void)input; (void)button;
    return 0;
}