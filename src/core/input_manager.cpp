/* Stub implementations for basic compilation */
#include "vizero/input_manager.h"
#include "vizero/application.h"
#include "vizero/editor_state.h"
#include "vizero/cursor.h"
#include "vizero/buffer.h"
#include "vizero/window.h"
#include "vizero/search.h"
#include "vizero/editor_window.h" // for window focus helpers
#include "vizero/mode_manager.h"
#include "vizero/plugin_manager.h"
// Forward declare helpers if not in header
int vizero_window_manager_focus_direction(vizero_window_manager_t* manager, char dir);
int vizero_window_manager_focus_number(vizero_window_manager_t* manager, int number);
#include <SDL.h>
#include <stdlib.h>
#include <stdio.h>
#include <string.h>


struct vizero_input_manager_t {
    vizero_application_t* app;
    int mode_changed_this_frame;
    int awaiting_window_cmd; // 1 if Ctrl+w was pressed
    int suppress_next_textinput; // 1 if next SDL_TEXTINPUT should be ignored (e.g., after Ctrl+Space)
    int pending_completion_frames; // Countdown for checking async completion results
    vizero_position_t pending_completion_position; // Position where completion was requested
};

vizero_input_manager_t* vizero_input_manager_create(vizero_application_t* app) {
    vizero_input_manager_t* input = (vizero_input_manager_t*)calloc(1, sizeof(vizero_input_manager_t));
    if (input) {
        input->app = app;
        input->mode_changed_this_frame = 0;
        input->awaiting_window_cmd = 0;
        input->suppress_next_textinput = 0;
        input->pending_completion_frames = 0;
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
    
    /* Async completion checking disabled - caused infinite loops */
    
    /* CAUTIOUS: Only process LSP messages and check for results if we're actually using completion */
    static int frames_since_startup = 0;
    frames_since_startup++;
    
    /* Skip LSP processing for the first 60 frames to let things stabilize */
    if (frames_since_startup > 60) {
        vizero_plugin_manager_t* plugin_manager = vizero_application_get_plugin_manager(input->app);
        if (plugin_manager) {
            /* Only check for completion results, don't force LSP processing every frame */
            vizero_completion_list_t* completion_result = NULL;
            int completion_check_result = vizero_plugin_manager_check_completion_results(plugin_manager, &completion_result);
            
            if (completion_check_result == 0 && completion_result) {
                printf("[DEBUG] Async completion result arrived with %zu items\n", 
                       completion_result->item_count);
                
                /* Show completion UI if we have results */
                if (completion_result->item_count > 0) {
                    vizero_editor_state_t* editor = vizero_application_get_editor(input->app);
                    if (editor) {
                        /* Get current cursor position as trigger position */
                        vizero_cursor_t* cursor = vizero_editor_get_current_cursor(editor);
                        vizero_position_t trigger_pos = {0, 0};
                        if (cursor) {
                            trigger_pos.line = vizero_cursor_get_line(cursor);
                            trigger_pos.column = vizero_cursor_get_column(cursor);
                        }
                        
                        /* Show completion dropdown */
                        vizero_editor_show_completion(editor, completion_result->items, 
                                                    completion_result->item_count, trigger_pos);
                        printf("[DEBUG] Completion UI shown with %zu items\n", completion_result->item_count);
                    }
                }
                
                /* Clean up completion result (editor copied the data it needs) */
                if (completion_result->items) {
                    for (size_t i = 0; i < completion_result->item_count; i++) {
                        free(completion_result->items[i].label);
                        free(completion_result->items[i].detail);
                        free(completion_result->items[i].documentation);  
                        free(completion_result->items[i].insert_text);
                        free(completion_result->items[i].filter_text);
                        free(completion_result->items[i].sort_text);
                    }
                    free(completion_result->items);
                }
                free(completion_result);
            }
        }
    }
    
    SDL_Event event;
    while (SDL_PollEvent(&event)) {
        switch (event.type) {
            case SDL_QUIT:
                /* Signal application to quit */
                if (input->app) {
                    vizero_application_quit(input->app);
                }
                break;
            case SDL_WINDOWEVENT:
                /* Handle window events */
                if (input->app && event.window.event == SDL_WINDOWEVENT_RESIZED) {
                    vizero_application_on_window_resize(input->app, event.window.data1, event.window.data2);
                }
                break;
            case SDL_KEYDOWN:
                /* Handle key press */
                if (input->app) {
                    /* Get editor state for key handling */
                    vizero_editor_state_t* editor = vizero_application_get_editor(input->app);
                    if (editor) {
                        /* Hide welcome message on any key press */
                        vizero_application_on_user_input(input->app);
                        
                        /* Check for completion UI key handling first */
                        if (vizero_editor_is_completion_visible(editor)) {
                            if (vizero_editor_handle_completion_key(editor, event.key.keysym.sym)) {
                                break; /* Completion UI consumed the key */
                            }
                        }
                        
                        /* Check for Ctrl+Space to trigger LSP completion (must be early to avoid being consumed) */
                        if ((event.key.keysym.mod & KMOD_CTRL) && (event.key.keysym.sym == SDLK_SPACE)) {
                            vizero_editor_mode_t current_mode = vizero_editor_get_mode(editor);
                            if (current_mode == VIZERO_MODE_INSERT) {
                                vizero_cursor_t* cursor = vizero_editor_get_current_cursor(editor);
                                if (cursor) {
                                    size_t cursor_line = vizero_cursor_get_line(cursor);
                                    size_t cursor_col = vizero_cursor_get_column(cursor);
                                    
                                    /* Find the start of the current word for proper replacement */
                                    vizero_buffer_t* buffer = vizero_editor_get_current_buffer(editor);
                                    size_t word_start_col = cursor_col;
                                    if (buffer && cursor_col > 0) {
                                        const char* line_text = vizero_buffer_get_line_text(buffer, cursor_line);
                                        if (line_text) {
                                            size_t line_len = strlen(line_text);
                                            /* Look backwards to find start of identifier */
                                            for (size_t i = cursor_col; i > 0; i--) {
                                                if (i - 1 >= line_len) {
                                                    /* Safety check: don't go beyond line length */
                                                    word_start_col = cursor_col;
                                                    break;
                                                }
                                                char ch = line_text[i - 1];
                                                if (!((ch >= 'a' && ch <= 'z') || (ch >= 'A' && ch <= 'Z') || 
                                                      (ch >= '0' && ch <= '9') || ch == '_')) {
                                                    word_start_col = i;
                                                    break;
                                                }
                                                word_start_col = i - 1;
                                            }
                                        }
                                    }
                                    
                                    vizero_position_t position = { cursor_line, cursor_col };
                                    vizero_position_t trigger_position = { cursor_line, word_start_col };
                                    
                                    printf("[DEBUG] Word boundaries: cursor=%zu, word_start=%zu\n", cursor_col, word_start_col);
                                    
                                    printf("[DEBUG] Triggering LSP completion at line %zu, column %zu\n", 
                                           position.line, position.column);
                                    
                                    vizero_plugin_manager_t* plugin_manager = vizero_application_get_plugin_manager(input->app);
                                    if (plugin_manager) {
                                        printf("[DEBUG] Plugin manager found, requesting LSP completion\n");
                                        
                                        vizero_completion_list_t* completion_list = NULL;
                                        printf("[DEBUG] Calling vizero_plugin_manager_lsp_completion...\n");
                                        
                                        int result = vizero_plugin_manager_lsp_completion(plugin_manager, 
                                                    vizero_editor_get_current_buffer(editor), position, &completion_list);
                                        
                                        printf("[DEBUG] LSP completion returned result=%d, completion_list=%p\n", 
                                               result, (void*)completion_list);
                                        
                                        if (result == 0 && completion_list && completion_list->item_count > 0) {
                                            printf("[DEBUG] Got completion with %zu items, showing UI\n", completion_list->item_count);
                                            
                                            /* Show completion UI immediately */
                                            vizero_editor_show_completion(editor, completion_list->items, 
                                                                        completion_list->item_count, trigger_position);
                                            
                                            /* Clean up completion list (editor copied the data) */
                                            printf("[DEBUG] Cleaning up completion list with %zu items\n", completion_list->item_count);
                                            if (completion_list->items) {
                                                for (size_t i = 0; i < completion_list->item_count; i++) {
                                                    printf("[DEBUG] Cleaning up completion item %zu\n", i);
                                                    if (completion_list->items[i].label) {
                                                        free(completion_list->items[i].label);
                                                    }
                                                    if (completion_list->items[i].detail) {
                                                        free(completion_list->items[i].detail);
                                                    }
                                                    if (completion_list->items[i].documentation) {
                                                        free(completion_list->items[i].documentation);
                                                    }
                                                    if (completion_list->items[i].insert_text) {
                                                        free(completion_list->items[i].insert_text);
                                                    }
                                                    if (completion_list->items[i].filter_text) {
                                                        free(completion_list->items[i].filter_text);
                                                    }
                                                    if (completion_list->items[i].sort_text) {
                                                        free(completion_list->items[i].sort_text);
                                                    }
                                                }
                                                printf("[DEBUG] Freeing completion items array\n");
                                                free(completion_list->items);
                                            }
                                            printf("[DEBUG] Freeing completion list\n");
                                            free(completion_list);
                                        } else {
                                            printf("[DEBUG] No immediate LSP completion results, result=%d\n", result);
                                            if (completion_list) {
                                                printf("[DEBUG] Freeing empty completion list\n");
                                                free(completion_list);
                                            }
                                        }
                                    }
                                }
                            } else {
                                printf("[DEBUG] LSP completion only available in insert mode (current: %d)\n", current_mode);
                            }
                            input->suppress_next_textinput = 1; /* Prevent space insertion from SDL_TEXTINPUT */
                            goto keydown_handled; /* Always consume Ctrl+Space and skip all other processing */
                        }
                        
                        /* Check for ESC key to dismiss popup */
                        if (event.key.keysym.sym == SDLK_ESCAPE && vizero_editor_is_popup_visible(editor)) {
                            vizero_editor_hide_popup(editor);
                            break; /* Consume the ESC key */
                        }
                        
                        /* Handle ESC key for mode manager when no popup is visible */
                        if (event.key.keysym.sym == SDLK_ESCAPE) {
                            vizero_mode_manager_t* mode_manager = vizero_editor_get_mode_manager(editor);
                            if (mode_manager) {
                                vizero_mode_manager_handle_key(mode_manager, 27, 0); /* 27 is ASCII for ESC */
                            }
                            break; /* Consume the ESC key */
                        }
                        
                        /* Check for popup interactions */
                        if (vizero_editor_is_popup_visible(editor)) {
                            /* Try buffer selector keys first */
                            if (vizero_editor_handle_buffer_selector_key(editor, event.key.keysym.sym)) {
                                break; /* Buffer selector consumed the key */
                            }
                            
                            /* Fall back to regular popup scrolling */
                            if (event.key.keysym.sym == SDLK_UP) {
                                vizero_editor_scroll_popup(editor, -1);
                                break; /* Consume the key */
                            } else if (event.key.keysym.sym == SDLK_DOWN) {
                                vizero_editor_scroll_popup(editor, 1);
                                break; /* Consume the key */
                            } else if (event.key.keysym.sym == SDLK_RETURN || event.key.keysym.sym == SDLK_KP_ENTER) {
                                /* For non-buffer-selector popups, Enter just closes them */
                                vizero_editor_hide_popup(editor);
                                break; /* Consume the key */
                            }
                        }
                        
                        /* Check for F11 key to toggle fullscreen */
                        if (event.key.keysym.sym == SDLK_F11) {
                            vizero_window_t* window = vizero_application_get_window(input->app);
                            if (window) {
                                vizero_window_toggle_fullscreen(window);
                            }
                            break; /* Consume the F11 key */
                        }
                        
                        if (vizero_editor_get_mode(editor) == VIZERO_MODE_NORMAL) {
                            // Use the focused window's cursor in windowed mode
                            vizero_window_manager_t* wm = vizero_editor_get_window_manager(editor);
                            vizero_editor_window_t* focused_window = wm ? vizero_window_manager_get_focused_window(wm) : NULL;
                            vizero_cursor_t* cursor = focused_window ? vizero_editor_window_get_cursor(focused_window, editor) : vizero_editor_get_current_cursor(editor);
                            vizero_buffer_t* buffer = vizero_editor_get_current_buffer(editor);
                            // --- Ctrl+w window switching logic ---
                            if (input->awaiting_window_cmd) {
                                // Awaiting direction/number after Ctrl+w
                                int handled = 0;
                                if (event.key.keysym.sym == SDLK_h || event.key.keysym.sym == SDLK_LEFT) {
                                    if (wm) vizero_window_manager_focus_direction(wm, 'h');
                                    handled = 1;
                                } else if (event.key.keysym.sym == SDLK_j || event.key.keysym.sym == SDLK_DOWN) {
                                    if (wm) vizero_window_manager_focus_direction(wm, 'j');
                                    handled = 1;
                                } else if (event.key.keysym.sym == SDLK_k || event.key.keysym.sym == SDLK_UP) {
                                    if (wm) vizero_window_manager_focus_direction(wm, 'k');
                                    handled = 1;
                                } else if (event.key.keysym.sym == SDLK_l || event.key.keysym.sym == SDLK_RIGHT) {
                                    if (wm) vizero_window_manager_focus_direction(wm, 'l');
                                    handled = 1;
                                } else if (event.key.keysym.sym >= SDLK_1 && event.key.keysym.sym <= SDLK_9) {
                                    if (wm) vizero_window_manager_focus_number(wm, event.key.keysym.sym - SDLK_0);
                                    handled = 1;
                                }
                                input->awaiting_window_cmd = 0;
                                if (handled) break; // Consume the key
                                // If not handled, fall through to normal handling
                            }
                            // Detect Ctrl+w
                            if ((event.key.keysym.mod & KMOD_CTRL) && (event.key.keysym.sym == SDLK_w)) {
                                input->awaiting_window_cmd = 1;
                                break; // Wait for next key
                            }

                            if (cursor && focused_window) {
                                int col = (int)vizero_cursor_get_column(cursor);
                                switch (event.key.keysym.sym) {
                                    case SDLK_h:
                                    case SDLK_LEFT:
                                        focused_window->preferred_column = col > 0 ? col - 1 : 0;
                                        if (event.key.keysym.mod & KMOD_SHIFT) {
                                            if (!vizero_editor_has_selection(editor)) {
                                                vizero_editor_start_selection(editor);
                                            }
                                            vizero_cursor_move_left(cursor);
                                            vizero_editor_update_selection(editor);
                                        } else {
                                            vizero_editor_clear_selection(editor);
                                            vizero_cursor_move_left(cursor);
                                        }
                                        break;
                                    case SDLK_j:
                                    case SDLK_DOWN:
                                    {
                                        /* Do NOT update preferred_column on vertical movement */
                                        extern int vizero_editor_window_move_visual_row(vizero_editor_window_t* window, struct vizero_editor_state_t* state, int direction); // direction: +1 down, -1 up
                                        if (event.key.keysym.mod & KMOD_SHIFT) {
                                            if (!vizero_editor_has_selection(editor)) {
                                                vizero_editor_start_selection(editor);
                                            }
                                            vizero_editor_window_move_visual_row(focused_window, editor, +1);
                                            vizero_editor_update_selection(editor);
                                        } else {
                                            vizero_editor_clear_selection(editor);
                                            vizero_editor_window_move_visual_row(focused_window, editor, +1);
                                        }
                                        break;
                                    }
                                    case SDLK_k:
                                    case SDLK_UP:
                                    {
                                        /* Do NOT update preferred_column on vertical movement */
                                        extern int vizero_editor_window_move_visual_row(vizero_editor_window_t* window, struct vizero_editor_state_t* state, int direction); // direction: +1 down, -1 up
                                        if (event.key.keysym.mod & KMOD_SHIFT) {
                                            if (!vizero_editor_has_selection(editor)) {
                                                vizero_editor_start_selection(editor);
                                            }
                                            vizero_editor_window_move_visual_row(focused_window, editor, -1);
                                            vizero_editor_update_selection(editor);
                                        } else {
                                            vizero_editor_clear_selection(editor);
                                            vizero_editor_window_move_visual_row(focused_window, editor, -1);
                                        }
                                        break;
                                    }
                                    case SDLK_l:
                                    case SDLK_RIGHT: {
                                        /* Prevent moving past end of line in normal mode */
                                        focused_window->preferred_column = col + 1;
                                        const char* line_text = buffer ? vizero_buffer_get_line_text(buffer, vizero_cursor_get_line(cursor)) : NULL;
                                        size_t line_len = line_text ? strlen(line_text) : 0;
                                        if (col < (int)line_len - 1) {
                                            if (event.key.keysym.mod & KMOD_SHIFT) {
                                                if (!vizero_editor_has_selection(editor)) {
                                                    vizero_editor_start_selection(editor);
                                                }
                                                vizero_cursor_move_right(cursor);
                                                vizero_editor_update_selection(editor);
                                            } else {
                                                vizero_editor_clear_selection(editor);
                                                vizero_cursor_move_right(cursor);
                                            }
                                        }
                                        break;
                                    }

                                    case SDLK_f:
                                        /* Ctrl+F for page down (vi style) */
                                        if (event.key.keysym.mod & KMOD_CTRL) {
                                            vizero_cursor_move_page_down(cursor);
                                        }
                                        break;
                                    case SDLK_b:
                                        /* Ctrl+B for page up (vi style) */
                                        if (event.key.keysym.mod & KMOD_CTRL) {
                                            vizero_cursor_move_page_up(cursor);
                                        }
                                        break;
                                    case SDLK_PAGEUP:
                                        /* PC Page Up key */
                                        vizero_cursor_move_page_up(cursor);
                                        break;
                                    case SDLK_PAGEDOWN:
                                        /* PC Page Down key */
                                        vizero_cursor_move_page_down(cursor);
                                        break;
                                    case SDLK_KP_9:
                                        /* Keypad Page Up (if Num Lock is off) */
                                        vizero_cursor_move_page_up(cursor);
                                        break;
                                    case SDLK_KP_3:
                                        /* Keypad Page Down (if Num Lock is off) */
                                        vizero_cursor_move_page_down(cursor);
                                        break;
                                    case SDLK_c:
                                        /* Ctrl+C for copy */
                                        if (event.key.keysym.mod & KMOD_CTRL) {
                                            vizero_editor_copy_selection(editor);
                                        }
                                        break;
                                    case SDLK_x:
                                        /* Ctrl+X for cut */
                                        if (event.key.keysym.mod & KMOD_CTRL) {
                                            vizero_editor_cut_selection(editor);
                                        }
                                        break;
                                    case SDLK_v:
                                        /* Ctrl+V for paste */
                                        if (event.key.keysym.mod & KMOD_CTRL) {
                                            vizero_editor_paste_at_cursor(editor);
                                        }
                                        break;
                                    case SDLK_z:
                                        /* Ctrl+Z for undo */
                                        if (event.key.keysym.mod & KMOD_CTRL) {
                                            vizero_editor_undo(editor);
                                        }
                                        break;
                                    case SDLK_a:
                                        if (event.key.keysym.mod & KMOD_CTRL) {
                                            /* Ctrl+A for select all */
                                            if (buffer) {
                                                size_t line_count = vizero_buffer_get_line_count(buffer);
                                                if (line_count > 0) {
                                                    vizero_editor_start_selection_at(editor, 0, 0);
                                                    const char* last_line = vizero_buffer_get_line_text(buffer, line_count - 1);
                                                    size_t end_col = last_line ? strlen(last_line) : 0;
                                                    vizero_cursor_set_position(cursor, line_count - 1, end_col);
                                                    vizero_editor_update_selection(editor);
                                                    vizero_editor_set_status_message(editor, "All text selected");
                                                }
                                            }
                                        } else {
                                            /* Vi-like append: move cursor right (if not at EOL), then enter insert mode */
                                            if (buffer && cursor) {
                                                const char* line_text = buffer ? vizero_buffer_get_line_text(buffer, vizero_cursor_get_line(cursor)) : NULL;
                                                size_t len = line_text ? strlen(line_text) : 0;
                                                if (col < (int)len) {
                                                    vizero_cursor_set_position(cursor, vizero_cursor_get_line(cursor), col + 1);
                                                }
                                                /* Enter insert mode via mode manager */
                                                vizero_mode_manager_t* mode_manager = vizero_editor_get_mode_manager(editor);
                                                if (mode_manager) {
                                                    vizero_mode_manager_enter_insert_mode(mode_manager);
                                                    input->mode_changed_this_frame = 1;
                                                }
                                            }
                                        }
                                        break;
                                    case SDLK_n:
                                        /* Search next (forward) or previous (backward with Shift) */
                                        if (!(event.key.keysym.mod & (KMOD_CTRL | KMOD_ALT))) {
                                            if (event.key.keysym.mod & KMOD_SHIFT) {
                                                /* Shift+N: Search previous */
                                                vizero_search_next_direction(editor, 0); /* 0 for backward */
                                            } else {
                                                /* N: Search next */
                                                vizero_search_next_direction(editor, 1); /* 1 for forward */
                                            }
                                        }
                                        break;
                                    case SDLK_SLASH:
                                        /* Forward search */
                                        if (!(event.key.keysym.mod & (KMOD_CTRL | KMOD_ALT))) {
                                            /* Enter command mode for search */
                                            vizero_editor_set_mode(editor, VIZERO_MODE_COMMAND);
                                            vizero_editor_clear_command_buffer(editor);
                                            vizero_editor_append_to_command(editor, '/');
                                            input->mode_changed_this_frame = 1;
                                        }
                                        break;
                                    case SDLK_i:
                                        /* Enter insert mode via mode manager */
                                        {
                                            vizero_mode_manager_t* mode_manager = vizero_editor_get_mode_manager(editor);
                                            if (mode_manager) {
                                                vizero_mode_manager_enter_insert_mode(mode_manager);
                                                input->mode_changed_this_frame = 1;
                                            }
                                        }
                                        break;

                                    case SDLK_SEMICOLON:
                                        /* Check if shift is held for colon */
                                        if (event.key.keysym.mod & KMOD_SHIFT) {
                                            /* Enter command mode via mode manager */
                                            vizero_mode_manager_t* mode_manager = vizero_editor_get_mode_manager(editor);
                                            if (mode_manager) {
                                                vizero_mode_manager_enter_command_mode(mode_manager);
                                                vizero_editor_clear_command_buffer(editor);
                                                vizero_editor_append_to_command(editor, ':');
                                                input->mode_changed_this_frame = 1;
                                            }
                                        }
                                        break;
                                    case SDLK_ESCAPE:
                                        /* ESC in normal mode does nothing (vi behavior) */
                                        break;
                                    case SDLK_TAB: {
                                        /* Smart tab behavior in normal mode */
                                        vizero_buffer_t* current_buffer = vizero_editor_get_current_buffer(editor);
                                        if (current_buffer) {
                                            vizero_position_t cursor_pos = vizero_cursor_get_position(cursor);
                                            const char* line_text = vizero_buffer_get_line_text(current_buffer, cursor_pos.line);
                                            
                                            if (line_text) {
                                                /* Find first non-space character */
                                                size_t first_non_space = 0;
                                                while (line_text[first_non_space] == ' ' || line_text[first_non_space] == '\t') {
                                                    first_non_space++;
                                                }
                                                
                                                /* If we're within the leading whitespace and there's a non-space character ahead */
                                                if (cursor_pos.column <= first_non_space && first_non_space > 0 && line_text[first_non_space] != '\0') {
                                                    vizero_cursor_set_position(cursor, cursor_pos.line, first_non_space);
                                                } else {
                                                    /* Regular tab behavior - insert 4 spaces */
                                                    const char* spaces = "    "; /* 4 spaces */
                                                    vizero_position_t start_pos = {cursor_pos.line, cursor_pos.column};
                                                    vizero_position_t end_pos = {cursor_pos.line, cursor_pos.column + 4};
                                                    vizero_editor_push_undo_operation(editor, VIZERO_UNDO_INSERT_TEXT, start_pos, end_pos, spaces);
                                                    
                                                    if (vizero_buffer_insert_text(buffer, cursor_pos.line, cursor_pos.column, spaces) == 0) {
                                                        /* Move cursor forward by 4 positions */
                                                        vizero_cursor_set_position(cursor, cursor_pos.line, cursor_pos.column + 4);
                                                    }
                                                }
                                            }
                                        }
                                        break;
                                    }
                                    default:
                                        break;
                                }
                            }
                        }
                       // } /* End of normal mode handling */
                    }
                    if (editor && vizero_editor_get_mode(editor) == VIZERO_MODE_INSERT) {
                        /* In insert mode, handle special keys (ESC handled by mode manager) */
                        vizero_buffer_t* buffer = vizero_editor_get_current_buffer(editor);
                        vizero_cursor_t* cursor = vizero_editor_get_current_cursor(editor);
                        
                        if (buffer && cursor) {
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
                                case SDLK_BACKSPACE: {
                                    /* Backspace - delete character before cursor */
                                    if (col > 0) {
                                        /* Get character to be deleted for undo */
                                        const char* line_text = vizero_buffer_get_line_text(buffer, line);
                                        if (line_text && col - 1 < strlen(line_text)) {
                                            char deleted_char[2] = {line_text[col - 1], '\0'};
                                            vizero_position_t pos = {line, col - 1};
                                            vizero_editor_push_undo_operation(editor, VIZERO_UNDO_DELETE_CHAR, pos, pos, deleted_char);
                                        }
                                        
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
                                }
                                case SDLK_DELETE: {
                                    /* Delete key - delete character at cursor */
                                    /* Get character to be deleted for undo */
                                    const char* line_text = vizero_buffer_get_line_text(buffer, line);
                                    if (line_text && col < strlen(line_text)) {
                                        char deleted_char[2] = {line_text[col], '\0'};
                                        vizero_position_t pos = {line, col};
                                        vizero_editor_push_undo_operation(editor, VIZERO_UNDO_DELETE_CHAR, pos, pos, deleted_char);
                                    }
                                    vizero_buffer_delete_char(buffer, line, col);
                                    break;
                                }
                                case SDLK_TAB: {
                                    /* Smart tab behavior */
                                    const char* line_text = vizero_buffer_get_line_text(buffer, line);
                                    
                                    if (line_text) {
                                        /* Find first non-space character */
                                        size_t first_non_space = 0;
                                        while (line_text[first_non_space] == ' ' || line_text[first_non_space] == '\t') {
                                            first_non_space++;
                                        }
                                        
                                        /* If we're within the leading whitespace and there's a non-space character ahead */
                                        if (col <= first_non_space && first_non_space > 0 && line_text[first_non_space] != '\0') {
                                            vizero_cursor_set_position(cursor, line, first_non_space);
                                            break;
                                        }
                                    }
                                    
                                    /* Regular tab behavior - insert 4 spaces */
                                    const char* spaces = "    "; /* 4 spaces */
                                    vizero_position_t start_pos = {line, col};
                                    vizero_position_t end_pos = {line, col + 4};
                                    vizero_editor_push_undo_operation(editor, VIZERO_UNDO_INSERT_TEXT, start_pos, end_pos, spaces);
                                    
                                    if (vizero_buffer_insert_text(buffer, line, col, spaces) == 0) {
                                        /* Move cursor forward by 4 positions */
                                        vizero_cursor_set_position(cursor, line, col + 4);
                                    }
                                    break;
                                }
                                default:
                                    /* Arrow keys work in insert mode too */
                                    switch (event.key.keysym.sym) {
                                        case SDLK_LEFT:
                                            /* Left arrow - move cursor left */
                                            vizero_cursor_move_left(cursor);
                                            break;
                                        case SDLK_RIGHT:
                                            /* Right arrow - move cursor right */
                                            vizero_cursor_move_right(cursor);
                                            break;
                                        case SDLK_UP:
                                            /* Up arrow - move cursor up */
                                            vizero_cursor_move_up(cursor);
                                            break;
                                        case SDLK_DOWN:
                                            /* Down arrow - move cursor down */
                                            vizero_cursor_move_down(cursor);
                                            break;
                                        case SDLK_c:
                                            /* Ctrl+C for copy in insert mode */
                                            if (event.key.keysym.mod & KMOD_CTRL) {
                                                vizero_editor_copy_selection(editor);
                                            }
                                            break;
                                        case SDLK_x:
                                            /* Ctrl+X for cut in insert mode */
                                            if (event.key.keysym.mod & KMOD_CTRL) {
                                                vizero_editor_cut_selection(editor);
                                            }
                                            break;
                                        case SDLK_v:
                                            /* Ctrl+V for paste in insert mode */
                                            if (event.key.keysym.mod & KMOD_CTRL) {
                                                vizero_editor_paste_at_cursor(editor);
                                            }
                                            break;
                                        case SDLK_z:
                                            /* Ctrl+Z for undo in insert mode */
                                            if (event.key.keysym.mod & KMOD_CTRL) {
                                                vizero_editor_undo(editor);
                                            }
                                            break;
                                        default:
                                            break;
                                    }
                                    break;
                            }
                        }
                    }
                    if (editor && vizero_editor_get_mode(editor) == VIZERO_MODE_COMMAND) {
                        /* In command mode, handle command input */
                        if (event.key.keysym.sym == SDLK_ESCAPE) {
                            /* ESC cancels command mode - use mode manager */
                            vizero_mode_manager_t* mode_manager = vizero_editor_get_mode_manager(editor);
                            if (mode_manager) {
                                vizero_mode_manager_enter_normal_mode(mode_manager);
                            }
                            vizero_editor_clear_command_buffer(editor);
                        } else if (event.key.keysym.sym == SDLK_RETURN) {
                            /* Enter executes command */
                            vizero_editor_execute_current_command(editor);
                            vizero_mode_manager_t* mode_manager = vizero_editor_get_mode_manager(editor);
                            if (mode_manager) {
                                vizero_mode_manager_enter_normal_mode(mode_manager);
                            }
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
                                vizero_mode_manager_t* mode_manager = vizero_editor_get_mode_manager(editor);
                                if (mode_manager) {
                                    vizero_mode_manager_enter_normal_mode(mode_manager);
                                }
                            }
                        }
                    }
                }
            keydown_handled:
                break;
            case SDL_TEXTINPUT:
                /* Handle text input */
                if (input->app) {
                    /* Check if we should suppress this text input (e.g., after Ctrl+Space) */
                    if (input->suppress_next_textinput) {
                        input->suppress_next_textinput = 0; /* Reset flag */
                        break; /* Ignore this text input event */
                    }
                    
                    vizero_editor_state_t* editor = vizero_application_get_editor(input->app);
                    /* Mode handling moved inline below */
                    if (editor && vizero_editor_get_mode(editor) == VIZERO_MODE_INSERT) {
                        /* Suppress the first SDL_TEXTINPUT event after switching to insert mode (e.g., after 'a') */
                        if (input->mode_changed_this_frame) {
                            break;
                        }
                        const char* text = event.text.text;
                        vizero_buffer_t* buffer = vizero_editor_get_current_buffer(editor);
                        vizero_cursor_t* cursor = vizero_editor_get_current_cursor(editor);
                        if (buffer && cursor) {
                            /* Insert each character from the text input */
                            for (const char* c = text; *c; c++) {
                                size_t line = vizero_cursor_get_line(cursor);
                                size_t col = vizero_cursor_get_column(cursor);
                                /* Track undo for character insertion */
                                vizero_position_t pos = {line, col};
                                char char_str[2] = {*c, '\0'};
                                vizero_editor_push_undo_operation(editor, VIZERO_UNDO_INSERT_CHAR, pos, pos, char_str);
                                if (vizero_buffer_insert_char(buffer, line, col, *c) == 0) {
                                    /* Move cursor forward after successful insertion */
                                    vizero_cursor_move_right(cursor);
                                }
                            }
                        }
                    } else if (editor && vizero_editor_get_mode(editor) == VIZERO_MODE_COMMAND) {
                        /* In command mode, append text to command buffer */
                        const char* text = event.text.text;
                        
                        /* Skip the mode-entering character if we just entered command mode */
                        if (input->mode_changed_this_frame && strlen(text) == 1 && 
                            (text[0] == ':' || text[0] == '/' || text[0] == '?')) {
                            break;
                        }
                        
                        for (const char* c = text; *c; c++) {
                            vizero_editor_append_to_command(editor, *c);
                        }
                    } else if (editor && vizero_editor_get_mode(editor) == VIZERO_MODE_NORMAL) {
                        /* In normal mode, handle vi commands */
                        const char* text = event.text.text;
                        
                        /* Handle single character vi commands */
                        if (strlen(text) == 1) {
                            char c = text[0];
                            /* Convert to key code for mode manager */
                            uint32_t key = (uint32_t)c;
                            
                            /* Use persistent mode manager from editor state */
                            vizero_mode_manager_t* mode_manager = vizero_editor_get_mode_manager(editor);
                            if (mode_manager) {
                                int handled = vizero_mode_manager_handle_key(mode_manager, key, 0);
                                if (handled && (c == ':' || c == '/' || c == '?')) {
                                    /* If mode manager handled a mode-changing key, set flag */
                                    input->mode_changed_this_frame = 1;
                                }
                            }
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
