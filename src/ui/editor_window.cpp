#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <ctype.h>
#include "vizero/editor_state.h"
#include "vizero/renderer.h"
#include "vizero/search.h"
#include "vizero/editor_window.h"
#include "vizero/buffer.h"
#include "vizero/cursor.h"
#include "vizero/editor_window_constants.h"
#include "vizero/plugin_manager.h"
#include "../editor/editor_state_internal.h"
#ifdef _WIN32
#include <direct.h>
#define getcwd _getcwd
#else
#include <unistd.h>
#endif
// Helper to get current working directory (static buffer)
static const char* vizero_get_cwd(void) {
    static char cwd[512];
    if (getcwd(cwd, sizeof(cwd))) {
        return cwd;
    } else {
        return "[unknown dir]";
    }
}
// Draw status bar with current directory
void vizero_draw_status_bar_with_cwd(vizero_editor_state_t* state, vizero_renderer_t* renderer, int screen_width, int screen_height) {
    // Draw background bar
    vizero_colour_t bar_colour = {0.15f, 0.15f, 0.18f, 1.0f};
    vizero_renderer_fill_rect(renderer, 0.0f, (float)(screen_height - 24), (float)screen_width, 24.0f, bar_colour);

    // Left: status message
    const char* status = vizero_editor_get_status_message(state);
    vizero_text_info_t info = {8.0f, (float)(screen_height - 18), {1.0f, 1.0f, 1.0f, 1.0f}, NULL};
    vizero_renderer_draw_text(renderer, status ? status : "", &info);

    // Right: current working directory
    const char* cwd = vizero_get_cwd();
    char cwd_buf[512];
    int ret = snprintf(cwd_buf, sizeof(cwd_buf), "CWD: %s", cwd);
    if (ret >= (int)sizeof(cwd_buf)) {
        // Truncated, add ellipsis
        strcpy(cwd_buf + sizeof(cwd_buf) - 4, "...");
    }
    int text_width = (int)strlen(cwd_buf) * 8;
    vizero_text_info_t info2 = {(float)(screen_width - text_width - 8), (float)(screen_height - 18), {0.7f, 0.7f, 1.0f, 1.0f}, NULL};
    vizero_renderer_draw_text(renderer, cwd_buf, &info2);
}
// Safely set the window title, freeing the old one
void vizero_editor_window_set_title(vizero_editor_window_t* window, const char* title) {
    if (!window) return;
    if (window->title) {
        free(window->title);
        window->title = NULL;
    }
    window->title = title ? strdup(title) : NULL;
}
/* Window manager structure */
struct vizero_window_manager_t {
    vizero_editor_window_t* windows[MAX_WINDOWS];
    size_t window_count;
    uint32_t next_window_id;
    uint32_t focused_window_id;
    
    /* Layout properties */
    int screen_width, screen_height;
    vizero_layout_type_t layout_type;
    
    /* Split configuration */
    uint32_t primary_window_id;    /* Main window in single mode, left/top in split */
    uint32_t secondary_window_id;  /* Secondary window in split mode */
    float split_ratio;             /* Ratio between primary and secondary (0.0-1.0) */
};
size_t vizero_window_manager_get_window_count_raw(vizero_window_manager_t* manager) {
    return manager ? manager->window_count : 0;
}

vizero_editor_window_t* vizero_window_manager_get_window_raw(vizero_window_manager_t* manager, size_t idx) {
    if (!manager || idx >= manager->window_count) return NULL;
    return manager->windows[idx];
}
/* Window manager creation and destruction */
vizero_window_manager_t* vizero_window_manager_create(void) {
    vizero_window_manager_t* manager = (vizero_window_manager_t*)calloc(1, sizeof(vizero_window_manager_t));
    if (manager) {
        manager->window_count = 0;
        manager->next_window_id = 1;
        manager->focused_window_id = 0;
        manager->screen_width = 800;   /* Default, should be updated */
        manager->screen_height = 600;  /* Default, should be updated */
        manager->layout_type = VIZERO_LAYOUT_SINGLE;
        manager->primary_window_id = 0;
        manager->secondary_window_id = 0;
        manager->split_ratio = 0.5f;   /* 50/50 split by default */
    }
    return manager;
}
/* ...existing code... */

// Window focus helpers (for vi-style window switching)
int vizero_window_manager_focus_direction(vizero_window_manager_t* manager, char dir) {
    (void)dir;
    if (!manager) return -1;
    // Only two windows supported for now: horizontal or vertical split
    if (manager->window_count < 2) return -1;
    vizero_editor_window_t* focused = vizero_window_manager_get_focused_window(manager);
    if (!focused) return -1;
    vizero_editor_window_t* other = NULL;
    // Find the other window
    for (size_t i = 0; i < manager->window_count; ++i) {
        if (manager->windows[i] && manager->windows[i]->window_id != focused->window_id) {
            other = manager->windows[i];
            break;
        }
    }
    if (!other) return -1;
    // For now, just toggle for any direction (future: use geometry)
    return vizero_window_manager_set_focus(manager, other->window_id);
}

int vizero_window_manager_focus_number(vizero_window_manager_t* manager, int number) {
    if (!manager || number < 1 || number > (int)manager->window_count) return -1;
    vizero_editor_window_t* win = manager->windows[number-1];
    if (!win) return -1;
    return vizero_window_manager_set_focus(manager, win->window_id);
}
void vizero_window_manager_destroy(vizero_window_manager_t* manager) {
    if (!manager) return;
    
    /* Destroy all windows using the proper destructor */
    while (manager->window_count > 0) {
        vizero_window_manager_destroy_window(manager, manager->windows[0]->window_id);
    }
    free(manager);
}

/* Window lifecycle */
vizero_editor_window_t* vizero_window_manager_create_window(vizero_window_manager_t* manager,
                                                           size_t buffer_index,
                                                           int x, int y, int width, int height) {
    if (!manager || manager->window_count >= MAX_WINDOWS) return NULL;
    
    vizero_editor_window_t* window = (vizero_editor_window_t*)calloc(1, sizeof(vizero_editor_window_t));
    if (!window) return NULL;
    
    /* Initialize window properties */
    window->x = x;
    window->y = y;
    window->width = width;
    window->height = height;
    window->buffer_index = buffer_index;
    
    window->is_focused = 0;
    window->is_maximized = 0;
    window->is_visible = 1;
    window->title_bar_height = DEFAULT_TITLE_BAR_HEIGHT;
    window->scroll_x = 0;
    window->scroll_y = 0;
    window->is_resizable = 1;
    window->min_width = DEFAULT_MIN_WIDTH;
    window->min_height = DEFAULT_MIN_HEIGHT;
    window->has_scrollbar = 1;
    window->has_title_bar = 1;
    window->window_id = manager->next_window_id++;
    
    /* Set default title - buffer info will be set later by caller */
    char default_title[64];
    sprintf(default_title, "[Window %u]", window->window_id);
    vizero_editor_window_set_title(window, default_title);
    
    /* Add to manager */
    manager->windows[manager->window_count] = window;
    manager->window_count++;
    
    /* Focus the new window if it's the first one */
    if (manager->window_count == 1) {
        manager->focused_window_id = window->window_id;
        window->is_focused = 1;
    }
    
    return window;
}

int vizero_window_manager_destroy_window(vizero_window_manager_t* manager, uint32_t window_id) {
    if (!manager) return -1;
    
    /* Find window */
    size_t window_index = SIZE_MAX;
    for (size_t i = 0; i < manager->window_count; i++) {
        if (manager->windows[i] && manager->windows[i]->window_id == window_id) {
            window_index = i;
            break;
        }
    }
    
    if (window_index == SIZE_MAX) return -1;
    
    vizero_editor_window_t* window = manager->windows[window_index];
    
    /* Clean up window */
    if (window->title) {
        free(window->title);
    }
    /* Note: Window no longer owns cursors - they're managed by editor state */
    free(window);
    
    /* Remove from array and shift remaining windows */
    for (size_t i = window_index; i < manager->window_count - 1; i++) {
        manager->windows[i] = manager->windows[i + 1];
    }
    manager->window_count--;
    
    /* Update focus if necessary */
    if (manager->focused_window_id == window_id) {
        if (manager->window_count > 0) {
            /* Focus the first available window */
            manager->focused_window_id = manager->windows[0]->window_id;
            manager->windows[0]->is_focused = 1;
        } else {
            manager->focused_window_id = 0;
        }
    }
    
    return 0;
}

int vizero_window_manager_close_window(vizero_window_manager_t* manager, uint32_t window_id, struct vizero_editor_state_t* state) {
    if (!manager) return -1;
    
    vizero_editor_window_t* window = vizero_window_manager_get_window_by_id(manager, window_id);
    if (!window) return -1;
    
    /* Check if buffer has unsaved changes */
    vizero_buffer_t* buffer = vizero_editor_window_get_buffer(window, state);
    if (buffer && vizero_buffer_is_modified(buffer)) {
        /* TODO: Show confirmation dialog or return error code for caller to handle */
        return -2; /* Indicates unsaved changes */
    }
    
    return vizero_window_manager_destroy_window(manager, window_id);
}

/* Window management */
vizero_editor_window_t* vizero_window_manager_get_focused_window(vizero_window_manager_t* manager) {
    if (!manager || manager->focused_window_id == 0) return NULL;
    
    return vizero_window_manager_get_window_by_id(manager, manager->focused_window_id);
}

int vizero_window_manager_set_focus(vizero_window_manager_t* manager, uint32_t window_id) {
    if (!manager) return -1;
    
    /* Clear focus from current window */
    if (manager->focused_window_id != 0) {
        vizero_editor_window_t* current = vizero_window_manager_get_window_by_id(manager, manager->focused_window_id);
        if (current) {
            current->is_focused = 0;
        }
    }
    
    /* Set focus to new window */
    vizero_editor_window_t* new_focused = vizero_window_manager_get_window_by_id(manager, window_id);
    if (!new_focused) return -1;
    
    new_focused->is_focused = 1;
    manager->focused_window_id = window_id;
    
    return 0;
}

int vizero_window_manager_get_window_count(vizero_window_manager_t* manager) {
    return manager ? (int)manager->window_count : 0;
}

vizero_editor_window_t* vizero_window_manager_get_window(vizero_window_manager_t* manager, size_t index) {
    if (!manager || index >= manager->window_count) return NULL;
    return manager->windows[index];
}

vizero_editor_window_t* vizero_window_manager_get_window_by_id(vizero_window_manager_t* manager, uint32_t window_id) {
    if (!manager) return NULL;
    
    for (size_t i = 0; i < manager->window_count; i++) {
        if (manager->windows[i] && manager->windows[i]->window_id == window_id) {
            return manager->windows[i];
        }
    }
    
    return NULL;
}

/* Window properties */

int vizero_editor_window_set_size(vizero_editor_window_t* window, int width, int height) {
    if (!window) return -1;
    
    /* Enforce minimum size */
    if (width < window->min_width) width = window->min_width;
    if (height < window->min_height) height = window->min_height;
    
    window->width = width;
    window->height = height;
    
    return 0;
}

int vizero_editor_window_set_position(vizero_editor_window_t* window, int x, int y) {
    if (!window) return -1;
    
    window->x = x;
    window->y = y;
    
    return 0;
}

int vizero_editor_window_maximize(vizero_editor_window_t* window) {
    if (!window) return -1;
    
    /* TODO: Store current position/size for restore */
    window->is_maximized = 1;
    
    return 0;
}

int vizero_editor_window_restore(vizero_editor_window_t* window) {
    if (!window) return -1;
    
    /* TODO: Restore previous position/size */
    window->is_maximized = 0;
    
    return 0;
}

/* Window state queries */
int vizero_editor_window_is_focused(vizero_editor_window_t* window) {
    return window ? window->is_focused : 0;
}

int vizero_editor_window_is_maximized(vizero_editor_window_t* window) {
    return window ? window->is_maximized : 0;
}

const char* vizero_editor_window_get_title(vizero_editor_window_t* window) {
    return window ? window->title : NULL;
}

/* Window content access */
vizero_buffer_t* vizero_editor_window_get_buffer(vizero_editor_window_t* window, struct vizero_editor_state_t* state) {
    if (!window || !state || window->buffer_index >= MAX_BUFFERS) return NULL;
    return vizero_editor_get_buffer(state, window->buffer_index);
}

vizero_cursor_t* vizero_editor_window_get_cursor(vizero_editor_window_t* window, struct vizero_editor_state_t* state) {
    if (!window || !state || window->buffer_index >= MAX_BUFFERS) return NULL;
    return vizero_editor_get_cursor(state, window->buffer_index);
}

/* Buffer index access */
size_t vizero_editor_window_get_buffer_index(vizero_editor_window_t* window) {
    return window ? window->buffer_index : 0;
}

int vizero_editor_window_set_buffer_index(vizero_editor_window_t* window, size_t buffer_index) {
    if (!window) return -1;
    window->buffer_index = buffer_index;
    return 0;
}




/* Rendering support */
int vizero_editor_window_get_content_area(vizero_editor_window_t* window,
                                         int* content_x, int* content_y,
                                         int* content_width, int* content_height) {
    if (!window || !content_x || !content_y || !content_width || !content_height) return -1;
    /* Calculate content area (excluding title bar and scrollbar) */
    *content_x = window->x;
    *content_y = window->y + (window->has_title_bar ? window->title_bar_height : 0);
    *content_width = window->width - (window->has_scrollbar ? 16 : 0); /* 16px scrollbar */
    *content_height = window->height - (window->has_title_bar ? window->title_bar_height : 0);
    return 0;
}

/* Word wrap rendering: call this from your main render loop for each window */
void vizero_editor_window_render_content(vizero_editor_window_t* window, vizero_editor_state_t* state, vizero_renderer_t* renderer) {
    if (!window || !state || !renderer) return;
    vizero_buffer_t* buffer = vizero_editor_window_get_buffer(window, state);
    if (!buffer) {
        // Optionally, draw a message or just return
        return;
    }

    vizero_settings_t* settings = vizero_editor_get_settings(state);
        int word_wrap = vizero_settings_get_bool(settings, VIZERO_SETTING_WORD_WRAP);
    int syntax_enabled = vizero_settings_get_bool(settings, VIZERO_SETTING_SYNTAX_HIGHLIGHTING);
    int show_line_numbers = vizero_settings_get_bool(settings, VIZERO_SETTING_LINE_NUMBERS);
    int line_number_width = show_line_numbers ? 6 * 8 : 0; // 6 chars wide, 8px per char

    int content_x, content_y, content_width, content_height;
    vizero_editor_window_get_content_area(window, &content_x, &content_y, &content_width, &content_height);

    int CHAR_WIDTH = 8;
    int CHAR_HEIGHT = 16;
    // Subtract line_number_width from content_width to get actual text area
    int max_cols = (content_width - line_number_width) / CHAR_WIDTH;
    if (max_cols < 1) max_cols = 1;
    // [DEBUG] printf removed
    int max_rows = content_height / CHAR_HEIGHT;
    // Disable horizontal scrolling when word wrap is enabled
    if (word_wrap) window->scroll_x = 0;

    // Get cursor position for this window
    vizero_cursor_t* cursor = vizero_editor_window_get_cursor(window, state);
    vizero_position_t cursor_pos = {0, 0};
    if (cursor) {
        cursor_pos = vizero_cursor_get_position(cursor);
    } else {
        // Defensive: if no cursor, just use (0,0) and avoid dereferencing
    }

    // Build a mapping of logical line/col to visual row/col
    int visual_cursor_row = 0, visual_cursor_col = 0;
    int found_cursor = 0;
    struct {
        int line;
        int start_col;
        int visual_row;
        int visual_col_start;
        int visual_col_end;
    } visual_map[2048];
    int visual_map_count = 0;

    size_t line_count = vizero_buffer_get_line_count(buffer);
    int row = 0;
    // Pass 1: Build visual map for the entire buffer
    for (size_t i = 0; i < line_count; ++i) {
        const char* line = vizero_buffer_get_line_text(buffer, i);
        size_t len = line ? strlen(line) : 0;
        size_t start = 0;
        int first_visual_row = 1;
        (void)first_visual_row; // Suppress unused variable warning
        int indent_len = 0;
        if (line) {
            while (line[indent_len] == ' ' || line[indent_len] == '\t') indent_len++;
        }
        while (start < len) {
            size_t actual_chunk = 0;
            int wrap_at_space = 0;
            if (word_wrap) {
                size_t remaining = len - start;
                if (remaining > (size_t)max_cols) {
                    size_t try_chunk = (size_t)max_cols;
                    size_t last_space = 0;
                    for (size_t j = 0; j < try_chunk; ++j) {
                        if (line[start + j] == ' ') last_space = j;
                    }
                    if (last_space > 0) {
                        actual_chunk = last_space;
                        wrap_at_space = 1;
                    } else {
                        actual_chunk = try_chunk;
                    }
                } else {
                    actual_chunk = remaining;
                }
            } else {
                actual_chunk = len - start;
            }
            if (actual_chunk == 0 && start < len) actual_chunk = 1;
            if (visual_map_count < (int)(sizeof(visual_map)/sizeof(visual_map[0]))) {
                visual_map[visual_map_count].line = (int)i;
                visual_map[visual_map_count].start_col = (int)start;
                visual_map[visual_map_count].visual_row = row;
                visual_map[visual_map_count].visual_col_start = 0;
                visual_map[visual_map_count].visual_col_end = (int)actual_chunk;
                visual_map_count++;
            }
            row++;
            if (wrap_at_space) {
                start += actual_chunk + 1;
            } else {
                start += actual_chunk;
            }
            if (!word_wrap) break;
            first_visual_row = 0;
        }
        if (len == 0) {
            if (visual_map_count < (int)(sizeof(visual_map)/sizeof(visual_map[0]))) {
                visual_map[visual_map_count].line = (int)i;
                visual_map[visual_map_count].start_col = 0;
                visual_map[visual_map_count].visual_row = row;
                visual_map[visual_map_count].visual_col_start = 0;
                visual_map[visual_map_count].visual_col_end = 0;
                visual_map_count++;
            }
            row++;
        }
    }

    // Map logical cursor position to visual row/col
    if (window->is_focused && cursor) {
        size_t cur_line = cursor_pos.line;
        size_t cur_col = cursor_pos.column;
        for (int v = 0; v < visual_map_count; ++v) {
            int chunk_len = visual_map[v].visual_col_end - visual_map[v].visual_col_start;
            int chunk_start = visual_map[v].start_col;
            int chunk_end = chunk_start + chunk_len;
            if (visual_map[v].line == (int)cur_line) {
                if (cur_col >= (size_t)chunk_start && cur_col <= (size_t)chunk_end) {
                    visual_cursor_row = visual_map[v].visual_row;
                    visual_cursor_col = (int)(cur_col - chunk_start);
                    found_cursor = 1;
                    break;
                }
            }
        }
    }

    // Pass 2: Render only the visible rows
    int first_visible_row = window->scroll_y;
    int last_visible_row = window->scroll_y + max_rows;
    for (int v = 0; v < visual_map_count; ++v) {
        if (visual_map[v].visual_row < first_visible_row || visual_map[v].visual_row >= last_visible_row) continue;
        int i = visual_map[v].line;
        int start = visual_map[v].start_col;
        const char* line = vizero_buffer_get_line_text(buffer, i);
        /* size_t len = line ? strlen(line) : 0; */ /* Currently unused */
        int first_visual_row = (visual_map[v].visual_col_start == 0);
        int indent_len = 0;
        if (line) {
            while (line[indent_len] == ' ' || line[indent_len] == '\t') indent_len++;
        }
        int text_x = content_x + line_number_width;
        int hanging_indent = (first_visual_row ? 0 : indent_len);
        text_x += hanging_indent * CHAR_WIDTH;
        size_t actual_chunk = (size_t)(visual_map[v].visual_col_end - visual_map[v].visual_col_start);
        char visual[1024];
        if (line && actual_chunk > 0 && actual_chunk < sizeof(visual)) {
            size_t copy_start = start;
            size_t copy_len = actual_chunk;
            if (!first_visual_row) {
                while (copy_len > 0 && (line[copy_start] == ' ' || line[copy_start] == '\t')) {
                    copy_start++;
                    copy_len--;
                }
            }
            strncpy(visual, line + copy_start, copy_len);
            visual[copy_len] = '\0';
        } else if (line && actual_chunk > 0) {
            strncpy(visual, line + start, actual_chunk);
            visual[actual_chunk] = '\0';
        } else {
            visual[0] = '\0';
        }
        if (show_line_numbers && first_visual_row) {
            char lnbuf[16];
            snprintf(lnbuf, sizeof(lnbuf), "%5d ", i+1);
            vizero_text_info_t lninfo = { (float)content_x, (float)(content_y + (visual_map[v].visual_row - window->scroll_y) * 16), {0.5f, 0.5f, 0.5f, 1.0f}, NULL };
            vizero_renderer_draw_text(renderer, lnbuf, &lninfo);
        }
        if (syntax_enabled && state->plugin_manager) {
            // --- PATCH: Use caller-allocated tokens buffer for plugin syntax highlighting ---
            vizero_syntax_token_t tokens[64]; // 64 tokens per line should be enough for most cases
            size_t token_count = 0;
            vizero_plugin_manager_highlight_syntax(
                state->plugin_manager, buffer, i, i+1, tokens, 64, &token_count);
            for (size_t col = 0; col < strlen(visual); col++) {
                size_t logical_col = start + col;
                
                /* Get default text colour from theme */
                vizero_colour_t colour = {1.0f, 1.0f, 1.0f, 1.0f}; // Fallback
                void* theme_manager = vizero_editor_get_theme_manager(state);
                if (theme_manager) {
                    const vizero_colour_theme_t* current_theme = vizero_theme_manager_get_current_theme((vizero_theme_manager_t*)theme_manager);
                    if (current_theme) {
                        colour = current_theme->foreground;
                    }
                }
                
                for (size_t t = 0; t < token_count; t++) {
                    vizero_syntax_token_t* token = &tokens[t];
                    if (token->range.start.line == (size_t)i && logical_col >= token->range.start.column && logical_col < token->range.end.column) {
                        colour.r = token->colour.r / 255.0f;
                        colour.g = token->colour.g / 255.0f;
                        colour.b = token->colour.b / 255.0f;
                        colour.a = token->colour.a / 255.0f;
                        break;
                    }
                }
                
                /* Check for search match highlighting */
                int is_search_match = 0;
                int is_current_match = 0;
                if (vizero_search_has_results(state)) {
                    const vizero_search_match_t* matches = vizero_search_get_all_matches(state);
                    int match_count = vizero_search_get_match_count(state);
                    int current_match_index = vizero_search_get_current_match_index(state);
                    
                    for (int m = 0; m < match_count; m++) {
                        if (matches[m].line == (int)i && 
                            logical_col >= (size_t)matches[m].column && 
                            logical_col < (size_t)(matches[m].column + matches[m].length)) {
                            is_search_match = 1;
                            if (m == current_match_index) {
                                is_current_match = 1;
                            }
                            break;
                        }
                    }
                }
                
                char ch[2] = {visual[col], '\0'};
                vizero_text_info_t info = { (float)(text_x + (int)col * 8), (float)(content_y + (visual_map[v].visual_row - window->scroll_y) * 16), colour, NULL };
                
                /* Draw search match background highlighting */
                if (is_search_match) {
                    vizero_colour_t bg_colour;
                    if (is_current_match) {
                        /* Current match: orange background */
                        bg_colour = {1.0f, 0.5f, 0.0f, 0.7f}; /* Orange with transparency */
                    } else {
                        /* Other matches: yellow background */
                        bg_colour = {1.0f, 1.0f, 0.0f, 0.4f}; /* Yellow with transparency */
                    }
                    
                    /* Draw background rectangle */
                    vizero_renderer_draw_rect(renderer, 
                        (float)(text_x + (int)col * 8), 
                        (float)(content_y + (visual_map[v].visual_row - window->scroll_y) * 16),
                        8.0f, 16.0f, bg_colour);
                }
                
                /* Diagnostic error underlines disabled during character rendering for performance */
                /* TODO: Implement efficient batch diagnostic rendering outside the character loop */
                
                vizero_renderer_draw_text(renderer, ch, &info);
                
                /* Diagnostic underlines disabled during character rendering for performance */
                /* TODO: Implement efficient batch diagnostic rendering outside the character loop */
            }
            // No free needed, stack buffer
        } else {
            vizero_text_info_t info = { (float)text_x, (float)(content_y + (visual_map[v].visual_row - window->scroll_y) * 16), {1.0f, 1.0f, 1.0f, 1.0f}, NULL };
            vizero_renderer_draw_text(renderer, visual, &info);
        }
    }

    // If cursor was not mapped, try to clamp it to the last visual segment of its logical line
    if (window->is_focused && !found_cursor && (int)cursor_pos.line < (int)line_count) {
        for (int v = visual_map_count - 1; v >= 0; --v) {
            if (visual_map[v].line == (int)cursor_pos.line) {
                visual_cursor_row = visual_map[v].visual_row;
                visual_cursor_col = visual_map[v].visual_col_end;
                found_cursor = 1;
                break;
            }
        }
    }
    // If cursor is on an empty line, show it at the start of the line
    if (window->is_focused && !found_cursor && (int)cursor_pos.line < (int)line_count) {
        size_t len = vizero_buffer_get_line_length(buffer, cursor_pos.line);
        if (len == 0) {
            visual_cursor_row = 0;
            visual_cursor_col = 0;
            found_cursor = 1;
        }
    }
    /* Diagnostic underlines disabled - keeping only hover popup functionality */

    // Scroll vertically to keep the cursor visible
    if (window->is_focused && found_cursor) {
        int visible_rows = content_height / 16;
        if (visual_cursor_row < window->scroll_y) {
            window->scroll_y = visual_cursor_row;
        } else if (visual_cursor_row >= window->scroll_y + visible_rows) {
            window->scroll_y = visual_cursor_row - visible_rows + 1;
        }
        float cursor_x = (float)(content_x + line_number_width + (float)visual_cursor_col * 8.0f);
        float cursor_y = (float)(content_y + (float)(visual_cursor_row - window->scroll_y) * 16.0f);
        
        /* Get cursor colour from theme and check mode for special styling */
        vizero_colour_t cursor_colour = {1.0f, 1.0f, 0.0f, 0.5f}; // Default fallback
        void* theme_manager = vizero_editor_get_theme_manager(state);
        if (theme_manager) {
            const vizero_colour_theme_t* current_theme = vizero_theme_manager_get_current_theme((vizero_theme_manager_t*)theme_manager);
            if (current_theme) {
                cursor_colour = current_theme->cursor;
            }
        }
        
        /* Check mode for cursor styling */
        vizero_editor_mode_t current_mode = vizero_editor_get_mode(state);
        if (current_mode == VIZERO_MODE_COMMAND) {
            /* Yellow underline cursor for command mode */
            vizero_colour_t yellow_colour = {1.0f, 1.0f, 0.0f, 0.8f}; // Solid yellow
            vizero_renderer_fill_rect(renderer, cursor_x, cursor_y + 14.0f, 8.0f, 2.0f, yellow_colour);
        } else if (current_mode == VIZERO_MODE_INSERT) {
            /* Underline cursor for insert mode */
            vizero_renderer_fill_rect(renderer, cursor_x, cursor_y + 14.0f, 8.0f, 2.0f, cursor_colour);
        } else {
            /* Normal block cursor for normal/visual modes */
            vizero_renderer_fill_rect(renderer, cursor_x, cursor_y, 8.0f, 16.0f, cursor_colour);
        }
    }
}

/* Split window functionality */
int vizero_window_manager_split_horizontal(vizero_window_manager_t* manager, uint32_t window_id) {
    if (!manager || manager->window_count >= MAX_WINDOWS) return -1;
    
    /* Find the window to split */
    vizero_editor_window_t* original_window = vizero_window_manager_get_window_by_id(manager, window_id);
    if (!original_window) return -1;
    
    /* If already in split mode, don't allow further splits for now */
    if (manager->layout_type != VIZERO_LAYOUT_SINGLE) return -1;
    
    /* Create new window with same buffer index */
    vizero_editor_window_t* new_window = vizero_window_manager_create_window(
        manager, original_window->buffer_index, 0, 0, 100, 100);
    if (!new_window) return -1;
    
    /* Configure horizontal split layout */
    manager->layout_type = VIZERO_LAYOUT_HORIZONTAL;
    manager->primary_window_id = window_id;          /* Original window goes to top */
    manager->secondary_window_id = new_window->window_id; /* New window goes to bottom */
    manager->split_ratio = 0.5f;                     /* 50/50 split */
    
    /* Focus the new window */
    vizero_window_manager_set_focus(manager, new_window->window_id);
    
    /* Update layout to apply new dimensions */
    vizero_window_manager_update_layout(manager, manager->screen_width, manager->screen_height);
    
    return 0;
}

int vizero_window_manager_split_vertical(vizero_window_manager_t* manager, uint32_t window_id) {
    if (!manager || manager->window_count >= MAX_WINDOWS) return -1;
    
    /* Find the window to split */
    vizero_editor_window_t* original_window = vizero_window_manager_get_window_by_id(manager, window_id);
    if (!original_window) return -1;
    
    /* If already in split mode, don't allow further splits for now */
    if (manager->layout_type != VIZERO_LAYOUT_SINGLE) return -1;
    
    /* Create new window with same buffer index */
    vizero_editor_window_t* new_window = vizero_window_manager_create_window(
        manager, original_window->buffer_index, 0, 0, 100, 100);
    if (!new_window) return -1;
    
    /* Configure vertical split layout */
    manager->layout_type = VIZERO_LAYOUT_VERTICAL;
    manager->primary_window_id = window_id;          /* Original window goes to left */
    manager->secondary_window_id = new_window->window_id; /* New window goes to right */
    manager->split_ratio = 0.5f;                     /* 50/50 split */
    
    /* Focus the new window */
    vizero_window_manager_set_focus(manager, new_window->window_id);
    
    /* Update layout to apply new dimensions */
    vizero_window_manager_update_layout(manager, manager->screen_width, manager->screen_height);
    
    return 0;
}

int vizero_window_manager_close_split(vizero_window_manager_t* manager, uint32_t window_id) {
    if (!manager || manager->layout_type == VIZERO_LAYOUT_SINGLE) return -1;
    
    /* Determine which window to keep and which to close */
    uint32_t keep_window_id = (window_id == manager->primary_window_id) ? 
                              manager->secondary_window_id : manager->primary_window_id;
    
    /* Close the specified window */
    vizero_window_manager_destroy_window(manager, window_id);
    
    /* Reset to single layout */
    manager->layout_type = VIZERO_LAYOUT_SINGLE;
    manager->primary_window_id = keep_window_id;
    manager->secondary_window_id = 0;
    manager->focused_window_id = keep_window_id;
    
    /* Update layout to fullscreen */
    vizero_window_manager_update_layout(manager, manager->screen_width, manager->screen_height);
    
    return 0;
}

/* Layout management */
int vizero_window_manager_update_layout(vizero_window_manager_t* manager, int screen_width, int screen_height) {
    if (!manager) return -1;
    
    manager->screen_width = screen_width;
    manager->screen_height = screen_height;
    
    switch (manager->layout_type) {
        case VIZERO_LAYOUT_SINGLE: {
            /* Single window takes full screen minus status bar */
            vizero_editor_window_t* window = vizero_window_manager_get_focused_window(manager);
            if (window) {
                window->x = 0;
                window->y = 0;
                window->width = screen_width;
                window->height = screen_height - 24; /* Reserve 24px for status bar */
                window->is_visible = 1;
            }
            break;
        }
        
        case VIZERO_LAYOUT_HORIZONTAL: {
            /* Horizontal split (top/bottom) minus status bar */
            vizero_editor_window_t* primary = vizero_window_manager_get_window_by_id(manager, manager->primary_window_id);
            vizero_editor_window_t* secondary = vizero_window_manager_get_window_by_id(manager, manager->secondary_window_id);
            
            if (primary && secondary) {
                int available_height = screen_height - 24; /* Reserve 24px for status bar */
                int split_height = (int)(available_height * manager->split_ratio);
                
                /* Top window */
                primary->x = 0;
                primary->y = 0;
                primary->width = screen_width;
                primary->height = split_height - 1; /* -1 for border */
                primary->is_visible = 1;
                
                /* Bottom window */
                secondary->x = 0;
                secondary->y = split_height + 1; /* +1 for border */
                secondary->width = screen_width;
                secondary->height = available_height - split_height - 1;
                secondary->is_visible = 1;
            }
            break;
        }
        
        case VIZERO_LAYOUT_VERTICAL: {
            /* Vertical split (left/right) minus status bar */
            vizero_editor_window_t* primary = vizero_window_manager_get_window_by_id(manager, manager->primary_window_id);
            vizero_editor_window_t* secondary = vizero_window_manager_get_window_by_id(manager, manager->secondary_window_id);
            
            if (primary && secondary) {
                int available_height = screen_height - 24; /* Reserve 24px for status bar */
                int split_width = (int)(screen_width * manager->split_ratio);
                
                /* Left window */
                primary->x = 0;
                primary->y = 0;
                primary->width = split_width - 1; /* -1 for border */
                primary->height = available_height;
                primary->is_visible = 1;
                
                /* Right window */
                secondary->x = split_width + 1; /* +1 for border */
                secondary->y = 0;
                secondary->width = screen_width - split_width - 1;
                secondary->height = available_height;
                secondary->is_visible = 1;
            }
            break;
        }
    }
    
    return 0;
}

vizero_layout_type_t vizero_window_manager_get_layout_type(vizero_window_manager_t* manager) {
    return manager ? manager->layout_type : VIZERO_LAYOUT_SINGLE;
}

int vizero_window_manager_get_visible_windows(vizero_window_manager_t* manager, 
                                            vizero_editor_window_t*** windows, 
                                            size_t* count) {
    if (!manager || !windows || !count) return -1;
    
    /* Count visible windows */
    size_t visible_count = 0;
    for (size_t i = 0; i < manager->window_count; i++) {
        if (manager->windows[i] && manager->windows[i]->is_visible) {
            visible_count++;
        }
    }
    
    if (visible_count == 0) {
        *windows = NULL;
        *count = 0;
        return 0;
    }
    
    /* Allocate array for visible windows */
    vizero_editor_window_t** visible_windows = (vizero_editor_window_t**)malloc(
        visible_count * sizeof(vizero_editor_window_t*));
    if (!visible_windows) return -1;
    
    /* Fill array with visible windows */
    size_t index = 0;
    for (size_t i = 0; i < manager->window_count; i++) {
        if (manager->windows[i] && manager->windows[i]->is_visible) {
            visible_windows[index++] = manager->windows[i];
        }
    }
    
    *windows = visible_windows;
    *count = visible_count;
    return 0;
}