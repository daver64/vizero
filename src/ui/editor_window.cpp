
#include <ctype.h>


/* Multi-window editor system implementation */
#include "vizero/editor_window.h"
#include "vizero/buffer.h"
#include "vizero/cursor.h"
#include <stdlib.h>
#include <string.h>
#include <stdio.h>

#ifdef _WIN32
#define strdup _strdup
#endif

#define MAX_WINDOWS 32
#define DEFAULT_TITLE_BAR_HEIGHT 24
#define DEFAULT_MIN_WIDTH 200
#define DEFAULT_MIN_HEIGHT 100

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
    if (!manager) return -1;
    // Only two windows supported for now: horizontal or vertical split
    if (manager->window_count < 2) return -1;
    vizero_editor_window_t* focused = vizero_window_manager_get_focused_window(manager);
    if (!focused) return -1;
    vizero_editor_window_t* other = NULL;
    // Find the other window
    for (int i = 0; i < manager->window_count; ++i) {
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
    
    /* Destroy all windows */
    for (size_t i = 0; i < manager->window_count; i++) {
        if (manager->windows[i]) {
            if (manager->windows[i]->title) {
                free(manager->windows[i]->title);
            }
            if (manager->windows[i]->cursor) {
                vizero_cursor_destroy(manager->windows[i]->cursor);
            }
            free(manager->windows[i]);
        }
    }
    
    free(manager);
}

/* Window lifecycle */
vizero_editor_window_t* vizero_window_manager_create_window(vizero_window_manager_t* manager,
                                                           vizero_buffer_t* buffer,
                                                           int x, int y, int width, int height) {
    if (!manager || manager->window_count >= MAX_WINDOWS) return NULL;
    
    vizero_editor_window_t* window = (vizero_editor_window_t*)calloc(1, sizeof(vizero_editor_window_t));
    if (!window) return NULL;
    
    /* Initialize window properties */
    window->x = x;
    window->y = y;
    window->width = width;
    window->height = height;
    window->buffer = buffer;
    
    /* Create cursor for the buffer if we have one */
    if (buffer) {
        window->cursor = vizero_cursor_create(buffer);
    } else {
        window->cursor = NULL;
    }
    
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
    
    /* Set default title */
    if (buffer) {
        const char* filename = vizero_buffer_get_filename(buffer);
        if (filename) {
            window->title = strdup(filename);
        } else {
            char default_title[64];
            sprintf(default_title, "[No Name %u]", window->window_id);
            window->title = strdup(default_title);
        }
    } else {
        window->title = strdup("[Empty]");
    }
    
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
    if (window->cursor) {
        vizero_cursor_destroy(window->cursor);
    }
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

int vizero_window_manager_close_window(vizero_window_manager_t* manager, uint32_t window_id) {
    if (!manager) return -1;
    
    vizero_editor_window_t* window = vizero_window_manager_get_window_by_id(manager, window_id);
    if (!window) return -1;
    
    /* Check if buffer has unsaved changes */
    if (window->buffer && vizero_buffer_is_modified(window->buffer)) {
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
int vizero_editor_window_set_title(vizero_editor_window_t* window, const char* title) {
    if (!window) return -1;
    
    if (window->title) {
        free(window->title);
    }
    
    window->title = title ? strdup(title) : strdup("[Untitled]");
    return 0;
}

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
vizero_buffer_t* vizero_editor_window_get_buffer(vizero_editor_window_t* window) {
    return window ? window->buffer : NULL;
}

vizero_cursor_t* vizero_editor_window_get_cursor(vizero_editor_window_t* window) {
    return window ? window->cursor : NULL;
}

int vizero_editor_window_set_cursor(vizero_editor_window_t* window, vizero_cursor_t* cursor) {
    if (!window) return 0;
    window->cursor = cursor;
    return 1;
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

/* Split window functionality */
int vizero_window_manager_split_horizontal(vizero_window_manager_t* manager, uint32_t window_id) {
    if (!manager || manager->layout_type != VIZERO_LAYOUT_SINGLE) return -1;
    
    /* Find the window to split */
    vizero_editor_window_t* original_window = vizero_window_manager_get_window_by_id(manager, window_id);
    if (!original_window) return -1;
    
    /* Create a new window with the same buffer */
    vizero_editor_window_t* new_window = vizero_window_manager_create_window(
        manager, original_window->buffer, 0, 0, 400, 300);
    if (!new_window) return -1;
    
    /* Configure split layout */
    manager->layout_type = VIZERO_LAYOUT_HORIZONTAL;
    manager->primary_window_id = window_id;
    manager->secondary_window_id = new_window->window_id;
    manager->split_ratio = 0.5f;
    
    /* Update layout */
    vizero_window_manager_update_layout(manager, manager->screen_width, manager->screen_height);
    
    return 0;
}

int vizero_window_manager_split_vertical(vizero_window_manager_t* manager, uint32_t window_id) {
    if (!manager || manager->layout_type != VIZERO_LAYOUT_SINGLE) return -1;
    
    /* Find the window to split */
    vizero_editor_window_t* original_window = vizero_window_manager_get_window_by_id(manager, window_id);
    if (!original_window) return -1;
    
    /* Create a new window with the same buffer */
    vizero_editor_window_t* new_window = vizero_window_manager_create_window(
        manager, original_window->buffer, 0, 0, 400, 300);
    if (!new_window) return -1;
    
    /* Configure split layout */
    manager->layout_type = VIZERO_LAYOUT_VERTICAL;
    manager->primary_window_id = window_id;
    manager->secondary_window_id = new_window->window_id;
    manager->split_ratio = 0.5f;
    
    /* Update layout */
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