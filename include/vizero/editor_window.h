
// ...existing code...



#ifndef VIZERO_EDITOR_WINDOW_H
#define VIZERO_EDITOR_WINDOW_H

#ifdef __cplusplus
extern "C" {
#endif

#include <stddef.h>
#include <stdint.h>

/* Forward declarations */
typedef struct vizero_buffer_t vizero_buffer_t;
typedef struct vizero_cursor_t vizero_cursor_t;
typedef struct vizero_editor_window_t vizero_editor_window_t;
typedef struct vizero_window_manager_t vizero_window_manager_t;

/* Layout types for window splits */
typedef enum {
    VIZERO_LAYOUT_SINGLE,      /* Single window (fullscreen) */
    VIZERO_LAYOUT_HORIZONTAL,  /* Horizontal split (top/bottom) */
    VIZERO_LAYOUT_VERTICAL     /* Vertical split (left/right) */
} vizero_layout_type_t;

/* Window structure */
struct vizero_editor_window_t {
    /* Position and size */
    int x, y;
    int width, height;
    
    /* Associated buffer and cursor */
    vizero_buffer_t* buffer;
    vizero_cursor_t* cursor;
    
    /* Window state */
    int is_focused;
    int is_maximized;
    int is_visible;
    
    /* Window properties */
    char* title;
    int title_bar_height;
    
    /* Scrolling */
    int scroll_x, scroll_y;
    
    /* Resize properties */
    int is_resizable;
    int min_width, min_height;
    
    /* UI elements */
    int has_scrollbar;
    int has_title_bar;
    
    /* Window ID for identification */
    uint32_t window_id;
};
/* Helper functions for internal window manager access (for editor_state.cpp) */
size_t vizero_window_manager_get_window_count_raw(vizero_window_manager_t* manager);
vizero_editor_window_t* vizero_window_manager_get_window_raw(vizero_window_manager_t* manager, size_t idx);

/* Window manager functions */
vizero_window_manager_t* vizero_window_manager_create(void);
void vizero_window_manager_destroy(vizero_window_manager_t* manager);

/* Window lifecycle */
vizero_editor_window_t* vizero_window_manager_create_window(vizero_window_manager_t* manager, 
                                                           vizero_buffer_t* buffer,
                                                           int x, int y, int width, int height);
int vizero_window_manager_destroy_window(vizero_window_manager_t* manager, uint32_t window_id);
int vizero_window_manager_close_window(vizero_window_manager_t* manager, uint32_t window_id);

/* Window management */
vizero_editor_window_t* vizero_window_manager_get_focused_window(vizero_window_manager_t* manager);
int vizero_window_manager_set_focus(vizero_window_manager_t* manager, uint32_t window_id);
int vizero_window_manager_get_window_count(vizero_window_manager_t* manager);
vizero_editor_window_t* vizero_window_manager_get_window(vizero_window_manager_t* manager, size_t index);
vizero_editor_window_t* vizero_window_manager_get_window_by_id(vizero_window_manager_t* manager, uint32_t window_id);

/* Window properties */
int vizero_editor_window_set_title(vizero_editor_window_t* window, const char* title);
int vizero_editor_window_set_size(vizero_editor_window_t* window, int width, int height);
int vizero_editor_window_set_position(vizero_editor_window_t* window, int x, int y);
int vizero_editor_window_maximize(vizero_editor_window_t* window);
int vizero_editor_window_restore(vizero_editor_window_t* window);

/* Window state queries */
int vizero_editor_window_is_focused(vizero_editor_window_t* window);
int vizero_editor_window_is_maximized(vizero_editor_window_t* window);
const char* vizero_editor_window_get_title(vizero_editor_window_t* window);

/* Window content access */
vizero_buffer_t* vizero_editor_window_get_buffer(vizero_editor_window_t* window);
vizero_cursor_t* vizero_editor_window_get_cursor(vizero_editor_window_t* window);

/* Buffer and cursor access */
vizero_buffer_t* vizero_editor_window_get_buffer(vizero_editor_window_t* window);
vizero_cursor_t* vizero_editor_window_get_cursor(vizero_editor_window_t* window);
int vizero_editor_window_set_cursor(vizero_editor_window_t* window, vizero_cursor_t* cursor);

/* Rendering support */
int vizero_editor_window_get_content_area(vizero_editor_window_t* window, 
                                         int* content_x, int* content_y, 
                                         int* content_width, int* content_height);

/* Split window functionality */
int vizero_window_manager_split_horizontal(vizero_window_manager_t* manager, uint32_t window_id);
int vizero_window_manager_split_vertical(vizero_window_manager_t* manager, uint32_t window_id);
int vizero_window_manager_close_split(vizero_window_manager_t* manager, uint32_t window_id);

/* Layout management */
int vizero_window_manager_update_layout(vizero_window_manager_t* manager, int screen_width, int screen_height);
vizero_layout_type_t vizero_window_manager_get_layout_type(vizero_window_manager_t* manager);
int vizero_window_manager_get_visible_windows(vizero_window_manager_t* manager, 
                                            vizero_editor_window_t*** windows, 
                                            size_t* count);
// Place these after the typedefs and struct declarations:
/* Window focus helpers (for vi-style window switching) */
int vizero_window_manager_focus_direction(vizero_window_manager_t* manager, char dir);
int vizero_window_manager_focus_number(vizero_window_manager_t* manager, int number);
#ifdef __cplusplus
}
#endif

#endif /* VIZERO_EDITOR_WINDOW_H */