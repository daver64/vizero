#ifndef VIZERO_STATUS_BAR_H
#define VIZERO_STATUS_BAR_H

#ifdef __cplusplus
extern "C" {
#endif

#include <stddef.h>
#include "renderer.h"

/* Forward declarations */
typedef struct vizero_status_bar_t vizero_status_bar_t;
typedef struct vizero_editor_state_t vizero_editor_state_t;

typedef enum {
    VIZERO_PANEL_FILENAME,
    VIZERO_PANEL_CURSOR_POSITION,
    VIZERO_PANEL_MODE,
    VIZERO_PANEL_TIME_DATE,
    VIZERO_PANEL_BUFFER_INFO,
    VIZERO_PANEL_READONLY_STATUS,
    VIZERO_PANEL_CUSTOM
} vizero_panel_type_t;

typedef enum {
    VIZERO_ALIGN_LEFT,
    VIZERO_ALIGN_CENTER,
    VIZERO_ALIGN_RIGHT
} vizero_panel_align_t;

typedef struct {
    vizero_panel_type_t type;
    vizero_panel_align_t alignment;
    int enabled;
    int min_width;
    char* custom_text;
    vizero_color_t custom_color;
    int has_custom_color;
} vizero_status_panel_t;

struct vizero_status_bar_t {
    vizero_status_panel_t panels[16];
    size_t panel_count;
    int width;
    int height;
    char* rendered_text;
    size_t rendered_capacity;
    char timedate_text[256];
};

/* Status bar creation and destruction */
vizero_status_bar_t* vizero_status_bar_create(int width, int height);
void vizero_status_bar_destroy(vizero_status_bar_t* status_bar);

/* Panel management */
int vizero_status_bar_add_panel(vizero_status_bar_t* status_bar, vizero_panel_type_t type, vizero_panel_align_t alignment, int min_width);
int vizero_status_bar_remove_panel(vizero_status_bar_t* status_bar, size_t panel_index);
int vizero_status_bar_set_panel_enabled(vizero_status_bar_t* status_bar, size_t panel_index, int enabled);
int vizero_status_bar_set_custom_text(vizero_status_bar_t* status_bar, size_t panel_index, const char* text);

/* Status bar updates */
void vizero_status_bar_update(vizero_status_bar_t* status_bar, vizero_editor_state_t* editor);
void vizero_status_bar_resize(vizero_status_bar_t* status_bar, int width, int height);

/* Rendering */
void vizero_status_bar_render(vizero_status_bar_t* status_bar, vizero_renderer_t* renderer, int x, int y, vizero_color_t bg_color, vizero_color_t text_color);

/* Panel information */
size_t vizero_status_bar_get_panel_count(vizero_status_bar_t* status_bar);
vizero_status_panel_t* vizero_status_bar_get_panel(vizero_status_bar_t* status_bar, size_t index);

#ifdef __cplusplus
}
#endif

#endif /* VIZERO_STATUS_BAR_H */
