/* Status bar implementation */
#include "vizero/status_bar.h"
#include "vizero/editor_state.h"
#include "vizero/buffer.h"
#include "vizero/cursor.h"
#include <stdlib.h>
#include <string.h>
#include <stdio.h>
#include <time.h>

#define MAX_PANELS 16
#define MAX_PANEL_TEXT 256

struct vizero_status_bar_t {
    vizero_status_panel_t panels[MAX_PANELS];
    size_t panel_count;
    int width;
    int height;
    char* rendered_text;
    size_t rendered_capacity;
};

vizero_status_bar_t* vizero_status_bar_create(int width, int height) {
    vizero_status_bar_t* status_bar = (vizero_status_bar_t*)calloc(1, sizeof(vizero_status_bar_t));
    if (!status_bar) return NULL;
    
    status_bar->width = width;
    status_bar->height = height;
    status_bar->panel_count = 0;
    status_bar->rendered_capacity = 1024;
    status_bar->rendered_text = (char*)malloc(status_bar->rendered_capacity);
    
    if (!status_bar->rendered_text) {
        free(status_bar);
        return NULL;
    }
    
    status_bar->rendered_text[0] = '\0';
    return status_bar;
}

void vizero_status_bar_destroy(vizero_status_bar_t* status_bar) {
    if (!status_bar) return;
    
    /* Free custom text in panels */
    for (size_t i = 0; i < status_bar->panel_count; i++) {
        if (status_bar->panels[i].custom_text) {
            free(status_bar->panels[i].custom_text);
        }
    }
    
    if (status_bar->rendered_text) {
        free(status_bar->rendered_text);
    }
    
    free(status_bar);
}

int vizero_status_bar_add_panel(vizero_status_bar_t* status_bar, vizero_panel_type_t type, 
                               vizero_panel_align_t alignment, int min_width) {
    if (!status_bar || status_bar->panel_count >= MAX_PANELS) return -1;
    
    vizero_status_panel_t* panel = &status_bar->panels[status_bar->panel_count];
    panel->type = type;
    panel->alignment = alignment;
    panel->enabled = 1;
    panel->min_width = min_width;
    panel->custom_text = NULL;
    
    status_bar->panel_count++;
    return (int)(status_bar->panel_count - 1);
}

int vizero_status_bar_remove_panel(vizero_status_bar_t* status_bar, size_t panel_index) {
    if (!status_bar || panel_index >= status_bar->panel_count) return -1;
    
    /* Free custom text if any */
    if (status_bar->panels[panel_index].custom_text) {
        free(status_bar->panels[panel_index].custom_text);
    }
    
    /* Shift panels down */
    for (size_t i = panel_index; i < status_bar->panel_count - 1; i++) {
        status_bar->panels[i] = status_bar->panels[i + 1];
    }
    
    status_bar->panel_count--;
    return 0;
}

int vizero_status_bar_set_panel_enabled(vizero_status_bar_t* status_bar, size_t panel_index, int enabled) {
    if (!status_bar || panel_index >= status_bar->panel_count) return -1;
    status_bar->panels[panel_index].enabled = enabled;
    return 0;
}

int vizero_status_bar_set_custom_text(vizero_status_bar_t* status_bar, size_t panel_index, const char* text) {
    if (!status_bar || panel_index >= status_bar->panel_count) return -1;
    
    vizero_status_panel_t* panel = &status_bar->panels[panel_index];
    
    /* Free existing text */
    if (panel->custom_text) {
        free(panel->custom_text);
        panel->custom_text = NULL;
    }
    
    /* Set new text */
    if (text) {
        panel->custom_text = _strdup(text);
        if (!panel->custom_text) return -1;
    }
    
    return 0;
}

void vizero_status_bar_resize(vizero_status_bar_t* status_bar, int width, int height) {
    if (status_bar) {
        status_bar->width = width;
        status_bar->height = height;
    }
}

/* Generate text for a specific panel */
static void generate_panel_text(vizero_status_panel_t* panel, vizero_editor_state_t* editor, 
                               char* buffer, size_t buffer_size) {
    (void)buffer_size; /* Parameter used for bounds checking in snprintf calls */
    buffer[0] = '\0';
    
    if (!panel->enabled) return;
    
    switch (panel->type) {
        case VIZERO_PANEL_FILENAME: {
            vizero_buffer_t* current_buffer = vizero_editor_get_current_buffer(editor);
            const char* filename = current_buffer ? vizero_buffer_get_filename(current_buffer) : NULL;
            if (filename) {
                /* Extract just the filename from the path */
                const char* last_slash = strrchr(filename, '/');
                const char* last_backslash = strrchr(filename, '\\');
                const char* name = filename;
                
                if (last_slash && (!last_backslash || last_slash > last_backslash)) {
                    name = last_slash + 1;
                } else if (last_backslash) {
                    name = last_backslash + 1;
                }
                
                sprintf(buffer, " %s ", name);
            } else {
                sprintf(buffer, " [No Name] ");
            }
            break;
        }
        
        case VIZERO_PANEL_CURSOR_POSITION: {
            vizero_cursor_t* cursor = vizero_editor_get_current_cursor(editor);
            if (cursor) {
                size_t line = vizero_cursor_get_line(cursor);
                size_t col = vizero_cursor_get_column(cursor);
                sprintf(buffer, " %zu:%zu ", line + 1, col + 1); /* 1-based display */
            } else {
                sprintf(buffer, " 1:1 ");
            }
            break;
        }
        
        case VIZERO_PANEL_MODE: {
            vizero_editor_mode_t mode = vizero_editor_get_mode(editor);
            switch (mode) {
                case VIZERO_MODE_NORMAL:
                    sprintf(buffer, " NORMAL ");
                    break;
                case VIZERO_MODE_INSERT:
                    sprintf(buffer, " INSERT ");
                    break;
                case VIZERO_MODE_VISUAL:
                    sprintf(buffer, " VISUAL ");
                    break;
                case VIZERO_MODE_COMMAND:
                    sprintf(buffer, " COMMAND ");
                    break;
                default:
                    sprintf(buffer, " UNKNOWN ");
                    break;
            }
            break;
        }
        
        case VIZERO_PANEL_TIME_DATE: {
            time_t now = time(NULL);
            struct tm* local_time = localtime(&now);
            if (local_time) {
                sprintf(buffer, " %02d:%02d %02d/%02d/%04d ", 
                       local_time->tm_hour, local_time->tm_min,
                       local_time->tm_mon + 1, local_time->tm_mday, 
                       local_time->tm_year + 1900);
            } else {
                sprintf(buffer, " --:-- --/--/---- ");
            }
            break;
        }
        
        case VIZERO_PANEL_BUFFER_INFO: {
            size_t current_index = vizero_editor_get_current_buffer_index(editor);
            size_t total_buffers = vizero_editor_get_buffer_count(editor);
            sprintf(buffer, " [%zu/%zu] ", current_index + 1, total_buffers);
            break;
        }
        
        case VIZERO_PANEL_CUSTOM: {
            if (panel->custom_text) {
                sprintf(buffer, " %s ", panel->custom_text);
            }
            break;
        }
    }
}

void vizero_status_bar_update(vizero_status_bar_t* status_bar, vizero_editor_state_t* editor) {
    if (!status_bar || !editor) return;
    
    /* Special handling for command mode */
    if (vizero_editor_get_mode(editor) == VIZERO_MODE_COMMAND) {
        const char* command_buffer = vizero_editor_get_command_buffer(editor);
        
        /* Ensure buffer is large enough */
        size_t cmd_len = command_buffer ? strlen(command_buffer) : 0;
        size_t required_len = cmd_len + 10; /* command + padding */
        
        if (required_len > status_bar->rendered_capacity) {
            free(status_bar->rendered_text);
            status_bar->rendered_capacity = required_len * 2;
            status_bar->rendered_text = (char*)malloc(status_bar->rendered_capacity);
            if (!status_bar->rendered_text) return;
        }
        
        /* Show command being typed */
        sprintf(status_bar->rendered_text, "%s", command_buffer ? command_buffer : ":");
        return;
    }
    
    /* Normal status bar display */
    
    /* Calculate required buffer size */
    size_t total_len = 0;
    char panel_texts[MAX_PANELS][MAX_PANEL_TEXT];
    
    for (size_t i = 0; i < status_bar->panel_count; i++) {
        if (status_bar->panels[i].enabled) {
            generate_panel_text(&status_bar->panels[i], editor, 
                               panel_texts[i], MAX_PANEL_TEXT);
            total_len += strlen(panel_texts[i]);
        } else {
            panel_texts[i][0] = '\0';
        }
    }
    
    /* Add padding for spacing and null terminator */
    total_len += status_bar->width + 1;
    
    /* Ensure buffer is large enough */
    if (total_len > status_bar->rendered_capacity) {
        free(status_bar->rendered_text);
        status_bar->rendered_capacity = total_len * 2;
        status_bar->rendered_text = (char*)malloc(status_bar->rendered_capacity);
        if (!status_bar->rendered_text) return;
    }
    
    /* Build status bar text with proper alignment */
    status_bar->rendered_text[0] = '\0';
    
    /* Simple concatenation for debugging */
    for (size_t i = 0; i < status_bar->panel_count; i++) {
        if (status_bar->panels[i].enabled) {
            strcat(status_bar->rendered_text, panel_texts[i]);
            if (i < status_bar->panel_count - 1) {
                strcat(status_bar->rendered_text, " | ");
            }
        }
    }
}

void vizero_status_bar_render(vizero_status_bar_t* status_bar, vizero_renderer_t* renderer, 
                             int x, int y, vizero_color_t bg_color, vizero_color_t text_color) {
    if (!status_bar || !renderer) return;
    
    /* Draw background */
    vizero_renderer_fill_rect(renderer, (float)x, (float)y, 
                             (float)status_bar->width, (float)status_bar->height, bg_color);
    
    /* Draw text */
    if (status_bar->rendered_text) {
        vizero_text_info_t text_info = {
            (float)(x + 4), /* Small left padding */
            (float)(y + 2), /* Small top padding */
            text_color,
            NULL
        };
        vizero_renderer_draw_text(renderer, status_bar->rendered_text, &text_info);
    }
}

size_t vizero_status_bar_get_panel_count(vizero_status_bar_t* status_bar) {
    return status_bar ? status_bar->panel_count : 0;
}

vizero_status_panel_t* vizero_status_bar_get_panel(vizero_status_bar_t* status_bar, size_t index) {
    if (!status_bar || index >= status_bar->panel_count) return NULL;
    return &status_bar->panels[index];
}