#include "vizero/status_bar.h"
#include "vizero/renderer.h"
#include "vizero/editor_state.h"
#include <string.h>
#include <stdio.h>
#include <SDL.h>

#ifdef __cplusplus
extern "C" {
#endif
void vizero_status_bar_render(vizero_status_bar_t *status_bar, vizero_renderer_t *renderer, int x, int y, vizero_colour_t bg_colour, vizero_colour_t text_colour) {
    (void)bg_colour; /* Use a fixed colour for now */
    if (!status_bar || !renderer) return;

    // Draw a slightly lighter blue background for the status bar
    vizero_colour_t panel_bg = {0.28f, 0.34f, 0.48f, 1.0f}; // lighter blue
    vizero_renderer_fill_rect(renderer, (float)x, (float)y, (float)status_bar->width, 24.0f, panel_bg);

    // Draw the main status bar text
    vizero_text_info_t info = {
        (float)x,
        (float)y,
        text_colour,
        NULL
    };
    vizero_renderer_draw_text(renderer, status_bar->rendered_text, &info);
    
    // Add flashing cursor for command mode
    // Note: We need to detect command mode here, but we don't have access to editor state
    // So we'll check if the text starts with ":"
    if (status_bar->rendered_text && status_bar->rendered_text[0] == ':') {
        // Calculate cursor position at end of command text
        int text_len = (int)strlen(status_bar->rendered_text);
        float cursor_x = (float)x + (text_len * 8.0f); // 8px per character
        float cursor_y = (float)y;
        
        // Create a flashing effect using SDL ticks
        uint32_t ticks = SDL_GetTicks();
        if ((ticks / 500) % 2 == 0) { // Flash every 500ms
            vizero_colour_t cursor_colour = {1.0f, 1.0f, 0.0f, 0.8f}; // Yellow
            vizero_renderer_fill_rect(renderer, cursor_x, cursor_y + 18.0f, 8.0f, 2.0f, cursor_colour);
        }
    }
    
    // Special handling for coloured panels - find and re-render them
    for (size_t i = 0; i < status_bar->panel_count; i++) {
        vizero_status_panel_t *panel = &status_bar->panels[i];
        if (panel->enabled && panel->has_custom_colour && panel->type == VIZERO_PANEL_READONLY_STATUS) {
            // Find the position of this panel in the rendered text
            // For simplicity, look for " ro " or " rw " patterns
            const char *panel_text = (panel->custom_colour.r > panel->custom_colour.g) ? " ro " : " rw ";
            char *pos = strstr(status_bar->rendered_text, panel_text);
            if (pos) {
                // Calculate the x position of this text
                int offset = (int)(pos - status_bar->rendered_text);
                float panel_x = (float)x + (offset * 8.0f); // Approximate 8px per character
                
                // Re-render this text with the custom colour
                vizero_text_info_t coloured_info = {
                    panel_x,
                    (float)y,
                    panel->custom_colour,
                    NULL
                };
                vizero_renderer_draw_text(renderer, panel_text, &coloured_info);
            }
        }
    }

    // Draw time/date panel, right-aligned if present
    if (status_bar->timedate_text[0] != '\0') {
        int timedate_len = (int)strlen(status_bar->timedate_text);
        int text_width = timedate_len * 8; // Approximate width, adjust if needed
        int right_x = x + status_bar->width - text_width - 4; // 4px right padding
        vizero_text_info_t td_info = {
            (float)right_x,
            (float)(y + 2),
            text_colour,
            NULL
        };
        vizero_renderer_draw_text(renderer, status_bar->timedate_text, &td_info);
    }
}
#ifdef __cplusplus
}
#endif