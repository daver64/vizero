#include "vizero/status_bar.h"
#include "vizero/renderer.h"
#include <string.h>
#include <stdio.h>

#ifdef __cplusplus
extern "C" {
#endif
void vizero_status_bar_render(vizero_status_bar_t *status_bar, vizero_renderer_t *renderer, int x, int y, vizero_color_t bg_color, vizero_color_t text_color) {
    if (!status_bar || !renderer) return;

    // Draw a slightly lighter blue background for the status bar
    vizero_color_t panel_bg = {0.28f, 0.34f, 0.48f, 1.0f}; // lighter blue
    vizero_renderer_fill_rect(renderer, (float)x, (float)y, (float)status_bar->width, 24.0f, panel_bg);

    // Draw the main status bar text
    vizero_text_info_t info = {
        (float)x,
        (float)y,
        text_color,
        NULL
    };
    vizero_renderer_draw_text(renderer, status_bar->rendered_text, &info);

    // Draw time/date panel, right-aligned if present
    if (status_bar->timedate_text[0] != '\0') {
        int timedate_len = (int)strlen(status_bar->timedate_text);
        int text_width = timedate_len * 8; // Approximate width, adjust if needed
        int right_x = x + status_bar->width - text_width - 4; // 4px right padding
        vizero_text_info_t td_info = {
            (float)right_x,
            (float)(y + 2),
            text_color,
            NULL
        };
        vizero_renderer_draw_text(renderer, status_bar->timedate_text, &td_info);
    }

    // Draw time/date panel, right-aligned if present
    if (status_bar->timedate_text[0] != '\0') {
        int timedate_len = (int)strlen(status_bar->timedate_text);
        int text_width = timedate_len * 8; // Approximate width, adjust if needed
        int right_x = x + status_bar->width - text_width - 4; // 4px right padding
        vizero_text_info_t td_info = {
            (float)right_x,
            (float)(y + 2),
            text_color,
            NULL
        };
        vizero_renderer_draw_text(renderer, status_bar->timedate_text, &td_info);
    }
}
#ifdef __cplusplus
}
#endif