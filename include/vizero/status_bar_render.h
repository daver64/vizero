#ifndef VIZERO_STATUS_BAR_RENDER_H
#define VIZERO_STATUS_BAR_RENDER_H

#include "vizero/status_bar.h"
#include "vizero/renderer.h"
#include "vizero/editor_state.h"
#include <stdint.h>

/* C linkage for C++ */
#ifdef __cplusplus
extern "C" {
#endif
void vizero_status_bar_render(vizero_status_bar_t *status_bar, vizero_renderer_t *renderer, int x, int y, uint32_t text_color);
#ifdef __cplusplus
}
#endif

#endif // VIZERO_STATUS_BAR_RENDER_H
