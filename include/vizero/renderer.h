#ifndef VIZERO_RENDERER_H
#define VIZERO_RENDERER_H

#ifdef __cplusplus
extern "C" {
#endif

#include <stdint.h>

/* Forward declarations */
typedef struct vizero_renderer_t vizero_renderer_t;
typedef struct vizero_window_t vizero_window_t;
typedef struct vizero_font_t vizero_font_t;

/* Colour structure */
typedef struct {
    float r, g, b, a;
} vizero_colour_t;

/* Text rendering info */
typedef struct {
    float x, y;
    vizero_colour_t colour;
    vizero_font_t* font;
} vizero_text_info_t;

/* Renderer creation and destruction */
vizero_renderer_t* vizero_renderer_create(vizero_window_t* window);
void vizero_renderer_destroy(vizero_renderer_t* renderer);

/* Rendering operations */
void vizero_renderer_clear(vizero_renderer_t* renderer, vizero_colour_t colour);
void vizero_renderer_present(vizero_renderer_t* renderer);
void vizero_renderer_update_viewport(vizero_renderer_t* renderer, int width, int height);

/* Text rendering */
void vizero_renderer_draw_text(vizero_renderer_t* renderer, const char* text, vizero_text_info_t* info);
void vizero_renderer_get_text_size(vizero_renderer_t* renderer, const char* text, vizero_font_t* font, float* width, float* height);

/* Rectangle rendering */
void vizero_renderer_draw_rect(vizero_renderer_t* renderer, float x, float y, float width, float height, vizero_colour_t colour);
void vizero_renderer_fill_rect(vizero_renderer_t* renderer, float x, float y, float width, float height, vizero_colour_t colour);

/* Line rendering */
void vizero_renderer_draw_line(vizero_renderer_t* renderer, float x1, float y1, float x2, float y2, vizero_colour_t colour);

/* Font management */
vizero_font_t* vizero_font_load(const char* path, int size);
void vizero_font_destroy(vizero_font_t* font);

/* Image management */
typedef struct vizero_image_t {
    unsigned int texture; /* GLuint but avoiding GL header dependency */
    int width;
    int height;
} vizero_image_t;

vizero_image_t* vizero_image_load(const char* path);
void vizero_image_destroy(vizero_image_t* image);
void vizero_renderer_draw_image(vizero_renderer_t* renderer, vizero_image_t* image, float x, float y, float width, float height);

#ifdef __cplusplus
}
#endif

#endif /* VIZERO_RENDERER_H */