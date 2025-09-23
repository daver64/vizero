#ifndef VIZERO_WINDOW_H
#define VIZERO_WINDOW_H

#ifdef __cplusplus
extern "C" {
#endif

#include <stdint.h>
#include <SDL.h>

/* Forward declarations */
typedef struct vizero_window_t vizero_window_t;

/* Window creation and destruction */
vizero_window_t* vizero_window_create(const char* title, int width, int height, int fullscreen);
vizero_window_t* vizero_window_create_with_position(const char* title, int x, int y, int width, int height, int fullscreen);
void vizero_window_destroy(vizero_window_t* window);

/* Window properties */
void vizero_window_get_size(vizero_window_t* window, int* width, int* height);
void vizero_window_get_position(vizero_window_t* window, int* x, int* y);
void vizero_window_set_size(vizero_window_t* window, int width, int height);
void vizero_window_set_position(vizero_window_t* window, int x, int y);
void vizero_window_set_title(vizero_window_t* window, const char* title);
int vizero_window_should_close(vizero_window_t* window);
void vizero_window_set_should_close(vizero_window_t* window, int should_close);
int vizero_window_is_fullscreen(vizero_window_t* window);
int vizero_window_is_maximized(vizero_window_t* window);
void vizero_window_toggle_fullscreen(vizero_window_t* window);

/* Window operations */
void vizero_window_swap_buffers(vizero_window_t* window);
void vizero_window_poll_events(vizero_window_t* window);

/* Internal access */
SDL_Window* vizero_window_get_sdl_window(vizero_window_t* window);
SDL_GLContext vizero_window_get_gl_context(vizero_window_t* window);

#ifdef __cplusplus
}
#endif

#endif /* VIZERO_WINDOW_H */