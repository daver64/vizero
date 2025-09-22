#include "vizero/window.h"
#include <SDL.h>
#include <stdio.h>
#include <stdlib.h>

struct vizero_window_t {
    SDL_Window* sdl_window;
    SDL_GLContext gl_context;
    int should_close;
};

vizero_window_t* vizero_window_create(const char* title, int width, int height, int fullscreen) {
    vizero_window_t* window = (vizero_window_t*)calloc(1, sizeof(vizero_window_t));
    if (!window) {
        return NULL;
    }
    
    Uint32 flags = SDL_WINDOW_OPENGL | SDL_WINDOW_RESIZABLE;
    if (fullscreen) {
        flags |= SDL_WINDOW_FULLSCREEN;
    }
    
    window->sdl_window = SDL_CreateWindow(title, 
                                         SDL_WINDOWPOS_CENTERED, 
                                         SDL_WINDOWPOS_CENTERED,
                                         width, height, flags);
    if (!window->sdl_window) {
        fprintf(stderr, "Failed to create window: %s\n", SDL_GetError());
        free(window);
        return NULL;
    }
    
    window->gl_context = SDL_GL_CreateContext(window->sdl_window);
    if (!window->gl_context) {
        fprintf(stderr, "Failed to create OpenGL context: %s\n", SDL_GetError());
        SDL_DestroyWindow(window->sdl_window);
        free(window);
        return NULL;
    }
    
    SDL_GL_MakeCurrent(window->sdl_window, window->gl_context);
    SDL_GL_SetSwapInterval(1); /* Enable VSync */
    
    window->should_close = 0;
    return window;
}

void vizero_window_destroy(vizero_window_t* window) {
    if (!window) {
        return;
    }
    
    if (window->gl_context) {
        SDL_GL_DeleteContext(window->gl_context);
    }
    
    if (window->sdl_window) {
        SDL_DestroyWindow(window->sdl_window);
    }
    
    free(window);
}

void vizero_window_get_size(vizero_window_t* window, int* width, int* height) {
    if (!window) {
        if (width) *width = 0;
        if (height) *height = 0;
        return;
    }
    
    SDL_GetWindowSize(window->sdl_window, width, height);
}

void vizero_window_set_size(vizero_window_t* window, int width, int height) {
    if (!window) {
        return;
    }
    
    SDL_SetWindowSize(window->sdl_window, width, height);
}

void vizero_window_set_title(vizero_window_t* window, const char* title) {
    if (!window) {
        return;
    }
    
    SDL_SetWindowTitle(window->sdl_window, title);
}

int vizero_window_should_close(vizero_window_t* window) {
    return window ? window->should_close : 1;
}

void vizero_window_set_should_close(vizero_window_t* window, int should_close) {
    if (window) {
        window->should_close = should_close;
    }
}

void vizero_window_swap_buffers(vizero_window_t* window) {
    if (!window) {
        return;
    }
    
    SDL_GL_SwapWindow(window->sdl_window);
}

void vizero_window_poll_events(vizero_window_t* window) {
    /* Event polling is handled by input manager */
    (void)window;
}

SDL_Window* vizero_window_get_sdl_window(vizero_window_t* window) {
    return window ? window->sdl_window : NULL;
}

SDL_GLContext vizero_window_get_gl_context(vizero_window_t* window) {
    return window ? window->gl_context : NULL;
}