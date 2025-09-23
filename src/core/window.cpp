#include "vizero/window.h"
#include <SDL.h>
#include <stdio.h>
#include <stdlib.h>

#ifdef _WIN32
#include <windows.h>
#include <SDL_syswm.h>
#endif

struct vizero_window_t {
    SDL_Window* sdl_window;
    SDL_GLContext gl_context;
    int should_close;
    int is_fullscreen;
    int windowed_width;
    int windowed_height;
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
    
#ifdef _WIN32
    /* Set window icon on Windows */
    SDL_SysWMinfo wmInfo;
    SDL_VERSION(&wmInfo.version);
    if (SDL_GetWindowWMInfo(window->sdl_window, &wmInfo)) {
        HWND hwnd = wmInfo.info.win.window;
        
        /* Load icon from resources */
        HICON icon = LoadIcon(GetModuleHandle(NULL), MAKEINTRESOURCE(101)); /* IDI_ICON1 = 101 */
        if (icon) {
            /* Set both large and small icons */
            SendMessage(hwnd, WM_SETICON, ICON_BIG, (LPARAM)icon);
            SendMessage(hwnd, WM_SETICON, ICON_SMALL, (LPARAM)icon);
        }
    }
#endif
    
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
    window->is_fullscreen = fullscreen;
    window->windowed_width = fullscreen ? 1024 : width;   /* Default windowed size */
    window->windowed_height = fullscreen ? 768 : height;
    
    return window;
}

vizero_window_t* vizero_window_create_with_position(const char* title, int x, int y, int width, int height, int fullscreen) {
    vizero_window_t* window = (vizero_window_t*)calloc(1, sizeof(vizero_window_t));
    if (!window) {
        return NULL;
    }
    
    Uint32 flags = SDL_WINDOW_OPENGL | SDL_WINDOW_RESIZABLE;
    if (fullscreen) {
        flags |= SDL_WINDOW_FULLSCREEN;
    }
    
    /* Use provided position, or center if invalid coordinates */
    int pos_x = (x >= 0) ? x : SDL_WINDOWPOS_CENTERED;
    int pos_y = (y >= 0) ? y : SDL_WINDOWPOS_CENTERED;
    
    window->sdl_window = SDL_CreateWindow(title, pos_x, pos_y, width, height, flags);
    if (!window->sdl_window) {
        fprintf(stderr, "Failed to create window: %s\n", SDL_GetError());
        free(window);
        return NULL;
    }
    
#ifdef _WIN32
    /* Set window icon on Windows */
    SDL_SysWMinfo wmInfo;
    SDL_VERSION(&wmInfo.version);
    if (SDL_GetWindowWMInfo(window->sdl_window, &wmInfo)) {
        HWND hwnd = wmInfo.info.win.window;
        
        /* Load icon from resources */
        HICON icon = LoadIcon(GetModuleHandle(NULL), MAKEINTRESOURCE(101)); /* IDI_ICON1 = 101 */
        if (icon) {
            /* Set both large and small icons */
            SendMessage(hwnd, WM_SETICON, ICON_BIG, (LPARAM)icon);
            SendMessage(hwnd, WM_SETICON, ICON_SMALL, (LPARAM)icon);
        }
    }
#endif
    
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
    window->is_fullscreen = fullscreen;
    window->windowed_width = fullscreen ? 1024 : width;   /* Default windowed size */
    window->windowed_height = fullscreen ? 768 : height;
    
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

void vizero_window_get_position(vizero_window_t* window, int* x, int* y) {
    if (!window) {
        if (x) *x = 0;
        if (y) *y = 0;
        return;
    }
    
    SDL_GetWindowPosition(window->sdl_window, x, y);
}

void vizero_window_set_position(vizero_window_t* window, int x, int y) {
    if (!window) {
        return;
    }
    
    SDL_SetWindowPosition(window->sdl_window, x, y);
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

int vizero_window_is_fullscreen(vizero_window_t* window) {
    return window ? window->is_fullscreen : 0;
}

int vizero_window_is_maximized(vizero_window_t* window) {
    if (!window || !window->sdl_window) {
        return 0;
    }
    
    Uint32 flags = SDL_GetWindowFlags(window->sdl_window);
    return (flags & SDL_WINDOW_MAXIMIZED) != 0;
}

void vizero_window_toggle_fullscreen(vizero_window_t* window) {
    if (!window || !window->sdl_window) return;
    
    if (window->is_fullscreen) {
        /* Switch to windowed mode */
        SDL_SetWindowFullscreen(window->sdl_window, 0);
        SDL_SetWindowSize(window->sdl_window, window->windowed_width, window->windowed_height);
        SDL_SetWindowPosition(window->sdl_window, SDL_WINDOWPOS_CENTERED, SDL_WINDOWPOS_CENTERED);
        window->is_fullscreen = 0;
    } else {
        /* Store current windowed size before going fullscreen */
        SDL_GetWindowSize(window->sdl_window, &window->windowed_width, &window->windowed_height);
        
        /* Switch to fullscreen mode */
        SDL_SetWindowFullscreen(window->sdl_window, SDL_WINDOW_FULLSCREEN_DESKTOP);
        window->is_fullscreen = 1;
    }
}