#include "vizero/application.h"
#include "vizero/window.h"
#include "vizero/renderer.h"
#include "vizero/input_manager.h"
#include "vizero/editor_state.h"
#include "vizero/editor_window.h"
#include "vizero/plugin_manager.h"
#include "vizero/colour_theme.h"
#include "vizero/session.h"
#include "vizero/status_bar.h"
#include "vizero/buffer.h"
#include "vizero/cursor.h"
#include "vizero/string_utils.h"
#include "vizero/settings.h"
#include "vizero/search.h"
#include "vizero/filewatch_poll.h"
#include "vizero/file_utils.h"
#include <SDL.h>
#include <GL/glew.h>
#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <time.h>


struct vizero_application_t {
    vizero_window_t* window;
    vizero_renderer_t* renderer;
    vizero_input_manager_t* input;
    vizero_editor_state_t* editor;
    vizero_plugin_manager_t* plugin_manager;
    vizero_theme_manager_t* theme_manager;
    void* session_manager; /* vizero_session_manager_t* - void* to avoid circular dependency */
    vizero_status_bar_t* status_bar;
    int should_quit;
    vizero_app_config_t config;
    
    /* Scrolling state */
    int scroll_x;
    int scroll_y;
    
    /* Welcome message state */
    int show_welcome;
    
    /* Logo display */
    vizero_image_t* logo_image;
    
    /* Temporary settings during initialization */
    vizero_settings_t* settings;
};
/* Forward declaration for syntax line rendering - currently unused */
#if 0
static void render_line_with_syntax(vizero_application_t* app, const char* line_text, size_t line_num,
                                   float base_x, float y, int scroll_x,
                                   vizero_syntax_token_t* tokens, size_t token_count);
#endif
vizero_application_t* vizero_application_create(const vizero_app_config_t* config) {
    vizero_application_t* app = (vizero_application_t*)calloc(1, sizeof(vizero_application_t));
    if (!app) {
        return NULL;
    }
    
    /* Copy config */
    app->config = *config;
    app->should_quit = 0;
    app->scroll_x = 0;
    app->scroll_y = 0;
    app->show_welcome = 1; /* Show welcome message initially */
    app->logo_image = NULL;
    
    return app;
}

void vizero_application_destroy(vizero_application_t* app) {
    if (!app) {
        return;
    }
    
    /* Clean up logo image */
    if (app->logo_image) {
        vizero_image_destroy(app->logo_image);
    }
    
    free(app);
}

int vizero_application_initialize(vizero_application_t* app) {
    if (!app) {
        return -1;
    }
    
    /* Initialize SDL */
    if (SDL_Init(SDL_INIT_VIDEO | SDL_INIT_EVENTS) < 0) {
        fprintf(stderr, "Failed to initialize SDL: %s\n", SDL_GetError());
        return -1;
    }
    
    /* Set OpenGL attributes */
    SDL_GL_SetAttribute(SDL_GL_CONTEXT_MAJOR_VERSION, 3);
    SDL_GL_SetAttribute(SDL_GL_CONTEXT_MINOR_VERSION, 3);
    SDL_GL_SetAttribute(SDL_GL_CONTEXT_PROFILE_MASK, SDL_GL_CONTEXT_PROFILE_CORE);
    SDL_GL_SetAttribute(SDL_GL_DOUBLEBUFFER, 1);
    
    /* Load settings early to get window position */
    vizero_settings_t* settings = vizero_settings_create();
    if (settings) {
        vizero_settings_load_from_file(settings);
    }
    
    /* Get saved window state */
    int saved_x, saved_y, saved_width, saved_height, saved_maximized;
    vizero_settings_load_window_state(settings, &saved_x, &saved_y, &saved_width, &saved_height, &saved_maximized);
    
    /* Use saved dimensions if available, otherwise use config defaults */
    int window_width = (saved_width > 0) ? saved_width : app->config.width;
    int window_height = (saved_height > 0) ? saved_height : app->config.height;
    
    /* Create window with saved position */
    app->window = vizero_window_create_with_position(app->config.title, 
                                                    saved_x, saved_y,
                                                    window_width, window_height,
                                                    app->config.fullscreen);
    if (!app->window) {
        if (settings) vizero_settings_destroy(settings);
        SDL_Quit();
        return -1;
    }
    
    /* Restore maximized state if needed */
    if (saved_maximized && !app->config.fullscreen) {
        SDL_MaximizeWindow(vizero_window_get_sdl_window(app->window));
    }
    
    /* Store settings pointer for later use */
    app->settings = settings;
    
    /* Initialize GLEW */
    if (glewInit() != GLEW_OK) {
        fprintf(stderr, "Failed to initialize GLEW\n");
        vizero_window_destroy(app->window);
        SDL_Quit();
        return -1;
    }
    
    /* Create renderer */
    app->renderer = vizero_renderer_create(app->window);
    if (!app->renderer) {
        vizero_window_destroy(app->window);
        SDL_Quit();
        return -1;
    }
    
    /* Create input manager */
    app->input = vizero_input_manager_create(app);
    if (!app->input) {
        vizero_renderer_destroy(app->renderer);
        vizero_window_destroy(app->window);
        SDL_Quit();
        return -1;
    }
    
    /* Create editor state with pre-loaded settings */
    app->editor = vizero_editor_state_create_with_settings(app->settings);
    if (!app->editor) {
        vizero_input_manager_destroy(app->input);
        vizero_renderer_destroy(app->renderer);
        vizero_window_destroy(app->window);
        SDL_Quit();
        return -1;
    }
    
    /* Create status bar */
    vizero_window_get_size(app->window, &window_width, &window_height);
    app->status_bar = vizero_status_bar_create(window_width, 20); /* 20 pixel height */
    if (!app->status_bar) {
        vizero_editor_state_destroy(app->editor);
        vizero_input_manager_destroy(app->input);
        vizero_renderer_destroy(app->renderer);
        vizero_window_destroy(app->window);
        SDL_Quit();
        return -1;
    }
    
    /* Setup default status bar panels */
    vizero_status_bar_add_panel(app->status_bar, VIZERO_PANEL_FILENAME, VIZERO_ALIGN_LEFT, 100);
    vizero_status_bar_add_panel(app->status_bar, VIZERO_PANEL_MODE, VIZERO_ALIGN_LEFT, 80);
    vizero_status_bar_add_panel(app->status_bar, VIZERO_PANEL_READONLY_STATUS, VIZERO_ALIGN_LEFT, 40);
    vizero_status_bar_add_panel(app->status_bar, VIZERO_PANEL_BUFFER_INFO, VIZERO_ALIGN_CENTER, 60);
    vizero_status_bar_add_panel(app->status_bar, VIZERO_PANEL_CURSOR_POSITION, VIZERO_ALIGN_RIGHT, 80);
    vizero_status_bar_add_panel(app->status_bar, VIZERO_PANEL_TIME_DATE, VIZERO_ALIGN_RIGHT, 120);
    
    /* Create plugin manager */
    app->plugin_manager = vizero_plugin_manager_create((vizero_editor_t*)app->editor);
    if (!app->plugin_manager) {
        vizero_status_bar_destroy(app->status_bar);
        vizero_editor_state_destroy(app->editor);
        vizero_input_manager_destroy(app->input);
        vizero_renderer_destroy(app->renderer);
        vizero_window_destroy(app->window);
        SDL_Quit();
        return -1;
    }
    
    /* Set up editor-plugin manager connection */
    vizero_editor_set_plugin_manager(app->editor, app->plugin_manager);
    
    /* Create theme manager */
    app->theme_manager = vizero_theme_manager_create();
    if (!app->theme_manager) {
        vizero_plugin_manager_destroy(app->plugin_manager);
        vizero_status_bar_destroy(app->status_bar);
        vizero_editor_state_destroy(app->editor);
        vizero_input_manager_destroy(app->input);
        vizero_renderer_destroy(app->renderer);
        vizero_window_destroy(app->window);
        SDL_Quit();
        return -1;
    }
    
    /* Load built-in themes */
    vizero_theme_manager_load_builtin_themes(app->theme_manager);
    
    /* Set up editor-theme manager connection */
    vizero_editor_set_theme_manager(app->editor, (void*)app->theme_manager);
    
    /* Load logo image */
    char* logo_path = vizero_get_resource_path("images/logo.bmp");
    if (logo_path) {
        app->logo_image = vizero_image_load(logo_path);
        if (!app->logo_image) {
            printf("Warning: Could not load logo image from %s\n", logo_path);
        }
        free(logo_path);
    } else {
        app->logo_image = NULL;
        printf("Warning: Could not determine logo image path\n");
    }
    
    /* Load default theme from settings or apply "Default" theme */
    const char* saved_theme = vizero_settings_get_string(app->settings, "theme");
    if (!saved_theme || vizero_theme_manager_set_current_theme(app->theme_manager, saved_theme) != 0) {
        vizero_theme_manager_set_current_theme(app->theme_manager, "Default");
    }
    
    /* Create session manager */
    app->session_manager = (void*)vizero_session_manager_create();
    if (!app->session_manager) {
        vizero_theme_manager_destroy(app->theme_manager);
        vizero_plugin_manager_destroy(app->plugin_manager);
        vizero_status_bar_destroy(app->status_bar);
        vizero_editor_state_destroy(app->editor);
        vizero_input_manager_destroy(app->input);
        vizero_renderer_destroy(app->renderer);
        vizero_window_destroy(app->window);
        SDL_Quit();
        return -1;
    }
    
    /* Set up editor-session manager connection */
    vizero_editor_set_session_manager(app->editor, app->session_manager);
    
    /* Load plugins from plugin directory */
    if (app->config.plugin_dir) {
        int loaded = vizero_plugin_manager_scan_directory(app->plugin_manager, app->config.plugin_dir);
        printf("Loaded %d plugins from %s\n", loaded, app->config.plugin_dir);
    }
    
    printf("Vizero initialized successfully\n");
    return 0;
}

void vizero_application_shutdown(vizero_application_t* app) {
    if (!app) {
        return;
    }
    
    /* Save window position before destroying anything */
    if (app->window && app->settings) {
        int x, y, width, height;
        vizero_window_get_position(app->window, &x, &y);
        vizero_window_get_size(app->window, &width, &height);
        
        /* Check if window is maximized */
        SDL_Window* sdl_window = vizero_window_get_sdl_window(app->window);
        int maximized = (SDL_GetWindowFlags(sdl_window) & SDL_WINDOW_MAXIMIZED) != 0;
        
        /* Save window state to settings */
        vizero_settings_save_window_state(app->settings, x, y, width, height, maximized);
        vizero_settings_save_to_file(app->settings);
    }
    
    if (app->plugin_manager) {
        vizero_plugin_manager_destroy(app->plugin_manager);
        app->plugin_manager = NULL;
    }
    
    if (app->theme_manager) {
        vizero_theme_manager_destroy(app->theme_manager);
        app->theme_manager = NULL;
    }
    
    if (app->session_manager) {
        vizero_session_manager_destroy((vizero_session_manager_t*)app->session_manager);
        app->session_manager = NULL;
    }
    
    if (app->status_bar) {
        vizero_status_bar_destroy(app->status_bar);
        app->status_bar = NULL;
    }
    
    if (app->input) {
        vizero_input_manager_destroy(app->input);
        app->input = NULL;
    }
    
    if (app->renderer) {
        vizero_renderer_destroy(app->renderer);
        app->renderer = NULL;
    }
    
    if (app->window) {
        vizero_window_destroy(app->window);
        app->window = NULL;
    }
    
    /* Destroy editor AFTER saving window state, since editor will destroy settings */
    if (app->editor) {
        vizero_editor_state_destroy(app->editor);
        app->editor = NULL;
    }
    
    /* Settings are destroyed by editor state, so just clear our reference */
    app->settings = NULL;
    
    SDL_Quit();
}

/* Helper function to render a single editor window */
static void render_editor_window(vizero_application_t* app, vizero_editor_window_t* window, 
                                vizero_buffer_t* buffer, vizero_cursor_t* cursor) {
    if (!app || !window || !buffer || !cursor) return;
    // Use the word wrap/content rendering from ui/editor_window.cpp
    vizero_editor_window_render_content(window, app->editor, app->renderer);
}

/* Helper function to render a line with syntax highlighting - currently unused */
#if 0
static void render_line_with_syntax(vizero_application_t* app, const char* line_text, size_t line_num,
                                   float base_x, float y, int scroll_x,
                                   vizero_syntax_token_t* tokens, size_t token_count) {
    if (!app || !line_text) return;
    
    size_t line_len = strlen(line_text);
    if (line_len == 0) return;
    
    /* Create array to store colour for each character */
    vizero_colour_t* char_colours = (vizero_colour_t*)malloc(line_len * sizeof(vizero_colour_t));
    if (!char_colours) return;
    
    /* Initialize all characters to default white colour */
    vizero_colour_t default_colour = {1.0f, 1.0f, 1.0f, 1.0f};
    for (size_t i = 0; i < line_len; i++) {
        char_colours[i] = default_colour;
    }
    
    /* Apply syntax highlighting colours */
    for (size_t t = 0; t < token_count; t++) {
        vizero_syntax_token_t* token = &tokens[t];
        if (token->range.start.line == line_num) {
            size_t start_col = token->range.start.column;
            size_t end_col = token->range.end.column;
            /* Clamp to line bounds */
            if (start_col >= line_len) continue;
            if (end_col > line_len) end_col = line_len;
            vizero_colour_t render_colour = {
                token->colour.r / 255.0f,
                token->colour.g / 255.0f,
                token->colour.b / 255.0f,
                token->colour.a / 255.0f
            };
            for (size_t col = start_col; col < end_col; col++) {
                char_colours[col] = render_colour;
            }
        }
    }
    
    /* Render characters with their assigned colours */
    for (size_t col = 0; col < line_len; col++) {
        if ((int)col < scroll_x) continue; /* Skip characters scrolled out of view */
        
        char char_str[2] = {line_text[col], '\0'};
        float char_x = base_x + ((col - scroll_x) * 8);
        
        vizero_text_info_t text_info = {
            char_x,
            y,
            char_colours[col],
            NULL
        };
        
        vizero_renderer_draw_text(app->renderer, char_str, &text_info);
    }
    
    free(char_colours);
}
#endif

/* Helper function to draw window borders */
static void draw_window_borders(vizero_application_t* app, vizero_editor_window_t** windows, size_t count) {
    if (!app || !windows || count < 2) return;
    
    vizero_colour_t border_colour = {0.5f, 0.5f, 0.5f, 1.0f};
    
    for (size_t i = 0; i < count; i++) {
        vizero_editor_window_t* window = windows[i];
        if (!window) continue;
        
        /* Draw window border */
        vizero_renderer_draw_rect(app->renderer, 
                                (float)window->x, (float)window->y, 
                                (float)window->width, (float)window->height, 
                                border_colour);
    }
}

/* Helper function for fallback single window rendering */
static void render_single_window_fallback(vizero_application_t* app, int window_width, int window_height) {
    /* Get current buffer and cursor for fallback */
    vizero_buffer_t* current_buffer = vizero_editor_get_current_buffer(app->editor);
    vizero_cursor_t* current_cursor = vizero_editor_get_current_cursor(app->editor);
    
    if (!current_buffer || !current_cursor) {
        return; /* Nothing to render */
    }
    
    /* Create a temporary window structure for rendering */
    vizero_editor_window_t temp_window;
    memset(&temp_window, 0, sizeof(temp_window));
    temp_window.x = 0;
    temp_window.y = 0;
    temp_window.width = window_width;
    temp_window.height = window_height - 24; /* Leave space for status bar */
    temp_window.is_focused = 1;
    temp_window.scroll_x = app->scroll_x;
    temp_window.scroll_y = app->scroll_y;
    
    /* Render using the temporary window */
    render_editor_window(app, &temp_window, current_buffer, current_cursor);
    
    /* Update app scroll values from temporary window */
    app->scroll_x = temp_window.scroll_x;
    app->scroll_y = temp_window.scroll_y;
}

int vizero_application_run(vizero_application_t* app) {
    if (!app) {
        return -1;
    }
    printf("Starting main loop...\n");

    /* File polling variables - currently disabled for debugging */
    (void)SDL_GetTicks(); /* Suppress warning about unused function call */
    /* uint32_t last_poll_time = SDL_GetTicks();
    const uint32_t poll_interval_ms = 2000; // 2 seconds */

    // Debug: Check initial conditions
    while (!app->should_quit && !vizero_window_should_close(app->window) && !vizero_editor_should_quit(app->editor)) {

        /* Process input events */
        vizero_input_manager_process_events(app->input);
        
        /* Clear screen with theme background colour */
        vizero_colour_t clear_colour = {0.1f, 0.1f, 0.2f, 1.0f}; /* Default fallback */
        if (app->theme_manager) {
            const vizero_colour_theme_t* current_theme = vizero_theme_manager_get_current_theme(app->theme_manager);
            if (current_theme) {
                clear_colour = current_theme->background;
            }
        }
        vizero_renderer_clear(app->renderer, clear_colour);
        
        /* Get window dimensions */
        int window_width, window_height;
        vizero_window_get_size(app->window, &window_width, &window_height);
        
        /* Update window manager layout */
        vizero_window_manager_t* window_manager = vizero_editor_get_window_manager(app->editor);
        if (window_manager) {
            vizero_window_manager_update_layout(window_manager, window_width, window_height);
            
            /* Get all visible windows for rendering */
            vizero_editor_window_t** visible_windows = NULL;
            size_t visible_count = 0;
            
            int get_windows_result = vizero_window_manager_get_visible_windows(window_manager, &visible_windows, &visible_count);
            
            if (get_windows_result == 0) {
                /* Render each visible window */
                for (size_t i = 0; i < visible_count; i++) {
                    vizero_editor_window_t* window = visible_windows[i];
                    if (!window) continue;
                    
                    /* Get window's buffer and cursor via editor state */
                    vizero_buffer_t* buffer = vizero_editor_window_get_buffer(window, app->editor);
                    vizero_cursor_t* cursor = vizero_editor_window_get_cursor(window, app->editor);
                    
                    if (!buffer || !cursor) continue;
                    
                    /* Render this window */
                    render_editor_window(app, window, buffer, cursor);
                }
                
                /* Draw window borders if multiple windows */
                if (visible_count > 1) {
                    draw_window_borders(app, visible_windows, visible_count);
                }
                
                /* Free the visible windows array */
                if (visible_windows) free(visible_windows);
            } else {
                /* Fallback to single window rendering */
                render_single_window_fallback(app, window_width, window_height);
            }
        } else {
            /* Fallback to legacy rendering */
            render_single_window_fallback(app, window_width, window_height);
        }
        
        /* Render logo if showing welcome and no files loaded */
        if (app->show_welcome && app->logo_image) {
            /* Check if any files are loaded */
            int has_files = 0;
            if (app->editor) {
                has_files = (vizero_editor_get_buffer_count(app->editor) > 1 || 
                           vizero_buffer_get_line_count(vizero_editor_get_current_buffer(app->editor)) > 1 ||
                           vizero_buffer_is_modified(vizero_editor_get_current_buffer(app->editor)));
            }
            
            if (!has_files) {
                /* Position logo in upper right corner at 128x128 */
                float logo_width = 128.0f;
                float logo_height = 128.0f;
                float logo_x = (float)window_width - logo_width - 20.0f;  /* 20px margin from right edge */
                float logo_y = 20.0f;  /* 20px margin from top edge */
                
                /* Render logo */
                vizero_renderer_draw_image(app->renderer, app->logo_image, logo_x, logo_y, logo_width, logo_height);
            }
        }
        
        /* Update and render status bar */
        vizero_status_bar_update(app->status_bar, app->editor);
        vizero_status_bar_resize(app->status_bar, window_width, 24);
        vizero_colour_t status_bg = {0.2f, 0.2f, 0.3f, 1.0f};
        vizero_colour_t status_text = {1.0f, 1.0f, 1.0f, 1.0f};
        vizero_status_bar_render(app->status_bar, app->renderer, 0, window_height - 24, status_bg, status_text);
        
        /* Render popup if visible */
        if (vizero_editor_is_popup_visible(app->editor)) {
            const char* popup_content = vizero_editor_get_popup_content(app->editor);
            if (popup_content) {
                /* Calculate popup dimensions */
                int popup_width = (int)(window_width * 0.8f);  /* 80% of window width */
                int popup_height = (int)(window_height * 0.6f); /* 60% of window height */
                int popup_x = (window_width - popup_width) / 2;
                int popup_y = (window_height - popup_height) / 2;

                /* Draw popup background */
                vizero_colour_t popup_bg = {0.1f, 0.1f, 0.1f, 0.9f}; /* Dark semi-transparent */
                vizero_renderer_fill_rect(app->renderer, (float)(popup_x - 10), (float)(popup_y - 10),
                                        (float)(popup_width + 20), (float)(popup_height + 20), popup_bg);

                /* Draw popup border */
                vizero_colour_t popup_border = {0.5f, 0.5f, 0.5f, 1.0f}; /* Gray border */
                vizero_renderer_draw_rect(app->renderer, (float)(popup_x - 10), (float)(popup_y - 10),
                                        (float)(popup_width + 20), (float)(popup_height + 20), popup_border);

                /* Draw popup text with scrolling support and per-line colour */
                int scroll_offset = vizero_editor_get_popup_scroll_offset(app->editor);
                int visible_lines = (popup_height - 80) / 16; /* Approximate lines that fit (16px per line) */

                /* Create a buffer for the visible portion of text */
                const char* line_start = popup_content;
                int current_line = 0;
                int lines_added = 0;
                float line_y = (float)popup_y;
                while (*line_start && lines_added < visible_lines) {
                    /* Skip lines before scroll offset */
                    if (current_line < scroll_offset) {
                        if (*line_start == '\n') current_line++;
                        line_start++;
                        continue;
                    }
                    /* Find end of line */
                    const char* line_end = strchr(line_start, '\n');
                    size_t line_len = line_end ? (size_t)(line_end - line_start) : strlen(line_start);
                    if (line_len > 511) line_len = 511;
                    char line_buf[512];
                    memcpy(line_buf, line_start, line_len);
                    line_buf[line_len] = '\0';

                    /* Determine colour by suffix */
                    vizero_colour_t colour = {1.0f, 1.0f, 1.0f, 1.0f};
                    if (strstr(line_buf, "[DIR]")) {
                        colour.r = 128.0f/255.0f; colour.g = 192.0f/255.0f; colour.b = 255.0f/255.0f; colour.a = 1.0f; // pale blue
                    } else if (strstr(line_buf, "[EXE]")) {
                        colour.r = 255.0f/255.0f; colour.g = 128.0f/255.0f; colour.b = 128.0f/255.0f; colour.a = 1.0f; // pale red
                    } else if (strstr(line_buf, "[FILE]")) {
                        colour.r = 255.0f/255.0f; colour.g = 255.0f/255.0f; colour.b = 192.0f/255.0f; colour.a = 1.0f; // pale yellow
                    }

                    vizero_text_info_t popup_text_info;
                    popup_text_info.x = (float)popup_x;
                    popup_text_info.y = line_y;
                    popup_text_info.colour = colour;
                    popup_text_info.font = NULL;
                    vizero_renderer_draw_text(app->renderer, line_buf, &popup_text_info);

                    lines_added++;
                    if (line_end) {
                        line_start = line_end + 1;
                    } else {
                        break;
                    }
                    line_y += 16.0f;
                }

                /* Draw dismiss instruction */
                uint32_t popup_duration = vizero_editor_get_popup_duration(app->editor);
                const char* instruction;
                if (popup_duration > 0) {
                    instruction = "\n\nPress ESC to close or wait for timeout...";
                } else {
                    instruction = "\n\nPress ESC to close, or use UP/DOWN arrows to scroll";
                }
                vizero_text_info_t popup_text_info;
                popup_text_info.x = (float)popup_x;
                popup_text_info.y = (float)(popup_y + popup_height - 40);
                popup_text_info.colour.r = 0.7f;
                popup_text_info.colour.g = 0.7f;
                popup_text_info.colour.b = 0.7f;
                popup_text_info.colour.a = 1.0f;
                popup_text_info.font = NULL;
                vizero_renderer_draw_text(app->renderer, instruction, &popup_text_info);
            }
        }
        
        /* File change polling temporarily disabled for debugging corruption. */
        // --- File change polling logic ---
        // uint32_t now = SDL_GetTicks();
        // if (now - last_poll_time > poll_interval_ms) {
        //     last_poll_time = now;
        //     vizero_editor_state_t* editor = app->editor;
        //     if (editor) {
        //         size_t buffer_count = vizero_editor_get_buffer_count(editor);
        //         for (size_t i = 0; i < buffer_count; ++i) {
        //             vizero_buffer_t* buf = vizero_editor_get_buffer(editor, i);
        //             if (!buf) continue;
        //             const char* fname = vizero_buffer_get_filename(buf);
        //             if (!fname || !*fname) continue;
        //             uint64_t disk_mtime = vizero_get_file_mtime(fname);
        //             uint64_t last_mtime = vizero_buffer_get_last_disk_mtime(buf);
        //             if (disk_mtime && disk_mtime != last_mtime) {
        //                 if (!vizero_buffer_is_modified(buf)) {
        //                     if (vizero_buffer_load_from_file(buf, fname) == 0) {
        //                         vizero_buffer_set_last_disk_mtime(buf, disk_mtime);
        //                         // Optionally: set a status message
        //                         char msg[256];
        //                         snprintf(msg, sizeof(msg), "File reloaded: %s", fname);
        //                         vizero_editor_set_status_message(editor, msg);
        //                     }
        //                 } else {
        //                     // Buffer is modified, warn user
        //                     char msg[256];
        //                     snprintf(msg, sizeof(msg), "File changed on disk: %s (unsaved changes)", fname);
        //                     vizero_editor_set_status_message(editor, msg);
        //                 }
        //             }
        //         }
        //     }
        // }
        
        /* Present frame */
        vizero_renderer_present(app->renderer);
        vizero_window_swap_buffers(app->window);
        
        /* Cap framerate */
        SDL_Delay(16); /* ~60 FPS */
    }
    
    printf("Main loop exited\n");
    return 0;
}

void vizero_application_quit(vizero_application_t* app) {
    if (app) {
        app->should_quit = 1;
    }
}

vizero_window_t* vizero_application_get_window(vizero_application_t* app) {
    return app ? app->window : NULL;
}

vizero_editor_state_t* vizero_application_get_editor(vizero_application_t* app) {
    return app ? app->editor : NULL;
}

vizero_plugin_manager_t* vizero_application_get_plugin_manager(vizero_application_t* app) {
    return app ? app->plugin_manager : NULL;
}

void vizero_application_on_window_resize(vizero_application_t* app, int width, int height) {
    if (!app) {
        return;
    }
    
    /* Update viewport */
    glViewport(0, 0, width, height);
    
    /* Update renderer viewport */
    if (app->renderer) {
        vizero_renderer_update_viewport(app->renderer, width, height);
    }
    
    /* TODO: Update editor layout */
}

void vizero_application_on_file_drop(vizero_application_t* app, const char* filename) {
    if (!app || !filename) {
        return;
    }
    
    /* Open dropped file */
    vizero_editor_open_buffer(app->editor, filename);
}

void vizero_application_on_user_input(vizero_application_t* app) {
    if (app) {
        app->show_welcome = 0; /* Hide welcome message on any user input */
    }
}