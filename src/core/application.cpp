#include "vizero/application.h"
#include "vizero/window.h"
#include "vizero/renderer.h"
#include "vizero/input_manager.h"
#include "vizero/editor_state.h"
#include "vizero/plugin_manager.h"
#include "vizero/status_bar.h"
#include "vizero/buffer.h"
#include "vizero/cursor.h"
#include "vizero/string_utils.h"
#include "vizero/settings.h"
#include "vizero/search.h"
#include <SDL.h>
#include <GL/glew.h>
#include <stdio.h>
#include <stdlib.h>
#include <string.h>

struct vizero_application_t {
    vizero_window_t* window;
    vizero_renderer_t* renderer;
    vizero_input_manager_t* input;
    vizero_editor_state_t* editor;
    vizero_plugin_manager_t* plugin_manager;
    vizero_status_bar_t* status_bar;
    int should_quit;
    vizero_app_config_t config;
    
    /* Scrolling state */
    int scroll_x;
    int scroll_y;
};

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
    
    return app;
}

void vizero_application_destroy(vizero_application_t* app) {
    if (!app) {
        return;
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
    
    /* Create window */
    app->window = vizero_window_create(app->config.title, 
                                      app->config.width, 
                                      app->config.height, 
                                      app->config.fullscreen);
    if (!app->window) {
        SDL_Quit();
        return -1;
    }
    
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
    
    /* Create editor state */
    app->editor = vizero_editor_state_create();
    if (!app->editor) {
        vizero_input_manager_destroy(app->input);
        vizero_renderer_destroy(app->renderer);
        vizero_window_destroy(app->window);
        SDL_Quit();
        return -1;
    }
    
    /* Create status bar */
    int window_width, window_height;
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
    
    if (app->plugin_manager) {
        vizero_plugin_manager_destroy(app->plugin_manager);
        app->plugin_manager = NULL;
    }
    
    if (app->status_bar) {
        vizero_status_bar_destroy(app->status_bar);
        app->status_bar = NULL;
    }
    
    if (app->editor) {
        vizero_editor_state_destroy(app->editor);
        app->editor = NULL;
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
    
    SDL_Quit();
}

int vizero_application_run(vizero_application_t* app) {
    if (!app) {
        return -1;
    }
    
    printf("Starting main loop...\n");
    
    while (!app->should_quit && !vizero_window_should_close(app->window) && !vizero_editor_should_quit(app->editor)) {
        /* Process input events */
        vizero_input_manager_process_events(app->input);
        
        /* Clear screen */
        vizero_color_t clear_color = {0.1f, 0.1f, 0.2f, 1.0f};
        vizero_renderer_clear(app->renderer, clear_color);
        
        /* Get current buffer for this frame */
        vizero_buffer_t* current_buffer = vizero_editor_get_current_buffer(app->editor);
        
        /* Calculate scroll offsets based on cursor position */
        vizero_position_t cursor_pos = vizero_cursor_get_position(vizero_editor_get_current_cursor(app->editor));
        int window_width, window_height;
        vizero_window_get_size(app->window, &window_width, &window_height);
        
        /* Text area dimensions (leaving space for status bar) */
        int text_area_height = window_height - 24 - 30; /* 24 for status bar, 30 for top margin */
        int visible_lines = text_area_height / 16; /* 16 pixels per line */
        int visible_cols = (window_width - 20) / 8; /* 8 pixels per char, 20 for margins */
        
        /* Vertical scrolling */
        if ((int)cursor_pos.line < app->scroll_y) {
            app->scroll_y = (int)cursor_pos.line;
        } else if ((int)cursor_pos.line >= app->scroll_y + visible_lines) {
            app->scroll_y = (int)cursor_pos.line - visible_lines + 1;
        }
        
        /* Horizontal scrolling */
        if ((int)cursor_pos.column < app->scroll_x) {
            app->scroll_x = (int)cursor_pos.column;
        } else if ((int)cursor_pos.column >= app->scroll_x + visible_cols) {
            app->scroll_x = (int)cursor_pos.column - visible_cols + 1;
        }
        
        /* Check if line numbers should be displayed */
        bool show_line_numbers = false;
        int line_number_width = 0;
        if (app->editor && current_buffer) {
            vizero_settings_t* settings = vizero_editor_get_settings(app->editor);
            if (settings) {
                show_line_numbers = vizero_settings_get_bool(settings, VIZERO_SETTING_SHOW_LINE_NUMBERS);
                if (show_line_numbers) {
                    size_t buffer_line_count = vizero_buffer_get_line_count(current_buffer);
                    /* Calculate width needed for line numbers (digits + space) */
                    line_number_width = snprintf(NULL, 0, "%zu", buffer_line_count) + 1;
                    line_number_width = (line_number_width < 4) ? 4 : line_number_width; /* Minimum 4 chars */
                    line_number_width *= 8; /* 8 pixels per character */
                }
            }
        }
        
        /* Render line numbers if enabled */
        if (show_line_numbers && current_buffer) {
            vizero_color_t line_num_color = {0.6f, 0.6f, 0.7f, 1.0f}; /* Light gray */
            size_t buffer_line_count = vizero_buffer_get_line_count(current_buffer);
            
            for (size_t line = app->scroll_y; line < buffer_line_count && line < app->scroll_y + visible_lines; line++) {
                char line_num_str[16];
                snprintf(line_num_str, sizeof(line_num_str), "%4zu", line + 1); /* 1-based line numbers */
                
                float line_num_y = 30.0f + ((line - app->scroll_y) * 16.0f);
                vizero_text_info_t line_num_info = {2.0f, line_num_y, line_num_color, NULL};
                vizero_renderer_draw_text(app->renderer, line_num_str, &line_num_info);
            }
        }
        
        /* Render editor content */
        vizero_color_t text_color = {1.0f, 1.0f, 1.0f, 1.0f}; /* White text */
        vizero_text_info_t text_info = {
            (show_line_numbers ? line_number_width + 10.0f : 10.0f) - (app->scroll_x * 8.0f),  /* x - adjust for horizontal scroll and line numbers */
            30.0f - (app->scroll_y * 16.0f), /* y - adjust for vertical scroll */
            text_color,
            NULL    /* font - not implemented yet */
        };
        
        /* Get buffer content and apply syntax highlighting */
        const char* buffer_text = vizero_buffer_get_text(current_buffer);
        
        if (buffer_text && strlen(buffer_text) > 0) {
            /* Try to get syntax highlighting from plugins */
            vizero_syntax_token_t* tokens = NULL;
            size_t token_count = 0;
            
            /* Calculate visible lines for syntax highlighting */
            size_t buffer_line_count = vizero_buffer_get_line_count(current_buffer);
            size_t start_line = (app->scroll_y > 0) ? app->scroll_y : 0;
            size_t syntax_visible_lines = ((window_height - 60) / 16) + 2; /* Add buffer */
            size_t end_line = start_line + syntax_visible_lines;
            if (end_line >= buffer_line_count) end_line = buffer_line_count - 1;
            
            int has_highlighting = vizero_plugin_manager_highlight_syntax(
                app->plugin_manager, current_buffer, start_line, end_line, &tokens, &token_count);
            
            /* Check if there's a text selection to highlight */
            int has_selection = vizero_editor_has_selection(app->editor);
            vizero_position_t selection_start = {0}, selection_end = {0};
            if (has_selection) {
                vizero_editor_get_selection_range(app->editor, &selection_start, &selection_end);
            }
            
            /* Check if there are search matches to highlight */
            int has_search_results = vizero_search_has_results(app->editor);
            const vizero_search_match_t* search_matches = NULL;
            int search_match_count = 0;
            int current_match_index = -1;
            if (has_search_results) {
                search_matches = vizero_search_get_all_matches(app->editor);
                search_match_count = vizero_search_get_match_count(app->editor);
                current_match_index = vizero_search_get_current_match_index(app->editor);
            }
            
            if (has_highlighting && tokens && token_count > 0) {
                /* Render text with syntax highlighting - word by word approach */
                for (size_t line = start_line; line <= end_line; line++) {
                    const char* line_text = vizero_buffer_get_line_text(current_buffer, line);
                    if (!line_text) continue;
                    
                    float line_x = (show_line_numbers ? line_number_width + 10.0f : 10.0f) - (app->scroll_x * 8.0f);
                    float line_y = 30.0f + ((line - app->scroll_y) * 16.0f);
                    
                    /* Render selection background for this line if needed */
                    if (has_selection && line >= selection_start.line && line <= selection_end.line) {
                        size_t sel_start_col = (line == selection_start.line) ? selection_start.column : 0;
                        size_t sel_end_col = (line == selection_end.line) ? selection_end.column : strlen(line_text);
                        
                        if (sel_end_col > sel_start_col) {
                            float sel_x = line_x + (sel_start_col * 8.0f);
                            float sel_width = (sel_end_col - sel_start_col) * 8.0f;
                            vizero_color_t selection_bg = {0.3f, 0.3f, 0.6f, 0.5f}; /* Blue selection background */
                            vizero_renderer_fill_rect(app->renderer, sel_x, line_y, sel_width, 16.0f, selection_bg);
                        }
                    }
                    
                    /* Render search match background for this line if needed */
                    if (has_search_results && search_matches) {
                        for (int match_idx = 0; match_idx < search_match_count; match_idx++) {
                            const vizero_search_match_t* match = &search_matches[match_idx];
                            if (match->line == (int)line) {
                                float match_x = line_x + (match->column * 8.0f);
                                float match_width = match->length * 8.0f;
                                
                                /* Use different colors for current match vs other matches */
                                vizero_color_t match_bg;
                                if (match_idx == current_match_index) {
                                    match_bg = {0.8f, 0.6f, 0.2f, 0.7f}; /* Orange for current match */
                                } else {
                                    match_bg = {0.8f, 0.8f, 0.2f, 0.5f}; /* Yellow for other matches */
                                }
                                
                                vizero_renderer_fill_rect(app->renderer, match_x, line_y, match_width, 16.0f, match_bg);
                            }
                        }
                    }
                    
                    /* Find all tokens for this line */
                    size_t line_len = strlen(line_text);
                    size_t last_column = 0;
                    
                    for (size_t i = 0; i < token_count; i++) {
                        if (tokens[i].range.start.line != line) continue;
                        
                        /* Render any uncolored text before this token */
                        if (tokens[i].range.start.column > last_column) {
                            size_t uncolored_len = tokens[i].range.start.column - last_column;
                            char* uncolored_text = (char*)malloc(uncolored_len + 1);
                            if (uncolored_text) {
                                strncpy(uncolored_text, line_text + last_column, uncolored_len);
                                uncolored_text[uncolored_len] = '\0';
                                
                                float text_x = line_x + (last_column * 8.0f);
                                vizero_color_t white_color = {1.0f, 1.0f, 1.0f, 1.0f};
                                vizero_text_info_t white_info = {text_x, line_y, white_color, NULL};
                                vizero_renderer_draw_text(app->renderer, uncolored_text, &white_info);
                                free(uncolored_text);
                            }
                        }
                        
                        /* Render the colored token */
                        size_t token_len = tokens[i].range.end.column - tokens[i].range.start.column;
                        if (token_len > 0 && tokens[i].range.start.column < line_len) {
                            char* token_text = (char*)malloc(token_len + 1);
                            if (token_text) {
                                strncpy(token_text, line_text + tokens[i].range.start.column, token_len);
                                token_text[token_len] = '\0';
                                
                                float text_x = line_x + (tokens[i].range.start.column * 8.0f);
                                vizero_color_t token_color;
                                token_color.r = tokens[i].color.r / 255.0f;
                                token_color.g = tokens[i].color.g / 255.0f;
                                token_color.b = tokens[i].color.b / 255.0f;
                                token_color.a = tokens[i].color.a / 255.0f;
                                
                                vizero_text_info_t token_info = {text_x, line_y, token_color, NULL};
                                vizero_renderer_draw_text(app->renderer, token_text, &token_info);
                                free(token_text);
                            }
                        }
                        
                        last_column = tokens[i].range.end.column;
                    }
                    
                    /* Render any remaining uncolored text at the end of the line */
                    if (last_column < line_len) {
                        char* remaining_text = (char*)malloc(line_len - last_column + 1);
                        if (remaining_text) {
                            strcpy(remaining_text, line_text + last_column);
                            
                            float text_x = line_x + (last_column * 8.0f);
                            vizero_color_t white_color = {1.0f, 1.0f, 1.0f, 1.0f};
                            vizero_text_info_t white_info = {text_x, line_y, white_color, NULL};
                            vizero_renderer_draw_text(app->renderer, remaining_text, &white_info);
                            free(remaining_text);
                        }
                    }
                    
                    /* If no tokens for this line, render normally */
                    if (last_column == 0) {
                        vizero_color_t white_color = {1.0f, 1.0f, 1.0f, 1.0f};
                        vizero_text_info_t white_info = {line_x, line_y, white_color, NULL};
                        vizero_renderer_draw_text(app->renderer, line_text, &white_info);
                    }
                }
                
                /* Clean up tokens */
                free(tokens);
            } else {
                /* No syntax highlighting - render line by line for search highlighting */
                vizero_buffer_t* fallback_buffer = vizero_editor_get_current_buffer(app->editor);
                if (fallback_buffer) {
                    size_t fallback_line_count = vizero_buffer_get_line_count(fallback_buffer);
                    size_t fallback_start_line = (app->scroll_y > 0) ? app->scroll_y : 0;
                    size_t fallback_visible_lines = ((window_height - 60) / 16) + 2; /* Add buffer */
                    size_t fallback_end_line = fallback_start_line + fallback_visible_lines;
                    if (fallback_end_line >= fallback_line_count) fallback_end_line = fallback_line_count;
                    
                    for (size_t line = fallback_start_line; line < fallback_end_line; line++) {
                        const char* line_text = vizero_buffer_get_line_text(fallback_buffer, line);
                        if (!line_text) continue;
                        
                        float line_x = (show_line_numbers ? line_number_width + 10.0f : 10.0f) - (app->scroll_x * 8.0f);
                        float line_y = 30.0f + ((line - app->scroll_y) * 16.0f);
                        
                        /* Render selection background for this line if needed */
                        if (has_selection && line >= selection_start.line && line <= selection_end.line) {
                            size_t sel_start_col = (line == selection_start.line) ? selection_start.column : 0;
                            size_t sel_end_col = (line == selection_end.line) ? selection_end.column : strlen(line_text);
                            
                            if (sel_end_col > sel_start_col) {
                                float sel_x = line_x + (sel_start_col * 8.0f);
                                float sel_width = (sel_end_col - sel_start_col) * 8.0f;
                                vizero_color_t selection_bg = {0.3f, 0.3f, 0.6f, 0.5f}; /* Blue selection background */
                                vizero_renderer_fill_rect(app->renderer, sel_x, line_y, sel_width, 16.0f, selection_bg);
                            }
                        }
                        
                        /* Render search match background for this line if needed */
                        if (has_search_results && search_matches) {
                            for (int match_idx = 0; match_idx < search_match_count; match_idx++) {
                                const vizero_search_match_t* match = &search_matches[match_idx];
                                if (match->line == (int)line) {
                                    float match_x = line_x + (match->column * 8.0f);
                                    float match_width = match->length * 8.0f;
                                    
                                    /* Use different colors for current match vs other matches */
                                    vizero_color_t match_bg;
                                    if (match_idx == current_match_index) {
                                        match_bg = {0.8f, 0.6f, 0.2f, 0.7f}; /* Orange for current match */
                                    } else {
                                        match_bg = {0.8f, 0.8f, 0.2f, 0.5f}; /* Yellow for other matches */
                                    }
                                    
                                    vizero_renderer_fill_rect(app->renderer, match_x, line_y, match_width, 16.0f, match_bg);
                                }
                            }
                        }
                        
                        /* Render the line text */
                        vizero_color_t white_color = {1.0f, 1.0f, 1.0f, 1.0f};
                        vizero_text_info_t line_info = {line_x, line_y, white_color, NULL};
                        vizero_renderer_draw_text(app->renderer, line_text, &line_info);
                    }
                } else {
                    /* No buffer - render normally */
                    vizero_renderer_draw_text(app->renderer, buffer_text, &text_info);
                }
            }
        } else {
            /* Show welcome message */
            vizero_renderer_draw_text(app->renderer, "Vizero - Vi Clone\nPress 'i' to enter insert mode\nPress ESC to return to normal mode\nPress Ctrl+C to quit", &text_info);
        }
        
        /* Draw cursor - cursor_pos already declared earlier */
        
        /* Get current line text to calculate visual column */
        int line_start = 0;
        int current_line = 0;
        
        /* Find the start of the current line */
        for (int i = 0; buffer_text && buffer_text[i] != '\0'; i++) {
            if (current_line == cursor_pos.line) {
                break;
            }
            if (buffer_text[i] == '\n') {
                current_line++;
                line_start = i + 1;
            }
        }
        
        /* Calculate visual column considering tabs */
        int visual_col = vizero_string_visual_column(buffer_text + line_start, (int)cursor_pos.column, 4);
        float cursor_x = (show_line_numbers ? line_number_width + 10.0f : 10.0f) + ((visual_col - app->scroll_x) * 8.0f);  /* Adjust for horizontal scroll and line numbers */
        float cursor_y = 30.0f + ((cursor_pos.line - app->scroll_y) * 16.0f);   /* Adjust for vertical scroll */
        
        /* Only draw cursor if it's visible on screen */
        float min_x = show_line_numbers ? line_number_width + 10.0f : 10.0f;
        if (cursor_x >= min_x && cursor_x < (float)(window_width - 10) && 
            cursor_y >= 30.0f && cursor_y < (float)(window_height - 24 - 16)) {
            vizero_color_t cursor_color = {1.0f, 1.0f, 0.0f, 1.0f}; /* Yellow cursor */
            vizero_renderer_fill_rect(app->renderer, cursor_x, cursor_y, 8.0f, 16.0f, cursor_color);
        }
        
        /* Update and render status bar - using window dimensions from earlier */
        vizero_status_bar_update(app->status_bar, app->editor);
        vizero_status_bar_resize(app->status_bar, window_width, 24);
        vizero_color_t status_bg = {0.2f, 0.2f, 0.3f, 1.0f};
        vizero_color_t status_text = {1.0f, 1.0f, 1.0f, 1.0f};
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
                vizero_color_t popup_bg = {0.1f, 0.1f, 0.1f, 0.9f}; /* Dark semi-transparent */
                vizero_renderer_fill_rect(app->renderer, (float)(popup_x - 10), (float)(popup_y - 10), 
                                        (float)(popup_width + 20), (float)(popup_height + 20), popup_bg);
                
                /* Draw popup border */
                vizero_color_t popup_border = {0.5f, 0.5f, 0.5f, 1.0f}; /* Gray border */
                vizero_renderer_draw_rect(app->renderer, (float)(popup_x - 10), (float)(popup_y - 10), 
                                        (float)(popup_width + 20), (float)(popup_height + 20), popup_border);
                
                /* Draw popup text */
                vizero_text_info_t popup_text_info;
                popup_text_info.x = (float)popup_x;
                popup_text_info.y = (float)popup_y;
                popup_text_info.color.r = 1.0f;
                popup_text_info.color.g = 1.0f;
                popup_text_info.color.b = 1.0f;
                popup_text_info.color.a = 1.0f;
                popup_text_info.font = NULL; /* Use default font */
                
                vizero_renderer_draw_text(app->renderer, popup_content, &popup_text_info);
                
                /* Draw dismiss instruction */
                const char* instruction = "\n\nPress ESC to close or wait 5 seconds...";
                popup_text_info.y = (float)(popup_y + popup_height - 40);
                popup_text_info.color.r = 0.7f;
                popup_text_info.color.g = 0.7f;
                popup_text_info.color.b = 0.7f;
                popup_text_info.color.a = 1.0f;
                vizero_renderer_draw_text(app->renderer, instruction, &popup_text_info);
            }
        }
        
        /* Present frame */
        vizero_renderer_present(app->renderer);
        vizero_window_swap_buffers(app->window);
        
        /* Cap framerate */
        SDL_Delay(16); /* ~60 FPS */
    }
    
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