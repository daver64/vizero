#ifndef VIZERO_COLOUR_THEME_H
#define VIZERO_COLOUR_THEME_H

#include <stddef.h>
#include "renderer.h"

#ifdef __cplusplus
extern "C" {
#endif

/* Colour theme structure */
typedef struct {
    /* UI colours */
    vizero_colour_t background;
    vizero_colour_t foreground;
    vizero_colour_t cursor;
    vizero_colour_t selection;
    vizero_colour_t line_number;
    vizero_colour_t status_bar_bg;
    vizero_colour_t status_bar_text;
    vizero_colour_t window_border;
    
    /* Syntax highlighting colours */
    vizero_colour_t keyword;
    vizero_colour_t string;
    vizero_colour_t comment;
    vizero_colour_t number;
    vizero_colour_t op;
    vizero_colour_t function;
    vizero_colour_t type;
    vizero_colour_t variable;
    vizero_colour_t constant;
    vizero_colour_t error;
    
    /* Theme metadata */
    char name[64];
    char author[64];
    char description[256];
} vizero_colour_theme_t;

/* Theme manager structure */
typedef struct {
    vizero_colour_theme_t* themes;
    size_t theme_count;
    size_t theme_capacity;
    size_t current_theme_index;
} vizero_theme_manager_t;

/* Theme manager functions */
vizero_theme_manager_t* vizero_theme_manager_create(void);
void vizero_theme_manager_destroy(vizero_theme_manager_t* manager);

/* Built-in themes */
void vizero_theme_manager_load_builtin_themes(vizero_theme_manager_t* manager);

/* Theme management */
int vizero_theme_manager_add_theme(vizero_theme_manager_t* manager, const vizero_colour_theme_t* theme);
const vizero_colour_theme_t* vizero_theme_manager_get_current_theme(const vizero_theme_manager_t* manager);
const vizero_colour_theme_t* vizero_theme_manager_get_theme_by_name(const vizero_theme_manager_t* manager, const char* name);
int vizero_theme_manager_set_current_theme(vizero_theme_manager_t* manager, const char* name);
size_t vizero_theme_manager_get_theme_count(const vizero_theme_manager_t* manager);
const vizero_colour_theme_t* vizero_theme_manager_get_theme_at_index(const vizero_theme_manager_t* manager, size_t index);

/* Apply the current theme (notify application about theme change) */
void vizero_theme_manager_apply_theme(vizero_theme_manager_t* manager);

/* Theme I/O */
int vizero_theme_save_to_file(const vizero_colour_theme_t* theme, const char* filename);
int vizero_theme_load_from_file(vizero_colour_theme_t* theme, const char* filename);

#ifdef __cplusplus
}
#endif

#endif /* VIZERO_COLOUR_THEME_H */