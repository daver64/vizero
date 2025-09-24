#include "vizero/colour_theme.h"
#include "vizero/settings.h"
#include <stdlib.h>
#include <string.h>
#include <stdio.h>

/* Create a theme manager */
vizero_theme_manager_t* vizero_theme_manager_create(void) {
    vizero_theme_manager_t* manager = (vizero_theme_manager_t*)malloc(sizeof(vizero_theme_manager_t));
    if (!manager) return NULL;
    
    manager->themes = NULL;
    manager->theme_count = 0;
    manager->theme_capacity = 0;
    manager->current_theme_index = 0;
    
    return manager;
}

/* Destroy a theme manager */
void vizero_theme_manager_destroy(vizero_theme_manager_t* manager) {
    if (!manager) return;
    
    free(manager->themes);
    free(manager);
}

/* Helper function to create a colour */
static vizero_colour_t make_colour(float r, float g, float b, float a) {
    vizero_colour_t colour;
    colour.r = r;
    colour.g = g;
    colour.b = b;
    colour.a = a;
    return colour;
}

/* Helper function to create a theme */
static vizero_colour_theme_t create_theme(const char* name, const char* author, const char* description) {
    vizero_colour_theme_t theme;
    memset(&theme, 0, sizeof(theme));
    
    strncpy(theme.name, name, sizeof(theme.name) - 1);
    strncpy(theme.author, author, sizeof(theme.author) - 1);
    strncpy(theme.description, description, sizeof(theme.description) - 1);
    
    return theme;
}

/* Load built-in themes */
void vizero_theme_manager_load_builtin_themes(vizero_theme_manager_t* manager) {
    if (!manager) return;
    
    /* Default theme (dark) */
    vizero_colour_theme_t default_theme = create_theme("Default", "Vizero Team", "Default dark theme");
    default_theme.background = make_colour(0.1f, 0.1f, 0.2f, 1.0f);
    default_theme.foreground = make_colour(1.0f, 1.0f, 1.0f, 1.0f);
    default_theme.cursor = make_colour(1.0f, 1.0f, 0.0f, 0.8f);
    default_theme.selection = make_colour(0.3f, 0.4f, 0.6f, 0.5f);
    default_theme.line_number = make_colour(0.5f, 0.5f, 0.5f, 1.0f);
    default_theme.status_bar_bg = make_colour(0.2f, 0.2f, 0.3f, 1.0f);
    default_theme.status_bar_text = make_colour(1.0f, 1.0f, 1.0f, 1.0f);
    default_theme.window_border = make_colour(0.5f, 0.5f, 0.5f, 1.0f);
    
    /* Syntax colours */
    default_theme.keyword = make_colour(0.8f, 0.6f, 1.0f, 1.0f);      /* Purple */
    default_theme.string = make_colour(0.6f, 1.0f, 0.6f, 1.0f);       /* Green */
    default_theme.comment = make_colour(0.6f, 0.6f, 0.6f, 1.0f);      /* Grey */
    default_theme.number = make_colour(1.0f, 0.8f, 0.6f, 1.0f);       /* Orange */
    default_theme.op = make_colour(1.0f, 1.0f, 1.0f, 1.0f);           /* White */
    default_theme.function = make_colour(0.6f, 0.8f, 1.0f, 1.0f);     /* Light blue */
    default_theme.type = make_colour(0.8f, 1.0f, 0.8f, 1.0f);         /* Light green */
    default_theme.variable = make_colour(1.0f, 1.0f, 1.0f, 1.0f);     /* White */
    default_theme.constant = make_colour(1.0f, 0.6f, 0.6f, 1.0f);     /* Light red */
    default_theme.error = make_colour(1.0f, 0.2f, 0.2f, 1.0f);        /* Red */
    
    vizero_theme_manager_add_theme(manager, &default_theme);
    
    /* Monokai theme */
    vizero_colour_theme_t monokai_theme = create_theme("Monokai", "Vizero Team", "Popular dark theme inspired by Sublime Text");
    monokai_theme.background = make_colour(0.16f, 0.16f, 0.14f, 1.0f);
    monokai_theme.foreground = make_colour(0.97f, 0.95f, 0.89f, 1.0f);
    monokai_theme.cursor = make_colour(0.97f, 0.95f, 0.89f, 1.0f);
    monokai_theme.selection = make_colour(0.26f, 0.26f, 0.22f, 1.0f);
    monokai_theme.line_number = make_colour(0.45f, 0.45f, 0.39f, 1.0f);
    monokai_theme.status_bar_bg = make_colour(0.14f, 0.14f, 0.12f, 1.0f);
    monokai_theme.status_bar_text = make_colour(0.97f, 0.95f, 0.89f, 1.0f);
    monokai_theme.window_border = make_colour(0.35f, 0.35f, 0.30f, 1.0f);
    
    /* Monokai syntax colours */
    monokai_theme.keyword = make_colour(0.96f, 0.43f, 0.60f, 1.0f);    /* Pink */
    monokai_theme.string = make_colour(0.89f, 0.86f, 0.38f, 1.0f);     /* Yellow */
    monokai_theme.comment = make_colour(0.45f, 0.45f, 0.39f, 1.0f);    /* Grey */
    monokai_theme.number = make_colour(0.68f, 0.51f, 1.0f, 1.0f);      /* Purple */
    monokai_theme.op = make_colour(0.96f, 0.43f, 0.60f, 1.0f);         /* Pink */
    monokai_theme.function = make_colour(0.65f, 0.89f, 0.18f, 1.0f);   /* Green */
    monokai_theme.type = make_colour(0.40f, 0.85f, 1.0f, 1.0f);        /* Blue */
    monokai_theme.variable = make_colour(0.97f, 0.95f, 0.89f, 1.0f);   /* Foreground */
    monokai_theme.constant = make_colour(0.68f, 0.51f, 1.0f, 1.0f);    /* Purple */
    monokai_theme.error = make_colour(0.96f, 0.20f, 0.33f, 1.0f);      /* Red */
    
    vizero_theme_manager_add_theme(manager, &monokai_theme);
    
    /* Solarized Dark theme */
    vizero_colour_theme_t solarized_theme = create_theme("Solarized Dark", "Vizero Team", "Popular colour theme by Ethan Schoonover");
    solarized_theme.background = make_colour(0.00f, 0.17f, 0.21f, 1.0f);  /* base03 */
    solarized_theme.foreground = make_colour(0.51f, 0.58f, 0.59f, 1.0f);  /* base0 */
    solarized_theme.cursor = make_colour(0.51f, 0.58f, 0.59f, 1.0f);      /* base0 */
    solarized_theme.selection = make_colour(0.03f, 0.21f, 0.26f, 1.0f);   /* base02 */
    solarized_theme.line_number = make_colour(0.36f, 0.43f, 0.44f, 1.0f); /* base01 */
    solarized_theme.status_bar_bg = make_colour(0.03f, 0.21f, 0.26f, 1.0f); /* base02 */
    solarized_theme.status_bar_text = make_colour(0.51f, 0.58f, 0.59f, 1.0f); /* base0 */
    solarized_theme.window_border = make_colour(0.36f, 0.43f, 0.44f, 1.0f); /* base01 */
    
    /* Solarized syntax colours */
    solarized_theme.keyword = make_colour(0.71f, 0.54f, 0.00f, 1.0f);   /* yellow */
    solarized_theme.string = make_colour(0.16f, 0.63f, 0.60f, 1.0f);    /* cyan */
    solarized_theme.comment = make_colour(0.36f, 0.43f, 0.44f, 1.0f);   /* base01 */
    solarized_theme.number = make_colour(0.83f, 0.21f, 0.51f, 1.0f);    /* magenta */
    solarized_theme.op = make_colour(0.52f, 0.60f, 0.00f, 1.0f);        /* green */
    solarized_theme.function = make_colour(0.15f, 0.55f, 0.82f, 1.0f);  /* blue */
    solarized_theme.type = make_colour(0.71f, 0.54f, 0.00f, 1.0f);      /* yellow */
    solarized_theme.variable = make_colour(0.51f, 0.58f, 0.59f, 1.0f);  /* base0 */
    solarized_theme.constant = make_colour(0.83f, 0.21f, 0.51f, 1.0f);  /* magenta */
    solarized_theme.error = make_colour(0.86f, 0.20f, 0.18f, 1.0f);     /* red */
    
    vizero_theme_manager_add_theme(manager, &solarized_theme);
}

/* Add a theme to the manager */
int vizero_theme_manager_add_theme(vizero_theme_manager_t* manager, const vizero_colour_theme_t* theme) {
    if (!manager || !theme) return -1;
    
    /* Grow array if needed */
    if (manager->theme_count >= manager->theme_capacity) {
        size_t new_capacity = manager->theme_capacity ? manager->theme_capacity * 2 : 4;
        vizero_colour_theme_t* new_themes = (vizero_colour_theme_t*)realloc(manager->themes, 
                                                                           new_capacity * sizeof(vizero_colour_theme_t));
        if (!new_themes) return -1;
        
        manager->themes = new_themes;
        manager->theme_capacity = new_capacity;
    }
    
    /* Add the theme */
    manager->themes[manager->theme_count] = *theme;
    manager->theme_count++;
    
    return 0;
}

/* Get current theme */
const vizero_colour_theme_t* vizero_theme_manager_get_current_theme(const vizero_theme_manager_t* manager) {
    if (!manager || manager->current_theme_index >= manager->theme_count) return NULL;
    return &manager->themes[manager->current_theme_index];
}

/* Get theme by name */
const vizero_colour_theme_t* vizero_theme_manager_get_theme_by_name(const vizero_theme_manager_t* manager, const char* name) {
    if (!manager || !name) return NULL;
    
    for (size_t i = 0; i < manager->theme_count; i++) {
        if (strcmp(manager->themes[i].name, name) == 0) {
            return &manager->themes[i];
        }
    }
    
    return NULL;
}

/* Set current theme by name */
int vizero_theme_manager_set_current_theme(vizero_theme_manager_t* manager, const char* name) {
    if (!manager || !name) return -1;
    
    for (size_t i = 0; i < manager->theme_count; i++) {
        if (strcmp(manager->themes[i].name, name) == 0) {
            manager->current_theme_index = i;
            return 0;
        }
    }
    
    return -1; /* Theme not found */
}

/* Get theme count */
size_t vizero_theme_manager_get_theme_count(const vizero_theme_manager_t* manager) {
    return manager ? manager->theme_count : 0;
}

/* Get theme at index */
const vizero_colour_theme_t* vizero_theme_manager_get_theme_at_index(const vizero_theme_manager_t* manager, size_t index) {
    if (!manager || index >= manager->theme_count) return NULL;
    return &manager->themes[index];
}

/* Apply the current theme (notify application about theme change) */
void vizero_theme_manager_apply_theme(vizero_theme_manager_t* manager) {
    /* This function exists to notify the application that the theme has changed.
     * The actual color application is done by the application layer by querying
     * the current theme when rendering. */
    (void)manager; /* Suppress unused parameter warning */
}