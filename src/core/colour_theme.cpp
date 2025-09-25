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
    
    /* MSVC Light theme */
    vizero_colour_theme_t msvc_light_theme = create_theme("MSVC Light", "Vizero Team", "Light theme inspired by Visual Studio");
    msvc_light_theme.background = make_colour(1.0f, 1.0f, 1.0f, 1.0f);       /* White */
    msvc_light_theme.foreground = make_colour(0.0f, 0.0f, 0.0f, 1.0f);       /* Black */
    msvc_light_theme.cursor = make_colour(0.0f, 0.0f, 0.0f, 1.0f);           /* Black */
    msvc_light_theme.selection = make_colour(0.26f, 0.40f, 0.70f, 0.4f);     /* Light blue */
    msvc_light_theme.line_number = make_colour(0.44f, 0.44f, 0.44f, 1.0f);   /* Gray */
    msvc_light_theme.status_bar_bg = make_colour(0.95f, 0.95f, 0.95f, 1.0f); /* Light gray */
    msvc_light_theme.status_bar_text = make_colour(0.0f, 0.0f, 0.0f, 1.0f);  /* Black */
    msvc_light_theme.window_border = make_colour(0.80f, 0.80f, 0.80f, 1.0f); /* Light gray */
    
    /* MSVC Light syntax colours */
    msvc_light_theme.keyword = make_colour(0.0f, 0.0f, 1.0f, 1.0f);          /* Blue */
    msvc_light_theme.string = make_colour(0.64f, 0.08f, 0.08f, 1.0f);        /* Dark red */
    msvc_light_theme.comment = make_colour(0.0f, 0.50f, 0.0f, 1.0f);         /* Green */
    msvc_light_theme.number = make_colour(0.0f, 0.0f, 0.0f, 1.0f);           /* Black */
    msvc_light_theme.op = make_colour(0.0f, 0.0f, 0.0f, 1.0f);               /* Black */
    msvc_light_theme.function = make_colour(0.16f, 0.16f, 0.16f, 1.0f);      /* Dark gray */
    msvc_light_theme.type = make_colour(0.0f, 0.50f, 0.50f, 1.0f);           /* Teal */
    msvc_light_theme.variable = make_colour(0.0f, 0.0f, 0.0f, 1.0f);         /* Black */
    msvc_light_theme.constant = make_colour(0.0f, 0.0f, 0.0f, 1.0f);         /* Black */
    msvc_light_theme.error = make_colour(1.0f, 0.0f, 0.0f, 1.0f);            /* Red */
    
    vizero_theme_manager_add_theme(manager, &msvc_light_theme);
    
    /* MSVC Blue theme */
    vizero_colour_theme_t msvc_blue_theme = create_theme("MSVC Blue", "Vizero Team", "Blue theme inspired by Visual Studio");
    msvc_blue_theme.background = make_colour(0.93f, 0.96f, 1.0f, 1.0f);      /* Very light blue */
    msvc_blue_theme.foreground = make_colour(0.0f, 0.0f, 0.0f, 1.0f);        /* Black */
    msvc_blue_theme.cursor = make_colour(0.0f, 0.0f, 0.0f, 1.0f);            /* Black */
    msvc_blue_theme.selection = make_colour(0.26f, 0.40f, 0.70f, 0.4f);      /* Light blue */
    msvc_blue_theme.line_number = make_colour(0.44f, 0.44f, 0.44f, 1.0f);    /* Gray */
    msvc_blue_theme.status_bar_bg = make_colour(0.85f, 0.90f, 0.95f, 1.0f);  /* Light blue gray */
    msvc_blue_theme.status_bar_text = make_colour(0.0f, 0.0f, 0.0f, 1.0f);   /* Black */
    msvc_blue_theme.window_border = make_colour(0.70f, 0.80f, 0.90f, 1.0f);  /* Blue gray */
    
    /* MSVC Blue syntax colours */
    msvc_blue_theme.keyword = make_colour(0.0f, 0.0f, 1.0f, 1.0f);           /* Blue */
    msvc_blue_theme.string = make_colour(0.64f, 0.08f, 0.08f, 1.0f);         /* Dark red */
    msvc_blue_theme.comment = make_colour(0.0f, 0.50f, 0.0f, 1.0f);          /* Green */
    msvc_blue_theme.number = make_colour(0.0f, 0.0f, 0.0f, 1.0f);            /* Black */
    msvc_blue_theme.op = make_colour(0.0f, 0.0f, 0.0f, 1.0f);                /* Black */
    msvc_blue_theme.function = make_colour(0.16f, 0.16f, 0.16f, 1.0f);       /* Dark gray */
    msvc_blue_theme.type = make_colour(0.0f, 0.50f, 0.50f, 1.0f);            /* Teal */
    msvc_blue_theme.variable = make_colour(0.0f, 0.0f, 0.0f, 1.0f);          /* Black */
    msvc_blue_theme.constant = make_colour(0.0f, 0.0f, 0.0f, 1.0f);          /* Black */
    msvc_blue_theme.error = make_colour(1.0f, 0.0f, 0.0f, 1.0f);             /* Red */
    
    vizero_theme_manager_add_theme(manager, &msvc_blue_theme);
    
    /* GVim Default theme */
    vizero_colour_theme_t gvim_default_theme = create_theme("GVim Default", "Vizero Team", "Classic GVim default color scheme");
    gvim_default_theme.background = make_colour(1.0f, 1.0f, 1.0f, 1.0f);     /* White */
    gvim_default_theme.foreground = make_colour(0.0f, 0.0f, 0.0f, 1.0f);     /* Black */
    gvim_default_theme.cursor = make_colour(0.0f, 0.0f, 0.0f, 1.0f);         /* Black */
    gvim_default_theme.selection = make_colour(0.69f, 0.88f, 1.0f, 0.8f);    /* Light blue */
    gvim_default_theme.line_number = make_colour(0.64f, 0.39f, 0.13f, 1.0f); /* Brown */
    gvim_default_theme.status_bar_bg = make_colour(0.85f, 0.85f, 0.85f, 1.0f); /* Light gray */
    gvim_default_theme.status_bar_text = make_colour(0.0f, 0.0f, 0.0f, 1.0f); /* Black */
    gvim_default_theme.window_border = make_colour(0.75f, 0.75f, 0.75f, 1.0f); /* Gray */
    
    /* GVim Default syntax colours */
    gvim_default_theme.keyword = make_colour(0.50f, 0.0f, 0.50f, 1.0f);      /* Purple */
    gvim_default_theme.string = make_colour(1.0f, 0.0f, 1.0f, 1.0f);         /* Magenta */
    gvim_default_theme.comment = make_colour(0.0f, 0.0f, 1.0f, 1.0f);        /* Blue */
    gvim_default_theme.number = make_colour(1.0f, 0.0f, 1.0f, 1.0f);         /* Magenta */
    gvim_default_theme.op = make_colour(0.0f, 0.0f, 0.0f, 1.0f);             /* Black */
    gvim_default_theme.function = make_colour(0.0f, 0.0f, 0.0f, 1.0f);       /* Black */
    gvim_default_theme.type = make_colour(0.0f, 0.50f, 0.0f, 1.0f);          /* Green */
    gvim_default_theme.variable = make_colour(0.0f, 0.0f, 0.0f, 1.0f);       /* Black */
    gvim_default_theme.constant = make_colour(1.0f, 0.0f, 1.0f, 1.0f);       /* Magenta */
    gvim_default_theme.error = make_colour(1.0f, 1.0f, 1.0f, 1.0f);          /* White on red bg */
    
    vizero_theme_manager_add_theme(manager, &gvim_default_theme);
    
    /* GVim Desert theme */
    vizero_colour_theme_t gvim_desert_theme = create_theme("GVim Desert", "Vizero Team", "Popular desert color scheme from GVim");
    gvim_desert_theme.background = make_colour(0.20f, 0.20f, 0.20f, 1.0f);   /* Dark gray */
    gvim_desert_theme.foreground = make_colour(1.0f, 1.0f, 1.0f, 1.0f);     /* White */
    gvim_desert_theme.cursor = make_colour(1.0f, 1.0f, 0.0f, 1.0f);         /* Yellow */
    gvim_desert_theme.selection = make_colour(0.5f, 0.5f, 0.0f, 0.5f);      /* Dark yellow */
    gvim_desert_theme.line_number = make_colour(1.0f, 0.8f, 0.4f, 1.0f);    /* Light orange */
    gvim_desert_theme.status_bar_bg = make_colour(0.15f, 0.15f, 0.15f, 1.0f); /* Darker gray */
    gvim_desert_theme.status_bar_text = make_colour(1.0f, 1.0f, 1.0f, 1.0f); /* White */
    gvim_desert_theme.window_border = make_colour(0.4f, 0.4f, 0.4f, 1.0f);  /* Gray */
    
    /* GVim Desert syntax colours */
    gvim_desert_theme.keyword = make_colour(1.0f, 0.8f, 0.4f, 1.0f);        /* Light orange */
    gvim_desert_theme.string = make_colour(0.9f, 0.7f, 0.7f, 1.0f);         /* Light pink */
    gvim_desert_theme.comment = make_colour(0.5f, 0.6f, 0.7f, 1.0f);        /* Light blue */
    gvim_desert_theme.number = make_colour(1.0f, 0.5f, 0.5f, 1.0f);         /* Light red */
    gvim_desert_theme.op = make_colour(1.0f, 1.0f, 1.0f, 1.0f);             /* White */
    gvim_desert_theme.function = make_colour(0.9f, 0.9f, 0.6f, 1.0f);       /* Light yellow */
    gvim_desert_theme.type = make_colour(0.6f, 1.0f, 0.6f, 1.0f);           /* Light green */
    gvim_desert_theme.variable = make_colour(1.0f, 1.0f, 1.0f, 1.0f);       /* White */
    gvim_desert_theme.constant = make_colour(1.0f, 0.5f, 0.5f, 1.0f);       /* Light red */
    gvim_desert_theme.error = make_colour(1.0f, 0.0f, 0.0f, 1.0f);          /* Red */
    
    vizero_theme_manager_add_theme(manager, &gvim_desert_theme);
    
    /* GVim Evening theme */
    vizero_colour_theme_t gvim_evening_theme = create_theme("GVim Evening", "Vizero Team", "Evening color scheme from GVim");
    gvim_evening_theme.background = make_colour(0.11f, 0.11f, 0.20f, 1.0f);  /* Dark blue */
    gvim_evening_theme.foreground = make_colour(1.0f, 1.0f, 1.0f, 1.0f);    /* White */
    gvim_evening_theme.cursor = make_colour(1.0f, 1.0f, 0.0f, 1.0f);        /* Yellow */
    gvim_evening_theme.selection = make_colour(0.3f, 0.3f, 0.5f, 0.7f);     /* Dark blue */
    gvim_evening_theme.line_number = make_colour(0.8f, 0.8f, 0.4f, 1.0f);   /* Light yellow */
    gvim_evening_theme.status_bar_bg = make_colour(0.08f, 0.08f, 0.15f, 1.0f); /* Darker blue */
    gvim_evening_theme.status_bar_text = make_colour(1.0f, 1.0f, 1.0f, 1.0f); /* White */
    gvim_evening_theme.window_border = make_colour(0.3f, 0.3f, 0.5f, 1.0f);  /* Blue gray */
    
    /* GVim Evening syntax colours */
    gvim_evening_theme.keyword = make_colour(1.0f, 1.0f, 0.0f, 1.0f);       /* Yellow */
    gvim_evening_theme.string = make_colour(1.0f, 0.6f, 1.0f, 1.0f);        /* Light magenta */
    gvim_evening_theme.comment = make_colour(0.5f, 0.8f, 1.0f, 1.0f);       /* Light blue */
    gvim_evening_theme.number = make_colour(0.0f, 1.0f, 1.0f, 1.0f);        /* Cyan */
    gvim_evening_theme.op = make_colour(1.0f, 1.0f, 1.0f, 1.0f);            /* White */
    gvim_evening_theme.function = make_colour(0.6f, 1.0f, 0.6f, 1.0f);      /* Light green */
    gvim_evening_theme.type = make_colour(0.0f, 1.0f, 0.0f, 1.0f);          /* Green */
    gvim_evening_theme.variable = make_colour(1.0f, 1.0f, 1.0f, 1.0f);      /* White */
    gvim_evening_theme.constant = make_colour(0.0f, 1.0f, 1.0f, 1.0f);      /* Cyan */
    gvim_evening_theme.error = make_colour(1.0f, 0.0f, 0.0f, 1.0f);         /* Red */
    
    vizero_theme_manager_add_theme(manager, &gvim_evening_theme);
    
    /* GitHub Light theme */
    vizero_colour_theme_t github_light_theme = create_theme("GitHub Light", "Vizero Team", "Light theme inspired by GitHub");
    github_light_theme.background = make_colour(1.0f, 1.0f, 1.0f, 1.0f);    /* White */
    github_light_theme.foreground = make_colour(0.14f, 0.16f, 0.18f, 1.0f); /* Dark gray */
    github_light_theme.cursor = make_colour(0.14f, 0.16f, 0.18f, 1.0f);     /* Dark gray */
    github_light_theme.selection = make_colour(0.67f, 0.84f, 1.0f, 0.6f);   /* Light blue */
    github_light_theme.line_number = make_colour(0.46f, 0.53f, 0.60f, 1.0f); /* Gray */
    github_light_theme.status_bar_bg = make_colour(0.96f, 0.97f, 0.98f, 1.0f); /* Very light gray */
    github_light_theme.status_bar_text = make_colour(0.14f, 0.16f, 0.18f, 1.0f); /* Dark gray */
    github_light_theme.window_border = make_colour(0.85f, 0.87f, 0.89f, 1.0f); /* Light gray */
    
    /* GitHub Light syntax colours */
    github_light_theme.keyword = make_colour(0.84f, 0.18f, 0.51f, 1.0f);    /* Pink */
    github_light_theme.string = make_colour(0.03f, 0.52f, 0.11f, 1.0f);     /* Green */
    github_light_theme.comment = make_colour(0.40f, 0.48f, 0.55f, 1.0f);    /* Gray */
    github_light_theme.number = make_colour(0.0f, 0.36f, 0.75f, 1.0f);      /* Blue */
    github_light_theme.op = make_colour(0.14f, 0.16f, 0.18f, 1.0f);         /* Dark gray */
    github_light_theme.function = make_colour(0.49f, 0.26f, 0.85f, 1.0f);   /* Purple */
    github_light_theme.type = make_colour(0.84f, 0.18f, 0.51f, 1.0f);       /* Pink */
    github_light_theme.variable = make_colour(0.90f, 0.35f, 0.0f, 1.0f);    /* Orange */
    github_light_theme.constant = make_colour(0.0f, 0.36f, 0.75f, 1.0f);    /* Blue */
    github_light_theme.error = make_colour(0.82f, 0.10f, 0.14f, 1.0f);      /* Red */
    
    vizero_theme_manager_add_theme(manager, &github_light_theme);
    
    /* Gruvbox Dark theme */
    vizero_colour_theme_t gruvbox_dark_theme = create_theme("Gruvbox Dark", "Vizero Team", "Retro groove color scheme");
    gruvbox_dark_theme.background = make_colour(0.16f, 0.16f, 0.16f, 1.0f);  /* Dark gray */
    gruvbox_dark_theme.foreground = make_colour(0.92f, 0.86f, 0.70f, 1.0f);  /* Light beige */
    gruvbox_dark_theme.cursor = make_colour(0.92f, 0.86f, 0.70f, 1.0f);      /* Light beige */
    gruvbox_dark_theme.selection = make_colour(0.24f, 0.24f, 0.24f, 1.0f);   /* Darker gray */
    gruvbox_dark_theme.line_number = make_colour(0.50f, 0.48f, 0.42f, 1.0f); /* Medium gray */
    gruvbox_dark_theme.status_bar_bg = make_colour(0.13f, 0.13f, 0.13f, 1.0f); /* Very dark gray */
    gruvbox_dark_theme.status_bar_text = make_colour(0.92f, 0.86f, 0.70f, 1.0f); /* Light beige */
    gruvbox_dark_theme.window_border = make_colour(0.33f, 0.32f, 0.29f, 1.0f); /* Dark beige */
    
    /* Gruvbox Dark syntax colours */
    gruvbox_dark_theme.keyword = make_colour(0.98f, 0.31f, 0.31f, 1.0f);     /* Red */
    gruvbox_dark_theme.string = make_colour(0.72f, 0.73f, 0.15f, 1.0f);      /* Yellow-green */
    gruvbox_dark_theme.comment = make_colour(0.50f, 0.48f, 0.42f, 1.0f);     /* Gray */
    gruvbox_dark_theme.number = make_colour(0.83f, 0.46f, 0.83f, 1.0f);      /* Purple */
    gruvbox_dark_theme.op = make_colour(0.92f, 0.86f, 0.70f, 1.0f);          /* Light beige */
    gruvbox_dark_theme.function = make_colour(0.55f, 0.75f, 0.26f, 1.0f);    /* Green */
    gruvbox_dark_theme.type = make_colour(0.98f, 0.74f, 0.27f, 1.0f);        /* Orange */
    gruvbox_dark_theme.variable = make_colour(0.51f, 0.64f, 0.80f, 1.0f);    /* Blue */
    gruvbox_dark_theme.constant = make_colour(0.83f, 0.46f, 0.83f, 1.0f);    /* Purple */
    gruvbox_dark_theme.error = make_colour(0.98f, 0.31f, 0.31f, 1.0f);       /* Red */
    
    vizero_theme_manager_add_theme(manager, &gruvbox_dark_theme);
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