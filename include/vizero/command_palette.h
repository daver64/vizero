#ifndef VIZERO_COMMAND_PALETTE_H
#define VIZERO_COMMAND_PALETTE_H

#include <stddef.h>
#include <stdbool.h>

#ifdef __cplusplus
extern "C" {
#endif

/* Forward declarations */
typedef struct vizero_editor_state_t vizero_editor_state_t;

/* Command palette command entry */
typedef struct vizero_command_entry_t {
    char name[128];                     /* Display name */
    char description[256];              /* Command description */
    char category[64];                  /* Command category */
    char keybind[32];                   /* Keyboard shortcut display */
    int (*execute)(vizero_editor_state_t* state, void* data); /* Command function */
    void* user_data;                    /* User data passed to execute */
    bool enabled;                       /* Whether command is currently enabled */
} vizero_command_entry_t;

/* Command palette state */
typedef struct vizero_command_palette_t {
    vizero_command_entry_t* commands;   /* Array of available commands */
    size_t command_count;               /* Number of commands */
    size_t command_capacity;            /* Allocated capacity */
    
    char search_query[256];             /* Current search query */
    size_t* filtered_indices;          /* Filtered command indices */
    size_t filtered_count;              /* Number of filtered commands */
    size_t selected_index;              /* Currently selected command */
    
    bool is_visible;                    /* Whether palette is open */
    bool fuzzy_search;                  /* Enable fuzzy matching */
} vizero_command_palette_t;

/* Command palette lifecycle */
vizero_command_palette_t* vizero_command_palette_create(void);
void vizero_command_palette_destroy(vizero_command_palette_t* palette);

/* Command management */
int vizero_command_palette_register_command(vizero_command_palette_t* palette, 
                                           const char* name, const char* description, 
                                           const char* category, const char* keybind,
                                           int (*execute)(vizero_editor_state_t*, void*),
                                           void* user_data);

int vizero_command_palette_unregister_command(vizero_command_palette_t* palette, const char* name);
void vizero_command_palette_clear_commands(vizero_command_palette_t* palette);

/* Built-in command registration */
void vizero_command_palette_register_builtin_commands(vizero_command_palette_t* palette);

/* Palette interaction */
void vizero_command_palette_show(vizero_command_palette_t* palette);
void vizero_command_palette_hide(vizero_command_palette_t* palette);
bool vizero_command_palette_is_visible(const vizero_command_palette_t* palette);

/* Search and filtering */
void vizero_command_palette_set_search_query(vizero_command_palette_t* palette, const char* query);
const char* vizero_command_palette_get_search_query(const vizero_command_palette_t* palette);
void vizero_command_palette_update_filter(vizero_command_palette_t* palette);

/* Navigation */
void vizero_command_palette_select_next(vizero_command_palette_t* palette);
void vizero_command_palette_select_previous(vizero_command_palette_t* palette);
void vizero_command_palette_select_by_index(vizero_command_palette_t* palette, size_t index);
size_t vizero_command_palette_get_selected_index(const vizero_command_palette_t* palette);

/* Execution */
int vizero_command_palette_execute_selected(vizero_command_palette_t* palette, vizero_editor_state_t* state);
int vizero_command_palette_execute_command(vizero_command_palette_t* palette, const char* name, vizero_editor_state_t* state);

/* Query results */
size_t vizero_command_palette_get_filtered_count(const vizero_command_palette_t* palette);
const vizero_command_entry_t* vizero_command_palette_get_filtered_command(const vizero_command_palette_t* palette, size_t index);

/* Configuration */
void vizero_command_palette_set_fuzzy_search(vizero_command_palette_t* palette, bool enabled);
bool vizero_command_palette_get_fuzzy_search(const vizero_command_palette_t* palette);

#ifdef __cplusplus
}
#endif

#endif /* VIZERO_COMMAND_PALETTE_H */