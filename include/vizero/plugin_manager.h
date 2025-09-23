#ifndef VIZERO_PLUGIN_MANAGER_H
#define VIZERO_PLUGIN_MANAGER_H

#ifdef __cplusplus
extern "C" {
#endif

#include "plugin_interface.h"
#include <stddef.h>

/* Plugin manager structure */
typedef struct vizero_plugin_manager_t vizero_plugin_manager_t;

/* Plugin loading and management */
vizero_plugin_manager_t* vizero_plugin_manager_create(vizero_editor_t* editor);
void vizero_plugin_manager_destroy(vizero_plugin_manager_t* manager);

/* Plugin discovery and loading */
int vizero_plugin_manager_load_plugin(vizero_plugin_manager_t* manager, const char* plugin_path);
int vizero_plugin_manager_unload_plugin(vizero_plugin_manager_t* manager, const char* plugin_name);
int vizero_plugin_manager_scan_directory(vizero_plugin_manager_t* manager, const char* directory);

/* Plugin queries */
size_t vizero_plugin_manager_get_plugin_count(vizero_plugin_manager_t* manager);
vizero_plugin_t* vizero_plugin_manager_get_plugin(vizero_plugin_manager_t* manager, size_t index);
vizero_plugin_t* vizero_plugin_manager_find_plugin(vizero_plugin_manager_t* manager, const char* name);

/* Event dispatching */
void vizero_plugin_manager_on_buffer_open(vizero_plugin_manager_t* manager, vizero_buffer_t* buffer, const char* filename);
void vizero_plugin_manager_on_buffer_close(vizero_plugin_manager_t* manager, vizero_buffer_t* buffer);
void vizero_plugin_manager_on_text_changed(vizero_plugin_manager_t* manager, vizero_buffer_t* buffer, vizero_range_t range, const char* new_text);
void vizero_plugin_manager_on_cursor_moved(vizero_plugin_manager_t* manager, vizero_cursor_t* cursor, vizero_position_t old_pos, vizero_position_t new_pos);
int vizero_plugin_manager_on_command(vizero_plugin_manager_t* manager, vizero_editor_t* editor, const char* command, const char* args);
int vizero_plugin_manager_on_key_input(vizero_plugin_manager_t* manager, vizero_editor_t* editor, uint32_t key, uint32_t modifiers);

/* Syntax highlighting */
int vizero_plugin_manager_highlight_syntax(vizero_plugin_manager_t* manager, vizero_buffer_t* buffer, 
                                          size_t start_line, size_t end_line, 
                                          vizero_syntax_token_t** tokens, size_t* token_count);

#ifdef __cplusplus
}
#endif

#endif /* VIZERO_PLUGIN_MANAGER_H */