#ifndef VIZERO_PLUGIN_MANAGER_H
#define VIZERO_PLUGIN_MANAGER_H

#ifdef __cplusplus
extern "C" {
#endif

#include "plugin_interface.h"
#include "plugin_registry.h"
#include <stddef.h>
#include <stdbool.h>

/* Plugin manager structure */
typedef struct vizero_plugin_manager_t vizero_plugin_manager_t;

/* Plugin loading and management */
vizero_plugin_manager_t* vizero_plugin_manager_create(vizero_editor_t* editor);
void vizero_plugin_manager_destroy(vizero_plugin_manager_t* manager);

/* Plugin discovery and loading */
int vizero_plugin_manager_load_plugin(vizero_plugin_manager_t* manager, const char* plugin_path);
int vizero_plugin_manager_unload_plugin(vizero_plugin_manager_t* manager, const char* plugin_name);
int vizero_plugin_manager_scan_directory(vizero_plugin_manager_t* manager, const char* directory);

/* Registry management */
int vizero_plugin_manager_load_manifest(vizero_plugin_manager_t* manager, const char* manifest_path);

/* On-demand loading */
int vizero_plugin_manager_load_plugins_for_file(vizero_plugin_manager_t* manager, const char* filename);
int vizero_plugin_manager_ensure_always_loaded(vizero_plugin_manager_t* manager);

/* Plugin queries */
bool vizero_plugin_manager_is_plugin_loaded(vizero_plugin_manager_t* manager, const char* plugin_name);

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
// Updated API: caller allocates tokens buffer, plugin fills it
int vizero_plugin_manager_highlight_syntax(
    vizero_plugin_manager_t* manager,
    vizero_buffer_t* buffer,
    size_t start_line,
    size_t end_line,
    vizero_syntax_token_t* tokens,
    size_t max_tokens,
    size_t* token_count);

/* LSP functionality */
int vizero_plugin_manager_lsp_completion(
    vizero_plugin_manager_t* manager,
    vizero_buffer_t* buffer,
    vizero_position_t position,
    vizero_completion_list_t** result);

int vizero_plugin_manager_lsp_hover(
    vizero_plugin_manager_t* manager,
    vizero_buffer_t* buffer,
    vizero_position_t position,
    char** hover_text);

int vizero_plugin_manager_lsp_goto_definition(
    vizero_plugin_manager_t* manager,
    vizero_buffer_t* buffer,
    vizero_position_t position,
    vizero_location_t** locations,
    size_t* location_count);

/* Process LSP messages for all plugins (non-blocking) */
void vizero_plugin_manager_process_lsp_messages(vizero_plugin_manager_t* manager);

/* Check for pending LSP completion results */
int vizero_plugin_manager_check_completion_results(
    vizero_plugin_manager_t* manager,
    vizero_completion_list_t** result);

#ifdef __cplusplus
}
#endif

#endif /* VIZERO_PLUGIN_MANAGER_H */