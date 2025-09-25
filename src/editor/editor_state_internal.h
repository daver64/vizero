#ifndef VIZERO_EDITOR_STATE_INTERNAL_H
#define VIZERO_EDITOR_STATE_INTERNAL_H

#include "vizero/editor_state.h"
#include "vizero/editor_window.h"
#include "vizero/buffer.h"
#include "vizero/cursor.h"
#include "vizero/project.h"
#include "vizero/plugin_manager.h"
#include "vizero/mode_manager.h"
#include "vizero/colour_theme.h"
#include <stddef.h>
#include <stdint.h>

#ifdef __cplusplus
extern "C" {
#endif

#define MAX_BUFFERS 128
#define MAX_COMMAND_LENGTH 256

struct vizero_editor_state_t {
    vizero_editor_mode_t mode;
    vizero_window_manager_t* window_manager;
    vizero_mode_manager_t* mode_manager;
    vizero_buffer_t* buffers[MAX_BUFFERS];
    vizero_cursor_t* cursors[MAX_BUFFERS];
    size_t buffer_count;
    size_t current_buffer_index;
    vizero_project_t* current_project;
    char command_buffer[MAX_COMMAND_LENGTH];
    size_t command_length;
    vizero_plugin_manager_t* plugin_manager;
    vizero_theme_manager_t* theme_manager;
    void* session_manager; /* vizero_session_manager_t* - void* to avoid circular dependency */
    char* status_message;
    unsigned int status_message_set_time;
    unsigned int status_message_timeout_ms;
    char* last_compile_output;
    int popup_visible;
    char* popup_content;
    uint32_t popup_start_time;
    uint32_t popup_duration_ms;
    int popup_scroll_offset;
    /* Buffer selection popup state */
    int popup_is_buffer_list;
    size_t popup_selected_buffer;
    /* Settings */
    vizero_settings_t* settings;
    /* Text selection */
    int has_selection;
    vizero_position_t selection_start;
    vizero_position_t selection_end;
    /* Clipboard */
    char* clipboard_content;
    size_t clipboard_size;
    /* Undo system */
    vizero_undo_stack_t* undo_stack;
    /* Application control */
    int should_quit;
    
    /* Startup directory for finding manual.md and other resources */
    char* startup_directory;
    
    /* Help system - temporary buffer restoration */
    int help_mode_active;
    vizero_buffer_t* help_original_buffer;
    vizero_cursor_t* help_original_cursor;
    size_t help_original_buffer_index;
    
    /* Most Recently Used buffer tracking */
    size_t buffer_mru[MAX_BUFFERS];  /* MRU buffer indices (most recent first) */
    size_t buffer_mru_count;         /* Number of entries in MRU list */
    
    /* LSP Completion UI state */
    int completion_visible;
    vizero_completion_item_t* completion_items;
    size_t completion_count;
    size_t completion_selected_index;
    vizero_position_t completion_trigger_position;
};

#ifdef __cplusplus
}
#endif

#endif // VIZERO_EDITOR_STATE_INTERNAL_H
