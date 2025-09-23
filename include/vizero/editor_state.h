#ifndef VIZERO_EDITOR_STATE_H
#define VIZERO_EDITOR_STATE_H

#ifdef __cplusplus
extern "C" {
#endif

#include <stddef.h>
#include "plugin_interface.h"  /* For vizero_position_t */
#include "settings.h"  /* For vizero_settings_t */

/* Forward declarations */
typedef struct vizero_editor_state_t vizero_editor_state_t;
typedef struct vizero_buffer_t vizero_buffer_t;
typedef struct vizero_cursor_t vizero_cursor_t;
typedef struct vizero_plugin_manager_t vizero_plugin_manager_t;
typedef struct vizero_project_t vizero_project_t;

/* Editor modes */
typedef enum {
    VIZERO_MODE_NORMAL,
    VIZERO_MODE_INSERT,
    VIZERO_MODE_VISUAL,
    VIZERO_MODE_VISUAL_LINE,
    VIZERO_MODE_VISUAL_BLOCK,
    VIZERO_MODE_COMMAND
} vizero_editor_mode_t;

/* Undo system */
typedef enum {
    VIZERO_UNDO_INSERT_CHAR,
    VIZERO_UNDO_DELETE_CHAR,
    VIZERO_UNDO_INSERT_LINE,
    VIZERO_UNDO_DELETE_LINE,
    VIZERO_UNDO_INSERT_TEXT,
    VIZERO_UNDO_DELETE_RANGE
} vizero_undo_type_t;

typedef struct {
    vizero_undo_type_t type;
    vizero_position_t position;
    vizero_position_t end_position;  /* For range operations */
    char* text;                      /* Text content for undo */
    size_t text_length;
} vizero_undo_operation_t;

#define MAX_UNDO_OPERATIONS 1000

typedef struct {
    vizero_undo_operation_t* operations;
    size_t count;
    size_t capacity;
    size_t current_index;
} vizero_undo_stack_t;

/* Editor state creation and destruction */
vizero_editor_state_t* vizero_editor_state_create(void);
vizero_editor_state_t* vizero_editor_state_create_with_settings(vizero_settings_t* settings);
void vizero_editor_state_destroy(vizero_editor_state_t* state);

/* Mode management */
vizero_editor_mode_t vizero_editor_get_mode(vizero_editor_state_t* state);
void vizero_editor_set_mode(vizero_editor_state_t* state, vizero_editor_mode_t mode);

/* Buffer management */
vizero_buffer_t* vizero_editor_get_current_buffer(vizero_editor_state_t* state);
vizero_cursor_t* vizero_editor_get_current_cursor(vizero_editor_state_t* state);
size_t vizero_editor_get_current_buffer_index(vizero_editor_state_t* state);
size_t vizero_editor_get_buffer_count(vizero_editor_state_t* state);
vizero_buffer_t* vizero_editor_get_buffer(vizero_editor_state_t* state, size_t index);

/* Buffer operations */
int vizero_editor_open_buffer(vizero_editor_state_t* state, const char* filename);
int vizero_editor_close_buffer(vizero_editor_state_t* state, vizero_buffer_t* buffer);
int vizero_editor_switch_buffer(vizero_editor_state_t* state, size_t buffer_index);
int vizero_editor_next_buffer(vizero_editor_state_t* state);
int vizero_editor_previous_buffer(vizero_editor_state_t* state);
int vizero_editor_create_new_buffer(vizero_editor_state_t* state, const char* name);
int vizero_editor_close_current_buffer(vizero_editor_state_t* state);
int vizero_editor_close_buffer_by_index(vizero_editor_state_t* state, size_t buffer_index);

/* Project management */
vizero_project_t* vizero_editor_get_current_project(vizero_editor_state_t* state);
int vizero_editor_open_project(vizero_editor_state_t* state, const char* root_directory);
int vizero_editor_close_project(vizero_editor_state_t* state);
int vizero_editor_save_project_workspace(vizero_editor_state_t* state, const char* filename);

/* Command execution */
int vizero_editor_execute_command(vizero_editor_state_t* state, const char* command);
void vizero_editor_set_status_message(vizero_editor_state_t* state, const char* message);
const char* vizero_editor_get_status_message(vizero_editor_state_t* state);

/* Command mode */
const char* vizero_editor_get_command_buffer(vizero_editor_state_t* state);
void vizero_editor_clear_command_buffer(vizero_editor_state_t* state);
int vizero_editor_append_to_command(vizero_editor_state_t* state, char c);
int vizero_editor_backspace_command(vizero_editor_state_t* state);
int vizero_editor_execute_current_command(vizero_editor_state_t* state);

/* Plugin manager integration */
void vizero_editor_set_plugin_manager(vizero_editor_state_t* state, vizero_plugin_manager_t* manager);
vizero_plugin_manager_t* vizero_editor_get_plugin_manager(vizero_editor_state_t* state);

/* Settings */
typedef struct vizero_settings_t vizero_settings_t;
vizero_settings_t* vizero_editor_get_settings(vizero_editor_state_t* state);

/* Text selection */
int vizero_editor_has_selection(vizero_editor_state_t* state);
void vizero_editor_start_selection(vizero_editor_state_t* state);
void vizero_editor_start_selection_at(vizero_editor_state_t* state, size_t line, size_t column);
void vizero_editor_update_selection(vizero_editor_state_t* state);
void vizero_editor_clear_selection(vizero_editor_state_t* state);
void vizero_editor_get_selection_range(vizero_editor_state_t* state, vizero_position_t* start, vizero_position_t* end);
char* vizero_editor_get_selected_text(vizero_editor_state_t* state);

/* Clipboard operations */
int vizero_editor_copy_selection(vizero_editor_state_t* state);
int vizero_editor_copy_current_line(vizero_editor_state_t* state);
int vizero_editor_cut_selection(vizero_editor_state_t* state);
int vizero_editor_cut_current_line(vizero_editor_state_t* state);
int vizero_editor_paste_at_cursor(vizero_editor_state_t* state);
const char* vizero_editor_get_clipboard_content(vizero_editor_state_t* state);

/* Undo/Redo operations */
int vizero_editor_undo(vizero_editor_state_t* state);
int vizero_editor_redo(vizero_editor_state_t* state);
void vizero_editor_push_undo_operation(vizero_editor_state_t* state, vizero_undo_type_t type, 
                                       vizero_position_t position, vizero_position_t end_position, 
                                       const char* text);

/* Popup system */
void vizero_editor_show_popup(vizero_editor_state_t* state, const char* content, uint32_t duration_ms);
void vizero_editor_hide_popup(vizero_editor_state_t* state);
int vizero_editor_is_popup_visible(vizero_editor_state_t* state);
const char* vizero_editor_get_popup_content(vizero_editor_state_t* state);
uint32_t vizero_editor_get_popup_duration(vizero_editor_state_t* state);
void vizero_editor_scroll_popup(vizero_editor_state_t* state, int lines);
int vizero_editor_get_popup_scroll_offset(vizero_editor_state_t* state);

/* Application control */
int vizero_editor_should_quit(vizero_editor_state_t* state);
void vizero_editor_set_quit_flag(vizero_editor_state_t* state);

/* Search and Replace */
typedef enum {
    VIZERO_SEARCH_FORWARD = 0,
    VIZERO_SEARCH_BACKWARD = 1
} vizero_search_direction_t;

typedef struct {
    int line;
    int column;
    int length;
} vizero_search_match_t;

/* Search functions */
int vizero_editor_search(vizero_editor_state_t* state, const char* pattern, vizero_search_direction_t direction);
int vizero_editor_search_next(vizero_editor_state_t* state);
int vizero_editor_search_previous(vizero_editor_state_t* state);
int vizero_editor_substitute(vizero_editor_state_t* state, const char* pattern, const char* replacement, 
                           int line_start, int line_end, int global);

/* Search state management */
void vizero_editor_clear_search(vizero_editor_state_t* state);
int vizero_editor_has_search_results(vizero_editor_state_t* state);
const char* vizero_editor_get_search_pattern(vizero_editor_state_t* state);
int vizero_editor_get_search_match_count(vizero_editor_state_t* state);
int vizero_editor_get_current_match_index(vizero_editor_state_t* state);

#ifdef __cplusplus
}
#endif

#endif /* VIZERO_EDITOR_STATE_H */