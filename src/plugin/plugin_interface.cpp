/* Plugin interface implementation - API glue code */
#include "vizero/plugin_interface.h"
#include "vizero/buffer.h"
#include "vizero/cursor.h"
#include "vizero/editor_state.h"
#include <string.h>

/* API implementation functions for plugins */

/* Buffer operations */
static const char* api_get_buffer_text(vizero_buffer_t* buffer) {
    /* Return full buffer text - would need to be implemented in buffer.cpp */
    return vizero_buffer_get_text(buffer);
}

static const char* api_get_buffer_filename(vizero_buffer_t* buffer) {
    return vizero_buffer_get_filename(buffer);
}

static size_t api_get_buffer_line_count(vizero_buffer_t* buffer) {
    return vizero_buffer_get_line_count(buffer);
}

static const char* api_get_buffer_line(vizero_buffer_t* buffer, size_t line_num) {
    return vizero_buffer_get_line_text(buffer, line_num);
}

static size_t api_get_buffer_line_length(vizero_buffer_t* buffer, size_t line_num) {
    return vizero_buffer_get_line_length(buffer, line_num);
}

static int api_insert_text(vizero_buffer_t* buffer, vizero_position_t pos, const char* text) {
    return vizero_buffer_insert_text(buffer, pos.line, pos.column, text);
}

static int api_delete_text(vizero_buffer_t* buffer, vizero_range_t range) {
    /* This would need a new buffer function for range deletion */
    return vizero_buffer_delete_range(buffer, range.start.line, range.start.column, 
                                     range.end.line, range.end.column);
}

/* Cursor operations */
static vizero_position_t api_get_cursor_position(vizero_cursor_t* cursor) {
    return vizero_cursor_get_position(cursor);
}

static int api_set_cursor_position(vizero_cursor_t* cursor, vizero_position_t pos) {
    vizero_cursor_set_position(cursor, pos.line, pos.column);
    return 0;
}

/* Editor operations */
static vizero_buffer_t* api_get_current_buffer(vizero_editor_t* editor) {
    return vizero_editor_get_current_buffer((vizero_editor_state_t*)editor);
}

static vizero_cursor_t* api_get_current_cursor(vizero_editor_t* editor) {
    return vizero_editor_get_current_cursor((vizero_editor_state_t*)editor);
}

static int api_execute_command(vizero_editor_t* editor, const char* command) {
    return vizero_editor_execute_command((vizero_editor_state_t*)editor, command);
}

static int api_set_status_message(vizero_editor_t* editor, const char* message) {
    vizero_editor_set_status_message((vizero_editor_state_t*)editor, message);
    return 0;
}

/* File operations */
static int api_open_file(vizero_editor_t* editor, const char* filename) {
    return vizero_editor_open_buffer((vizero_editor_state_t*)editor, filename);
}

static int api_save_file(vizero_editor_t* editor, const char* filename) {
    /* Save current buffer to specified filename */
    vizero_buffer_t* buffer = vizero_editor_get_current_buffer((vizero_editor_state_t*)editor);
    if (!buffer) return -1;
    
    return vizero_buffer_save_to_file(buffer, filename);
}

/* Rendering operations */
static int api_add_syntax_tokens(vizero_editor_t* editor, vizero_syntax_token_t* tokens, size_t count) {
    /* This would need renderer integration */
    (void)editor; (void)tokens; (void)count;
    return 0; /* Stub for now */
}

static int api_clear_syntax_tokens(vizero_editor_t* editor) {
    /* This would need renderer integration */
    (void)editor;
    return 0; /* Stub for now */
}

/* Initialize a plugin API structure with all function pointers */
void vizero_plugin_interface_init_api(vizero_editor_api_t* api, vizero_editor_t* editor) {
    if (!api) return;
    
    (void)editor; /* Unused parameter */
    
    /* Buffer operations */
    api->get_buffer_text = api_get_buffer_text;
    api->get_buffer_filename = api_get_buffer_filename;
    api->get_buffer_line_count = api_get_buffer_line_count;
    api->get_buffer_line = api_get_buffer_line;
    api->get_buffer_line_length = api_get_buffer_line_length;
    api->insert_text = api_insert_text;
    api->delete_text = api_delete_text;
    
    /* Cursor operations */
    api->get_cursor_position = api_get_cursor_position;
    api->set_cursor_position = api_set_cursor_position;
    
    /* Editor operations */
    api->get_current_buffer = api_get_current_buffer;
    api->get_current_cursor = api_get_current_cursor;
    api->execute_command = api_execute_command;
    api->set_status_message = api_set_status_message;
    
    /* File operations */
    api->open_file = api_open_file;
    api->save_file = api_save_file;
    
    /* Rendering operations */
    api->add_syntax_tokens = api_add_syntax_tokens;
    api->clear_syntax_tokens = api_clear_syntax_tokens;
}

/* Helper function to validate plugin API version compatibility */
int vizero_plugin_interface_check_version(const vizero_plugin_info_t* info) {
    if (!info) return 0;
    
    /* Check major version compatibility */
    if (info->api_version_major != VIZERO_PLUGIN_API_VERSION_MAJOR) {
        return 0; /* Incompatible major version */
    }
    
    /* Check minor version - we support backwards compatibility */
    if (info->api_version_minor > VIZERO_PLUGIN_API_VERSION_MINOR) {
        return 0; /* Plugin requires newer API */
    }
    
    return 1; /* Compatible */
}
