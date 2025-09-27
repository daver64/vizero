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

static int api_insert_text_multiline(vizero_buffer_t* buffer, vizero_position_t pos, const char* text) {
    if (!buffer || !text) return -1;
    
    size_t current_line = pos.line;
    size_t current_col = pos.column;
    const char* segment_start = text;
    
    for (const char* c = text; *c; c++) {
        if (*c == '\n') {
            /* Insert text segment before newline */
            if (c > segment_start) {
                /* Create temporary string for this segment */
                size_t segment_len = c - segment_start;
                char* segment = (char*)malloc(segment_len + 1);
                if (!segment) return -1;
                strncpy(segment, segment_start, segment_len);
                segment[segment_len] = '\0';
                
                /* Insert the segment */
                if (vizero_buffer_insert_text(buffer, current_line, current_col, segment) != 0) {
                    free(segment);
                    return -1;
                }
                current_col += segment_len;
                free(segment);
            }
            
            /* Split the line at current position to create newline */
            if (vizero_buffer_split_line(buffer, current_line, current_col) != 0) {
                return -1;
            }
            
            /* Move to next line, column 0 */
            current_line++;
            current_col = 0;
            segment_start = c + 1;  /* Start after the newline */
        }
    }
    
    /* Insert remaining text after last newline (if any) */
    if (*segment_start) {
        if (vizero_buffer_insert_text(buffer, current_line, current_col, segment_start) != 0) {
            return -1;
        }
    }
    
    return 0;
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

static int api_set_status_message_with_timeout(vizero_editor_t* editor, const char* message, unsigned int timeout_ms) {
    vizero_editor_set_status_message_with_timeout((vizero_editor_state_t*)editor, message, timeout_ms);
    return 0;
}

/* Buffer readonly operations */
static int api_is_buffer_readonly(vizero_buffer_t* buffer) {
    return vizero_buffer_is_readonly(buffer);
}

static void api_set_buffer_readonly(vizero_buffer_t* buffer, int readonly) {
    vizero_buffer_set_readonly(buffer, readonly);
}

static int api_is_buffer_scratch(vizero_buffer_t* buffer) {
    return vizero_buffer_is_scratch(buffer);
}

static void api_set_buffer_scratch(vizero_buffer_t* buffer, int scratch) {
    vizero_buffer_set_scratch(buffer, scratch);
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

/* Popup operations */
static int api_show_popup(vizero_editor_t* editor, const char* content, uint32_t duration_ms) {
    if (!editor || !content) return -1;
    
    vizero_editor_show_popup((vizero_editor_state_t*)editor, content, duration_ms);
    return 0;
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
    api->insert_text_multiline = api_insert_text_multiline;
    api->delete_text = api_delete_text;
    api->is_buffer_readonly = api_is_buffer_readonly;
    api->set_buffer_readonly = api_set_buffer_readonly;
    api->is_buffer_scratch = api_is_buffer_scratch;
    api->set_buffer_scratch = api_set_buffer_scratch;
    
    /* Cursor operations */
    api->get_cursor_position = api_get_cursor_position;
    api->set_cursor_position = api_set_cursor_position;
    
    /* Editor operations */
    api->get_current_buffer = api_get_current_buffer;
    api->get_current_cursor = api_get_current_cursor;
    api->execute_command = api_execute_command;
    api->set_status_message = api_set_status_message;
    api->set_status_message_with_timeout = api_set_status_message_with_timeout;
    
    /* File operations */
    api->open_file = api_open_file;
    api->save_file = api_save_file;
    
    /* Rendering operations */
    api->add_syntax_tokens = api_add_syntax_tokens;
    api->clear_syntax_tokens = api_clear_syntax_tokens;
    
    /* Popup operations */
    api->show_popup = api_show_popup;
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
