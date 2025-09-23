#include "vizero/plugin_interface.h"
#include <stdio.h>
#include <stdlib.h>
#include <string.h>

/* Plugin state */
static struct {
    vizero_editor_t* editor;
    const vizero_editor_api_t* api;
    int initialized;
} plugin_state = {0};

/* Plugin information */
VIZERO_PLUGIN_DEFINE_INFO(
    "Example Plugin",
    "1.0.0", 
    "Vizero Team",
    "Example plugin demonstrating the plugin system",
    VIZERO_PLUGIN_TYPE_GENERIC
)

/* Wrapper function for cleanup callback */
static void cleanup_wrapper(void) {
    /* The actual cleanup will be called separately */
}

/* Plugin callback implementations */
static int on_buffer_open(vizero_buffer_t* buffer, const char* filename) {
    (void)buffer; /* Unused parameter */
    
    if (plugin_state.api && plugin_state.editor) {
        char message[256];
        snprintf(message, sizeof(message), "Example plugin: Opened file %s", filename);
        plugin_state.api->set_status_message(plugin_state.editor, message);
    }
    return 0;
}

static void on_buffer_close(vizero_buffer_t* buffer) {
    (void)buffer; /* Unused parameter */
    
    if (plugin_state.api && plugin_state.api->set_status_message) {
        plugin_state.api->set_status_message(plugin_state.editor, "Example plugin: Buffer closed");
    }
}

static int on_text_changed(vizero_buffer_t* buffer, vizero_range_t range, const char* new_text) {
    (void)range; /* Unused parameter */
    (void)new_text; /* Unused parameter */
    
    /* Example: Count characters in the buffer */
    if (plugin_state.api && plugin_state.api->get_buffer_text) {
        const char* text = plugin_state.api->get_buffer_text(buffer);
        if (text) {
            size_t char_count = strlen(text);
            char message[128];
            snprintf(message, sizeof(message), "Character count: %zu", char_count);
            if (plugin_state.api->set_status_message) {
                plugin_state.api->set_status_message(plugin_state.editor, message);
            }
        }
    }
    return 0;
}

static void on_cursor_moved(vizero_cursor_t* cursor, vizero_position_t old_pos, vizero_position_t new_pos) {
    (void)cursor; /* Unused parameter */
    (void)old_pos; /* Unused parameter */
    
    /* Example: Show cursor position */
    if (plugin_state.api && plugin_state.api->set_status_message) {
        char message[128];
        snprintf(message, sizeof(message), "Cursor: Line %zu, Col %zu", new_pos.line + 1, new_pos.column + 1);
        plugin_state.api->set_status_message(plugin_state.editor, message);
    }
}

static int on_command(vizero_editor_t* editor, const char* command, const char* args) {
    /* Example: Handle custom command */
    if (strcmp(command, "example") == 0) {
        if (plugin_state.api && plugin_state.api->set_status_message) {
            char message[256];
            if (args && strlen(args) > 0) {
                snprintf(message, sizeof(message), "Example plugin received command with args: %s", args);
            } else {
                snprintf(message, sizeof(message), "Example plugin: Hello from the example command!");
            }
            plugin_state.api->set_status_message(editor, message);
        }
        return 1; /* Command handled */
    }
    
    return 0; /* Command not handled */
}

static int on_key_input(vizero_editor_t* editor, uint32_t key, uint32_t modifiers) {
    (void)modifiers; /* Unused parameter */
    
    /* Example: Handle F1 key to show plugin info */
    if (key == 0x3A) { /* F1 key (SDL scancode) */
        if (plugin_state.api && plugin_state.api->set_status_message) {
            const vizero_plugin_info_t* info = vizero_plugin_get_info();
            char message[256];
            snprintf(message, sizeof(message), "%s v%s - %s", info->name, info->version, info->description);
            plugin_state.api->set_status_message(editor, message);
        }
        return 1; /* Key handled */
    }
    
    return 0; /* Key not handled */
}

/* Plugin entry points */
VIZERO_PLUGIN_API int vizero_plugin_init(vizero_plugin_t* plugin, vizero_editor_t* editor, const vizero_editor_api_t* api) {
    if (!plugin || !editor || !api) {
        return -1;
    }
    
    /* Store references */
    plugin_state.editor = editor;
    plugin_state.api = api;
    plugin_state.initialized = 1;
    
    /* Set up callbacks */
    plugin->callbacks.init = NULL; /* Already called */
    plugin->callbacks.cleanup = cleanup_wrapper;
    plugin->callbacks.on_buffer_open = on_buffer_open;
    plugin->callbacks.on_buffer_close = on_buffer_close;
    plugin->callbacks.on_text_changed = on_text_changed;
    plugin->callbacks.on_cursor_moved = on_cursor_moved;
    plugin->callbacks.on_command = on_command;
    plugin->callbacks.highlight_syntax = NULL; /* Not a syntax highlighter */
    plugin->callbacks.on_key_input = on_key_input;
    
    /* Log initialization */
    printf("Example plugin initialized successfully\n");
    
    if (api->set_status_message) {
        api->set_status_message(editor, "Example plugin loaded - Press F1 for info, use :example command");
    }
    
    return 0;
}

VIZERO_PLUGIN_API void vizero_plugin_cleanup(vizero_plugin_t* plugin) {
    (void)plugin; /* Unused parameter */
    
    if (!plugin_state.initialized) {
        return;
    }
    
    printf("Example plugin cleanup\n");
    
    if (plugin_state.api && plugin_state.api->set_status_message) {
        plugin_state.api->set_status_message(plugin_state.editor, "Example plugin unloaded");
    }
    
    /* Clear state */
    plugin_state.editor = NULL;
    plugin_state.api = NULL;
    plugin_state.initialized = 0;
}