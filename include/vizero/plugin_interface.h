#ifndef VIZERO_PLUGIN_INTERFACE_H
#define VIZERO_PLUGIN_INTERFACE_H

#ifdef __cplusplus
extern "C" {
#endif

#include <stdint.h>
#include <stddef.h>
#include <stdbool.h>

/* Version information */
#define VIZERO_PLUGIN_API_VERSION_MAJOR 1
#define VIZERO_PLUGIN_API_VERSION_MINOR 0
#define VIZERO_PLUGIN_API_VERSION_PATCH 0

/* Plugin export macros */
#ifdef _WIN32
    #ifdef VIZERO_PLUGIN_EXPORT
        #define VIZERO_PLUGIN_API __declspec(dllexport)
    #else
        #define VIZERO_PLUGIN_API __declspec(dllimport)
    #endif
#else
    #define VIZERO_PLUGIN_API __attribute__((visibility("default")))
#endif

/* Forward declarations */
typedef struct vizero_editor_t vizero_editor_t;
typedef struct vizero_buffer_t vizero_buffer_t;
typedef struct vizero_cursor_t vizero_cursor_t;
typedef struct vizero_plugin_t vizero_plugin_t;

/* Plugin types */
typedef enum {
    VIZERO_PLUGIN_TYPE_SYNTAX_HIGHLIGHTER,
    VIZERO_PLUGIN_TYPE_COMMAND_EXTENSION,
    VIZERO_PLUGIN_TYPE_FILTER,
    VIZERO_PLUGIN_TYPE_UI_EXTENSION,
    VIZERO_PLUGIN_TYPE_FILE_TYPE_HANDLER,
    VIZERO_PLUGIN_TYPE_LANGUAGE_SERVER,        /* NEW */
    VIZERO_PLUGIN_TYPE_GENERIC
} vizero_plugin_type_t;

/* Plugin information structure */
typedef struct {
    const char* name;
    const char* version;
    const char* author;
    const char* description;
    vizero_plugin_type_t type;
    uint32_t api_version_major;
    uint32_t api_version_minor;
    uint32_t api_version_patch;
} vizero_plugin_info_t;

/* Text position structure */
typedef struct {
    size_t line;
    size_t column;
} vizero_position_t;

/* Text range structure */
typedef struct {
    vizero_position_t start;
    vizero_position_t end;
} vizero_range_t;

/* Colour structure for syntax highlighting */
typedef struct {
    uint8_t r, g, b, a;
} vizero_plugin_colour_t;

/* Syntax highlight token */
typedef struct {
    vizero_range_t range;
    vizero_plugin_colour_t colour;
    uint32_t flags; /* bold, italic, underline, etc. */
} vizero_syntax_token_t;

/* LSP completion item kinds */
typedef enum {
    VIZERO_COMPLETION_TEXT = 1,
    VIZERO_COMPLETION_METHOD = 2,
    VIZERO_COMPLETION_FUNCTION = 3,
    VIZERO_COMPLETION_CONSTRUCTOR = 4,
    VIZERO_COMPLETION_FIELD = 5,
    VIZERO_COMPLETION_VARIABLE = 6,
    VIZERO_COMPLETION_CLASS = 7,
    VIZERO_COMPLETION_INTERFACE = 8,
    VIZERO_COMPLETION_MODULE = 9,
    VIZERO_COMPLETION_PROPERTY = 10,
    VIZERO_COMPLETION_UNIT = 11,
    VIZERO_COMPLETION_VALUE = 12,
    VIZERO_COMPLETION_ENUM = 13,
    VIZERO_COMPLETION_KEYWORD = 14,
    VIZERO_COMPLETION_SNIPPET = 15,
    VIZERO_COMPLETION_COLOR = 16,
    VIZERO_COMPLETION_FILE = 17,
    VIZERO_COMPLETION_REFERENCE = 18
} vizero_completion_kind_t;

/* LSP completion item */
typedef struct {
    char* label;                        /* "push_back" */
    char* detail;                       /* "void push_back(const T&)" */
    char* documentation;                /* "Appends element to container" */
    char* insert_text;                  /* Text to insert when selected */
    char* filter_text;                  /* Text used for filtering */
    char* sort_text;                    /* Text used for sorting */
    vizero_completion_kind_t kind;      /* Function, Variable, etc. */
    bool deprecated;                    /* Is this item deprecated? */
} vizero_completion_item_t;

/* LSP completion list */
typedef struct {
    vizero_completion_item_t* items;
    size_t item_count;
    bool is_incomplete;                 /* More items available */
} vizero_completion_list_t;

/* LSP location (for go-to-definition, etc.) */
typedef struct {
    char* file_path;
    vizero_position_t position;
} vizero_location_t;

/* LSP diagnostic severity */
typedef enum {
    VIZERO_DIAGNOSTIC_ERROR = 1,
    VIZERO_DIAGNOSTIC_WARNING = 2,
    VIZERO_DIAGNOSTIC_INFORMATION = 3,
    VIZERO_DIAGNOSTIC_HINT = 4
} vizero_diagnostic_severity_t;

/* LSP diagnostic */
typedef struct {
    vizero_range_t range;
    vizero_diagnostic_severity_t severity;
    char* message;
    char* source;                       /* "clangd", "gcc", etc. */
    int code;                          /* Optional error code */
} vizero_diagnostic_t;

/* Editor API functions provided to plugins */
typedef struct {
    /* Buffer operations */
    const char* (*get_buffer_text)(vizero_buffer_t* buffer);
    const char* (*get_buffer_filename)(vizero_buffer_t* buffer);
    size_t (*get_buffer_line_count)(vizero_buffer_t* buffer);
    const char* (*get_buffer_line)(vizero_buffer_t* buffer, size_t line_num);
    size_t (*get_buffer_line_length)(vizero_buffer_t* buffer, size_t line_num);
    int (*insert_text)(vizero_buffer_t* buffer, vizero_position_t pos, const char* text);
    int (*delete_text)(vizero_buffer_t* buffer, vizero_range_t range);
    
    /* Cursor operations */
    vizero_position_t (*get_cursor_position)(vizero_cursor_t* cursor);
    int (*set_cursor_position)(vizero_cursor_t* cursor, vizero_position_t pos);
    
    /* Editor operations */
    vizero_buffer_t* (*get_current_buffer)(vizero_editor_t* editor);
    vizero_cursor_t* (*get_current_cursor)(vizero_editor_t* editor);
    int (*execute_command)(vizero_editor_t* editor, const char* command);
    int (*set_status_message)(vizero_editor_t* editor, const char* message);
    
    /* File operations */
    int (*open_file)(vizero_editor_t* editor, const char* filename);
    int (*save_file)(vizero_editor_t* editor, const char* filename);
    
    /* Rendering hints */
    int (*add_syntax_tokens)(vizero_editor_t* editor, vizero_syntax_token_t* tokens, size_t count);
    int (*clear_syntax_tokens)(vizero_editor_t* editor);
} vizero_editor_api_t;

/* Plugin callback functions */
typedef struct {
    /* Required: Plugin initialization */
    int (*init)(vizero_editor_t* editor, const vizero_editor_api_t* api);
    
    /* Required: Plugin cleanup */
    void (*cleanup)(void);
    
    /* Optional: Called when a buffer is opened */
    int (*on_buffer_open)(vizero_buffer_t* buffer, const char* filename);
    
    /* Optional: Called when a buffer is closed */
    void (*on_buffer_close)(vizero_buffer_t* buffer);
    
    /* Optional: Called when text is modified */
    int (*on_text_changed)(vizero_buffer_t* buffer, vizero_range_t range, const char* new_text);
    
    /* Optional: Called when cursor moves */
    void (*on_cursor_moved)(vizero_cursor_t* cursor, vizero_position_t old_pos, vizero_position_t new_pos);
    
    /* Optional: Called for command processing */
    int (*on_command)(vizero_editor_t* editor, const char* command, const char* args);
    
    /* Optional: Called for syntax highlighting (updated signature) */
    int (*highlight_syntax)(vizero_buffer_t* buffer, size_t start_line, size_t end_line, vizero_syntax_token_t* tokens, size_t max_tokens);
    
    /* Optional: Called for key input processing */
    int (*on_key_input)(vizero_editor_t* editor, uint32_t key, uint32_t modifiers);
    
    /* LSP callbacks - for VIZERO_PLUGIN_TYPE_LANGUAGE_SERVER plugins */
    int (*lsp_initialize)(const char* project_root, const char* session_config);
    int (*lsp_completion)(vizero_buffer_t* buffer, vizero_position_t position, vizero_completion_list_t** result);
    int (*lsp_hover)(vizero_buffer_t* buffer, vizero_position_t position, char** hover_text);
    int (*lsp_goto_definition)(vizero_buffer_t* buffer, vizero_position_t position, vizero_location_t** locations, size_t* location_count);
    int (*lsp_get_diagnostics)(vizero_buffer_t* buffer, vizero_diagnostic_t** diagnostics, size_t* diagnostic_count);
    void (*lsp_shutdown)(void);
} vizero_plugin_callbacks_t;

/* Main plugin structure */
struct vizero_plugin_t {
    vizero_plugin_info_t info;
    vizero_plugin_callbacks_t callbacks;
    void* user_data;
    void* dll_handle; /* Internal use */
};

/* Required plugin entry points - must be implemented by all plugins */

/**
 * Get plugin information
 * This function must be exported by every plugin
 */
VIZERO_PLUGIN_API const vizero_plugin_info_t* vizero_plugin_get_info(void);

/**
 * Initialize plugin
 * This function must be exported by every plugin
 */
VIZERO_PLUGIN_API int vizero_plugin_init(vizero_plugin_t* plugin, vizero_editor_t* editor, const vizero_editor_api_t* api);

/**
 * Cleanup plugin
 * This function must be exported by every plugin
 */
VIZERO_PLUGIN_API void vizero_plugin_cleanup(vizero_plugin_t* plugin);

/* Utility macros for plugin implementation */
#define VIZERO_PLUGIN_DEFINE_INFO(name, version, author, desc, type) \
    static const vizero_plugin_info_t plugin_info = { \
        name, \
        version, \
        author, \
        desc, \
        type, \
        VIZERO_PLUGIN_API_VERSION_MAJOR, \
        VIZERO_PLUGIN_API_VERSION_MINOR, \
        VIZERO_PLUGIN_API_VERSION_PATCH \
    }; \
    VIZERO_PLUGIN_API const vizero_plugin_info_t* vizero_plugin_get_info(void) { \
        return &plugin_info; \
    }

/* Plugin interface utility functions */

/**
 * Initialize a plugin API structure with function pointers
 * This is used by the plugin manager to provide the API to plugins
 */
void vizero_plugin_interface_init_api(vizero_editor_api_t* api, vizero_editor_t* editor);

/**
 * Check if a plugin's API version is compatible
 * Returns 1 if compatible, 0 if not
 */
int vizero_plugin_interface_check_version(const vizero_plugin_info_t* info);

#ifdef __cplusplus
}
#endif

#endif /* VIZERO_PLUGIN_INTERFACE_H */