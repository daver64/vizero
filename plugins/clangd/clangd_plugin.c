#include "vizero/plugin_interface.h"
#include "vizero/lsp_client.h"
#include "vizero/buffer.h"
#include "vizero/json_parser.h"
#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <stdbool.h>
#include <time.h>
#include <stdarg.h>

#ifdef _WIN32
#include <windows.h>
#include <shlwapi.h>
#include <sys/stat.h>
#include <direct.h>
#pragma comment(lib, "shlwapi.lib")
#define asprintf _asprintf

/* Windows doesn't have asprintf, so we implement it */
static int _asprintf(char** strp, const char* fmt, ...) {
    va_list args;
    va_start(args, fmt);
    int len = _vscprintf(fmt, args);
    if (len < 0) {
        va_end(args);
        return -1;
    }
    *strp = (char*)malloc(len + 1);
    if (!*strp) {
        va_end(args);
        return -1;
    }
    int result = vsprintf(*strp, fmt, args);
    va_end(args);
    return result;
}
#else
#include <unistd.h>
#include <sys/stat.h>
#include <libgen.h>
#endif

/* Lightweight logging macros: set CLANGD_LOG_LEVEL=1 to enable debug prints */
#ifndef CLANGD_LOG_LEVEL
#define CLANGD_LOG_LEVEL 0
#endif

#if CLANGD_LOG_LEVEL
#define CLANGD_DBG(fmt, ...) fprintf(stdout, "[CLANGD] " fmt "\n", ##__VA_ARGS__)
#else
#define CLANGD_DBG(fmt, ...) ((void)0)
#endif

#define CLANGD_ERR(fmt, ...) fprintf(stderr, "[CLANGD][ERROR] " fmt "\n", ##__VA_ARGS__)


/* Diagnostic popup structure */
typedef struct {
    vizero_diagnostic_t* diagnostics;
    size_t diagnostic_count;
    size_t selected_index;
    bool is_visible;
    float popup_x, popup_y;
    float popup_width, popup_height;
    char* buffer_path;  /* Which file these diagnostics belong to */
    size_t error_count;
    size_t warning_count;
    size_t info_count;
} diagnostic_popup_t;

/* Plugin state */
typedef struct {
    vizero_lsp_client_t* lsp_client;
    bool initialized;
    char* root_path;
    vizero_completion_item_t* completion_items;
    size_t completion_count;
    size_t completion_capacity;
    
    /* API pointer */
    const vizero_editor_api_t* api;
    vizero_editor_t* editor;
    
    /* Async completion state */
    int pending_completion_request_id;
    bool completion_request_pending;
    vizero_completion_list_t* last_completion_result;
    
    /* Diagnostics storage */
    vizero_diagnostic_t* diagnostics;
    size_t diagnostic_count;
    size_t diagnostic_capacity;
    char* diagnostic_buffer_path; /* Path of buffer that diagnostics apply to */
    
    /* Diagnostic popup */
    diagnostic_popup_t popup;
} clangd_state_t;

static clangd_state_t g_state = {0};

/* Forward declarations */
static int clangd_on_text_changed(vizero_buffer_t* buffer, vizero_range_t range, const char* new_text);
static int clangd_lsp_initialize(const char* project_root, const char* session_config);
static int clangd_lsp_completion(vizero_buffer_t* buffer, vizero_position_t position, vizero_completion_list_t** result);
static int clangd_lsp_hover(vizero_buffer_t* buffer, vizero_position_t position, char** hover_text);
static int clangd_lsp_goto_definition(vizero_buffer_t* buffer, vizero_position_t position,
                                     vizero_location_t** locations, size_t* location_count);
static int clangd_lsp_get_diagnostics(vizero_buffer_t* buffer, vizero_diagnostic_t** diagnostics,
                                     size_t* diagnostic_count);
VIZERO_PLUGIN_API void clangd_manual_diagnostic_refresh(vizero_buffer_t* buffer);
VIZERO_PLUGIN_API void clangd_show_diagnostic_popup(vizero_buffer_t* buffer);
static void clangd_lsp_shutdown(void);

/* Diagnostic popup functions */
static void init_diagnostic_popup(diagnostic_popup_t* popup);
static void cleanup_diagnostic_popup(diagnostic_popup_t* popup);
static void show_diagnostic_popup(diagnostic_popup_t* popup, vizero_diagnostic_t* diagnostics, size_t count, const char* buffer_path);
static void hide_diagnostic_popup(diagnostic_popup_t* popup);
static int handle_diagnostic_popup_input(diagnostic_popup_t* popup, int key, int modifiers);
static void render_diagnostic_popup(diagnostic_popup_t* popup, void* renderer);
static char* format_diagnostics_for_popup(vizero_diagnostic_t* diagnostics, size_t count, size_t error_count, size_t warning_count, size_t info_count);

/* Plugin callbacks */
static int clangd_on_buffer_open(vizero_buffer_t* buffer, const char* filename);

static char* find_clangd_executable(void);
static void on_lsp_response(int request_id, const char* result, const char* error, void* user_data);
static void on_lsp_notification(const char* method, const char* params, void* user_data);
static void on_lsp_error(const char* error_message, void* user_data);
static void request_diagnostic_refresh(const char* file_path);

/* Plugin info */
/* (defined at the end of the file using VIZERO_PLUGIN_DEFINE_INFO macro) */

/* Plugin callbacks */

VIZERO_PLUGIN_API int vizero_plugin_init(vizero_plugin_t* plugin, vizero_editor_t* editor, const vizero_editor_api_t* api) {
    if (!plugin || !editor || !api) {
        CLANGD_ERR("Plugin initialization FAILED - null parameters");
        return -1;
    }
    
    /* Store API pointer and editor reference */
    g_state.api = api;
    g_state.editor = editor;
    
    /* Set up callbacks */
    plugin->callbacks.init = NULL; /* Already called */
    plugin->callbacks.cleanup = NULL;
    plugin->callbacks.on_buffer_open = clangd_on_buffer_open;
    plugin->callbacks.on_buffer_close = NULL;
    plugin->callbacks.on_text_changed = NULL;
    plugin->callbacks.on_cursor_moved = NULL;
    plugin->callbacks.on_command = NULL;
    plugin->callbacks.highlight_syntax = NULL; /* LSP plugins don't do syntax highlighting */
    plugin->callbacks.on_key_input = NULL;
    plugin->callbacks.on_text_changed = clangd_on_text_changed;
    plugin->callbacks.lsp_initialize = clangd_lsp_initialize;
    plugin->callbacks.lsp_completion = clangd_lsp_completion;
    plugin->callbacks.lsp_hover = clangd_lsp_hover;
    plugin->callbacks.lsp_goto_definition = clangd_lsp_goto_definition;
    plugin->callbacks.lsp_get_diagnostics = clangd_lsp_get_diagnostics;
    plugin->callbacks.lsp_shutdown = clangd_lsp_shutdown;

    /* Find clangd executable */
    char* clangd_path = find_clangd_executable();
    if (!clangd_path) {
        /* clangd not found - disable functionality but don't fail initialization */
        CLANGD_ERR("clangd executable not found, disabling LSP functionality");
        CLANGD_ERR("Plugin will load but LSP features will be unavailable");
        
        /* Clear LSP callback functions to prevent calls when clangd is not available */
        plugin->callbacks.lsp_initialize = NULL;
        plugin->callbacks.lsp_completion = NULL;
        plugin->callbacks.lsp_hover = NULL;
        plugin->callbacks.lsp_goto_definition = NULL;
        plugin->callbacks.lsp_get_diagnostics = NULL;
        plugin->callbacks.lsp_shutdown = NULL;
        
        /* Mark as disabled */
        g_state.initialized = false;
        g_state.lsp_client = NULL;
        return 0;
    }
    /* Found clangd at specified path */
    
    /* Test if clangd actually exists before creating LSP client */
    /* Cross-platform file existence check */
    int file_exists = 0;
#ifdef _WIN32
    if (PathFileExistsA(clangd_path)) {
        file_exists = 1;
    }
#else
    struct stat st;
    if (stat(clangd_path, &st) == 0) {
        file_exists = 1;
    }
#endif
    
    if (!file_exists) {
        free(clangd_path);
        /* clangd executable not found - disable functionality */
        CLANGD_ERR("clangd executable not accessible, disabling LSP functionality");
        CLANGD_ERR("Plugin will load but LSP features will be unavailable");
        
        /* Clear LSP callback functions */
        plugin->callbacks.lsp_initialize = NULL;
        plugin->callbacks.lsp_completion = NULL;
        plugin->callbacks.lsp_hover = NULL;
        plugin->callbacks.lsp_goto_definition = NULL;
        plugin->callbacks.lsp_get_diagnostics = NULL;
        plugin->callbacks.lsp_shutdown = NULL;
        
        /* Mark as disabled */
        g_state.initialized = false;
        g_state.lsp_client = NULL;
        return 0;
    }
    
    /* Create LSP client */
    g_state.lsp_client = vizero_lsp_client_create(clangd_path, NULL);
    free(clangd_path);
    
    if (!g_state.lsp_client) {
        CLANGD_ERR("Failed to create LSP client, disabling LSP functionality");
        
        /* Clear LSP callback functions to prevent crashes */
        plugin->callbacks.lsp_initialize = NULL;
        plugin->callbacks.lsp_completion = NULL;
        plugin->callbacks.lsp_hover = NULL;
        plugin->callbacks.lsp_goto_definition = NULL;
        plugin->callbacks.lsp_get_diagnostics = NULL;
        plugin->callbacks.lsp_shutdown = NULL;
        
        g_state.initialized = false;
        g_state.lsp_client = NULL;
        return 0; /* Don't fail plugin loading, just disable LSP */
    }
    
    /* Set up LSP callbacks */
    lsp_client_callbacks_t lsp_callbacks = {
        .on_response = on_lsp_response,
        .on_notification = on_lsp_notification,
        .on_error = on_lsp_error
    };
    vizero_lsp_client_set_callbacks(g_state.lsp_client, &lsp_callbacks, &g_state);
    
    /* Initialize completion items buffer */
    g_state.completion_capacity = 100;
    g_state.completion_items = (vizero_completion_item_t*)malloc(
        g_state.completion_capacity * sizeof(vizero_completion_item_t));
    if (!g_state.completion_items) {
        CLANGD_ERR("Failed to allocate completion buffer, disabling LSP functionality");
        vizero_lsp_client_destroy(g_state.lsp_client);
        g_state.lsp_client = NULL;
        g_state.initialized = false;
        
        /* Clear LSP callback functions to prevent crashes */
        plugin->callbacks.lsp_initialize = NULL;
        plugin->callbacks.lsp_completion = NULL;
        plugin->callbacks.lsp_hover = NULL;
        plugin->callbacks.lsp_goto_definition = NULL;
        plugin->callbacks.lsp_get_diagnostics = NULL;
        plugin->callbacks.lsp_shutdown = NULL;
        
        return 0; /* Don't fail plugin loading, just disable LSP */
    }
    
    /* Initialize LSP immediately since we have a clangd client */
    if (clangd_lsp_initialize(".", NULL) != 0) {
        CLANGD_ERR("Failed to initialize LSP during plugin load");
        /* Don't fail plugin loading, just disable LSP functionality */
    }
    
    /* Initialize diagnostic popup */
    init_diagnostic_popup(&g_state.popup);
    return 0;
}

VIZERO_PLUGIN_API void vizero_plugin_cleanup(vizero_plugin_t* plugin) {
    (void)plugin; /* Unused parameter */
    
    if (g_state.lsp_client) {
        vizero_lsp_client_destroy(g_state.lsp_client);
        g_state.lsp_client = NULL;
    }
    
    free(g_state.root_path);
    g_state.root_path = NULL;
    
    free(g_state.completion_items);
    g_state.completion_items = NULL;
    
    if (g_state.last_completion_result) {
        if (g_state.last_completion_result->items) {
            for (size_t i = 0; i < g_state.last_completion_result->item_count; i++) {
                free(g_state.last_completion_result->items[i].label);
                free(g_state.last_completion_result->items[i].detail);
                free(g_state.last_completion_result->items[i].documentation);
                free(g_state.last_completion_result->items[i].insert_text);
                free(g_state.last_completion_result->items[i].filter_text);
                free(g_state.last_completion_result->items[i].sort_text);
            }
            free(g_state.last_completion_result->items);
        }
        free(g_state.last_completion_result);
        g_state.last_completion_result = NULL;
    }
    
    /* Clean up diagnostics */
    if (g_state.diagnostics) {
        for (size_t i = 0; i < g_state.diagnostic_count; i++) {
            free(g_state.diagnostics[i].message);
            free(g_state.diagnostics[i].source);
        }
        free(g_state.diagnostics);
        g_state.diagnostics = NULL;
    }
    free(g_state.diagnostic_buffer_path);
    g_state.diagnostic_buffer_path = NULL;
    g_state.diagnostic_count = 0;
    g_state.diagnostic_capacity = 0;
    
    /* Cleanup diagnostic popup */
    cleanup_diagnostic_popup(&g_state.popup);
    
    g_state.initialized = false;
}

/* Non-blocking function to process LSP messages and check for completion results */
VIZERO_PLUGIN_API int clangd_process_lsp_messages(void) {
    if (!g_state.lsp_client) {
        return -1;
    }
    
    /* Process any pending LSP messages */
    vizero_lsp_client_process_messages(g_state.lsp_client);
    
    return 0;
}

/* Check if completion results are available */
VIZERO_PLUGIN_API int clangd_get_completion_results(vizero_completion_list_t** result) {
    if (!result) {
        return -1;
    }
    
    *result = NULL;
    
    if (g_state.last_completion_result) {
        *result = g_state.last_completion_result;
        g_state.last_completion_result = NULL; /* Transfer ownership */
        CLANGD_DBG("Completion results retrieved");
        return 0;
    }
    
    return -1; /* No results available */
}

/* Plugin callback implementations */
/* Send didChange notification when buffer content changes */
static void send_did_change_notification(const char* file_path, const char* content) {
    if (!g_state.lsp_client || !g_state.initialized || !file_path || !content) {
        return;
    }
    
    static int version_number = 1;
    version_number++;
    
    CLANGD_DBG("Sending didChange notification for: %s (version %d)", file_path, version_number);
    
    /* Escape the file path for JSON */
    char* escaped_path = vizero_lsp_client_escape_json_string(file_path);
    if (!escaped_path) {
        CLANGD_ERR("Failed to escape file path for didChange");
        return;
    }
    
    /* Escape buffer content */
    char* escaped_content = vizero_lsp_client_escape_json_string(content);
    if (!escaped_content) {
        vizero_lsp_client_free_string(escaped_path);
        CLANGD_ERR("Failed to escape buffer content for didChange");
        return;
    }
    
    /* Convert to proper URI format */
    char* uri;
    #ifdef _WIN32
    size_t path_len = strlen(escaped_path);
    char* normalized_path = (char*)malloc(path_len + 1);
    strcpy(normalized_path, escaped_path);
    for (size_t i = 0; i < path_len; i++) {
        if (normalized_path[i] == '\\') {
            normalized_path[i] = '/';
        }
    }
    asprintf(&uri, "file:///%s", normalized_path);
    free(normalized_path);
    #else
    asprintf(&uri, "file://%s", escaped_path);
    #endif
    
    /* Build didChange notification parameters */
    char* params;
    int param_len = asprintf(&params,
        "{"
        "\"textDocument\":{"
          "\"uri\":\"%s\","
          "\"version\":%d"
        "},"
        "\"contentChanges\":["
          "{"
            "\"text\":\"%s\""
          "}"
        "]"
        "}",
        uri, version_number, escaped_content
    );
    
    free(uri);
    vizero_lsp_client_free_string(escaped_path);
    vizero_lsp_client_free_string(escaped_content);
    
    if (param_len < 0 || !params) {
        CLANGD_ERR("Failed to create didChange params");
        return;
    }
    
    /* Send notification */
    int result = vizero_lsp_client_send_notification(g_state.lsp_client, "textDocument/didChange", params);
    free(params);
    
    if (result < 0) {
        CLANGD_ERR("Failed to send didChange notification");
        return;
    }
    
    CLANGD_DBG("didChange notification sent successfully for: %s", file_path);
}

/* Request diagnostics refresh from clangd by sending a lightweight request */
static void request_diagnostic_refresh(const char* file_path) {
    if (!g_state.lsp_client || !g_state.initialized || !file_path) {
        printf("[CLANGD] Cannot request diagnostic refresh - not initialized\n");
        return;
    }
    
    CLANGD_DBG("Requesting diagnostic refresh for: %s", file_path);
    
    /* Escape the file path for JSON */
    char* escaped_path = vizero_lsp_client_escape_json_string(file_path);
    if (!escaped_path) {
        CLANGD_ERR("Failed to escape file path for diagnostic refresh");
        return;
    }
    
    /* Convert to proper URI format */
    char* uri;
    #ifdef _WIN32
    size_t path_len = strlen(escaped_path);
    char* normalized_path = (char*)malloc(path_len + 1);
    strcpy(normalized_path, escaped_path);
    for (size_t i = 0; i < path_len; i++) {
        if (normalized_path[i] == '\\') {
            normalized_path[i] = '/';
        }
    }
    asprintf(&uri, "file:///%s", normalized_path);
    free(normalized_path);
    #else
    asprintf(&uri, "file://%s", escaped_path);
    #endif
    
    /* Send a lightweight textDocument/documentSymbol request to trigger analysis */
    char* params;
    int param_len = asprintf(&params,
        "{"
        "\"textDocument\":{"
          "\"uri\":\"%s\""
        "}"
        "}",
        uri
    );
    
    free(uri);
    vizero_lsp_client_free_string(escaped_path);
    
    if (param_len < 0 || !params) {
        CLANGD_ERR("Failed to create diagnostic refresh params");
        return;
    }
    
    /* Send request to trigger fresh analysis */
    int result = vizero_lsp_client_send_request(g_state.lsp_client, "textDocument/documentSymbol", params);
    free(params);
    
    if (result < 0) {
        CLANGD_ERR("Failed to send diagnostic refresh request");
        return;
    }
    
    CLANGD_DBG("Diagnostic refresh request sent successfully");
}

static int clangd_on_buffer_open(vizero_buffer_t* buffer, const char* filename) {
    /* Buffer opened for clangd processing */
    
    if (!g_state.lsp_client || !g_state.initialized) {
        CLANGD_ERR("LSP not available or not initialized");
        return 0; /* Not an error - clangd just not available */
    }
    
    if (!filename || !buffer) {
        CLANGD_ERR("No filename or buffer provided");
        return 0;
    }
    
    /* Send textDocument/didOpen notification to clangd */
    CLANGD_DBG("Sending didOpen notification for: %s", filename);
    
    /* Escape the file path for JSON */
    char* escaped_path = vizero_lsp_client_escape_json_string(filename);
    if (!escaped_path) {
        printf("[CLANGD] Failed to escape file path\n");
        return -1;
    }
    
    /* Get buffer content */
    const char* content = g_state.api ? g_state.api->get_buffer_text(buffer) : "";
    if (!content) content = "";
    char* escaped_content = vizero_lsp_client_escape_json_string(content);
    if (!escaped_content) {
        vizero_lsp_client_free_string(escaped_path);
        printf("[CLANGD] Failed to escape buffer content\n");
        return -1;
    }
    
    /* Convert to proper URI format */
    char* uri;
    #ifdef _WIN32
    size_t path_len = strlen(escaped_path);
    char* normalized_path = (char*)malloc(path_len + 1);
    strcpy(normalized_path, escaped_path);
    for (size_t i = 0; i < path_len; i++) {
        if (normalized_path[i] == '\\') {
            normalized_path[i] = '/';
        }
    }
    asprintf(&uri, "file:///%s", normalized_path);
    free(normalized_path);
    #else
    asprintf(&uri, "file://%s", escaped_path);
    #endif
    
    char* params;
    int param_len = asprintf(&params,
        "{"
        "\"textDocument\":{"
          "\"uri\":\"%s\","
          "\"languageId\":\"c\","
          "\"version\":1,"
          "\"text\":\"%s\""
        "}"
        "}",
        uri, escaped_content
    );
    
    free(uri);
    
    vizero_lsp_client_free_string(escaped_path);
    vizero_lsp_client_free_string(escaped_content);
    
    if (param_len < 0 || !params) {
        CLANGD_ERR("Failed to create didOpen params");
        return -1;
    }
    
    /* Send notification (no request ID needed) */
    int result = vizero_lsp_client_send_notification(g_state.lsp_client, "textDocument/didOpen", params);
    free(params);
    
    if (result < 0) {
        CLANGD_ERR("Failed to send didOpen notification");
        return -1;
    }
    
    /* didOpen notification sent */
    
    /* Create sample diagnostics for demonstration */
    if (g_state.diagnostics) {
        for (size_t i = 0; i < g_state.diagnostic_count; i++) {
            free(g_state.diagnostics[i].message);
            free(g_state.diagnostics[i].source);
        }
        free(g_state.diagnostics);
    }
    if (g_state.diagnostic_buffer_path) {
        free(g_state.diagnostic_buffer_path);
    }
    
    /* Initialize empty diagnostics for this file */
    g_state.diagnostic_buffer_path = strdup(filename);
    g_state.diagnostic_capacity = 16;
    g_state.diagnostics = malloc(sizeof(vizero_diagnostic_t) * g_state.diagnostic_capacity);
    g_state.diagnostic_count = 0;  /* Start with no diagnostics - will be populated by publishDiagnostics */
    
    /* Ensure diagnostic array is zeroed out to avoid garbage data */
    if (g_state.diagnostics) {
        memset(g_state.diagnostics, 0, sizeof(vizero_diagnostic_t) * g_state.diagnostic_capacity);
    }
    
    /* Diagnostics storage initialized */
    
    return 0;
}

VIZERO_PLUGIN_DEFINE_INFO(
    "clangd Language Server",
    "1.0.0",
    "Vizero Team",
    "clangd Language Server Plugin for C/C++ IntelliSense",
    VIZERO_PLUGIN_TYPE_LANGUAGE_SERVER
)

/* LSP implementation */
static int clangd_lsp_initialize(const char* project_root, const char* session_config) {
    if (g_state.initialized) {
        return 0; /* Already initialized */
    }
    
    if (!g_state.lsp_client) {
        return -1; /* clangd not available */
    }
    
    /* Store root path */
    if (project_root) {
        g_state.root_path = strdup(project_root);
    }
    
    /* Start the LSP client */
    CLANGD_DBG("Starting clangd process...");
    if (vizero_lsp_client_start(g_state.lsp_client) != 0) {
        CLANGD_ERR("Failed to start clangd process");
        return -1;
    }
    CLANGD_DBG("clangd process started successfully");
    
    /* Send initialize request */
    CLANGD_DBG("Sending LSP initialize request...");
    char* init_params;
    int param_len = asprintf(&init_params,
        "{"
        "\"processId\":null,"
        "\"rootPath\":\"%s\","
        "\"rootUri\":\"file://%s\","
        "\"capabilities\":{"
          "\"textDocument\":{"
            "\"completion\":{"
              "\"completionItem\":{"
                "\"snippetSupport\":false,"
                "\"documentationFormat\":[\"markdown\",\"plaintext\"]"
              "}"
            "},"
            "\"hover\":{"
              "\"contentFormat\":[\"markdown\",\"plaintext\"]"
            "},"
            "\"definition\":{"
              "\"linkSupport\":false"
            "},"
            "\"publishDiagnostics\":{}"
          "}"
        "},"
        "\"initializationOptions\":{}"
        "}",
        project_root ? project_root : "",
        project_root ? project_root : ""
    );
    
    if (param_len < 0 || !init_params) {
        return -1;
    }
    
    int request_id = vizero_lsp_client_send_request(g_state.lsp_client, "initialize", init_params);
    free(init_params);
    
    if (request_id < 0) {
        CLANGD_ERR("Failed to send initialize request");
        return -1;
    }
    
    CLANGD_DBG("Initialize request sent with ID: %d", request_id);
    
    /* Send initialized notification */
    vizero_lsp_client_send_notification(g_state.lsp_client, "initialized", "{}");
    CLANGD_DBG("Sent initialized notification");
    
    g_state.initialized = true;
    CLANGD_DBG("clangd initialization complete");
    return 0;
}

static int clangd_on_text_changed(vizero_buffer_t* buffer, vizero_range_t range, const char* new_text) {
    /* Text change callbacks disabled - keeping only hover popup functionality */
    return 0;
}

static int clangd_lsp_completion(vizero_buffer_t* buffer, vizero_position_t position, vizero_completion_list_t** result) {
    CLANGD_DBG("*** LSP COMPLETION TRIGGERED ***");
    
    if (!g_state.initialized || !buffer || !result || !g_state.lsp_client) {
     CLANGD_ERR("Completion failed: initialized=%d, buffer=%p, result=%p, lsp_client=%p",
         g_state.initialized, (void*)buffer, (void*)result, (void*)g_state.lsp_client);
        return -1;
    }
    
    /* Prevent concurrent completion requests that could corrupt state */
    if (g_state.completion_request_pending) {
        CLANGD_DBG("Completion request already pending, rejecting new request");
        return -1;
    }
    
    *result = NULL;
    
    /* Use real LSP completion with improved JSON parsing */
    
    /* Clean up any existing completion result to prevent memory leaks and double-free */
    if (g_state.last_completion_result) {
    CLANGD_DBG("Cleaning up previous completion result");
        if (g_state.last_completion_result->items) {
            for (size_t i = 0; i < g_state.last_completion_result->item_count; i++) {
                if (g_state.last_completion_result->items[i].label) {
                    free(g_state.last_completion_result->items[i].label);
                    g_state.last_completion_result->items[i].label = NULL;
                }
                if (g_state.last_completion_result->items[i].detail) {
                    free(g_state.last_completion_result->items[i].detail);
                    g_state.last_completion_result->items[i].detail = NULL;
                }
                if (g_state.last_completion_result->items[i].documentation) {
                    free(g_state.last_completion_result->items[i].documentation);
                    g_state.last_completion_result->items[i].documentation = NULL;
                }
                if (g_state.last_completion_result->items[i].insert_text) {
                    free(g_state.last_completion_result->items[i].insert_text);
                    g_state.last_completion_result->items[i].insert_text = NULL;
                }
                if (g_state.last_completion_result->items[i].filter_text) {
                    free(g_state.last_completion_result->items[i].filter_text);
                    g_state.last_completion_result->items[i].filter_text = NULL;
                }
                if (g_state.last_completion_result->items[i].sort_text) {
                    free(g_state.last_completion_result->items[i].sort_text);
                    g_state.last_completion_result->items[i].sort_text = NULL;
                }
            }
            free(g_state.last_completion_result->items);
            g_state.last_completion_result->items = NULL;
        }
        free(g_state.last_completion_result);
        g_state.last_completion_result = NULL;
    }
    
    /* Reset completion state completely */
    g_state.completion_count = 0;
    g_state.completion_request_pending = false;
    g_state.pending_completion_request_id = -1;
    
    /* Get file path from buffer */
    const char* file_path = vizero_buffer_get_filename(buffer);
    if (!file_path) {
        CLANGD_ERR("No file path available for buffer");
        return -1;
    }
    
    /* Ensure we have absolute path for URI */
    char* absolute_path = NULL;
    #ifdef _WIN32
    if (file_path[0] != '/' && (strlen(file_path) < 2 || file_path[1] != ':')) {
        /* Relative path - make it absolute */
        char* cwd = _getcwd(NULL, 0);
        if (cwd) {
            asprintf(&absolute_path, "%s/%s", cwd, file_path);
            free(cwd);
            file_path = absolute_path;
        }
    }
    #else
    if (file_path[0] != '/') {
        /* Relative path - make it absolute */
        char* cwd = getcwd(NULL, 0);
        if (cwd) {
            asprintf(&absolute_path, "%s/%s", cwd, file_path);
            free(cwd);
            file_path = absolute_path;
        }
    }
    #endif
    
    CLANGD_DBG("Requesting completion for file: %s at line %zu, column %zu", 
           file_path, position.line, position.column);
    
    /* Send didOpen notification if we haven't already (workaround for timing issue) */
    CLANGD_DBG("Sending didOpen notification before completion...");
    char* didopen_escaped_path = vizero_lsp_client_escape_json_string(file_path);
    if (didopen_escaped_path) {
        /* Convert to proper URI format */
        char* didopen_uri;
        #ifdef _WIN32
        size_t path_len = strlen(didopen_escaped_path);
        char* normalized_path = (char*)malloc(path_len + 1);
        strcpy(normalized_path, didopen_escaped_path);
        for (size_t i = 0; i < path_len; i++) {
            if (normalized_path[i] == '\\') {
                normalized_path[i] = '/';
            }
        }
        asprintf(&didopen_uri, "file:///%s", normalized_path);
        free(normalized_path);
        #else
        asprintf(&didopen_uri, "file://%s", didopen_escaped_path);
        #endif
        
        /* Get buffer content and escape it for JSON */
        const char* buffer_content = vizero_buffer_get_text(buffer);
        char* escaped_content = buffer_content ? vizero_lsp_client_escape_json_string(buffer_content) : NULL;
        
        char* didopen_params;
        int param_len = asprintf(&didopen_params,
            "{"
            "\"textDocument\":{"
              "\"uri\":\"%s\","
              "\"languageId\":\"c\","
              "\"version\":1,"
              "\"text\":\"%s\""
            "}"
            "}",
            didopen_uri, escaped_content ? escaped_content : ""
        );
        
        if (escaped_content) vizero_lsp_client_free_string(escaped_content);
        
        free(didopen_uri);
        
            if (param_len > 0 && didopen_params) {
            vizero_lsp_client_send_notification(g_state.lsp_client, "textDocument/didOpen", didopen_params);
            free(didopen_params);
            CLANGD_DBG("didOpen notification sent");
        }
        vizero_lsp_client_free_string(didopen_escaped_path);
    }
    
    /* Build completion request parameters */
    /* Convert file path to proper URI format (don't JSON-escape the path for URI) */
    char* uri;
    #ifdef _WIN32
    /* Windows: Convert backslashes to forward slashes and add drive letter handling */
    size_t path_len = strlen(file_path);
    char* normalized_path = (char*)malloc(path_len + 1);
    strcpy(normalized_path, file_path);
    for (size_t i = 0; i < path_len; i++) {
        if (normalized_path[i] == '\\') {
            normalized_path[i] = '/';
        }
    }
    asprintf(&uri, "file:///%s", normalized_path);
    free(normalized_path);
    #else
    asprintf(&uri, "file://%s", file_path);
    #endif
    

    
    char* params;
    int param_len = asprintf(&params,
        "{"
        "\"textDocument\":{"
          "\"uri\":\"%s\""
        "},"
        "\"position\":{"
          "\"line\":%zu,"
          "\"character\":%zu"
        "},"
        "\"context\":{"
          "\"triggerKind\":1"
        "}"
        "}",
        uri, position.line, position.column
    );
    
    free(uri);
    
    if (param_len < 0 || !params) {
        return -1;
    }
    
    /* Reset completion state */
    g_state.completion_count = 0;
    
    /* Send completion request */
    int request_id = vizero_lsp_client_send_request(g_state.lsp_client, "textDocument/completion", params);
    free(params);
    
    if (request_id < 0) {
        return -1;
    }
    
    CLANGD_DBG("Completion request sent (ID: %d), returning immediately", request_id);
    
    /* Store request ID for tracking */
    g_state.pending_completion_request_id = request_id;
    g_state.completion_request_pending = true;
    g_state.completion_count = 0; /* Reset count */
    
    CLANGD_DBG("Set pending completion request ID: %d", request_id);
    
    /* Return immediately for async operation - results will be polled later */
    CLANGD_DBG("Completion request sent asynchronously, returning -1 to indicate pending operation");
    if (absolute_path) free(absolute_path);
    return -1;
}

static int clangd_lsp_hover(vizero_buffer_t* buffer, vizero_position_t position, char** hover_text) {
    if (!g_state.initialized || !buffer || !hover_text) {
        return -1;
    }
    
    /* Get buffer file path */
    const char* file_path = g_state.api ? g_state.api->get_buffer_filename(buffer) : NULL;
    if (!file_path) {
        return -1;
    }
    
    /* Prepare hover request parameters */
    char* escaped_path = vizero_lsp_client_escape_json_string(file_path);
    if (!escaped_path) {
        return -1;
    }
    
    char* params;
    int param_result = asprintf(&params,
        "{"
        "\"textDocument\": {"
        "\"uri\": \"file://%s\""
        "},"
        "\"position\": {"
        "\"line\": %zu,"
        "\"character\": %zu"
        "}"
        "}",
        escaped_path, position.line, position.column);
    
    vizero_lsp_client_free_string(escaped_path);
    
    if (param_result < 0) {
        return -1;
    }
    
    /* Send hover request to clangd */
    int request_id = vizero_lsp_client_send_request(g_state.lsp_client, "textDocument/hover", params);
    free(params);
    
    if (request_id < 0) {
        return -1;
    }
    
    /* For now, provide a sample hover response since we don't have async response handling */
    char sample_hover[512];
    snprintf(sample_hover, sizeof(sample_hover), 
            "Hover info at line %zu, column %zu\nFile: %s\n\nType information and documentation\nwould appear here from clangd.\n\nThis is a demonstration of the\nhover popup functionality.", 
            position.line + 1, position.column + 1, file_path);
    *hover_text = strdup(sample_hover);
    return 0;
}

static int clangd_lsp_goto_definition(vizero_buffer_t* buffer, vizero_position_t position,
                                     vizero_location_t** locations, size_t* location_count) {
    if (!g_state.initialized || !buffer || !locations || !location_count) {
        return -1;
    }
    
    /* TODO: Implement goto definition */
    *locations = NULL;
    *location_count = 0;
    return 0;
}

static int clangd_lsp_get_diagnostics(vizero_buffer_t* buffer, vizero_diagnostic_t** diagnostics,
                                     size_t* diagnostic_count) {
    if (!g_state.initialized || !buffer || !diagnostics || !diagnostic_count) {
        printf("[CLANGD] get_diagnostics failed - initialized=%d, buffer=%p, diagnostics=%p, diagnostic_count=%p\n",
               g_state.initialized, (void*)buffer, (void*)diagnostics, (void*)diagnostic_count);
        return -1;
    }
    
    /* Get buffer file path and content */
    const char* file_path = g_state.api ? g_state.api->get_buffer_filename(buffer) : NULL;
    const char* content = g_state.api ? g_state.api->get_buffer_text(buffer) : NULL;
    
    /* Just return existing diagnostics - do NOT send didChange during normal rendering */
    
    /* Diagnostic retrieval disabled - keeping only hover popup functionality */
    *diagnostics = NULL;
    *diagnostic_count = 0;
    return 0;
}

VIZERO_PLUGIN_API void clangd_manual_diagnostic_refresh(vizero_buffer_t* buffer) {
    /* Legacy function - now redirects to diagnostic popup */
    clangd_show_diagnostic_popup(buffer);
}

VIZERO_PLUGIN_API void clangd_show_diagnostic_popup(vizero_buffer_t* buffer) {
    if (!g_state.initialized || !buffer || !g_state.lsp_client) {
        CLANGD_ERR("Show diagnostic popup failed - not initialized or invalid buffer");
        return;
    }
    
    const char* file_path = g_state.api ? g_state.api->get_buffer_filename(buffer) : NULL;
    const char* content = g_state.api ? g_state.api->get_buffer_text(buffer) : NULL;
    
    if (!file_path || !content) {
        CLANGD_ERR("Show diagnostic popup failed - no file path or content");
        return;
    }
    
    CLANGD_DBG("Showing diagnostic popup for: %s", file_path);
    
    /* First, check if we already have diagnostics for this file */
    if (g_state.diagnostics && g_state.diagnostic_count > 0 && 
        g_state.diagnostic_buffer_path && strcmp(g_state.diagnostic_buffer_path, file_path) == 0) {
        /* Showing existing diagnostics */
        show_diagnostic_popup(&g_state.popup, g_state.diagnostics, g_state.diagnostic_count, file_path);
        
        /* Also refresh diagnostics in background for next time */
        CLANGD_DBG("Also sending didChange to refresh diagnostics for next time...");
        send_did_change_notification(file_path, content);
        return;
    }
    
    /* No existing diagnostics, need to wait for fresh ones */
    /* Requesting fresh diagnostics */
    
    /* Show status message to user while waiting */
    if (g_state.api && g_state.api->set_status_message && g_state.editor) {
        g_state.api->set_status_message(g_state.editor, "Checking for errors...");
    }
    
    /* Send didChange to get fresh diagnostics */
    send_did_change_notification(file_path, content);
    
    /* The diagnostic popup will be shown when publishDiagnostics arrives */
    CLANGD_DBG("didChange sent - popup will show when diagnostics arrive");
}

static void clangd_lsp_shutdown(void) {
    if (g_state.lsp_client && g_state.initialized) {
        vizero_lsp_client_stop(g_state.lsp_client);
        g_state.initialized = false;
    }
}

/* Utility functions */
static char* find_clangd_executable(void) {
    char* clangd_path = NULL;
    
#ifdef _WIN32
    /* Try to find clangd.exe in the clangd/ directory next to the executable */
    char exe_path[MAX_PATH];
    if (GetModuleFileNameA(NULL, exe_path, MAX_PATH) > 0) {
        /* Remove filename, keep directory - cross-platform way */
        char* last_slash = strrchr(exe_path, '\\');
        if (last_slash) {
            *last_slash = '\0';
        }
        
        char* test_path = (char*)malloc(MAX_PATH);
        if (test_path) {
            snprintf(test_path, MAX_PATH, "%s\\clangd\\bin\\clangd.exe", exe_path);
            
            /* Cross-platform file existence check */
            struct stat st;
            if (stat(test_path, &st) == 0) {
                clangd_path = test_path;
            } else {
                free(test_path);
            }
        }
    }
    
    /* If not found, try system PATH */
    if (!clangd_path) {
        clangd_path = strdup("clangd.exe");
    }
#else
    /* Try to find clangd in the clangd/ directory next to the executable */
    char exe_path[1024];
    ssize_t len = readlink("/proc/self/exe", exe_path, sizeof(exe_path) - 1);
    if (len > 0) {
        exe_path[len] = '\0';
        char* dir = dirname(exe_path);
        
        char* test_path = (char*)malloc(2048);
        if (test_path) {
            snprintf(test_path, 2048, "%s/clangd/bin/clangd", dir);
            
            struct stat st;
            if (stat(test_path, &st) == 0 && (st.st_mode & S_IXUSR)) {
                clangd_path = test_path;
            } else {
                free(test_path);
            }
        }
    }
    
    /* If not found, try system PATH */
    if (!clangd_path) {
        clangd_path = strdup("clangd");
    }
#endif
    
    return clangd_path;
}

/* LSP callback implementations */
static void on_lsp_response(int request_id, const char* result, const char* error, void* user_data) {
    /* Add comprehensive null checking to prevent crashes */
    if (!user_data) {
        CLANGD_ERR("NULL user_data in LSP response");
        return;
    }
    
    clangd_state_t* state = (clangd_state_t*)user_data;
    
    CLANGD_DBG("LSP Response received: request_id=%d", request_id);
    
    if (error) {
        CLANGD_ERR("LSP Error: %s", error);
        /* Reset completion state on error */
        if (state->completion_request_pending && request_id == state->pending_completion_request_id) {
            state->completion_request_pending = false;
            state->pending_completion_request_id = -1;
        }
        return;
    }
    
    if (!result) {
        CLANGD_DBG("No result in LSP response");
        /* Reset completion state when no result */
        if (state->completion_request_pending && request_id == state->pending_completion_request_id) {
            state->completion_request_pending = false;
            state->pending_completion_request_id = -1;
        }
        return;
    }
    
    /* Safety check: limit result size to prevent crashes */
    size_t result_len = strlen(result);
    if (result_len > 10 * 1024 * 1024) { /* 10MB limit */
        CLANGD_ERR("Result too large (%zu bytes), ignoring", result_len);
        return;
    }
    
    CLANGD_DBG("LSP Result (first 200 chars): %.200s%s", 
        result, result_len > 200 ? "..." : "");
    
    /* Check if this is a response to our pending completion request */
    if (state->completion_request_pending && request_id == state->pending_completion_request_id) {
    CLANGD_DBG("Received completion response for request ID: %d", request_id);
    state->completion_request_pending = false;
        
    /* Parse completion results using proper JSON parser */
    CLANGD_DBG("Parsing completion results from response using proper JSON parser");
        
        /* Parse the JSON response */
        vizero_json_t* json = vizero_json_parse(result, result_len);
        if (!json) {
            CLANGD_ERR("Failed to parse JSON response");
            printf("[CLANGD] No completion items found - invalid JSON\n");
            state->completion_count = 0;
        } else {
            state->completion_count = 0;
            
            /* Get the items array */
            vizero_json_t* items_array = vizero_json_get_object(json, "items");
            if (!items_array) {
                printf("[CLANGD] No completion items found - no items array\n");
                state->completion_count = 0;
            } else {
                int array_size = vizero_json_array_size(items_array);
                CLANGD_DBG("Found %d completion items in response", array_size);
                
                if (array_size > 0) {
                    /* Process completion items */
                    int max_items = (array_size < 20) ? array_size : 20; /* Limit to 20 items */
                    
                    for (int i = 0; i < max_items; i++) {
                        vizero_json_t* item = vizero_json_array_get(items_array, i);
                        if (!item) continue;
                        
                        /* Extract completion item fields */
                        char* label = vizero_json_get_string(item, "label");
                        char* detail = vizero_json_get_string(item, "detail");
                        char* documentation = vizero_json_get_string(item, "documentation");
                        char* insert_text = vizero_json_get_string(item, "insertText");
                        char* filter_text = vizero_json_get_string(item, "filterText");
                        char* sort_text = vizero_json_get_string(item, "sortText");
                        int kind = vizero_json_get_int(item, "kind", 1); /* Default to text */
                        
                        if (label) {
                            state->completion_items[state->completion_count].label = label;
                            state->completion_items[state->completion_count].detail = detail ? detail : strdup("");
                            state->completion_items[state->completion_count].documentation = documentation;
                            state->completion_items[state->completion_count].insert_text = insert_text ? insert_text : strdup(label);
                            state->completion_items[state->completion_count].filter_text = filter_text;
                            state->completion_items[state->completion_count].sort_text = sort_text;
                            
                            /* Map LSP completion kinds to vizero kinds */
                            switch (kind) {
                                case 3: state->completion_items[state->completion_count].kind = VIZERO_COMPLETION_FUNCTION; break;
                                case 6: state->completion_items[state->completion_count].kind = VIZERO_COMPLETION_VARIABLE; break;
                                case 9: state->completion_items[state->completion_count].kind = VIZERO_COMPLETION_MODULE; break;
                                case 14: state->completion_items[state->completion_count].kind = VIZERO_COMPLETION_KEYWORD; break;
                                default: state->completion_items[state->completion_count].kind = VIZERO_COMPLETION_TEXT; break;
                            }
                            
                            state->completion_items[state->completion_count].deprecated = false;
                            state->completion_count++;
                        } else {
                            /* Free unused strings */
                            if (detail) free(detail);
                            if (documentation) free(documentation);
                            if (insert_text) free(insert_text);
                            if (filter_text) free(filter_text);
                            if (sort_text) free(sort_text);
                        }
                        
                        vizero_json_free(item);
                    }
                }
                
                vizero_json_free(items_array);
            }
            
            vizero_json_free(json);
            
            if (state->completion_count == 0) {
                printf("[CLANGD] No completion items found\n");
                /* Add a placeholder item to show that completion was triggered */
                state->completion_items[0].label = strdup("printf");
                state->completion_items[0].detail = strdup("function");
                state->completion_items[0].documentation = strdup("Standard output function");
                state->completion_items[0].insert_text = strdup("printf");
                state->completion_items[0].filter_text = NULL;
                state->completion_items[0].sort_text = NULL;
                state->completion_items[0].kind = VIZERO_COMPLETION_FUNCTION;
                state->completion_items[0].deprecated = false;
                state->completion_count = 1;
                CLANGD_DBG("Added fallback printf completion item");
            } else {
                CLANGD_DBG("Successfully parsed %d completion items", state->completion_count);
            }
        }
        
        /* Create completion list and store it for later retrieval */
        vizero_completion_list_t* list = (vizero_completion_list_t*)malloc(sizeof(vizero_completion_list_t));
            if (list) {
            list->items = (vizero_completion_item_t*)malloc(state->completion_count * sizeof(vizero_completion_item_t));
            if (list->items) {
                memcpy(list->items, state->completion_items, state->completion_count * sizeof(vizero_completion_item_t));
                CLANGD_DBG("Copied %zu completion items to list", state->completion_count);
                for (size_t i = 0; i < state->completion_count; i++) {
                    CLANGD_DBG("Item %zu: label='%s', insert_text='%s'", i, 
                        list->items[i].label ? list->items[i].label : "NULL",
                        list->items[i].insert_text ? list->items[i].insert_text : "NULL");
                }
            }
            list->item_count = state->completion_count;
            list->is_incomplete = false;
            
            /* Store for later retrieval */
            state->last_completion_result = list;
            CLANGD_DBG("Created safe completion result with %zu items", state->completion_count);
        }
    } else {
        CLANGD_DBG("Response not for completion request");
    }
}

static void on_lsp_notification(const char* method, const char* params, void* user_data) {
    (void)user_data;
    printf("[CLANGD] Received LSP notification: method='%s'\n", method ? method : "null");
    
    if (!method || !params) {
        return;
    }
    
    /* Handle publishDiagnostics notifications */
    if (strcmp(method, "textDocument/publishDiagnostics") == 0) {
        printf("[CLANGD] Received publishDiagnostics notification!\n");
        printf("[CLANGD] Diagnostics params (first 300 chars): %.300s\n", params ? params : "null");
        
        /* Clear existing diagnostics */
        if (g_state.diagnostics) {
            for (size_t i = 0; i < g_state.diagnostic_count; i++) {
                free(g_state.diagnostics[i].message);
                free(g_state.diagnostics[i].source);
            }
            free(g_state.diagnostics);
            g_state.diagnostics = NULL;
        }
        if (g_state.diagnostic_buffer_path) {
            free(g_state.diagnostic_buffer_path);
            g_state.diagnostic_buffer_path = NULL;
        }
        g_state.diagnostic_count = 0;
        
        /* Simple JSON parsing for diagnostics (in a real implementation, use a proper JSON parser) */
        const char* uri_start = strstr(params, "\"uri\":\"");
        if (uri_start) {
            uri_start += 7; /* Skip "uri":" */
            const char* uri_end = strchr(uri_start, '"');
            if (uri_end) {
                /* Extract file path from URI */
                size_t uri_len = uri_end - uri_start;
                char* uri = malloc(uri_len + 1);
                if (uri) {
                    strncpy(uri, uri_start, uri_len);
                    uri[uri_len] = '\0';
                    
                    /* Convert file:// URI to path */
                    if (strncmp(uri, "file:///", 8) == 0) {
                        /* Windows file:///C:/path format */
                        g_state.diagnostic_buffer_path = strdup(uri + 8);
                    } else if (strncmp(uri, "file://", 7) == 0) {
                        g_state.diagnostic_buffer_path = strdup(uri + 7);
                    } else {
                        g_state.diagnostic_buffer_path = strdup(uri);
                    }
                    free(uri);
                    
                    /* New diagnostics received */
                    /* Processing diagnostic parameters */
                    
                    /* Parse diagnostics array - simple parsing for now */
                    const char* diagnostics_start = strstr(params, "\"diagnostics\":");
                    if (diagnostics_start) {
                        /* Find the opening bracket of diagnostics array */
                        const char* array_start = strchr(diagnostics_start, '[');
                        const char* array_end = array_start ? strstr(array_start, "]") : NULL;
                        if (array_start && array_end) {
                            /* Check for empty array first */
                            const char* content_check = array_start + 1;
                            while (content_check < array_end && (*content_check == ' ' || *content_check == '\n' || *content_check == '\t')) {
                                content_check++;
                            }
                            
                            if (content_check >= array_end || *content_check == ']') {
                                /* Empty diagnostics array */
                                printf("[CLANGD] Empty diagnostics array - no errors found\n");
                                g_state.diagnostic_count = 0;
                                return;
                            }
                            
                            /* Count diagnostic objects by counting opening braces */
                            int diagnostic_count = 0;
                            const char* ptr = array_start;
                            while ((ptr = strchr(ptr, '{')) != NULL && ptr < array_end) {
                                diagnostic_count++;
                                ptr++;
                            }
                            
                            /* Processing diagnostic response */
                            
                            if (diagnostic_count > 0) {
                                    g_state.diagnostic_capacity = diagnostic_count;
                                    g_state.diagnostics = malloc(sizeof(vizero_diagnostic_t) * g_state.diagnostic_capacity);
                                    if (g_state.diagnostics) {
                                        g_state.diagnostic_count = diagnostic_count;
                                    
                                    /* Parse each diagnostic from the JSON response */
                                    int parsed_count = 0;
                                    const char* diagnostic_start = strchr(array_start, '{');
                                    
                                    while (diagnostic_start && parsed_count < diagnostic_count && parsed_count < 4) {
                                        /* Find the end of this diagnostic object */
                                        const char* diagnostic_end = diagnostic_start;
                                        int brace_count = 0;
                                        do {
                                            if (*diagnostic_end == '{') brace_count++;
                                            else if (*diagnostic_end == '}') brace_count--;
                                            diagnostic_end++;
                                        } while (brace_count > 0 && *diagnostic_end);
                                        
                                        /* Extract line number from "line": */
                                        const char* line_pos = strstr(diagnostic_start, "\"line\":");
                                        int line_num = 0;
                                        if (line_pos && line_pos < diagnostic_end) {
                                            line_pos += 7; /* Skip "line": */
                                            while (*line_pos == ' ') line_pos++; /* Skip whitespace */
                                            line_num = atoi(line_pos);
                                        }
                                        
                                        /* Extract column number from "character": */
                                        const char* char_pos = strstr(diagnostic_start, "\"character\":");
                                        int char_num = 0;
                                        if (char_pos && char_pos < diagnostic_end) {
                                            char_pos += 12; /* Skip "character": */
                                            while (*char_pos == ' ') char_pos++; /* Skip whitespace */
                                            char_num = atoi(char_pos);
                                        }
                                        
                                        /* Extract severity from "severity": */
                                        const char* severity_pos = strstr(diagnostic_start, "\"severity\":");
                                        int severity = 1; /* Default to error */
                                        if (severity_pos && severity_pos < diagnostic_end) {
                                            severity_pos += 11; /* Skip "severity": */
                                            while (*severity_pos == ' ') severity_pos++; /* Skip whitespace */
                                            severity = atoi(severity_pos);
                                        }

                                        /* Extract message from "message": */
                                        const char* msg_start = strstr(diagnostic_start, "\"message\":\"");
                                        char* message = NULL;
                                        if (msg_start && msg_start < diagnostic_end) {
                                            msg_start += 11; /* Skip "message":" */
                                            const char* msg_end = strchr(msg_start, '"');
                                            if (msg_end && msg_end < diagnostic_end) {
                                                size_t msg_len = msg_end - msg_start;
                                                message = malloc(msg_len + 1);
                                                if (message) {
                                                    strncpy(message, msg_start, msg_len);
                                                    message[msg_len] = '\0';
                                                }
                                            }
                                        }
                                        
                                        if (!message) {
                                            message = strdup("LSP diagnostic");
                                        }
                                        
                                        /* Store the diagnostic */
                                        g_state.diagnostics[parsed_count].range.start.line = line_num;
                                        g_state.diagnostics[parsed_count].range.start.column = char_num;
                                        g_state.diagnostics[parsed_count].range.end.line = line_num;
                                        g_state.diagnostics[parsed_count].range.end.column = char_num + 10; /* Reasonable underline length */
                                        
                                        /* Map LSP severity to vizero severity: 1=Error, 2=Warning, 3=Info, 4=Hint */
                                        if (severity == 1) {
                                            g_state.diagnostics[parsed_count].severity = VIZERO_DIAGNOSTIC_ERROR;
                                        } else if (severity == 2) {
                                            g_state.diagnostics[parsed_count].severity = VIZERO_DIAGNOSTIC_WARNING;
                                        } else {
                                            g_state.diagnostics[parsed_count].severity = VIZERO_DIAGNOSTIC_INFORMATION;
                                        }
                                        
                                        g_state.diagnostics[parsed_count].message = message;
                                        g_state.diagnostics[parsed_count].source = strdup("clangd");
                                        
                                        const char* severity_str = (severity == 1) ? "ERROR" : 
                                                                  (severity == 2) ? "WARNING" : 
                                                                  (severity == 3) ? "INFO" : "HINT";
                                        /* Diagnostic parsed successfully */
                                        
                                        parsed_count++;
                                        
                                        /* Find next diagnostic */
                                        diagnostic_start = strchr(diagnostic_end, '{');
                                    }
                                    
                                    g_state.diagnostic_count = parsed_count;
                                    
                                    /* Diagnostics processed */
                                    
                                    /* Diagnostics parsed and stored - ready for manual Ctrl+D */
                                    if (g_state.diagnostic_count > 0) {
                                        /* Diagnostics ready for display */
                                    } else {
                                        printf("[CLANGD] No diagnostics found\n");
                                    }
                                }
                            } else {
                                printf("[CLANGD] No diagnostics in response\n");
                                g_state.diagnostic_count = 0;
                            }
                        }
                    }
                }
            }
        }
    }
}

static void on_lsp_error(const char* error_message, void* user_data) {
    /* Handle LSP client errors */
    (void)error_message;
    (void)user_data;
    printf("[CLANGD] LSP Error: %s\n", error_message ? error_message : "Unknown error");
}

/* Diagnostic popup implementation */
static void init_diagnostic_popup(diagnostic_popup_t* popup) {
    if (!popup) return;
    
    memset(popup, 0, sizeof(diagnostic_popup_t));
    popup->is_visible = false;
    popup->selected_index = 0;
    popup->popup_width = 600.0f;
    popup->popup_height = 300.0f;
}

static void cleanup_diagnostic_popup(diagnostic_popup_t* popup) {
    if (!popup) return;
    
    if (popup->diagnostics) {
        for (size_t i = 0; i < popup->diagnostic_count; i++) {
            free(popup->diagnostics[i].message);
            free(popup->diagnostics[i].source);
        }
        free(popup->diagnostics);
        popup->diagnostics = NULL;
    }
    
    free(popup->buffer_path);
    popup->buffer_path = NULL;
    
    popup->diagnostic_count = 0;
    popup->is_visible = false;
}

static void show_diagnostic_popup(diagnostic_popup_t* popup, vizero_diagnostic_t* diagnostics, size_t count, const char* buffer_path) {
    if (!popup || !diagnostics || count == 0) {
        printf("[CLANGD] Cannot show popup - invalid parameters\n");
        return;
    }
    
    /* Clean up existing popup data */
    cleanup_diagnostic_popup(popup);
    
    /* Copy diagnostics */
    popup->diagnostics = malloc(count * sizeof(vizero_diagnostic_t));
    if (!popup->diagnostics) {
        printf("[CLANGD] Failed to allocate memory for popup diagnostics\n");
        return;
    }
    
    popup->diagnostic_count = count;
    popup->error_count = 0;
    popup->warning_count = 0;
    popup->info_count = 0;
    
    for (size_t i = 0; i < count; i++) {
        popup->diagnostics[i] = diagnostics[i];
        popup->diagnostics[i].message = strdup(diagnostics[i].message);
        popup->diagnostics[i].source = strdup(diagnostics[i].source);
        
        /* Count by severity */
        switch (diagnostics[i].severity) {
            case VIZERO_DIAGNOSTIC_ERROR:
                popup->error_count++;
                break;
            case VIZERO_DIAGNOSTIC_WARNING:
                popup->warning_count++;
                break;
            default:
                popup->info_count++;
                break;
        }
    }
    
    popup->buffer_path = strdup(buffer_path);
    popup->selected_index = 0;
    popup->is_visible = true;
    
    /* Format diagnostics as text for existing popup system */
    char* popup_text = format_diagnostics_for_popup(diagnostics, count, popup->error_count, popup->warning_count, popup->info_count);
    
    /* Show popup using existing editor popup system */
    if (g_state.api && popup_text) {
        /* Get current editor from API - we need a way to get this */
        /* For now, we'll store editor reference in plugin state */
        if (g_state.editor && g_state.api->show_popup) {
            int result = g_state.api->show_popup(g_state.editor, popup_text, 0); /* No timeout - stays until ESC */
            printf("[CLANGD] Showing diagnostic popup with %zu diagnostics (%zu errors, %zu warnings, %zu info)\n",
                   count, popup->error_count, popup->warning_count, popup->info_count);
        } else {
            printf("[CLANGD] Cannot show popup - editor=%p, show_popup=%p\n", 
                   (void*)g_state.editor, (void*)(g_state.api ? g_state.api->show_popup : NULL));
        }
        free(popup_text);
    } else {
        printf("[CLANGD] Failed to format popup text - api=%p, popup_text=%p\n", 
               (void*)g_state.api, (void*)popup_text);
    }
}

static void hide_diagnostic_popup(diagnostic_popup_t* popup) {
    if (!popup) return;
    popup->is_visible = false;
}

static int handle_diagnostic_popup_input(diagnostic_popup_t* popup, int key, int modifiers) {
    if (!popup || !popup->is_visible) return 0;
    
    (void)modifiers; // Unused for now
    
    switch (key) {
        case SDLK_ESCAPE:
            hide_diagnostic_popup(popup);
            return 1; // Consumed
            
        case SDLK_UP:
        case SDLK_k: // Vi-style up
            if (popup->selected_index > 0) {
                popup->selected_index--;
            }
            return 1; // Consumed
            
        case SDLK_DOWN:
        case SDLK_j: // Vi-style down
            if (popup->selected_index < popup->diagnostic_count - 1) {
                popup->selected_index++;
            }
            return 1; // Consumed
            
        case SDLK_RETURN:
            // TODO: Jump to diagnostic location
            printf("[CLANGD] Jump to diagnostic at line %zu, col %zu\n",
                   popup->diagnostics[popup->selected_index].range.start.line + 1,
                   popup->diagnostics[popup->selected_index].range.start.column + 1);
            hide_diagnostic_popup(popup);
            return 1; // Consumed
            
        default:
            return 0; // Not consumed
    }
}

static void render_diagnostic_popup(diagnostic_popup_t* popup, void* renderer) {
    if (!popup || !popup->is_visible || !renderer) return;
    
    // TODO: Implement OpenGL rendering
    // For now, just mark that we would render
    printf("[CLANGD] Would render popup with %zu diagnostics at (%.1f, %.1f)\n",
           popup->diagnostic_count, popup->popup_x, popup->popup_y);
}

static char* format_diagnostics_for_popup(vizero_diagnostic_t* diagnostics, size_t count, size_t error_count, size_t warning_count, size_t info_count) {
    if (!diagnostics || count == 0) return NULL;
    
    /* Calculate buffer size needed */
    size_t buffer_size = 1024; /* Base size for header */
    for (size_t i = 0; i < count; i++) {
        buffer_size += strlen(diagnostics[i].message) + 100; /* Message + formatting */
    }
    
    char* result = malloc(buffer_size);
    if (!result) return NULL;
    
    /* Create header */
    snprintf(result, buffer_size, 
        "Diagnostics (%zu errors, %zu warnings, %zu info)\n"
        "Press ESC to close\n"
        "==========================================\n\n",
        error_count, warning_count, info_count);
    
    /* Add each diagnostic */
    for (size_t i = 0; i < count; i++) {
        char* severity_str;
        switch (diagnostics[i].severity) {
            case VIZERO_DIAGNOSTIC_ERROR:
                severity_str = "ERROR";
                break;
            case VIZERO_DIAGNOSTIC_WARNING:
                severity_str = "WARNING";
                break;
            default:
                severity_str = "INFO";
                break;
        }
        
        char temp[512];
        snprintf(temp, sizeof(temp), "[%s] Line %zu, Col %zu:\n  %s\n\n",
                 severity_str,
                 diagnostics[i].range.start.line + 1,  /* Convert 0-based to 1-based */
                 diagnostics[i].range.start.column + 1,
                 diagnostics[i].message);
        
        strncat(result, temp, buffer_size - strlen(result) - 1);
    }
    
    return result;
}

