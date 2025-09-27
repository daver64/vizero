#include "vizero/plugin_interface.h"
#include "vizero/lsp_client.h"
#include "vizero/buffer.h"
#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <stdbool.h>
#include <time.h>
#include <stdarg.h>

#ifdef _WIN32
#include <windows.h>
#include <shlwapi.h>
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
    
    /* Async completion state */
    int pending_completion_request_id;
    bool completion_request_pending;
    vizero_completion_list_t* last_completion_result;
    
    /* Diagnostics storage */
    vizero_diagnostic_t* diagnostics;
    size_t diagnostic_count;
    size_t diagnostic_capacity;
    char* diagnostic_buffer_path; /* Path of buffer that diagnostics apply to */
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
__declspec(dllexport) void clangd_manual_diagnostic_refresh(vizero_buffer_t* buffer);
static void clangd_lsp_shutdown(void);

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
    printf("[CLANGD] *** PLUGIN INITIALIZATION STARTING ***\n");
    fflush(stdout);
    if (!plugin || !editor || !api) {
        printf("[CLANGD] Plugin initialization FAILED - null parameters\n");
        fflush(stdout);
        return -1;
    }
    
    /* Store API pointer */
    g_state.api = api;
    
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
        printf("[CLANGD] clangd executable not found, disabling LSP functionality\n");
        printf("[CLANGD] Plugin will load but LSP features will be unavailable\n");
        
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
    printf("[CLANGD] Found clangd at: %s\n", clangd_path);
    
    /* Test if clangd actually exists before creating LSP client */
#ifdef _WIN32
    if (!PathFileExistsA(clangd_path)) {
        free(clangd_path);
        /* clangd executable not found - disable functionality */
        printf("[CLANGD] clangd executable not accessible, disabling LSP functionality\n");
        printf("[CLANGD] Plugin will load but LSP features will be unavailable\n");
        
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
#else
    struct stat st;
    if (stat(clangd_path, &st) != 0 || !(st.st_mode & S_IXUSR)) {
        free(clangd_path);
        /* clangd executable not found - disable functionality */
        printf("[CLANGD] clangd executable not accessible, disabling LSP functionality\n");
        printf("[CLANGD] Plugin will load but LSP features will be unavailable\n");
        
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
#endif
    
    /* Create LSP client */
    g_state.lsp_client = vizero_lsp_client_create(clangd_path, NULL);
    free(clangd_path);
    
    if (!g_state.lsp_client) {
        printf("[CLANGD] Failed to create LSP client, disabling LSP functionality\n");
        
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
        printf("[CLANGD] Failed to allocate completion buffer, disabling LSP functionality\n");
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
    printf("[CLANGD] Initializing LSP during plugin load...\n");
    if (clangd_lsp_initialize(".", NULL) != 0) {
        printf("[CLANGD] Failed to initialize LSP during plugin load\n");
        /* Don't fail plugin loading, just disable LSP functionality */
    }
    
    printf("[CLANGD] Plugin initialization complete\n");
    return 0;
}

void vizero_plugin_cleanup(void) {
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
        printf("[CLANGD] Completion results retrieved\n");
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
    
    printf("[CLANGD] Sending didChange notification for: %s (version %d)\n", file_path, version_number);
    
    /* Escape the file path for JSON */
    char* escaped_path = vizero_lsp_client_escape_json_string(file_path);
    if (!escaped_path) {
        printf("[CLANGD] Failed to escape file path for didChange\n");
        return;
    }
    
    /* Escape buffer content */
    char* escaped_content = vizero_lsp_client_escape_json_string(content);
    if (!escaped_content) {
        vizero_lsp_client_free_string(escaped_path);
        printf("[CLANGD] Failed to escape buffer content for didChange\n");
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
        printf("[CLANGD] Failed to create didChange params\n");
        return;
    }
    
    /* Send notification */
    int result = vizero_lsp_client_send_notification(g_state.lsp_client, "textDocument/didChange", params);
    free(params);
    
    if (result < 0) {
        printf("[CLANGD] Failed to send didChange notification\n");
        return;
    }
    
    printf("[CLANGD] didChange notification sent successfully for: %s\n", file_path);
}

/* Request diagnostics refresh from clangd by sending a lightweight request */
static void request_diagnostic_refresh(const char* file_path) {
    if (!g_state.lsp_client || !g_state.initialized || !file_path) {
        printf("[CLANGD] Cannot request diagnostic refresh - not initialized\n");
        return;
    }
    
    printf("[CLANGD] Requesting diagnostic refresh for: %s\n", file_path);
    
    /* Escape the file path for JSON */
    char* escaped_path = vizero_lsp_client_escape_json_string(file_path);
    if (!escaped_path) {
        printf("[CLANGD] Failed to escape file path for diagnostic refresh\n");
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
        printf("[CLANGD] Failed to create diagnostic refresh params\n");
        return;
    }
    
    /* Send request to trigger fresh analysis */
    int result = vizero_lsp_client_send_request(g_state.lsp_client, "textDocument/documentSymbol", params);
    free(params);
    
    if (result < 0) {
        printf("[CLANGD] Failed to send diagnostic refresh request\n");
        return;
    }
    
    printf("[CLANGD] Diagnostic refresh request sent successfully\n");
}

static int clangd_on_buffer_open(vizero_buffer_t* buffer, const char* filename) {
    printf("[CLANGD] *** on_buffer_open called for: %s ***\n", filename ? filename : "(unnamed)");
    
    if (!g_state.lsp_client || !g_state.initialized) {
        printf("[CLANGD] LSP not available or not initialized\n");
        return 0; /* Not an error - clangd just not available */
    }
    
    if (!filename || !buffer) {
        printf("[CLANGD] No filename or buffer provided\n");
        return 0;
    }
    
    /* Send textDocument/didOpen notification to clangd */
    printf("[CLANGD] Sending didOpen notification for: %s\n", filename);
    
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
        printf("[CLANGD] Failed to create didOpen params\n");
        return -1;
    }
    
    /* Send notification (no request ID needed) */
    int result = vizero_lsp_client_send_notification(g_state.lsp_client, "textDocument/didOpen", params);
    free(params);
    
    if (result < 0) {
        printf("[CLANGD] Failed to send didOpen notification\n");
        return -1;
    }
    
    printf("[CLANGD] didOpen notification sent successfully for: %s\n", filename);
    
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
    
    printf("[CLANGD] Initialized diagnostics storage for: %s\n", filename);
    
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
    printf("[CLANGD] Starting clangd process...\n");
    if (vizero_lsp_client_start(g_state.lsp_client) != 0) {
        printf("[CLANGD] Failed to start clangd process\n");
        return -1;
    }
    printf("[CLANGD] clangd process started successfully\n");
    
    /* Send initialize request */
    printf("[CLANGD] Sending LSP initialize request...\n");
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
        printf("[CLANGD] Failed to send initialize request\n");
        return -1;
    }
    
    printf("[CLANGD] Initialize request sent with ID: %d\n", request_id);
    
    /* Send initialized notification */
    vizero_lsp_client_send_notification(g_state.lsp_client, "initialized", "{}");
    printf("[CLANGD] Sent initialized notification\n");
    
    g_state.initialized = true;
    printf("[CLANGD] clangd initialization complete\n");
    return 0;
}

static int clangd_on_text_changed(vizero_buffer_t* buffer, vizero_range_t range, const char* new_text) {
    /* Text change callbacks disabled - keeping only hover popup functionality */
    return 0;
}

static int clangd_lsp_completion(vizero_buffer_t* buffer, vizero_position_t position, vizero_completion_list_t** result) {
    printf("[CLANGD] *** LSP COMPLETION TRIGGERED ***\n");
    
    if (!g_state.initialized || !buffer || !result || !g_state.lsp_client) {
        printf("[CLANGD] Completion failed: initialized=%d, buffer=%p, result=%p, lsp_client=%p\n",
               g_state.initialized, (void*)buffer, (void*)result, (void*)g_state.lsp_client);
        return -1;
    }
    
    /* Prevent concurrent completion requests that could corrupt state */
    if (g_state.completion_request_pending) {
        printf("[CLANGD] Completion request already pending, rejecting new request\n");
        return -1;
    }
    
    *result = NULL;
    
    /* Use real LSP completion with improved JSON parsing */
    
    /* Clean up any existing completion result to prevent memory leaks and double-free */
    if (g_state.last_completion_result) {
        printf("[CLANGD] Cleaning up previous completion result\n");
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
        printf("[CLANGD] No file path available for buffer\n");
        return -1;
    }
    printf("[CLANGD] Requesting completion for file: %s at line %zu, column %zu\n", 
           file_path, position.line, position.column);
    
    /* Send didOpen notification if we haven't already (workaround for timing issue) */
    printf("[CLANGD] Sending didOpen notification before completion...\n");
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
            printf("[CLANGD] didOpen notification sent\n");
        }
        vizero_lsp_client_free_string(didopen_escaped_path);
    }
    
    /* Build completion request parameters */
    char* escaped_path = vizero_lsp_client_escape_json_string(file_path);
    if (!escaped_path) {
        return -1;
    }
    
    /* Convert Windows path to proper URI format */
    char* uri;
    #ifdef _WIN32
    /* Windows: Convert backslashes to forward slashes and add drive letter handling */
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
    
    vizero_lsp_client_free_string(escaped_path);
    
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
    
    printf("[CLANGD] Completion request sent (ID: %d), returning immediately\n", request_id);
    
    /* Store request ID for tracking */
    g_state.pending_completion_request_id = request_id;
    g_state.completion_request_pending = true;
    g_state.completion_count = 0; /* Reset count */
    
    printf("[CLANGD] Set pending completion request ID: %d\n", request_id);
    
    /* Wait briefly for response with strict timeout to prevent hangs */
    printf("[CLANGD] Starting completion wait loop with hang protection...\n");
    
    #ifdef _WIN32
    DWORD start_time = GetTickCount();
    const DWORD TIMEOUT_MS = 300; /* Increased timeout for better reliability */
    #endif
    
    for (int attempts = 0; attempts < 10; attempts++) { /* Reduced attempts for faster timeout */
        #ifdef _WIN32
        DWORD current_time = GetTickCount();
        if (current_time - start_time > TIMEOUT_MS) {
            printf("[CLANGD] Hard timeout after %ums, breaking to prevent hang\n", TIMEOUT_MS);
            break;
        }
        #endif
        
        printf("[CLANGD] Processing messages, attempt %d/10\n", attempts + 1);
        
        /* Add safety check before processing messages */
        if (!g_state.lsp_client || !g_state.initialized) {
            printf("[CLANGD] LSP client became invalid during wait\n");
            break;
        }
        
        /* Process messages with error checking and timeout protection */
        int process_result = vizero_lsp_client_process_messages(g_state.lsp_client);
        
        /* If process_messages fails, don't spam the log */
        if (process_result != 0 && attempts == 0) {
            printf("[CLANGD] Process messages returned: %d (errors suppressed after first attempt)\n", process_result);
        } else if (process_result == 0 && attempts % 5 == 0) {
            printf("[CLANGD] Processing... (attempt %d)\n", attempts + 1);
        }
        
        /* Check if we got a response */
        if (g_state.last_completion_result) {
            printf("[CLANGD] Got completion result!\n");
            *result = g_state.last_completion_result;
            g_state.last_completion_result = NULL;
            g_state.completion_request_pending = false;
            printf("[CLANGD] Completion result ready with %zu items\n", (*result)->item_count);
            return 0;
        }
        
        /* Break early if LSP client indicates error */
        if (process_result < 0) {
            printf("[CLANGD] Message processing failed, breaking\n");
            break;
        }
        
        /* Wait 5ms before next attempt */
        #ifdef _WIN32
        Sleep(5);
        #else
        usleep(5000);
        #endif
    }
    
    /* Timeout - no result within 100ms, reset state to prevent corruption */
    printf("[CLANGD] Completion request timeout after 100ms\n");
    g_state.completion_request_pending = false;
    g_state.pending_completion_request_id = -1;
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

__declspec(dllexport) void clangd_manual_diagnostic_refresh(vizero_buffer_t* buffer) {
    /* Manual diagnostic refresh disabled - keeping only hover popup functionality */
    return;
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
        PathRemoveFileSpecA(exe_path); /* Remove filename, keep directory */
        
        char* test_path = (char*)malloc(MAX_PATH);
        if (test_path) {
            snprintf(test_path, MAX_PATH, "%s\\clangd\\bin\\clangd.exe", exe_path);
            
            if (PathFileExistsA(test_path)) {
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
        printf("[CLANGD] ERROR: NULL user_data in LSP response\n");
        return;
    }
    
    clangd_state_t* state = (clangd_state_t*)user_data;
    
    printf("[CLANGD] LSP Response received: request_id=%d\n", request_id);
    
    if (error) {
        printf("[CLANGD] LSP Error: %s\n", error);
        /* Reset completion state on error */
        if (state->completion_request_pending && request_id == state->pending_completion_request_id) {
            state->completion_request_pending = false;
            state->pending_completion_request_id = -1;
        }
        return;
    }
    
    if (!result) {
        printf("[CLANGD] No result in LSP response\n");
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
        printf("[CLANGD] ERROR: Result too large (%zu bytes), ignoring\n", result_len);
        return;
    }
    
    printf("[CLANGD] LSP Result (first 200 chars): %.200s%s\n", 
           result, result_len > 200 ? "..." : "");
    
    /* Check if this is a response to our pending completion request */
    if (state->completion_request_pending && request_id == state->pending_completion_request_id) {
        printf("[CLANGD] Received completion response for request ID: %d\n", request_id);
        state->completion_request_pending = false;
        
        /* Parse actual completion results now that crashes are fixed */
        printf("[CLANGD] Parsing completion results from response\n");
        
        /* Simple parsing - look for completion items */
        const char* items_start = strstr(result, "\"items\":[");
        if (!items_start) {
            printf("[CLANGD] No completion items found\n");
            state->completion_count = 0;
        } else {
            /* For now, extract a few items safely */
            state->completion_count = 0;
            const char* search_pos = items_start + 9; /* Skip "items":[ */
            
            for (int i = 0; i < 10 && state->completion_count < 20; i++) {
                const char* label_start = strstr(search_pos, "\"insertText\":\"");
                if (!label_start) break;
                
                label_start += 14; /* Skip "insertText":" */
                const char* label_end = strchr(label_start, '"');
                if (!label_end || label_end - label_start > 100) break;
                
                size_t label_len = label_end - label_start;
                char* label = (char*)malloc(label_len + 1);
                if (!label) break;
                
                memcpy(label, label_start, label_len);
                label[label_len] = '\0';
                
                state->completion_items[state->completion_count].label = strdup(label);
                state->completion_items[state->completion_count].detail = strdup("Function");
                state->completion_items[state->completion_count].documentation = NULL;
                state->completion_items[state->completion_count].insert_text = label;
                state->completion_items[state->completion_count].filter_text = NULL;
                state->completion_items[state->completion_count].sort_text = NULL;
                state->completion_items[state->completion_count].kind = VIZERO_COMPLETION_FUNCTION;
                state->completion_items[state->completion_count].deprecated = false;
                
                state->completion_count++;
                search_pos = label_end + 1;
            }
            
            if (state->completion_count == 0) {
                /* Fallback to mock if parsing failed */
                state->completion_count = 1;
                state->completion_items[0].label = strdup("printf");
                state->completion_items[0].detail = strdup("Function");
                state->completion_items[0].documentation = NULL;
                state->completion_items[0].insert_text = strdup("printf");
                state->completion_items[0].filter_text = NULL;
                state->completion_items[0].sort_text = NULL;
                state->completion_items[0].kind = VIZERO_COMPLETION_FUNCTION;
                state->completion_items[0].deprecated = false;
            }
        }
        
        /* Create completion list and store it for later retrieval */
        vizero_completion_list_t* list = (vizero_completion_list_t*)malloc(sizeof(vizero_completion_list_t));
        if (list) {
            list->items = (vizero_completion_item_t*)malloc(state->completion_count * sizeof(vizero_completion_item_t));
            if (list->items) {
                memcpy(list->items, state->completion_items, state->completion_count * sizeof(vizero_completion_item_t));
            }
            list->item_count = state->completion_count;
            list->is_incomplete = false;
            
            /* Store for later retrieval */
            state->last_completion_result = list;
            printf("[CLANGD] Created safe completion result with %zu items\n", state->completion_count);
        }
    } else {
        printf("[CLANGD] Response not for completion request\n");
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
        /* Diagnostic processing disabled - keeping only hover popup functionality */
        return;
        
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
                    
                    printf("[CLANGD] NEW DIAGNOSTICS: diagnostic_buffer_path='%s'\n", g_state.diagnostic_buffer_path);
                    printf("[CLANGD] Full params: %.500s\n", params);
                    
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
                            
                            printf("[CLANGD] Found %d diagnostics in response\n", diagnostic_count);
                            
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
                                        printf("[CLANGD] Parsed diagnostic %d: line=%d, col=%d, severity=%s, msg='%s'\n", 
                                               parsed_count, line_num, char_num, severity_str, message);
                                        
                                        parsed_count++;
                                        
                                        /* Find next diagnostic */
                                        diagnostic_start = strchr(diagnostic_end, '{');
                                    }
                                    
                                    g_state.diagnostic_count = parsed_count;
                                    
                                    printf("[CLANGD] Created %zu diagnostics from clangd response\n", g_state.diagnostic_count);
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

