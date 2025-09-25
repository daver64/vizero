#ifndef VIZERO_LSP_CLIENT_H
#define VIZERO_LSP_CLIENT_H

#ifdef __cplusplus
extern "C" {
#endif

#include <stddef.h>
#include <stdbool.h>
#include <stdint.h>

/* Forward declarations */
typedef struct vizero_lsp_client_t vizero_lsp_client_t;

/* LSP message types */
typedef enum {
    LSP_MESSAGE_REQUEST,
    LSP_MESSAGE_RESPONSE,
    LSP_MESSAGE_NOTIFICATION
} lsp_message_type_t;

/* LSP message structure */
typedef struct {
    lsp_message_type_t type;
    int id;                    /* For requests/responses */
    char* method;              /* "textDocument/completion" */
    char* params;              /* JSON parameters */
    char* result;              /* JSON result (responses only) */
    char* error;               /* JSON error (error responses only) */
} lsp_message_t;

/* LSP completion item */
typedef struct {
    char* label;               /* Text shown in completion list */
    char* detail;              /* Additional detail (optional) */
    char* documentation;       /* Documentation string (optional) */
    char* insert_text;         /* Text to insert (defaults to label) */
    int kind;                  /* CompletionItemKind (1=Text, 2=Method, 3=Function, etc.) */
} vizero_lsp_completion_item_t;

/* LSP client callbacks */
typedef struct {
    void (*on_response)(int request_id, const char* result, const char* error, void* user_data);
    void (*on_notification)(const char* method, const char* params, void* user_data);
    void (*on_error)(const char* error_message, void* user_data);
} lsp_client_callbacks_t;

/* LSP client management */
vizero_lsp_client_t* vizero_lsp_client_create(const char* server_path, const char* working_directory);
void vizero_lsp_client_destroy(vizero_lsp_client_t* client);

/* Connection management */
int vizero_lsp_client_start(vizero_lsp_client_t* client);
void vizero_lsp_client_stop(vizero_lsp_client_t* client);
bool vizero_lsp_client_is_running(const vizero_lsp_client_t* client);

/* Message handling */
int vizero_lsp_client_send_request(vizero_lsp_client_t* client, const char* method, const char* params);
int vizero_lsp_client_send_notification(vizero_lsp_client_t* client, const char* method, const char* params);
int vizero_lsp_client_process_messages(vizero_lsp_client_t* client);

/* Callbacks */
void vizero_lsp_client_set_callbacks(vizero_lsp_client_t* client, const lsp_client_callbacks_t* callbacks, void* user_data);

/* Utility functions */
char* vizero_lsp_client_escape_json_string(const char* str);
void vizero_lsp_client_free_string(char* str);

#ifdef __cplusplus
}
#endif

#endif /* VIZERO_LSP_CLIENT_H */