#include "vizero/lsp_client.h"
#include <stdlib.h>
#include <string.h>
#include <stdio.h>
#include <errno.h>
#include <stdarg.h>
#include "vizero/log.h"

#ifdef _WIN32
#include <windows.h>
#include <process.h>
#define snprintf _snprintf
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
#include <sys/wait.h>
#include <signal.h>
#include <stdarg.h>
#endif

/* LSP client structure */
struct vizero_lsp_client_t {
    char* server_path;
    char* working_directory;
    bool is_running;
    int next_request_id;
    
    /* Process handles */
#ifdef _WIN32
    HANDLE process_handle;
    HANDLE stdin_write;
    HANDLE stdout_read;
    HANDLE stderr_read;
#else
    pid_t process_id;
    int stdin_fd;
    int stdout_fd;
    int stderr_fd;
#endif
    
    /* Callbacks */
    lsp_client_callbacks_t callbacks;
    void* user_data;
    
    /* Message buffer */
    char* read_buffer;
    size_t read_buffer_size;
    size_t read_buffer_used;
};

/* Internal functions */
static int lsp_client_start_process(vizero_lsp_client_t* client);
static void lsp_client_stop_process(vizero_lsp_client_t* client);
static int lsp_client_write_message(vizero_lsp_client_t* client, const char* message);
static int lsp_client_read_messages(vizero_lsp_client_t* client);
static void lsp_client_parse_message(vizero_lsp_client_t* client, const char* content);


vizero_lsp_client_t* vizero_lsp_client_create(const char* server_path, const char* working_directory) {
    if (!server_path) {
        return NULL;
    }
    
    vizero_lsp_client_t* client = (vizero_lsp_client_t*)calloc(1, sizeof(vizero_lsp_client_t));
    if (!client) {
        return NULL;
    }
    
    client->server_path = strdup(server_path);
    if (!client->server_path) {
        free(client);
        return NULL;
    }
    
    if (working_directory) {
        client->working_directory = strdup(working_directory);
        if (!client->working_directory) {
            free(client->server_path);
            free(client);
            return NULL;
        }
    }
    
    client->next_request_id = 1;
    client->read_buffer_size = 32768; /* Start with 32KB to handle large completion responses */
    client->read_buffer = (char*)malloc(client->read_buffer_size);
    if (!client->read_buffer) {
        free(client->working_directory);
        free(client->server_path);
        free(client);
        return NULL;
    }
    
    return client;
}

void vizero_lsp_client_destroy(vizero_lsp_client_t* client) {
    if (!client) {
        return;
    }
    
    if (client->is_running) {
        vizero_lsp_client_stop(client);
    }
    
    free(client->server_path);
    free(client->working_directory);
    free(client->read_buffer);
    free(client);
}

int vizero_lsp_client_start(vizero_lsp_client_t* client) {
    if (!client || client->is_running) {
        return -1;
    }
    
    if (lsp_client_start_process(client) != 0) {
        return -1;
    }
    
    client->is_running = true;
    return 0;
}

void vizero_lsp_client_stop(vizero_lsp_client_t* client) {
    if (!client || !client->is_running) {
        return;
    }
    
    /* Send shutdown request */
    vizero_lsp_client_send_request(client, "shutdown", "null");
    
    /* Send exit notification */
    vizero_lsp_client_send_notification(client, "exit", "null");
    
    /* Stop the process */
    lsp_client_stop_process(client);
    client->is_running = false;
}

bool vizero_lsp_client_is_running(const vizero_lsp_client_t* client) {
    return client && client->is_running;
}

int vizero_lsp_client_send_request(vizero_lsp_client_t* client, const char* method, const char* params) {
    if (!client || !client->is_running || !method) {
        return -1;
    }
    
    int request_id = client->next_request_id++;
    
    /* Build JSON-RPC request */
    char* message;
    int message_len = asprintf(&message,
        "{\"jsonrpc\":\"2.0\",\"id\":%d,\"method\":\"%s\",\"params\":%s}",
        request_id, method, params ? params : "null");
    
    if (message_len < 0 || !message) {
        return -1;
    }
    
    /* Send message */
    int result = lsp_client_write_message(client, message);
    free(message);
    
    return result == 0 ? request_id : -1;
}

int vizero_lsp_client_send_notification(vizero_lsp_client_t* client, const char* method, const char* params) {
    if (!client || !client->is_running || !method) {
        return -1;
    }
    
    /* Build JSON-RPC notification */
    char* message;
    int message_len = asprintf(&message,
        "{\"jsonrpc\":\"2.0\",\"method\":\"%s\",\"params\":%s}",
        method, params ? params : "null");
    
    if (message_len < 0 || !message) {
        return -1;
    }
    
    /* Send message */
    int result = lsp_client_write_message(client, message);
    free(message);
    
    return result;
}

int vizero_lsp_client_process_messages(vizero_lsp_client_t* client) {
    if (!client || !client->is_running) {
        return -1;
    }
    
    return lsp_client_read_messages(client);
}

void vizero_lsp_client_set_callbacks(vizero_lsp_client_t* client, const lsp_client_callbacks_t* callbacks, void* user_data) {
    if (!client) {
        return;
    }
    
    if (callbacks) {
        client->callbacks = *callbacks;
    } else {
        memset(&client->callbacks, 0, sizeof(client->callbacks));
    }
    
    client->user_data = user_data;
}

char* vizero_lsp_client_escape_json_string(const char* str) {
    if (!str) {
        return NULL;
    }
    
    size_t len = strlen(str);
    size_t escaped_len = len * 2 + 1; /* Worst case: every char needs escaping */
    char* escaped = (char*)malloc(escaped_len);
    if (!escaped) {
        return NULL;
    }
    
    const char* src = str;
    char* dst = escaped;
    
    while (*src) {
        switch (*src) {
            case '"':
                *dst++ = '\\';
                *dst++ = '"';
                break;
            case '\\':
                *dst++ = '\\';
                *dst++ = '\\';
                break;
            case '\b':
                *dst++ = '\\';
                *dst++ = 'b';
                break;
            case '\f':
                *dst++ = '\\';
                *dst++ = 'f';
                break;
            case '\n':
                *dst++ = '\\';
                *dst++ = 'n';
                break;
            case '\r':
                *dst++ = '\\';
                *dst++ = 'r';
                break;
            case '\t':
                *dst++ = '\\';
                *dst++ = 't';
                break;
            default:
                *dst++ = *src;
                break;
        }
        src++;
    }
    
    *dst = '\0';
    return escaped;
}

void vizero_lsp_client_free_string(char* str) {
    free(str);
}

/* Platform-specific process management */
#ifdef _WIN32

static int lsp_client_start_process(vizero_lsp_client_t* client) {
    SECURITY_ATTRIBUTES sa = {0};
    sa.nLength = sizeof(SECURITY_ATTRIBUTES);
    sa.bInheritHandle = TRUE;
    sa.lpSecurityDescriptor = NULL;
    
    /* Create pipes for stdin/stdout/stderr */
    HANDLE stdin_read, stdout_write, stderr_write;
    
    if (!CreatePipe(&stdin_read, &client->stdin_write, &sa, 0) ||
        !CreatePipe(&client->stdout_read, &stdout_write, &sa, 0) ||
        !CreatePipe(&client->stderr_read, &stderr_write, &sa, 0)) {
        return -1;
    }
    
    /* Make sure the parent ends of pipes are not inherited */
    SetHandleInformation(client->stdin_write, HANDLE_FLAG_INHERIT, 0);
    SetHandleInformation(client->stdout_read, HANDLE_FLAG_INHERIT, 0);
    SetHandleInformation(client->stderr_read, HANDLE_FLAG_INHERIT, 0);
    
    /* Set up process info */
    STARTUPINFOA si = {0};
    si.cb = sizeof(STARTUPINFOA);
    si.hStdInput = stdin_read;
    si.hStdOutput = stdout_write;
    si.hStdError = stderr_write;
    si.dwFlags |= STARTF_USESTDHANDLES;
    
    PROCESS_INFORMATION pi = {0};
    
    /* Create the process */
    char* command_line = strdup(client->server_path);
    BOOL success = CreateProcessA(
        NULL,                           /* Application name */
        command_line,                   /* Command line */
        NULL,                           /* Process attributes */
        NULL,                           /* Thread attributes */
        TRUE,                           /* Inherit handles */
        CREATE_NO_WINDOW,               /* Creation flags */
        NULL,                           /* Environment */
        client->working_directory,      /* Current directory */
        &si,                            /* Startup info */
        &pi                             /* Process info */
    );
    
    free(command_line);
    
    /* Clean up handles we don't need */
    CloseHandle(stdin_read);
    CloseHandle(stdout_write);
    CloseHandle(stderr_write);
    
    if (!success) {
        CloseHandle(client->stdin_write);
        CloseHandle(client->stdout_read);
        CloseHandle(client->stderr_read);
        return -1;
    }
    
    client->process_handle = pi.hProcess;
    CloseHandle(pi.hThread); /* Don't need thread handle */
    
    return 0;
}

static void lsp_client_stop_process(vizero_lsp_client_t* client) {
    if (client->process_handle) {
        TerminateProcess(client->process_handle, 0);
        WaitForSingleObject(client->process_handle, 5000); /* Wait up to 5 seconds */
        CloseHandle(client->process_handle);
        client->process_handle = NULL;
    }
    
    if (client->stdin_write) {
        CloseHandle(client->stdin_write);
        client->stdin_write = NULL;
    }
    
    if (client->stdout_read) {
        CloseHandle(client->stdout_read);
        client->stdout_read = NULL;
    }
    
    if (client->stderr_read) {
        CloseHandle(client->stderr_read);
        client->stderr_read = NULL;
    }
}

static int lsp_client_write_message(vizero_lsp_client_t* client, const char* message) {
    if (!client->stdin_write) {
        return -1;
    }
    
    /* LSP uses Content-Length header */
    char* full_message;
    int full_len = asprintf(&full_message, "Content-Length: %zu\r\n\r\n%s", strlen(message), message);
    if (full_len < 0 || !full_message) {
        return -1;
    }
    
    DWORD bytes_written;
    BOOL success = WriteFile(client->stdin_write, full_message, (DWORD)full_len, &bytes_written, NULL);
    free(full_message);
    
    return success && bytes_written == (DWORD)full_len ? 0 : -1;
}

static int lsp_client_read_messages(vizero_lsp_client_t* client) {
    if (!client || !client->stdout_read) {
        return -1;
    }
    
    DWORD bytes_available = 0;
    if (!PeekNamedPipe(client->stdout_read, NULL, 0, NULL, &bytes_available, NULL) || bytes_available == 0) {
        return 0; /* No data available */
    }
    
    /* Limit bytes to read to prevent excessive memory usage */
    if (bytes_available > 64 * 1024) {
        bytes_available = 64 * 1024;
    }
    
    /* Ensure buffer has space with safety checks */
    if (client->read_buffer_used + bytes_available >= client->read_buffer_size) {
        size_t new_size = client->read_buffer_size * 2;
        if (new_size > 10 * 1024 * 1024) { /* Limit to 10MB */
            return -1;
        }
        char* new_buffer = (char*)realloc(client->read_buffer, new_size);
        if (!new_buffer) {
            return -1;
        }
        client->read_buffer = new_buffer;
        client->read_buffer_size = new_size;
    }
    
    DWORD bytes_read;
    if (!ReadFile(client->stdout_read, client->read_buffer + client->read_buffer_used, bytes_available, &bytes_read, NULL)) {
        return -1;
    }
    
    client->read_buffer_used += bytes_read;
    if (client->read_buffer_used >= client->read_buffer_size) {
        return -1; /* Buffer overflow protection */
    }
    client->read_buffer[client->read_buffer_used] = '\0';
    
    /* Process complete messages with strict loop protection */
    char* buffer_start = client->read_buffer;
    int message_count = 0;
    const int MAX_MESSAGES_PER_CALL = 5; /* Allow more messages but still prevent infinite loops */
    size_t previous_buffer_used = client->read_buffer_used;
    
    while (client->read_buffer_used > 10 && message_count < MAX_MESSAGES_PER_CALL) {
        /* Infinite loop detection: if buffer size didn't change, break */
        if (message_count > 0 && client->read_buffer_used == previous_buffer_used) {
            VIZERO_ERR("[LSP] Detected potential infinite loop, breaking");
            break;
        }
        previous_buffer_used = client->read_buffer_used;
        
        /* Look for Content-Length header */
        char* content_length_start = strstr(buffer_start, "Content-Length: ");
        if (!content_length_start || content_length_start >= client->read_buffer + client->read_buffer_used) {
            VIZERO_ERR("[LSP] No Content-Length header found, breaking");
            break;
        }
        
        /* Safety: ensure we have enough bytes for length parsing */
        if (content_length_start + 16 >= client->read_buffer + client->read_buffer_used) {
            VIZERO_ERR("[LSP] Incomplete Content-Length header, breaking");
            break;
        }
        
        int content_length = atoi(content_length_start + 16);
        if (content_length <= 0 || content_length > 512 * 1024) { /* Max 512KB per message */
            VIZERO_ERR("[LSP] Invalid content length %d, breaking", content_length);
            break;
        }
        
        /* Find start of content (after \r\n\r\n) */
        char* content_start = strstr(content_length_start, "\r\n\r\n");
        if (!content_start || content_start >= client->read_buffer + client->read_buffer_used) {
            VIZERO_ERR("[LSP] No message separator found, breaking");
            break;
        }
        content_start += 4;
        
        /* Safety: ensure content_start is within bounds */
        if (content_start >= client->read_buffer + client->read_buffer_used) {
            VIZERO_ERR("[LSP] Content start beyond buffer, breaking");
            break;
        }
        
        /* Check if we have the complete message */
        size_t header_len = content_start - buffer_start;
        if (client->read_buffer_used < header_len + content_length) {
         VIZERO_DBG("[LSP] Incomplete message (%zu < %zu + %d), breaking", 
             client->read_buffer_used, header_len, content_length);
            break; /* Incomplete message */
        }
        
        /* Processing LSP message */
        
        /* Extract and process the message safely */
        char* message_content = (char*)malloc(content_length + 1);
        if (message_content) {
            memcpy(message_content, content_start, content_length);
            message_content[content_length] = '\0';
            lsp_client_parse_message(client, message_content);
            free(message_content);
        } else {
            VIZERO_ERR("[LSP] Failed to allocate message buffer");
            break;
        }
        
        /* Move remaining data to start of buffer */
        size_t message_total_len = header_len + content_length;
        if (message_total_len > client->read_buffer_used || message_total_len == 0) {
            break; /* Safety check */
        }
        
        client->read_buffer_used -= message_total_len;
        if (client->read_buffer_used > 0) {
            memmove(client->read_buffer, buffer_start + message_total_len, client->read_buffer_used);
        }
        client->read_buffer[client->read_buffer_used] = '\0';
        buffer_start = client->read_buffer;
        message_count++;
        
        VIZERO_DBG("[LSP] Processed message %d, %zu bytes remaining", message_count, client->read_buffer_used);
    }
    
    if (message_count >= MAX_MESSAGES_PER_CALL) {
        VIZERO_INFO("[LSP] Hit message processing limit (%d), deferring remaining messages", MAX_MESSAGES_PER_CALL);
    }
    
    return 0;
}

#else /* Unix implementation */

static int lsp_client_start_process(vizero_lsp_client_t* client) {
    int stdin_pipe[2], stdout_pipe[2], stderr_pipe[2];
    
    /* Create pipes */
    if (pipe(stdin_pipe) != 0 || pipe(stdout_pipe) != 0 || pipe(stderr_pipe) != 0) {
        return -1;
    }
    
    client->process_id = fork();
    if (client->process_id == -1) {
        /* Fork failed */
        close(stdin_pipe[0]); close(stdin_pipe[1]);
        close(stdout_pipe[0]); close(stdout_pipe[1]);
        close(stderr_pipe[0]); close(stderr_pipe[1]);
        return -1;
    }
    
    if (client->process_id == 0) {
        /* Child process */
        
        /* Set up standard streams */
        dup2(stdin_pipe[0], STDIN_FILENO);
        dup2(stdout_pipe[1], STDOUT_FILENO);
        dup2(stderr_pipe[1], STDERR_FILENO);
        
        /* Close unused pipe ends */
        close(stdin_pipe[1]);
        close(stdout_pipe[0]);
        close(stderr_pipe[0]);
        
        /* Change to working directory if specified */
        if (client->working_directory) {
            chdir(client->working_directory);
        }
        
        /* Execute the server */
        execl("/bin/sh", "sh", "-c", client->server_path, (char*)NULL);
        _exit(1); /* If exec fails */
    }
    
    /* Parent process */
    client->stdin_fd = stdin_pipe[1];
    client->stdout_fd = stdout_pipe[0];
    client->stderr_fd = stderr_pipe[0];
    
    /* Close unused pipe ends */
    close(stdin_pipe[0]);
    close(stdout_pipe[1]);
    close(stderr_pipe[1]);
    
    return 0;
}

static void lsp_client_stop_process(vizero_lsp_client_t* client) {
    if (client->process_id > 0) {
        kill(client->process_id, SIGTERM);
        waitpid(client->process_id, NULL, 0);
        client->process_id = 0;
    }
    
    if (client->stdin_fd >= 0) {
        close(client->stdin_fd);
        client->stdin_fd = -1;
    }
    
    if (client->stdout_fd >= 0) {
        close(client->stdout_fd);
        client->stdout_fd = -1;
    }
    
    if (client->stderr_fd >= 0) {
        close(client->stderr_fd);
        client->stderr_fd = -1;
    }
}

static int lsp_client_write_message(vizero_lsp_client_t* client, const char* message) {
    if (client->stdin_fd < 0) {
        return -1;
    }
    
    /* LSP uses Content-Length header */
    char* full_message;
    int full_len = asprintf(&full_message, "Content-Length: %zu\r\n\r\n%s", strlen(message), message);
    if (full_len < 0 || !full_message) {
        return -1;
    }
    
    ssize_t bytes_written = write(client->stdin_fd, full_message, full_len);
    free(full_message);
    
    return bytes_written == full_len ? 0 : -1;
}

static int lsp_client_read_messages(vizero_lsp_client_t* client) {
    if (client->stdout_fd < 0) {
        return -1;
    }
    
    /* Try to read available data */
    ssize_t bytes_read = read(client->stdout_fd, 
                              client->read_buffer + client->read_buffer_used, 
                              client->read_buffer_size - client->read_buffer_used - 1);
    
    if (bytes_read <= 0) {
        return bytes_read; /* 0 = no data, -1 = error */
    }
    
    client->read_buffer_used += bytes_read;
    client->read_buffer[client->read_buffer_used] = '\0';
    
    /* Process complete messages (same logic as Windows) */
    char* buffer_start = client->read_buffer;
    while (client->read_buffer_used > 0) {
        char* content_length_start = strstr(buffer_start, "Content-Length: ");
        if (!content_length_start) {
            break;
        }
        
        int content_length = atoi(content_length_start + 16);
        if (content_length <= 0) {
            break;
        }
        
        char* content_start = strstr(content_length_start, "\r\n\r\n");
        if (!content_start) {
            break;
        }
        content_start += 4;
        
        size_t header_len = content_start - buffer_start;
        if (client->read_buffer_used < header_len + content_length) {
            break;
        }
        
        char* message_content = (char*)malloc(content_length + 1);
        if (message_content) {
            memcpy(message_content, content_start, content_length);
            message_content[content_length] = '\0';
            lsp_client_parse_message(client, message_content);
            free(message_content);
        }
        
        size_t message_total_len = header_len + content_length;
        client->read_buffer_used -= message_total_len;
        if (client->read_buffer_used > 0) {
            memmove(client->read_buffer, buffer_start + message_total_len, client->read_buffer_used);
        }
        client->read_buffer[client->read_buffer_used] = '\0';
        buffer_start = client->read_buffer;
    }
    
    return 0;
}

#endif

/* Safe JSON field extraction */
static char* safe_extract_json_string(const char* json, const char* field_name, size_t json_len) {
    if (!json || !field_name || json_len == 0) {
        return NULL;
    }
    
    /* Build search pattern: "fieldname": */
    size_t pattern_len = strlen(field_name) + 4; /* "": */
    char* pattern = (char*)malloc(pattern_len + 1);
    if (!pattern) return NULL;
    
    snprintf(pattern, pattern_len + 1, "\"%s\":", field_name);
    
    const char* field_start = strstr(json, pattern);
    free(pattern);
    
    if (!field_start || field_start >= json + json_len) {
        return NULL;
    }
    
    /* Skip to value part */
    const char* value_start = field_start + strlen(field_name) + 3;
    
    /* Skip whitespace */
    while (value_start < json + json_len && (*value_start == ' ' || *value_start == '\t' || *value_start == '\n' || *value_start == '\r')) {
        value_start++;
    }
    
    if (value_start >= json + json_len) {
        return NULL;
    }
    
    /* Handle string values */
    if (*value_start == '"') {
        value_start++; /* Skip opening quote */
        const char* value_end = value_start;
        
        /* Find closing quote, handling escaped quotes */
        while (value_end < json + json_len && *value_end != '"') {
            if (*value_end == '\\' && value_end + 1 < json + json_len) {
                value_end += 2; /* Skip escaped character */
            } else {
                value_end++;
            }
        }
        
        if (value_end >= json + json_len || *value_end != '"') {
            return NULL; /* No closing quote found */
        }
        
        size_t value_len = value_end - value_start;
        if (value_len == 0 || value_len > 10000) { /* Reasonable limit */
            return NULL;
        }
        
        char* result = (char*)malloc(value_len + 1);
        if (!result) return NULL;
        
        memcpy(result, value_start, value_len);
        result[value_len] = '\0';
        return result;
    }
    
    return NULL;
}

static int safe_extract_json_int(const char* json, const char* field_name, size_t json_len) {
    if (!json || !field_name || json_len == 0) {
        return -1;
    }
    
    /* Build search pattern */
    size_t pattern_len = strlen(field_name) + 4;
    char* pattern = (char*)malloc(pattern_len + 1);
    if (!pattern) return -1;
    
    snprintf(pattern, pattern_len + 1, "\"%s\":", field_name);
    
    const char* field_start = strstr(json, pattern);
    free(pattern);
    
    if (!field_start || field_start >= json + json_len) {
        return -1;
    }
    
    /* Skip to value part */
    const char* value_start = field_start + strlen(field_name) + 3;
    
    /* Skip whitespace */
    while (value_start < json + json_len && (*value_start == ' ' || *value_start == '\t' || *value_start == '\n' || *value_start == '\r')) {
        value_start++;
    }
    
    if (value_start >= json + json_len) {
        return -1;
    }
    
    /* Parse integer */
    if (*value_start >= '0' && *value_start <= '9') {
        return atoi(value_start);
    }
    
    return -1;
}

/* Message parsing (platform-independent) with robust JSON handling */
static void lsp_client_parse_message(vizero_lsp_client_t* client, const char* content) {
    if (!client || !content) {
        return;
    }
    
    size_t content_len = strlen(content);
    if (content_len == 0 || content_len > 1024 * 1024) {
        return;
    }
    
    /* Parsing JSON message */
    
    /* Check if it's a response (has "id" field) */
    int id = safe_extract_json_int(content, "id", content_len);
    if (id >= 0) {
        /* Extract result and error fields safely */
        char* result_str = safe_extract_json_string(content, "result", content_len);
        char* error_str = safe_extract_json_string(content, "error", content_len);
        
        if (client->callbacks.on_response) {
            /* Pass the entire content as result since we need the full JSON structure */
            client->callbacks.on_response(id, content, error_str, client->user_data);
        }
        
        if (result_str) free(result_str);
        if (error_str) free(error_str);
        
    } else {
        /* Extract method name safely */
        char* method = safe_extract_json_string(content, "method", content_len);
        if (method) {
            if (client->callbacks.on_notification) {
                client->callbacks.on_notification(method, content, client->user_data);
            }
            
            free(method);
        }
    }
    
    /* Message parsing complete */
}

