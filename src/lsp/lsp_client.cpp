#include "vizero/lsp_client.h"
#include <stdlib.h>
#include <string.h>
#include <stdio.h>
#include <errno.h>
#include <stdarg.h>

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
static lsp_message_t* lsp_message_create(void);
static void lsp_message_destroy(lsp_message_t* message);

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
    client->read_buffer_size = 4096;
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
    BOOL success = WriteFile(client->stdin_write, full_message, full_len, &bytes_written, NULL);
    free(full_message);
    
    return success && bytes_written == full_len ? 0 : -1;
}

static int lsp_client_read_messages(vizero_lsp_client_t* client) {
    if (!client->stdout_read) {
        return -1;
    }
    
    DWORD bytes_available = 0;
    if (!PeekNamedPipe(client->stdout_read, NULL, 0, NULL, &bytes_available, NULL) || bytes_available == 0) {
        return 0; /* No data available */
    }
    
    /* Ensure buffer has space */
    if (client->read_buffer_used + bytes_available >= client->read_buffer_size) {
        size_t new_size = client->read_buffer_size * 2;
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
    client->read_buffer[client->read_buffer_used] = '\0';
    
    /* Process complete messages */
    char* buffer_start = client->read_buffer;
    while (client->read_buffer_used > 0) {
        /* Look for Content-Length header */
        char* content_length_start = strstr(buffer_start, "Content-Length: ");
        if (!content_length_start) {
            break;
        }
        
        int content_length = atoi(content_length_start + 16);
        if (content_length <= 0) {
            break;
        }
        
        /* Find start of content (after \r\n\r\n) */
        char* content_start = strstr(content_length_start, "\r\n\r\n");
        if (!content_start) {
            break;
        }
        content_start += 4;
        
        /* Check if we have the complete message */
        size_t header_len = content_start - buffer_start;
        if (client->read_buffer_used < header_len + content_length) {
            break; /* Incomplete message */
        }
        
        /* Extract and process the message */
        char* message_content = (char*)malloc(content_length + 1);
        if (message_content) {
            memcpy(message_content, content_start, content_length);
            message_content[content_length] = '\0';
            lsp_client_parse_message(client, message_content);
            free(message_content);
        }
        
        /* Move remaining data to start of buffer */
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

/* Message parsing (platform-independent) */
static void lsp_client_parse_message(vizero_lsp_client_t* client, const char* content) {
    /* This is a simplified JSON parser - in a real implementation,
     * you might want to use a proper JSON library like cJSON */
    
    /* Check if it's a response (has "id" field) */
    if (strstr(content, "\"id\":")) {
        /* Parse response */
        int id = -1;
        const char* id_start = strstr(content, "\"id\":");
        if (id_start) {
            id = atoi(id_start + 5);
        }
        
        const char* result = strstr(content, "\"result\":");
        const char* error = strstr(content, "\"error\":");
        
        if (client->callbacks.on_response) {
            client->callbacks.on_response(id, 
                                        result ? result + 9 : NULL,
                                        error ? error + 8 : NULL,
                                        client->user_data);
        }
    } else {
        /* Parse notification */
        const char* method_start = strstr(content, "\"method\":\"");
        if (method_start) {
            method_start += 10;
            const char* method_end = strchr(method_start, '"');
            if (method_end) {
                size_t method_len = method_end - method_start;
                char* method = (char*)malloc(method_len + 1);
                if (method) {
                    memcpy(method, method_start, method_len);
                    method[method_len] = '\0';
                    
                    const char* params = strstr(content, "\"params\":");
                    
                    if (client->callbacks.on_notification) {
                        client->callbacks.on_notification(method,
                                                         params ? params + 9 : NULL,
                                                         client->user_data);
                    }
                    
                    free(method);
                }
            }
        }
    }
}

static lsp_message_t* lsp_message_create(void) {
    return (lsp_message_t*)calloc(1, sizeof(lsp_message_t));
}

static void lsp_message_destroy(lsp_message_t* message) {
    if (!message) {
        return;
    }
    
    free(message->method);
    free(message->params);
    free(message->result);
    free(message->error);
    free(message);
}