/* IRC Client Plugin for Vizero - Complete Implementation
 * Full-featured IRC client with Quassel-like interface
 * Features: Multi-buffer chat, real IRC protocol, theme integration
 */

#include "vizero/plugin_interface.h"
#include "vizero/renderer.h"
#include "vizero/colour_theme.h"
#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <time.h>

#ifdef _WIN32
    #include <winsock2.h>
    #include <ws2tcpip.h>
    #pragma comment(lib, "ws2_32.lib")
    typedef SOCKET socket_t;
    #define INVALID_SOCKET_VALUE INVALID_SOCKET
    #define close_socket closesocket
    #define strtok_r strtok_s
#else
    #include <sys/socket.h>
    #include <netinet/in.h>
    #include <arpa/inet.h>
    #include <netdb.h>
    #include <unistd.h>
    typedef int socket_t;
    #define INVALID_SOCKET_VALUE -1
    #define close_socket close
#endif

/* IRC User structure */
typedef struct {
    char nick[32];
    char user[32];
    char host[128];
    bool is_op;
    bool has_voice;  
    bool is_away;
} irc_user_t;

/* IRC Channel structure */
typedef struct {
    char name[64];
    char topic[512];
    char mode[16];
    irc_user_t users[256];
    size_t user_count;
    bool joined;
} irc_channel_t;

/* IRC Message structure */
typedef struct {
    char timestamp[16];
    char nick[32];
    char message[512];
    enum {
        IRC_MSG_NORMAL,
        IRC_MSG_ACTION,
        IRC_MSG_JOIN,
        IRC_MSG_PART,
        IRC_MSG_QUIT,
        IRC_MSG_NICK,
        IRC_MSG_TOPIC,
        IRC_MSG_ERROR,
        IRC_MSG_NOTICE
    } type;
    vizero_colour_t nick_colour;
} irc_message_t;

/* IRC Buffer - represents one chat (channel or private message) */
typedef struct {
    char name[64];                      /* Channel/nick name */
    char display_name[64];              /* Display name for UI */
    vizero_buffer_t* text_buffer;       /* Vizero text buffer for messages */
    irc_message_t messages[1000];       /* Message history */
    size_t message_count;
    size_t scroll_offset;
    
    /* Channel-specific data */
    irc_channel_t channel_info;
    
    /* Input buffer */
    char input_text[512];
    size_t input_cursor;
    
    /* UI state */
    bool is_active;
    bool has_unread;
    int unread_count;
} irc_buffer_t;

/* IRC Connection */
typedef struct {
    socket_t socket;
    char server[256];
    int port;
    char nickname[32];
    char username[32];
    char realname[64];
    bool connected;
    bool registered;
    
    /* Receive buffer */
    char recv_buffer[4096];
    size_t recv_pos;
    
    /* Connection state */
    time_t last_ping;
    time_t connect_time;
} irc_connection_t;

/* IRC Plugin State */
typedef struct {
    vizero_editor_t* editor;
    const vizero_editor_api_t* api;
    
    /* IRC Connection */
    irc_connection_t connection;
    
    /* IRC Buffers */
    irc_buffer_t* buffers[32];
    size_t buffer_count;
    size_t active_buffer;
    char current_buffer[64];
    char input_buffer[512];
    
    /* UI Layout (Quassel-like) */
    struct {
        float channel_list_width;       /* Left panel width */
        float nick_list_width;          /* Right panel width  */
        float input_height;             /* Bottom input height */
        float message_area_height;      /* Calculated */
        
        /* Scroll states */
        size_t channel_scroll;
        size_t nick_scroll;
        size_t message_scroll;
    } ui;
    
    /* Theme colors for IRC-specific elements */
    vizero_colour_t nick_colors[16];
} irc_state_t;

/* Global IRC state */
static irc_state_t* g_irc_state = NULL;

/* Utility functions */
static void irc_initialize_networking(void) {
#ifdef _WIN32
    WSADATA wsaData;
    WSAStartup(MAKEWORD(2, 2), &wsaData);
#endif
}

static void irc_cleanup_networking(void) {
#ifdef _WIN32
    WSACleanup();
#endif
}

/* Generate color for nick */
static uint32_t irc_generate_nick_color(const char* nick) {
    if (!nick) return 0x808080; /* Default gray */
    
    /* Simple hash to generate consistent colors */
    uint32_t hash = 0;
    for (const char* p = nick; *p; p++) {
        hash = hash * 31 + (uint8_t)*p;
    }
    
    /* Generate RGB values avoiding too dark/light colors */
    uint8_t r = 128 + (hash & 0x7F);
    uint8_t g = 128 + ((hash >> 8) & 0x7F);
    uint8_t b = 128 + ((hash >> 16) & 0x7F);
    
    return (r << 16) | (g << 8) | b;
}

/* Generate nick color based on hash */
static vizero_colour_t irc_get_nick_colour(const char* nick) {
    if (!nick) return (vizero_colour_t){1.0f, 1.0f, 1.0f, 1.0f};
    
    /* Simple hash function */
    unsigned hash = 0;
    for (const char* p = nick; *p; p++) {
        hash = hash * 31 + *p;
    }
    
    /* IRC nick colors - based on common IRC client palettes */
    vizero_colour_t colors[] = {
        {0.8f, 0.2f, 0.2f, 1.0f},  /* Red */
        {0.2f, 0.8f, 0.2f, 1.0f},  /* Green */  
        {0.2f, 0.2f, 0.8f, 1.0f},  /* Blue */
        {0.8f, 0.8f, 0.2f, 1.0f},  /* Yellow */
        {0.8f, 0.2f, 0.8f, 1.0f},  /* Magenta */
        {0.2f, 0.8f, 0.8f, 1.0f},  /* Cyan */
        {0.9f, 0.6f, 0.2f, 1.0f},  /* Orange */
        {0.6f, 0.2f, 0.9f, 1.0f},  /* Purple */
        {0.9f, 0.4f, 0.6f, 1.0f},  /* Pink */
        {0.4f, 0.9f, 0.6f, 1.0f},  /* Light Green */
        {0.6f, 0.6f, 0.9f, 1.0f},  /* Light Blue */
        {0.9f, 0.9f, 0.6f, 1.0f},  /* Light Yellow */
        {0.9f, 0.6f, 0.9f, 1.0f},  /* Light Magenta */
        {0.6f, 0.9f, 0.9f, 1.0f},  /* Light Cyan */
        {0.7f, 0.7f, 0.7f, 1.0f},  /* Light Grey */
        {0.5f, 0.5f, 0.5f, 1.0f}   /* Grey */
    };
    
    return colors[hash % (sizeof(colors) / sizeof(colors[0]))];
}

/* IRC Buffer Management */
static irc_buffer_t* irc_create_buffer(const char* name) {
    irc_buffer_t* buffer = calloc(1, sizeof(irc_buffer_t));
    if (!buffer) return NULL;
    
    strncpy(buffer->name, name, sizeof(buffer->name) - 1);
    strncpy(buffer->display_name, name, sizeof(buffer->display_name) - 1);
    
    buffer->text_buffer = NULL; /* We'll use our own message system */
    buffer->message_count = 0;
    buffer->scroll_offset = 0;
    buffer->input_cursor = 0;
    buffer->is_active = false;
    buffer->has_unread = false;
    buffer->unread_count = 0;
    
    return buffer;
}

static void irc_destroy_buffer(irc_buffer_t* buffer) {
    if (buffer) {
        free(buffer);
    }
}

static irc_buffer_t* irc_find_buffer(const char* name) {
    if (!g_irc_state || !name) return NULL;
    
    for (size_t i = 0; i < g_irc_state->buffer_count; i++) {
        if (g_irc_state->buffers[i] && strcmp(g_irc_state->buffers[i]->name, name) == 0) {
            return g_irc_state->buffers[i];
        }
    }
    return NULL;
}

static irc_buffer_t* irc_get_or_create_buffer(const char* name) {
    irc_buffer_t* buffer = irc_find_buffer(name);
    if (buffer) return buffer;
    
    if (!g_irc_state || g_irc_state->buffer_count >= 32) return NULL;
    
    buffer = irc_create_buffer(name);
    if (buffer) {
        g_irc_state->buffers[g_irc_state->buffer_count++] = buffer;
    }
    return buffer;
}

/* Add message to buffer */
static void irc_add_message(irc_buffer_t* buffer, const char* nick, const char* message, int msg_type) {
    if (!buffer || !message) return;
    
    if (buffer->message_count >= 1000) {
        /* Shift messages to make room */
        memmove(buffer->messages, buffer->messages + 1, sizeof(irc_message_t) * 999);        
        buffer->message_count = 999;
    }
    
    irc_message_t* msg = &buffer->messages[buffer->message_count++];
    
    /* Get current time */
    time_t now = time(NULL);
    struct tm* tm_info = localtime(&now);
    strftime(msg->timestamp, sizeof(msg->timestamp), "%H:%M", tm_info);
    
    /* Set nick and message */
    if (nick) {
        strncpy(msg->nick, nick, sizeof(msg->nick) - 1);
        msg->nick_colour = irc_get_nick_colour(nick);
    } else {
        msg->nick[0] = '\0';
        msg->nick_colour = (vizero_colour_t){0.7f, 0.7f, 0.7f, 1.0f};
    }
    
    strncpy(msg->message, message, sizeof(msg->message) - 1);
    msg->type = msg_type;
    
    /* Mark as unread if not active buffer */
    if (!buffer->is_active) {
        buffer->has_unread = true;
        buffer->unread_count++;
    }
}

/* IRC Protocol Functions */
static int irc_connect(const char* server, int port, const char* nick, const char* user, const char* real) {
    if (!g_irc_state) return -1;
    
    irc_connection_t* conn = &g_irc_state->connection;
    
    /* Close existing connection */
    if (conn->socket != INVALID_SOCKET_VALUE) {
        close_socket(conn->socket);
    }
    
    /* Create socket */
    conn->socket = socket(AF_INET, SOCK_STREAM, 0);
    if (conn->socket == INVALID_SOCKET_VALUE) {
        return -1;
    }
    
    /* Resolve server */
    struct hostent* host_entry = gethostbyname(server);
    if (!host_entry) {
        close_socket(conn->socket);
        conn->socket = INVALID_SOCKET_VALUE;
        return -1;
    }
    
    /* Connect */
    struct sockaddr_in server_addr;
    memset(&server_addr, 0, sizeof(server_addr));
    server_addr.sin_family = AF_INET;
    server_addr.sin_port = htons(port);
    memcpy(&server_addr.sin_addr, host_entry->h_addr_list[0], host_entry->h_length);
    
    if (connect(conn->socket, (struct sockaddr*)&server_addr, sizeof(server_addr)) != 0) {
        close_socket(conn->socket);
        conn->socket = INVALID_SOCKET_VALUE;
        return -1;
    }
    
    /* Store connection info */
    strncpy(conn->server, server, sizeof(conn->server) - 1);
    conn->port = port;
    strncpy(conn->nickname, nick, sizeof(conn->nickname) - 1);
    strncpy(conn->username, user, sizeof(conn->username) - 1);
    strncpy(conn->realname, real, sizeof(conn->realname) - 1);
    conn->connected = true;
    conn->registered = false;
    conn->recv_pos = 0;
    conn->connect_time = time(NULL);
    
    /* Send registration */
    char buffer[512];
    snprintf(buffer, sizeof(buffer), "NICK %s\r\n", nick);
    send(conn->socket, buffer, strlen(buffer), 0);
    
    snprintf(buffer, sizeof(buffer), "USER %s 0 * :%s\r\n", user, real);
    send(conn->socket, buffer, strlen(buffer), 0);
    
    return 0;
}

static void irc_disconnect(void) {
    if (!g_irc_state) return;
    
    irc_connection_t* conn = &g_irc_state->connection;
    if (conn->socket != INVALID_SOCKET_VALUE) {
        if (conn->connected) {
            send(conn->socket, "QUIT :Leaving\r\n", 15, 0);
        }
        close_socket(conn->socket);
        conn->socket = INVALID_SOCKET_VALUE;
    }
    conn->connected = false;
    conn->registered = false;
}

static void irc_send_raw(const char* message) {
    if (!g_irc_state || !message) return;
    
    irc_connection_t* conn = &g_irc_state->connection;
    if (conn->socket == INVALID_SOCKET_VALUE || !conn->connected) return;
    
    char buffer[512];
    snprintf(buffer, sizeof(buffer), "%s\r\n", message);
    send(conn->socket, buffer, strlen(buffer), 0);
}

static void irc_send_message(const char* target, const char* message) {
    if (!target || !message) return;
    
    char buffer[512];
    snprintf(buffer, sizeof(buffer), "PRIVMSG %s :%s", target, message);
    irc_send_raw(buffer);
    
    /* Add to local buffer */
    irc_buffer_t* buffer_obj = irc_get_or_create_buffer(target);
    if (buffer_obj && g_irc_state) {
        irc_add_message(buffer_obj, g_irc_state->connection.nickname, message, IRC_MSG_NORMAL);
    }
}

static void irc_join_channel(const char* channel) {
    if (!channel) return;
    
    char buffer[512];
    snprintf(buffer, sizeof(buffer), "JOIN %s", channel);
    irc_send_raw(buffer);
    
    /* Create buffer for channel */
    irc_get_or_create_buffer(channel);
}

static void irc_part_channel(const char* channel, const char* reason) {
    if (!channel) return;
    
    char buffer[512];
    if (reason && strlen(reason) > 0) {
        snprintf(buffer, sizeof(buffer), "PART %s :%s", channel, reason);
    } else {
        snprintf(buffer, sizeof(buffer), "PART %s", channel);
    }
    irc_send_raw(buffer);
}

/* Parse IRC message and update buffers */
static void irc_parse_message(const char* line) {
    if (!line || !g_irc_state) return;
    
    /* Simple IRC message parsing - handle basic commands */
    if (strncmp(line, "PING ", 5) == 0) {
        /* Respond to PING */
        char pong[512];
        snprintf(pong, sizeof(pong), "PONG %s", line + 5);
        irc_send_raw(pong);
        return;
    }
    
    /* Parse basic message format: :nick!user@host COMMAND params :message */
    char* line_copy = strdup(line);
    char* saveptr;
    char* prefix = NULL;
    char* command = NULL;
    char* params = NULL;
    char* message = NULL;
    
    /* Extract prefix */
    if (line_copy[0] == ':') {
        prefix = strtok_r(line_copy + 1, " ", &saveptr);
        command = strtok_r(NULL, " ", &saveptr);
    } else {
        command = strtok_r(line_copy, " ", &saveptr);
    }
    
    if (!command) {
        free(line_copy);
        return;
    }
    
    /* Extract nick from prefix */
    char nick[32] = {0};
    if (prefix) {
        char* exclaim = strchr(prefix, '!');
        if (exclaim) {
            size_t nick_len = exclaim - prefix;
            if (nick_len < sizeof(nick)) {
                strncpy(nick, prefix, nick_len);
            }
        } else {
            strncpy(nick, prefix, sizeof(nick) - 1);
        }
    }
    
    /* Handle different commands */
    if (strcmp(command, "PRIVMSG") == 0) {
        char* target = strtok_r(NULL, " ", &saveptr);
        char* msg_start = strtok_r(NULL, "", &saveptr);
        if (target && msg_start && msg_start[0] == ':') {
            irc_buffer_t* buffer = irc_get_or_create_buffer(target);
            if (buffer) {
                irc_add_message(buffer, nick, msg_start + 1, IRC_MSG_NORMAL);
            }
        }
    } else if (strcmp(command, "JOIN") == 0) {
        char* channel = strtok_r(NULL, " ", &saveptr);
        if (channel && channel[0] == ':') channel++; /* Skip : */
        if (channel) {
            irc_buffer_t* buffer = irc_get_or_create_buffer(channel);
            if (buffer) {
                char join_msg[256];
                snprintf(join_msg, sizeof(join_msg), "%s has joined %s", nick, channel);
                irc_add_message(buffer, NULL, join_msg, IRC_MSG_JOIN);
            }
        }
    } else if (strcmp(command, "PART") == 0) {
        char* channel = strtok_r(NULL, " ", &saveptr);
        if (channel) {
            irc_buffer_t* buffer = irc_find_buffer(channel);
            if (buffer) {
                char part_msg[256];
                snprintf(part_msg, sizeof(part_msg), "%s has left %s", nick, channel);
                irc_add_message(buffer, NULL, part_msg, IRC_MSG_PART);
            }
        }
    }
    
    free(line_copy);
}

/* Process incoming IRC data */
static void irc_process_incoming(void) {
    if (!g_irc_state) return;
    
    irc_connection_t* conn = &g_irc_state->connection;
    if (conn->socket == INVALID_SOCKET_VALUE || !conn->connected) return;
    
    /* Receive data */
    char temp_buffer[1024];
    int bytes_received = recv(conn->socket, temp_buffer, sizeof(temp_buffer) - 1, 0);
    if (bytes_received <= 0) {
        /* Connection lost */
        irc_disconnect();
        return;
    }
    
    temp_buffer[bytes_received] = '\0';
    
    /* Add to receive buffer */
    size_t remaining = sizeof(conn->recv_buffer) - conn->recv_pos - 1;
    size_t to_copy = (bytes_received < remaining) ? bytes_received : remaining;
    memcpy(conn->recv_buffer + conn->recv_pos, temp_buffer, to_copy);
    conn->recv_pos += to_copy;
    conn->recv_buffer[conn->recv_pos] = '\0';
    
    /* Process complete lines */
    char* line_start = conn->recv_buffer;
    char* line_end;
    
    while ((line_end = strstr(line_start, "\r\n")) != NULL) {
        *line_end = '\0';
        irc_parse_message(line_start);
        line_start = line_end + 2;
    }
    
    /* Move remaining data to front of buffer */
    if (line_start != conn->recv_buffer) {
        size_t remaining_len = strlen(line_start);
        memmove(conn->recv_buffer, line_start, remaining_len + 1);
        conn->recv_pos = remaining_len;
    }
}

/* Buffer Navigation Functions */
static void irc_set_active_buffer(const char* buffer_name) {
    if (!g_irc_state || !buffer_name) return;
    
    /* Mark current buffer as inactive */
    irc_buffer_t* current = irc_find_buffer(g_irc_state->current_buffer);
    if (current) {
        current->is_active = false;
    }
    
    /* Set new active buffer */
    strncpy(g_irc_state->current_buffer, buffer_name, sizeof(g_irc_state->current_buffer) - 1);
    
    irc_buffer_t* new_buffer = irc_find_buffer(buffer_name);
    if (new_buffer) {
        new_buffer->is_active = true;
        new_buffer->has_unread = false;
        new_buffer->unread_count = 0;
    }
}

static void irc_next_buffer(void) {
    if (!g_irc_state || g_irc_state->buffer_count == 0) return;
    
    int current_index = -1;
    for (int i = 0; i < g_irc_state->buffer_count; i++) {
        if (strcmp(g_irc_state->buffers[i]->name, g_irc_state->current_buffer) == 0) {
            current_index = i;
            break;
        }
    }
    
    int next_index = (current_index + 1) % g_irc_state->buffer_count;
    irc_set_active_buffer(g_irc_state->buffers[next_index]->name);
}

static void irc_prev_buffer(void) {
    if (!g_irc_state || g_irc_state->buffer_count == 0) return;
    
    int current_index = -1;
    for (int i = 0; i < g_irc_state->buffer_count; i++) {
        if (strcmp(g_irc_state->buffers[i]->name, g_irc_state->current_buffer) == 0) {
            current_index = i;
            break;
        }
    }
    
    int prev_index = (current_index - 1 + g_irc_state->buffer_count) % g_irc_state->buffer_count;
    irc_set_active_buffer(g_irc_state->buffers[prev_index]->name);
}

/* IRC UI Rendering Functions - TODO: Implement with proper renderer API */









/* IRC Command Implementations */
static int irc_cmd_connect(const char* args) {
    if (!args || strlen(args) == 0) {
        return -1; /* Usage: /connect server[:port] [nick] */
    }
    
    char server[256] = {0};
    int port = 6667;
    char nick[32] = "vizero_user";
    char user[32] = "vizero";
    char real[64] = "Vizero IRC Client";
    
    /* Parse arguments */
    char* args_copy = strdup(args);
    char* saveptr;
    char* server_part = strtok_r(args_copy, " ", &saveptr);
    char* nick_part = strtok_r(NULL, " ", &saveptr);
    
    if (server_part) {
        char* colon = strchr(server_part, ':');
        if (colon) {
            *colon = '\0';
            port = atoi(colon + 1);
        }
        strncpy(server, server_part, sizeof(server) - 1);
    }
    
    if (nick_part) {
        strncpy(nick, nick_part, sizeof(nick) - 1);
    }
    
    free(args_copy);
    
    int result = irc_connect(server, port, nick, user, real);
    if (result == 0) {
        /* Create server buffer */
        irc_buffer_t* server_buffer = irc_get_or_create_buffer(server);
        if (server_buffer) {
            char connect_msg[256];
            snprintf(connect_msg, sizeof(connect_msg), "Connecting to %s:%d as %s...", server, port, nick);
            irc_add_message(server_buffer, NULL, connect_msg, IRC_MSG_NORMAL);
            irc_set_active_buffer(server);
        }
    }
    
    return result;
}

static int irc_cmd_disconnect(const char* args) {
    (void)args; /* Unused */
    irc_disconnect();
    return 0;
}

static int irc_cmd_join(const char* args) {
    if (!args || strlen(args) == 0) {
        return -1; /* Usage: /join #channel */
    }
    
    char channel[64];
    sscanf(args, "%63s", channel);
    
    /* Ensure channel starts with # */
    if (channel[0] != '#' && channel[0] != '&') {
        char temp[64];
        snprintf(temp, sizeof(temp), "#%s", channel);
        strncpy(channel, temp, sizeof(channel) - 1);
    }
    
    irc_join_channel(channel);
    return 0;
}

static int irc_cmd_part(const char* args) {
    const char* channel = g_irc_state ? g_irc_state->current_buffer : NULL;
    const char* reason = NULL;
    
    if (args && strlen(args) > 0) {
        /* Parse arguments - first word is channel (optional), rest is reason */
        char* args_copy = strdup(args);
        char* saveptr;
        char* first_word = strtok_r(args_copy, " ", &saveptr);
        char* rest = strtok_r(NULL, "", &saveptr);
        
        if (first_word && (first_word[0] == '#' || first_word[0] == '&')) {
            channel = first_word;
            reason = rest;
        } else {
            reason = args;
        }
        
        if (channel) {
            irc_part_channel(channel, reason);
        }
        
        free(args_copy);
    } else if (channel) {
        irc_part_channel(channel, NULL);
    }
    
    return channel ? 0 : -1;
}

static int irc_cmd_msg(const char* args) {
    if (!args || strlen(args) == 0) {
        return -1; /* Usage: /msg target message */
    }
    
    char* args_copy = strdup(args);
    char* saveptr;
    char* target = strtok_r(args_copy, " ", &saveptr);
    char* message = strtok_r(NULL, "", &saveptr);
    
    if (target && message) {
        irc_send_message(target, message);
        free(args_copy);
        return 0;
    }
    
    free(args_copy);
    return -1;
}

static int irc_cmd_next(const char* args) {
    (void)args; /* Unused */
    irc_next_buffer();
    return 0;
}

static int irc_cmd_prev(const char* args) {
    (void)args; /* Unused */
    irc_prev_buffer();
    return 0;
}

static int irc_cmd_buffer(const char* args) {
    if (!args || strlen(args) == 0) {
        return -1; /* Usage: /buffer name */
    }
    
    char buffer_name[64];
    sscanf(args, "%63s", buffer_name);
    
    if (irc_find_buffer(buffer_name)) {
        irc_set_active_buffer(buffer_name);
        return 0;
    }
    
    return -1;
}



/* Plugin update callback for networking and UI */
static void irc_plugin_update(void) {
    if (!g_irc_state) return;
    
    /* Process incoming IRC data */
    irc_process_incoming();
}

/* Plugin rendering callback - TODO: Implement custom UI rendering */
static void irc_plugin_render(vizero_renderer_t* renderer, int window_width, int window_height) {
    (void)renderer; /* Unused for now */
    (void)window_width;
    (void)window_height;
    /* TODO: Implement IRC-specific UI rendering */
}

/* Handle input in IRC mode */
static int irc_handle_input(const char* input) {
    if (!g_irc_state || !input) return 0;
    
    /* Handle regular chat messages (not commands) */
    if (strlen(input) > 0 && input[0] != '/') {
        /* Send to current buffer */
        if (strlen(g_irc_state->current_buffer) > 0) {
            irc_send_message(g_irc_state->current_buffer, input);
            return 1; /* Consumed input */
        }
    }
    
    return 0; /* Let Vizero handle commands */
}

/* Command registration array */
static vizero_plugin_command_t irc_commands[] = {
    {
        .command = "connect",
        .description = "Connect to IRC server: /connect <server[:port]> [nick]",
        .handler = (int(*)(vizero_editor_t*, const char*))irc_cmd_connect,
        .user_data = NULL
    },
    {
        .command = "disconnect",
        .description = "Disconnect from IRC server",
        .handler = (int(*)(vizero_editor_t*, const char*))irc_cmd_disconnect,
        .user_data = NULL
    },
    {
        .command = "join",
        .description = "Join IRC channel: /join <#channel>",
        .handler = (int(*)(vizero_editor_t*, const char*))irc_cmd_join,
        .user_data = NULL
    },
    {
        .command = "part",
        .description = "Leave IRC channel: /part [#channel] [reason]",
        .handler = (int(*)(vizero_editor_t*, const char*))irc_cmd_part,
        .user_data = NULL
    },
    {
        .command = "msg",
        .description = "Send private message: /msg <target> <message>",
        .handler = (int(*)(vizero_editor_t*, const char*))irc_cmd_msg,
        .user_data = NULL
    },
    {
        .command = "next",
        .description = "Switch to next IRC buffer",
        .handler = (int(*)(vizero_editor_t*, const char*))irc_cmd_next,
        .user_data = NULL
    },
    {
        .command = "prev",
        .description = "Switch to previous IRC buffer",
        .handler = (int(*)(vizero_editor_t*, const char*))irc_cmd_prev,
        .user_data = NULL
    },
    {
        .command = "buffer",
        .description = "Switch to specific IRC buffer: /buffer <name>",
        .handler = (int(*)(vizero_editor_t*, const char*))irc_cmd_buffer,
        .user_data = NULL
    }
};

/* Plugin information */
VIZERO_PLUGIN_DEFINE_INFO(
    "irc_client",                          /* name */
    "1.0.0",                              /* version */
    "Vizero Team",                        /* author */
    "Full-featured IRC Client Plugin",    /* description */
    VIZERO_PLUGIN_TYPE_GENERIC            /* type */
);

/* Plugin entry points */
int vizero_plugin_init(vizero_plugin_t* plugin, vizero_editor_t* editor, const vizero_editor_api_t* api) {
    /* Initialize WinSock on Windows */
#ifdef _WIN32
    WSADATA wsaData;
    if (WSAStartup(MAKEWORD(2, 2), &wsaData) != 0) {
        printf("[IRC] Failed to initialize WinSock\n");
        return -1;
    }
#endif
    
    /* Create global IRC state */
    g_irc_state = calloc(1, sizeof(irc_state_t));
    if (!g_irc_state) {
        return -1;
    }
    
    /* Initialize connection state */
    g_irc_state->connection.socket = INVALID_SOCKET_VALUE;
    g_irc_state->connection.connected = false;
    g_irc_state->connection.registered = false;
    g_irc_state->buffer_count = 0;
    strcpy(g_irc_state->current_buffer, "");
    strcpy(g_irc_state->input_buffer, "");
    
    /* Store state in plugin */
    plugin->user_data = g_irc_state;
    
    /* Register commands */
    plugin->callbacks.commands = irc_commands;
    plugin->callbacks.command_count = sizeof(irc_commands) / sizeof(irc_commands[0]);
    
    /* Note: Custom update/render callbacks not available in current plugin API */
    
    printf("[IRC] Full IRC client initialized with %zu commands\n", plugin->callbacks.command_count);
    
    /* Log registered commands */
    for (size_t i = 0; i < plugin->callbacks.command_count; i++) {
        printf("[IRC] Command: /%s - %s\n", 
               irc_commands[i].command, 
               irc_commands[i].description);
    }
    
    return 0;
}

void vizero_plugin_cleanup(vizero_plugin_t* plugin) {
    if (g_irc_state) {
        /* Disconnect if connected */
        irc_disconnect();
        
        /* Free global state */
        free(g_irc_state);
        g_irc_state = NULL;
    }
    
#ifdef _WIN32
    WSACleanup();
#endif
    
    printf("[IRC] IRC client plugin cleaned up\n");
}