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
#include <SDL.h>
#include "vizero/log.h"

#ifdef HAVE_SDL2_TTF
#include <SDL_ttf.h>
#endif

#ifdef _WIN32
    #include <winsock2.h>
    #include <ws2tcpip.h>
    #pragma comment(lib, "ws2_32.lib")
    typedef SOCKET socket_t;
    #define INVALID_SOCKET_VALUE INVALID_SOCKET
    #define close_socket closesocket
    #define strtok_r strtok_s
    typedef int socklen_t;
#else
    #include <sys/socket.h>
    #include <sys/select.h>
    #include <netinet/in.h>
    #include <arpa/inet.h>
    #include <netdb.h>
    #include <unistd.h>
    #include <fcntl.h>
    #include <errno.h>
    typedef int socket_t;
    #define INVALID_SOCKET_VALUE -1
    #define close_socket close
#endif

/* Forward declarations */
static void irc_set_active_buffer(const char* buffer_name);

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
        IRC_MSG_NOTICE,
        IRC_MSG_SERVER
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
    bool connecting; /* For non-blocking connect */
    
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
    
    /* SDL rendering context for custom UI */
    SDL_Renderer* sdl_renderer;
    SDL_Texture* render_texture;
#ifdef HAVE_SDL2_TTF
    TTF_Font* font;
#endif
    int texture_width;
    int texture_height;
    bool wants_full_window;
    bool just_escaped;   /* flag to prevent immediate auto-triggering after escape */
    
    /* Buffer management for new approach */
    vizero_buffer_t* original_buffer;  /* Buffer that was active when IRC loaded */
    vizero_buffer_t* irc_buffer;       /* Dedicated IRC buffer created on connect */
    
    /* Command mode tracking */
    bool in_vi_command;  /* true when user is typing a vi command starting with : */
    char vi_command_buffer[64];  /* buffer to collect vi command characters */
    

} irc_state_t;

/* Global IRC state */
static irc_state_t* g_irc_state = NULL;

/* Forward declarations */
static void irc_send_raw(const char* message);

/* Utility functions */
static void irc_initialize_networking(void) {
    /* Currently unused function */
#ifdef _WIN32
    WSADATA wsaData;
    WSAStartup(MAKEWORD(2, 2), &wsaData);
#endif
}

static void irc_cleanup_networking(void) {
    /* Currently unused function */
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
    
    /* Make socket non-blocking to prevent freezing */
#ifdef _WIN32
    u_long mode = 1;
    if (ioctlsocket(conn->socket, FIONBIO, &mode) != 0) {
        VIZERO_ERR("[IRC] Failed to set socket non-blocking");
        close_socket(conn->socket);
        conn->socket = INVALID_SOCKET_VALUE;
        return -1;
    }
#else
    int flags = fcntl(conn->socket, F_GETFL, 0);
    if (flags == -1 || fcntl(conn->socket, F_SETFL, flags | O_NONBLOCK) == -1) {
        VIZERO_ERR("[IRC] Failed to set socket non-blocking");
        close_socket(conn->socket);
        conn->socket = INVALID_SOCKET_VALUE;
        return -1;
    }
#endif
    
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
    server_addr.sin_port = htons((u_short)port);
    memcpy(&server_addr.sin_addr, host_entry->h_addr_list[0], host_entry->h_length);
    
    int connect_result = connect(conn->socket, (struct sockaddr*)&server_addr, sizeof(server_addr));
    if (connect_result != 0) {
#ifdef _WIN32
        int error = WSAGetLastError();
        if (error != WSAEWOULDBLOCK) {
            VIZERO_ERR("[IRC] Connect failed with error: %d", error);
            close_socket(conn->socket);
            conn->socket = INVALID_SOCKET_VALUE;
            return -1;
        }
        /* Connection in progress, continue */
        VIZERO_INFO("[IRC] Connection in progress...");
#else
        if (errno != EINPROGRESS) {
            VIZERO_ERR("[IRC] Connect failed with error: %s", strerror(errno));
            close_socket(conn->socket);
            conn->socket = INVALID_SOCKET_VALUE;
            return -1;
        }
        /* Connection in progress, continue */
        VIZERO_INFO("[IRC] Connection in progress...");
#endif
    } else {
        VIZERO_INFO("[IRC] Connected immediately");
    }
    
    /* Store connection info */
    size_t server_len = strlen(server);
    if (server_len >= sizeof(conn->server)) server_len = sizeof(conn->server) - 1;
    memcpy(conn->server, server, server_len);
    conn->server[server_len] = '\0';
    
    conn->port = port;
    
    size_t nick_len = strlen(nick);
    if (nick_len >= sizeof(conn->nickname)) nick_len = sizeof(conn->nickname) - 1;
    memcpy(conn->nickname, nick, nick_len);
    conn->nickname[nick_len] = '\0';
    
    size_t user_len = strlen(user);
    if (user_len >= sizeof(conn->username)) user_len = sizeof(conn->username) - 1;
    memcpy(conn->username, user, user_len);
    conn->username[user_len] = '\0';
    
    size_t real_len = strlen(real);
    if (real_len >= sizeof(conn->realname)) real_len = sizeof(conn->realname) - 1;
    memcpy(conn->realname, real, real_len);
    conn->realname[real_len] = '\0';
    conn->connecting = (connect_result != 0); /* True if connection in progress */
    conn->connected = (connect_result == 0);  /* True if connected immediately */
    conn->registered = false;
    conn->recv_pos = 0;
    conn->connect_time = time(NULL);
    
    /* Send registration only if connected immediately */
    if (conn->connected) {
        char buffer[512];
    VIZERO_INFO("[IRC] Registering with server...");
        
        snprintf(buffer, sizeof(buffer), "NICK %s", nick);
        irc_send_raw(buffer);
        
        snprintf(buffer, sizeof(buffer), "USER %s 0 * :%s", user, real);
        irc_send_raw(buffer);
    }
    
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
    conn->connecting = false;
    conn->registered = false;
}

static void irc_send_raw(const char* message) {
    if (!g_irc_state || !message) {
        VIZERO_ERR("[IRC] send_raw: null state or message");
        return;
    }
    
    irc_connection_t* conn = &g_irc_state->connection;
    if (conn->socket == INVALID_SOCKET_VALUE) {
        VIZERO_ERR("[IRC] send_raw: invalid socket");
        return;
    }
    if (!conn->connected) {
        VIZERO_INFO("[IRC] send_raw: not connected");
        return;
    }
    
    char buffer[515];  /* Increased to accommodate message + \r\n + null terminator */
    snprintf(buffer, sizeof(buffer), "%s\r\n", message);
    /* Sending IRC message */
    
    int bytes_sent = send(conn->socket, buffer, (int)strlen(buffer), 0);
    if (bytes_sent < 0) {
        VIZERO_ERR("[IRC] Send failed: %d", bytes_sent);
    } else {
        /* Message sent successfully */
    }
}

static void irc_send_message(const char* target, const char* message) {
    if (!target || !message) return;
    
    VIZERO_DBG("[IRC] Sending message to %s: %s", target, message);
    
    char buffer[512];
    snprintf(buffer, sizeof(buffer), "PRIVMSG %s :%s", target, message);
    irc_send_raw(buffer);
    
    /* Add to local buffer */
    irc_buffer_t* buffer_obj = irc_get_or_create_buffer(target);
    if (buffer_obj && g_irc_state) {
        irc_add_message(buffer_obj, g_irc_state->connection.nickname, message, IRC_MSG_NORMAL);
    }
}

/* Forward declarations */
static void irc_clear_users_from_channel(irc_buffer_t* buffer);

/* Check if we're currently in an IRC buffer */
static bool irc_is_in_irc_buffer(vizero_editor_t* editor) {
    if (!g_irc_state || !g_irc_state->api || !g_irc_state->api->get_current_buffer) {
        return false;
    }
    
    vizero_buffer_t* current_buffer = g_irc_state->api->get_current_buffer(editor);
    
    /* Check if current buffer is the main IRC buffer we created */
    if (current_buffer == g_irc_state->irc_buffer) {
        return true;
    }
    
    /* Fallback: check by buffer name/filename */
    if (g_irc_state->api->get_buffer_filename) {
        const char* filename = g_irc_state->api->get_buffer_filename(current_buffer);
        if (filename && strcmp(filename, "*IRC*") == 0) {
            /* Update our stored pointer since we found the IRC buffer */
            g_irc_state->irc_buffer = current_buffer;
            return true;
        }
    }
    
    return false;
}

static void irc_join_channel(const char* channel) {
    if (!channel) return;
    
    char buffer[512];
    snprintf(buffer, sizeof(buffer), "JOIN %s", channel);
    irc_send_raw(buffer);
    
    /* Create buffer for channel and switch to it */
    irc_buffer_t* channel_buffer = irc_get_or_create_buffer(channel);
    if (channel_buffer) {
        /* Clear user list for fresh start */
        irc_clear_users_from_channel(channel_buffer);
    }
    irc_set_active_buffer(channel);
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

/* Add user to channel */
static void irc_add_user_to_channel(irc_buffer_t* buffer, const char* nick) {
    if (!buffer || !nick || buffer->channel_info.user_count >= 256) return;
    
    /* Check if user already exists */
    for (size_t i = 0; i < buffer->channel_info.user_count; i++) {
        if (strcmp(buffer->channel_info.users[i].nick, nick) == 0) {
            return; /* User already exists */
        }
    }
    
    /* Add new user */
    irc_user_t* user = &buffer->channel_info.users[buffer->channel_info.user_count];
    strncpy(user->nick, nick, sizeof(user->nick) - 1);
    user->nick[sizeof(user->nick) - 1] = '\0';
    user->is_op = false;
    user->has_voice = false;
    
    /* Check for operator/voice prefixes */
    if (nick[0] == '@') {
        user->is_op = true;
        memmove(user->nick, user->nick + 1, strlen(user->nick));
    } else if (nick[0] == '+') {
        user->has_voice = true;
        memmove(user->nick, user->nick + 1, strlen(user->nick));
    }
    
    buffer->channel_info.user_count++;
}

/* Remove user from channel */
static void irc_remove_user_from_channel(irc_buffer_t* buffer, const char* nick) {
    if (!buffer || !nick) return;
    
    for (size_t i = 0; i < buffer->channel_info.user_count; i++) {
        if (strcmp(buffer->channel_info.users[i].nick, nick) == 0) {
            /* Move remaining users down */
            for (size_t j = i; j < buffer->channel_info.user_count - 1; j++) {
                buffer->channel_info.users[j] = buffer->channel_info.users[j + 1];
            }
            buffer->channel_info.user_count--;
            break;
        }
    }
}

/* Clear all users from channel */
static void irc_clear_users_from_channel(irc_buffer_t* buffer) {
    if (!buffer) return;
    buffer->channel_info.user_count = 0;
}

/* Parse IRC message and update buffers */
static void irc_parse_message(const char* line) {
    if (!line || !g_irc_state) return;
    
    /* Received IRC message */
    
    /* Simple IRC message parsing - handle basic commands */
    if (strncmp(line, "PING ", 5) == 0) {
        /* Respond to PING */
        char pong[512];
        snprintf(pong, sizeof(pong), "PONG %s", line + 5);
        VIZERO_DBG("[IRC] Responding to PING");
        irc_send_raw(pong);
        return;
    }
    
    /* Parse basic message format: :nick!user@host COMMAND params :message */
    char* line_copy = strdup(line);
    char* saveptr;
    char* prefix = NULL;
    char* command = NULL;
    //char* params = NULL;
    //char* message = NULL;
    
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
                
                /* Add user to channel user list */
                irc_add_user_to_channel(buffer, nick);
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
                
                /* Remove user from channel user list */
                irc_remove_user_from_channel(buffer, nick);
            }
        }
    } else if (strcmp(command, "QUIT") == 0) {
        /* User quit - remove from all channels */
        for (size_t i = 0; i < g_irc_state->buffer_count; i++) {
            irc_buffer_t* buffer = g_irc_state->buffers[i];
            if (buffer && buffer->name[0] == '#') { /* Channel buffer */
                irc_remove_user_from_channel(buffer, nick);
                
                /* Add quit message to channel */
                char* reason = strtok_r(NULL, "", &saveptr);
                if (reason && reason[0] == ':') reason++; /* Skip : */
                
                char quit_msg[256];
                if (reason && strlen(reason) > 0) {
                    snprintf(quit_msg, sizeof(quit_msg), "%s has quit (%s)", nick, reason);
                } else {
                    snprintf(quit_msg, sizeof(quit_msg), "%s has quit", nick);
                }
                irc_add_message(buffer, NULL, quit_msg, IRC_MSG_QUIT);
            }
        }
    } else {
        /* Handle numeric server messages */
        int numeric = atoi(command);
        if (numeric >= 001 && numeric <= 999) {
            /* Handle specific numerics */
            if (numeric == 353) { /* RPL_NAMREPLY - channel user list */
                /* Format: :server 353 nick = #channel :nick1 nick2 @nick3 +nick4 */
                char* rest = strtok_r(NULL, "", &saveptr);
                if (rest) {
                    /* Find channel name and user list */
                    char* channel_start = strchr(rest, '#');
                    if (channel_start) {
                        char* space = strchr(channel_start, ' ');
                        if (space) {
                            *space = '\0';
                            char* user_list = strchr(space + 1, ':');
                            if (user_list) {
                                user_list++; /* Skip ':' */
                                
                                /* Find buffer for this channel */
                                irc_buffer_t* channel_buffer = irc_find_buffer(channel_start);
                                if (channel_buffer) {
                                    /* Parse user list */
                                    char* user_list_copy = strdup(user_list);
                                    char* user = strtok(user_list_copy, " ");
                                    while (user) {
                                        irc_add_user_to_channel(channel_buffer, user);
                                        user = strtok(NULL, " ");
                                    }
                                    free(user_list_copy);
                                }
                            }
                            *space = ' '; /* Restore original string */
                        }
                    }
                }
                /* Fall through to add to server buffer too */
            } else if (numeric == 366) { /* RPL_ENDOFNAMES - end of user list */
                /* Names list is complete, nothing special to do */
            }
            
            /* Add server messages to the server buffer */
            irc_buffer_t* server_buffer = NULL;
            if (g_irc_state->buffer_count > 0) {
                server_buffer = g_irc_state->buffers[0]; /* First buffer is usually server */
            }
            
            if (!server_buffer) {
                server_buffer = irc_get_or_create_buffer(g_irc_state->connection.server);
            }
            
            if (server_buffer) {
                /* Extract the message part after the nick */
                char* rest = strtok_r(NULL, "", &saveptr);
                if (rest) {
                    /* Skip our nick and get the actual message */
                    char* msg_start = strchr(rest, ':');
                    if (msg_start) {
                        irc_add_message(server_buffer, NULL, msg_start + 1, IRC_MSG_SERVER);
                    } else {
                        irc_add_message(server_buffer, NULL, rest, IRC_MSG_SERVER);
                    }
                }
            }
        }
    }
    
    free(line_copy);
}

/* Process incoming IRC data */
static void irc_process_incoming(void) {
    if (!g_irc_state) return;
    
    irc_connection_t* conn = &g_irc_state->connection;
    if (conn->socket == INVALID_SOCKET_VALUE) return;
    
    /* Check if connection is still in progress */
    if (conn->connecting) {
        /* Check if connection completed */
        fd_set write_fds;
        fd_set error_fds;
        struct timeval timeout = {0, 0}; /* Non-blocking */
        
        FD_ZERO(&write_fds);
        FD_ZERO(&error_fds);
        FD_SET(conn->socket, &write_fds);
        FD_SET(conn->socket, &error_fds);
        
        int result = select((int)conn->socket + 1, NULL, &write_fds, &error_fds, &timeout);
        if (result > 0) {
            if (FD_ISSET(conn->socket, &error_fds)) {
                /* Connection failed */
                VIZERO_ERR("[IRC] Connection failed during select");
                irc_disconnect();
                return;
            } else if (FD_ISSET(conn->socket, &write_fds)) {
                /* Connection completed - check if successful */
                int error = 0;
                socklen_t len = sizeof(error);
                if (getsockopt(conn->socket, SOL_SOCKET, SO_ERROR, (char*)&error, &len) == 0 && error == 0) {
                    VIZERO_INFO("[IRC] Connection established!");
                    conn->connecting = false;
                    conn->connected = true;
                    
                    /* Send registration */
                    char buffer[512];
                    VIZERO_INFO("[IRC] Registering with server...");
                    snprintf(buffer, sizeof(buffer), "NICK %s\r\n", conn->nickname);
                    send(conn->socket, buffer, (int)strlen(buffer), 0);
                    snprintf(buffer, sizeof(buffer), "USER %s 0 * :%s\r\n", conn->username, conn->realname);
                    send(conn->socket, buffer, (int)strlen(buffer), 0);
                } else {
                    VIZERO_ERR("[IRC] Connection failed with socket error: %d", error);
                    irc_disconnect();
                    return;
                }
            }
        }
        /* If still connecting, return and try again later */
        if (conn->connecting) return;
    }
    
    if (!conn->connected) return;
    
    /* Receive data (non-blocking) */
    char temp_buffer[1024];
    int bytes_received = recv(conn->socket, temp_buffer, sizeof(temp_buffer) - 1, 0);
    if (bytes_received < 0) {
#ifdef _WIN32
        if (WSAGetLastError() == WSAEWOULDBLOCK) {
            /* No data available, not an error */
            return;
        }
#else
        if (errno == EWOULDBLOCK || errno == EAGAIN) {
            /* No data available, not an error */
            return;
        }
#endif
        /* Real error - connection lost */
        irc_disconnect();
        return;
    } else if (bytes_received == 0) {
        /* Connection closed by server */
        irc_disconnect();
        return;
    }
    
    temp_buffer[bytes_received] = '\0';
    
    /* Add to receive buffer */
    size_t remaining = sizeof(conn->recv_buffer) - conn->recv_pos - 1;
    size_t to_copy = ((size_t)bytes_received < remaining) ? (size_t)bytes_received : remaining;
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
    size_t buffer_name_len = strlen(buffer_name);
    if (buffer_name_len >= sizeof(g_irc_state->current_buffer)) {
        buffer_name_len = sizeof(g_irc_state->current_buffer) - 1;
    }
    memcpy(g_irc_state->current_buffer, buffer_name, buffer_name_len);
    g_irc_state->current_buffer[buffer_name_len] = '\0';
    
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
    for (size_t i = 0; i < g_irc_state->buffer_count; i++) {
        if (strcmp(g_irc_state->buffers[i]->name, g_irc_state->current_buffer) == 0) {
            current_index = (int)i;
            break;
        }
    }
    
    int next_index = (current_index + 1) % (int)g_irc_state->buffer_count;
    irc_set_active_buffer(g_irc_state->buffers[next_index]->name);
}

static void irc_prev_buffer(void) {
    if (!g_irc_state || g_irc_state->buffer_count == 0) return;
    
    int current_index = -1;
    for (size_t i = 0; i < g_irc_state->buffer_count; i++) {
        if (strcmp(g_irc_state->buffers[i]->name, g_irc_state->current_buffer) == 0) {
            current_index = (int)i;
            break;
        }
    }
    
    int prev_index = (int)((current_index - 1 + g_irc_state->buffer_count) % g_irc_state->buffer_count);
    irc_set_active_buffer(g_irc_state->buffers[prev_index]->name);
}

/* SDL Color conversion helper */
static SDL_Color irc_to_sdl_color(uint8_t r, uint8_t g, uint8_t b, uint8_t a) {
    SDL_Color color = {r, g, b, a};
    return color;
}

/* Simple text rendering without TTF - draws colored rectangles as placeholders */
static void irc_draw_simple_text(SDL_Renderer* renderer, const char* text, int x, int y, SDL_Color color) {
    if (!renderer || !text) return;
    
    /* Set text color */
    SDL_SetRenderDrawColor(renderer, color.r, color.g, color.b, color.a);
    
    /* Draw a simple rectangle as text placeholder */
    int text_len = (int)strlen(text);
    if (text_len > 0) {
        SDL_Rect text_rect = {x, y, text_len * 6, 12}; /* Approximate text size */
        SDL_RenderFillRect(renderer, &text_rect);
        
        /* Draw text outline in different color */
        SDL_SetRenderDrawColor(renderer, 255 - color.r, 255 - color.g, 255 - color.b, color.a);
        SDL_RenderDrawRect(renderer, &text_rect);
    }
}

#ifdef HAVE_SDL2_TTF
/* TTF text rendering when available */
static void irc_draw_ttf_text(SDL_Renderer* renderer, TTF_Font* font, const char* text, int x, int y, SDL_Color color) {
    if (!renderer || !font || !text) return;
    
    SDL_Surface* text_surface = TTF_RenderText_Solid(font, text, color);
    if (text_surface) {
        SDL_Texture* text_texture = SDL_CreateTextureFromSurface(renderer, text_surface);
        if (text_texture) {
            SDL_Rect dest_rect = {x, y, text_surface->w, text_surface->h};
            SDL_RenderCopy(renderer, text_texture, NULL, &dest_rect);
            SDL_DestroyTexture(text_texture);
        }
        SDL_FreeSurface(text_surface);
    }
}
#endif

/* IRC UI Rendering Functions using OpenGL */

static void irc_render_channel_list_gl(vizero_renderer_t* renderer, int x, int y, int width, int height) {
    if (!g_irc_state || !renderer) return;
    
    /* Process incoming IRC messages during rendering */
    static int render_counter = 0;
    if ((render_counter++ % 10) == 0) { /* Only process every 10th frame to avoid spam */
        irc_process_incoming();
    }
    
    /* Channel list background */
    vizero_colour_t bg_color = {0.1f, 0.1f, 0.15f, 1.0f};
    vizero_renderer_fill_rect(renderer, (float)x, (float)y, (float)width, (float)height, bg_color);
    
    /* Border */
    vizero_colour_t border_color = {0.3f, 0.3f, 0.3f, 1.0f};
    vizero_renderer_draw_line(renderer, (float)(x + width), (float)y, (float)(x + width), (float)(y + height), border_color);
    
    /* Title */
    vizero_text_info_t text_info = {0};
    text_info.x = (float)(x + 5);
    text_info.y = (float)(y + 5);
    text_info.colour = (vizero_colour_t){1.0f, 1.0f, 1.0f, 1.0f};
    text_info.font = NULL;
    vizero_renderer_draw_text(renderer, "Channels", &text_info);
    
    /* Render buffer list */
    int line_y = y + 25;
    int line_height = 20;
    
    for (size_t i = 0; i < g_irc_state->buffer_count && line_y < y + height - line_height; i++) {
        irc_buffer_t* buffer = g_irc_state->buffers[i];
        if (!buffer) continue;
        
        /* Highlight active buffer */
        if (buffer->is_active) {
            vizero_colour_t active_bg = {0.2f, 0.3f, 0.5f, 1.0f};
            vizero_renderer_fill_rect(renderer, (float)x, (float)line_y, (float)width, (float)line_height, active_bg);
        }
        
        /* Buffer name */
        text_info.x = (float)(x + 10);
        text_info.y = (float)(line_y + 2);
        text_info.colour = buffer->has_unread ? 
            (vizero_colour_t){1.0f, 1.0f, 0.5f, 1.0f} : /* Yellow for unread */
            (vizero_colour_t){0.9f, 0.9f, 0.9f, 1.0f};   /* Light gray for normal */
        
        vizero_renderer_draw_text(renderer, buffer->name, &text_info);
        
        line_y += line_height;
    }
}

static void irc_render_nick_list_gl(vizero_renderer_t* renderer, int x, int y, int width, int height) {
    /* Nick list background */
    vizero_colour_t bg_color = {0.1f, 0.1f, 0.15f, 1.0f};
    vizero_renderer_fill_rect(renderer, (float)x, (float)y, (float)width, (float)height, bg_color);
    
    /* Border */
    vizero_colour_t border_color = {0.3f, 0.3f, 0.3f, 1.0f};
    vizero_renderer_draw_line(renderer, (float)x, (float)y, (float)x, (float)(y + height), border_color);
    
    /* Title */
    vizero_text_info_t text_info = {0};
    text_info.x = (float)(x + 5);
    text_info.y = (float)(y + 5);
    text_info.colour = (vizero_colour_t){1.0f, 1.0f, 1.0f, 1.0f};
    text_info.font = NULL;
    vizero_renderer_draw_text(renderer, "Users", &text_info);
    
    /* Find current buffer and render its users */
    irc_buffer_t* current_buffer = irc_find_buffer(g_irc_state->current_buffer);
    if (!current_buffer) return;
    
    int line_y = y + 25;
    int line_height = 18;
    
    for (size_t i = 0; i < current_buffer->channel_info.user_count && line_y < y + height - line_height; i++) {
        irc_user_t* user = &current_buffer->channel_info.users[i];
        
        /* Choose color based on operator status */
        vizero_colour_t text_color;
        if (user->is_op) {
            text_color = (vizero_colour_t){1.0f, 0.5f, 0.5f, 1.0f}; /* Red for ops */
        } else if (user->has_voice) {
            text_color = (vizero_colour_t){0.5f, 1.0f, 0.5f, 1.0f}; /* Green for voice */
        } else {
            text_color = (vizero_colour_t){0.9f, 0.9f, 0.9f, 1.0f}; /* Light gray for normal */
        }
        
        /* Format nick with prefix */
        char display_nick[64];
        char prefix = ' ';
        if (user->is_op) prefix = '@';
        else if (user->has_voice) prefix = '+';
        
        snprintf(display_nick, sizeof(display_nick), "%c%s", prefix, user->nick);
        
        text_info.x = (float)(x + 5);
        text_info.y = (float)(line_y + 2);
        text_info.colour = text_color;
        
        vizero_renderer_draw_text(renderer, display_nick, &text_info);
        
        line_y += line_height;
    }
}

static void irc_render_message_area_gl(vizero_renderer_t* renderer, int x, int y, int width, int height) {
    /* Message area background */
    vizero_colour_t bg_color = {0.05f, 0.05f, 0.1f, 1.0f};
    vizero_renderer_fill_rect(renderer, (float)x, (float)y, (float)width, (float)height, bg_color);
    
    /* Find current buffer */
    irc_buffer_t* current_buffer = irc_find_buffer(g_irc_state->current_buffer);
    if (!current_buffer) {
        /* Show "No active buffer" message */
        vizero_text_info_t text_info = {0};
        text_info.x = (float)(x + 10);
        text_info.y = (float)(y + 10);
        text_info.colour = (vizero_colour_t){0.5f, 0.5f, 0.5f, 1.0f};
        text_info.font = NULL;
        vizero_renderer_draw_text(renderer, "No active buffer", &text_info);
        return;
    }
    
    /* Render messages */
    int line_y = y + height - 20; /* Start from bottom */
    int line_height = 18;
    int max_line_width = width - 10; /* Leave margin */
    
    for (int i = (int)current_buffer->message_count - 1; i >= 0 && line_y > y; i--) {
        irc_message_t* msg = &current_buffer->messages[i];
        
        vizero_text_info_t text_info = {0};
        text_info.x = (float)(x + 5);
        text_info.y = (float)line_y;
        text_info.font = NULL;
        
        /* Format message line */
        char line[1024];
        if (strlen(msg->nick) > 0) {
            snprintf(line, sizeof(line), "[%s] <%s> %s", msg->timestamp, msg->nick, msg->message);
            text_info.colour = (vizero_colour_t){0.9f, 0.9f, 0.9f, 1.0f};
        } else {
            snprintf(line, sizeof(line), "[%s] %s", msg->timestamp, msg->message);
            /* Different colors for different message types */
            switch (msg->type) {
                case IRC_MSG_SERVER:
                    text_info.colour = (vizero_colour_t){0.7f, 0.7f, 1.0f, 1.0f}; /* Light blue for server */
                    break;
                case IRC_MSG_JOIN:
                case IRC_MSG_PART:
                    text_info.colour = (vizero_colour_t){0.7f, 1.0f, 0.7f, 1.0f}; /* Light green for join/part */
                    break;
                default:
                    text_info.colour = (vizero_colour_t){0.8f, 0.8f, 0.8f, 1.0f};
                    break;
            }
        }
        
        /* Simple word wrapping - break long lines */
        int line_len = (int)strlen(line);
        int chars_per_line = max_line_width / 8; /* Rough estimate: 8 pixels per character */
        if (chars_per_line < 20) chars_per_line = 20; /* Minimum line length */
        
        if (line_len <= chars_per_line) {
            /* Line fits, render normally */
            vizero_renderer_draw_text(renderer, line, &text_info);
            line_y -= line_height;
        } else {
            /* Line too long, break it up */
            char temp_line[1024];
            int pos = 0;
            while (pos < line_len && line_y > y) {
                int chunk_len = chars_per_line;
                if (pos + chunk_len > line_len) {
                    chunk_len = line_len - pos;
                }
                
                /* Try to break at word boundary */
                if (pos + chunk_len < line_len) {
                    while (chunk_len > 0 && line[pos + chunk_len] != ' ') {
                        chunk_len--;
                    }
                    if (chunk_len == 0) chunk_len = chars_per_line; /* Force break if no spaces */
                }
                
                strncpy(temp_line, line + pos, chunk_len);
                temp_line[chunk_len] = '\0';
                
                text_info.y = (float)line_y;
                vizero_renderer_draw_text(renderer, temp_line, &text_info);
                
                pos += chunk_len;
                if (pos < line_len && line[pos] == ' ') pos++; /* Skip space */
                line_y -= line_height;
            }
        }
    }
}

static void irc_render_input_box_gl(vizero_renderer_t* renderer, int x, int y, int width, int height) {
    /* Input box background */
    vizero_colour_t bg_color = {0.15f, 0.15f, 0.2f, 1.0f};
    vizero_renderer_fill_rect(renderer, (float)x, (float)y, (float)width, (float)height, bg_color);
    
    /* Border */
    vizero_colour_t border_color = {0.3f, 0.3f, 0.3f, 1.0f};
    vizero_renderer_draw_line(renderer, (float)x, (float)y, (float)(x + width), (float)y, border_color);
    
    /* Input prompt */
    vizero_text_info_t text_info = {0};
    text_info.x = (float)(x + 5);
    text_info.y = (float)(y + 8);
    text_info.colour = (vizero_colour_t){0.7f, 0.7f, 0.7f, 1.0f};
    text_info.font = NULL;
    
    char prompt[600];  /* Increased to accommodate current_buffer + input_buffer + formatting */
    snprintf(prompt, sizeof(prompt), "[%s] %s", g_irc_state->current_buffer, g_irc_state->input_buffer);
    vizero_renderer_draw_text(renderer, prompt, &text_info);
}

/* Legacy SDL Rendering Functions (unused now) */

static void irc_render_channel_list(SDL_Renderer* renderer, int x, int y, int width, int height) {
    if (!g_irc_state || !renderer) return;
    
    /* Process incoming IRC messages during rendering */
    static int render_counter = 0;
    if ((render_counter++ % 10) == 0) { /* Only process every 10th frame to avoid spam */
        irc_process_incoming();
    }
    
    /* Background */
    SDL_SetRenderDrawColor(renderer, 32, 32, 32, 255);
    SDL_Rect bg_rect = {x, y, width, height};
    SDL_RenderFillRect(renderer, &bg_rect);
    
    /* Border */
    SDL_SetRenderDrawColor(renderer, 128, 128, 128, 255);
    SDL_Rect border_rect = {x + width - 1, y, 1, height};
    SDL_RenderFillRect(renderer, &border_rect);
    
    int line_y = y + 5;
    int line_height = 20;
    
    /* Render buffer list */
    for (size_t i = 0; i < g_irc_state->buffer_count && line_y < y + height - line_height; i++) {
        irc_buffer_t* buffer = g_irc_state->buffers[i];
        if (!buffer) continue;
        
        /* Highlight active buffer */
        if (buffer->is_active) {
            SDL_SetRenderDrawColor(renderer, 64, 64, 128, 128);
            SDL_Rect highlight_rect = {x, line_y - 2, width - 1, line_height};
            SDL_RenderFillRect(renderer, &highlight_rect);
        }
        
        /* Choose text color based on activity */
        SDL_Color text_color;
        if (buffer->has_unread) {
            text_color = irc_to_sdl_color(128, 255, 128, 255); /* Green for unread */
        } else if (buffer->is_active) {
            text_color = irc_to_sdl_color(255, 255, 255, 255); /* White for active */
        } else {
            text_color = irc_to_sdl_color(192, 192, 192, 255); /* Gray for inactive */
        }
        
        /* Format buffer name with unread count */
        char display_name[64];
        if (buffer->unread_count > 0) {
            snprintf(display_name, sizeof(display_name), "%s (%d)", buffer->name, buffer->unread_count);
        } else {
            strncpy(display_name, buffer->name, sizeof(display_name) - 1);
            display_name[sizeof(display_name) - 1] = '\0';
        }
        
        /* Render text */
#ifdef HAVE_SDL2_TTF
        if (g_irc_state->font) {
            irc_draw_ttf_text(renderer, g_irc_state->font, display_name, x + 5, line_y, text_color);
        } else
#endif
        {
            irc_draw_simple_text(renderer, display_name, x + 5, line_y, text_color);
        }
        
        line_y += line_height;
    }
}

static void irc_render_nick_list(SDL_Renderer* renderer, int x, int y, int width, int height) {
    if (!g_irc_state || !renderer) return;
    
    /* Background */
    SDL_SetRenderDrawColor(renderer, 32, 32, 32, 255);
    SDL_Rect bg_rect = {x, y, width, height};
    SDL_RenderFillRect(renderer, &bg_rect);
    
    /* Border */
    SDL_SetRenderDrawColor(renderer, 128, 128, 128, 255);
    SDL_Rect border_rect = {x, y, 1, height};
    SDL_RenderFillRect(renderer, &border_rect);
    
    /* Find current buffer */
    irc_buffer_t* current_buffer = irc_find_buffer(g_irc_state->current_buffer);
    if (!current_buffer) return;
    
    int line_y = y + 5;
    int line_height = 18;
    
    /* Render nick list for current channel */
    for (size_t i = 0; i < current_buffer->channel_info.user_count && line_y < y + height - line_height; i++) {
        irc_user_t* user = &current_buffer->channel_info.users[i];
        
        /* Choose color based on operator status */
        SDL_Color text_color;
        if (user->is_op) {
            text_color = irc_to_sdl_color(255, 128, 128, 255); /* Red for ops */
        } else if (user->has_voice) {
            text_color = irc_to_sdl_color(128, 255, 128, 255); /* Green for voice */
        } else {
            text_color = irc_to_sdl_color(255, 255, 255, 255); /* White for normal */
        }
        
        /* Format nick with prefix */
        char display_nick[64];
        char prefix = ' ';
        if (user->is_op) prefix = '@';
        else if (user->has_voice) prefix = '+';
        
        if (prefix != ' ') {
            snprintf(display_nick, sizeof(display_nick), "%c%s", prefix, user->nick);
        } else {
            strncpy(display_nick, user->nick, sizeof(display_nick) - 1);
            display_nick[sizeof(display_nick) - 1] = '\0';
        }
        
        /* Render text */
#ifdef HAVE_SDL2_TTF
        if (g_irc_state->font) {
            irc_draw_ttf_text(renderer, g_irc_state->font, display_nick, x + 5, line_y, text_color);
        } else
#endif
        {
            irc_draw_simple_text(renderer, display_nick, x + 5, line_y, text_color);
        }
        
        line_y += line_height;
    }
}

static void irc_render_message_area(SDL_Renderer* renderer, int x, int y, int width, int height) {
    if (!g_irc_state || !renderer) return;
    
    /* Background */
    SDL_SetRenderDrawColor(renderer, 16, 16, 16, 255);
    SDL_Rect bg_rect = {x, y, width, height};
    SDL_RenderFillRect(renderer, &bg_rect);
    
    /* Find current buffer */
    irc_buffer_t* current_buffer = irc_find_buffer(g_irc_state->current_buffer);
    if (!current_buffer) return;
    
    int line_height = 16;
    int max_lines = height / line_height;
    int start_index = (current_buffer->message_count > (size_t)max_lines) ? 
                     (int)(current_buffer->message_count - (size_t)max_lines) : 0;
    
    int line_y = y + 5;
    
    /* Render messages */
    for (size_t i = (size_t)start_index; i < current_buffer->message_count && line_y < y + height - line_height; i++) {
        irc_message_t* msg = &current_buffer->messages[i];
        
        char formatted_msg[1024];
        
        /* Format timestamp */
        if (strlen(msg->timestamp) > 0) {
            snprintf(formatted_msg, sizeof(formatted_msg), "[%s]", msg->timestamp);
        } else {
            strcpy(formatted_msg, "[??:??:??]");
        }
        
        /* Choose colors based on message type */
        SDL_Color msg_color;
        
        if (msg->type == IRC_MSG_JOIN || msg->type == IRC_MSG_PART) {
            /* System messages in gray */
            msg_color = irc_to_sdl_color(128, 128, 128, 255);
            strncat(formatted_msg, " ", sizeof(formatted_msg) - strlen(formatted_msg) - 1);
            strncat(formatted_msg, msg->message, sizeof(formatted_msg) - strlen(formatted_msg) - 1);
        } else {
            /* Regular chat messages */
            if (msg->nick && strlen(msg->nick) > 0) {
                /* Generate nick color */
                uint32_t nick_color = irc_generate_nick_color(msg->nick);
                uint8_t nick_r = (nick_color >> 16) & 0xFF;
                uint8_t nick_g = (nick_color >> 8) & 0xFF;
                uint8_t nick_b = nick_color & 0xFF;
                (void)nick_r; (void)nick_g; (void)nick_b; /* TODO: Use for colored nicks */
                
                /* Add nick part */
                char nick_part[128];
                snprintf(nick_part, sizeof(nick_part), " <%s> ", msg->nick);
                strncat(formatted_msg, nick_part, sizeof(formatted_msg) - strlen(formatted_msg) - 1);
                
                /* For now, use white for message text */
                msg_color = irc_to_sdl_color(255, 255, 255, 255);
            } else {
                /* Message without nick */
                msg_color = irc_to_sdl_color(255, 255, 255, 255);
                strncat(formatted_msg, " ", sizeof(formatted_msg) - strlen(formatted_msg) - 1);
            }
            strncat(formatted_msg, msg->message, sizeof(formatted_msg) - strlen(formatted_msg) - 1);
        }
        
        /* Render text */
#ifdef HAVE_SDL2_TTF
        if (g_irc_state->font) {
            irc_draw_ttf_text(renderer, g_irc_state->font, formatted_msg, x + 5, line_y, msg_color);
        } else
#endif
        {
            irc_draw_simple_text(renderer, formatted_msg, x + 5, line_y, msg_color);
        }
        
        line_y += line_height;
    }
}

static void irc_render_input_box(SDL_Renderer* renderer, int x, int y, int width, int height) {
    if (!g_irc_state || !renderer) return;
    
    /* Background */
    SDL_SetRenderDrawColor(renderer, 48, 48, 48, 255);
    SDL_Rect bg_rect = {x, y, width, height};
    SDL_RenderFillRect(renderer, &bg_rect);
    
    /* Border */
    SDL_SetRenderDrawColor(renderer, 128, 128, 128, 255);
    SDL_Rect border_rect = {x, y, width, 1};
    SDL_RenderFillRect(renderer, &border_rect);
    
    /* Draw prompt */
    char prompt[256];
    snprintf(prompt, sizeof(prompt), "[%s] > %s", 
             strlen(g_irc_state->current_buffer) > 0 ? g_irc_state->current_buffer : "no-buffer",
             g_irc_state->input_buffer);
    
    SDL_Color text_color = irc_to_sdl_color(255, 255, 255, 255);
#ifdef HAVE_SDL2_TTF
    if (g_irc_state->font) {
        irc_draw_ttf_text(renderer, g_irc_state->font, prompt, x + 5, y + 5, text_color);
    } else
#endif
    {
        irc_draw_simple_text(renderer, prompt, x + 5, y + 5, text_color);
    }
}









/* IRC Command Implementations */
static int irc_cmd_connect(vizero_editor_t* editor, const char* args) {
    if (!args || strlen(args) == 0) {
        printf("[IRC] Usage: connect server[:port] [nick]\n");
        return -1;
    }
    
    /* Try to force normal mode before starting IRC */
    printf("[IRC] Attempting to set normal mode before starting IRC...\n");
    
    /* Store editor reference for later use */
    g_irc_state->editor = editor;
    
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
        /* Validate nickname - can't start with # or other invalid characters */
        if (nick_part[0] == '#' || nick_part[0] == '&' || nick_part[0] == '!' || nick_part[0] == '+') {
            printf("[IRC] Error: Invalid nickname '%s' - nicknames cannot start with #, &, !, or +\n", nick_part);
            printf("[IRC] Using default nickname 'vizero_user' instead\n");
        } else {
            strncpy(nick, nick_part, sizeof(nick) - 1);
        }
    }

    free(args_copy);

    printf("[IRC] Attempting to connect to %s:%d as %s\n", server, port, nick);
    int result = irc_connect(server, port, nick, user, real);
    if (result == 0) {
        printf("[IRC] Connected successfully!\n");
        
        /* Create dedicated IRC buffer using :enew command with name */
        if (g_irc_state && g_irc_state->api && g_irc_state->api->execute_command) {
            printf("[IRC] Creating dedicated IRC buffer...\n");
            int enew_result = g_irc_state->api->execute_command(editor, "enew *IRC*");
            if (enew_result == 0) {
                /* Get the newly created buffer */
                if (g_irc_state->api->get_current_buffer) {
                    g_irc_state->irc_buffer = g_irc_state->api->get_current_buffer(editor);
                    printf("[IRC] Dedicated IRC buffer created and active\n");
                } else {
                    printf("[IRC] Warning: Could not get reference to new IRC buffer\n");
                }
            } else {
                printf("[IRC] Warning: Failed to create dedicated IRC buffer (error %d)\n", enew_result);
            }
        }
        
        /* Create server buffer */
        irc_buffer_t* server_buffer = irc_get_or_create_buffer(server);
        if (server_buffer) {
            char connect_msg[256];
            snprintf(connect_msg, sizeof(connect_msg), "Connecting to %s:%d as %s...", server, port, nick);
            irc_add_message(server_buffer, NULL, connect_msg, IRC_MSG_NORMAL);
            irc_set_active_buffer(server);
        }
    } else {
        printf("[IRC] Connection failed with error: %d\n", result);
    }
    
    return result;
}

static int irc_cmd_disconnect(vizero_editor_t* editor, const char* args) {
    (void)args; /* Unused */
    printf("[IRC] Disconnecting from server...\n");
    
    /* Switch back to original buffer */
    if (g_irc_state && g_irc_state->original_buffer && g_irc_state->api && g_irc_state->api->execute_command) {
        printf("[IRC] Returning to original buffer...\n");
        g_irc_state->api->execute_command(editor, "bp");
        printf("[IRC] Returned to previous buffer after disconnect\n");
    }
    
    /* Clear IRC buffer reference */
    if (g_irc_state) {
        g_irc_state->irc_buffer = NULL;
    }
    
    irc_disconnect();
    return 0;
}

static int irc_cmd_join(vizero_editor_t* editor, const char* args) {
    (void)editor; /* Unused */
    if (!args || strlen(args) == 0) {
        printf("[IRC] Usage: join #channel\n");
        return -1;
    }
    
    char channel[64];
    sscanf(args, "%63s", channel);
    
    /* Ensure channel starts with # */
    if (channel[0] != '#' && channel[0] != '&') {
        char temp[65];  /* Increased to accommodate "#" + 63-char channel name */
        snprintf(temp, sizeof(temp), "#%s", channel);
        size_t temp_len = strlen(temp);
        if (temp_len >= sizeof(channel)) temp_len = sizeof(channel) - 1;
        memcpy(channel, temp, temp_len);
        channel[temp_len] = '\0';
    }
    
    irc_join_channel(channel);
    return 0;
}

static int irc_cmd_part(vizero_editor_t* editor, const char* args) {
    (void)editor; /* Unused */
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

static int irc_cmd_msg(vizero_editor_t* editor, const char* args) {
    (void)editor; /* Unused */
    if (!args || strlen(args) == 0) {
        printf("[IRC] Usage: msg target message\n");
        return -1;
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

static int irc_cmd_next(vizero_editor_t* editor, const char* args) {
    (void)editor; (void)args; /* Unused */
    irc_next_buffer();
    return 0;
}

static int irc_cmd_prev(vizero_editor_t* editor, const char* args) {
    (void)editor; (void)args; /* Unused */
    irc_prev_buffer();
    return 0;
}

static int irc_cmd_buffer(vizero_editor_t* editor, const char* args) {
    (void)editor; /* Unused */
    if (!args || strlen(args) == 0) {
        printf("[IRC] Usage: buffer name\n");
        return -1;
    }
    
    char buffer_name[64];
    sscanf(args, "%63s", buffer_name);
    
    if (irc_find_buffer(buffer_name)) {
        irc_set_active_buffer(buffer_name);
        return 0;
    }
    
    return -1;
}

/* Command to disable IRC protection and restore buffer writability */
static int irc_cmd_disable(vizero_editor_t* editor, const char* args) {
    (void)args; /* Unused */
    
    printf("[IRC] Disabling IRC plugin and restoring buffer writability\n");
    
    /* Disconnect if connected */
    irc_disconnect();
    
    /* Clear all buffers */
    if (g_irc_state) {
        for (size_t i = 0; i < g_irc_state->buffer_count; i++) {
            if (g_irc_state->buffers[i]) {
                free(g_irc_state->buffers[i]);
                g_irc_state->buffers[i] = NULL;
            }
        }
        g_irc_state->buffer_count = 0;
        g_irc_state->current_buffer[0] = '\0';
    }
    
    /* Switch back to original buffer if we have a reference to it */
    if (g_irc_state && g_irc_state->original_buffer && g_irc_state->api && g_irc_state->api->execute_command) {
        /* Note: We can't directly switch to a buffer pointer, so we'll use :bp to go back */
        /* This is a limitation - ideally we'd need a "switch_to_buffer" API function */
        printf("[IRC] Attempting to return to original buffer...\n");
        g_irc_state->api->execute_command(editor, "bp");
        printf("[IRC] Returned to previous buffer - IRC disabled\n");
    } else {
        printf("[IRC] IRC disabled - original buffer reference not available\n");
    }
    
    /* Clear IRC buffer reference */
    if (g_irc_state) {
        g_irc_state->irc_buffer = NULL;
    }
    
    return 0;
}



/* Plugin update callback for networking and UI */
static void irc_plugin_update(void) {
    if (!g_irc_state) return;
    
    /* Process incoming IRC data */
    irc_process_incoming();
}

/* Main IRC UI Rendering Function */
static int irc_render_full_window(vizero_editor_t* editor, vizero_renderer_t* renderer, int width, int height) {
    (void)editor; /* Unused */
    
    if (!g_irc_state || !renderer) return 0;
    
    /* Only render if we have buffers (IRC is active) */
    if (g_irc_state->buffer_count == 0) return 0;
    
    /* Quassel-like layout:
     * - Left panel: Channel list (150px wide)
     * - Right panel: Nick list (120px wide)
     * - Bottom: Input box (30px high)
     * - Center: Message area (remaining space)
     */
    
    int channel_list_width = 150;
    int nick_list_width = 120;
    int input_height = 30;
    
    int message_x = channel_list_width;
    int message_y = 0;
    int message_width = width - channel_list_width - nick_list_width;
    int message_height = height - input_height;
    
    /* Render panels using OpenGL */
    irc_render_channel_list_gl(renderer, 0, 0, channel_list_width, height);
    irc_render_nick_list_gl(renderer, width - nick_list_width, 0, nick_list_width, height - input_height);
    irc_render_message_area_gl(renderer, message_x, message_y, message_width, message_height);
    irc_render_input_box_gl(renderer, message_x, height - input_height, message_width, input_height);
    
    return 1; /* Successfully rendered */
}

/* Check if IRC plugin wants full window control */
static int irc_wants_full_window(vizero_editor_t* editor) {
    if (!g_irc_state) {
        printf("[IRC] wants_full_window: no state\n");
        return 0;
    }
    
    /* Only take over full window when we're connected to IRC AND in an IRC buffer AND user wants full window */
    int connected = g_irc_state->connection.connected;
    int in_irc_buffer = irc_is_in_irc_buffer(editor);
    int user_wants_full = g_irc_state->wants_full_window;
    int wants = (connected && in_irc_buffer && user_wants_full) ? 1 : 0;
    
    static int last_wants = -1; 
    last_wants = wants;
    
    return wants;
}

/* Track if we're in command mode for buffer protection */
static int g_in_command_mode = 0;

/* Handle any command execution - permanently protect buffer when IRC plugin is active */
static int irc_on_any_command(vizero_editor_t* editor, const char* command, const char* args) {
    (void)args; /* Unused */
    
    if (!g_irc_state) return 0;
    
    printf("[IRC] Command executed: '%s'\n", command ? command : "NULL");
    
    /* PROTECTION: Keep IRC buffer readonly during command processing */
    if (g_irc_state && g_irc_state->irc_buffer && g_irc_state->api && g_irc_state->api->get_current_buffer && g_irc_state->api->set_buffer_readonly) {
        vizero_buffer_t* current_buffer = g_irc_state->api->get_current_buffer(editor);
        if (current_buffer == g_irc_state->irc_buffer) {
            printf("[IRC] Ensuring IRC buffer remains readonly during command processing\n");
            g_irc_state->api->set_buffer_readonly(current_buffer, 1);
        }
    }
    
    g_in_command_mode = 0; /* Reset command mode */
    return 0; /* Don't consume the command */
}

/* Handle key input in IRC mode */
static int irc_on_key_input(vizero_editor_t* editor, uint32_t key, uint32_t modifiers) {
    (void)editor; /* Unused */
    
    if (!g_irc_state) return 0;
    
    /* Only handle input when we're connected to IRC or in an IRC buffer */
    if (!g_irc_state->connection.connected && !irc_is_in_irc_buffer(editor)) {
        return 0; /* Let other plugins/editor handle the input */
    }
    
    /* If we're in vi command mode but not in full-window mode, reset and let editor handle */
    if (g_irc_state->in_vi_command && !g_irc_state->wants_full_window) {
        printf("[IRC] In vi command mode but not full-window - resetting and letting editor handle\n");
        g_irc_state->in_vi_command = false;
        g_irc_state->vi_command_buffer[0] = '\0';
        return 0; /* Let editor handle it */
    }
    
    /* If we're in vi command mode AND in full-window mode, collect the command and execute it ourselves */
    if (g_irc_state->in_vi_command && g_irc_state->wants_full_window) {
        if (key == 13) { /* Enter - execute the collected command */
            printf("[IRC] Vi command completed: '%s'\n", g_irc_state->vi_command_buffer);
            
            /* Execute common vi commands directly - strip the leading colon */
            const char* command = g_irc_state->vi_command_buffer;
            if (command[0] == ':') command++; /* Skip leading colon */
            
            if (strcmp(command, "bn") == 0) {
                printf("[IRC] Executing buffer next command\n");
                printf("[IRC] Current buffer before command: %s\n", 
                       g_irc_state->api && g_irc_state->api->get_current_buffer ? "available" : "unavailable");
                
                if (g_irc_state->api && g_irc_state->api->execute_command) {
                    /* Try different command formats to see what works */
                    printf("[IRC] Trying execute_command with 'bn'\n");
                    int result1 = g_irc_state->api->execute_command(editor, "bn");
                    printf("[IRC] Result for 'bn': %d\n", result1);
                    
                    if (result1 != 0) {
                        printf("[IRC] Trying execute_command with 'bnext'\n");
                        int result2 = g_irc_state->api->execute_command(editor, "bnext");
                        printf("[IRC] Result for 'bnext': %d\n", result2);
                        
                        if (result2 != 0) {
                            printf("[IRC] Trying execute_command with 'buffer next'\n");
                            int result3 = g_irc_state->api->execute_command(editor, "buffer next");
                            printf("[IRC] Result for 'buffer next': %d\n", result3);
                        }
                    }
                    
                    /* Check if we're still in the same buffer after the command */
                    if (irc_is_in_irc_buffer(editor)) {
                        printf("[IRC] Still in IRC buffer after command execution\n");
                    } else {
                        printf("[IRC] *** SUCCESS: Buffer switched! You are now in a different buffer (likely hello.c) ***\n");
                        printf("[IRC] *** Use :bp to return to the IRC buffer ***\n");
                    }
                } else {
                    printf("[IRC] ERROR: execute_command API not available\n");
                }
            } else if (strcmp(command, "bp") == 0) {
                printf("[IRC] Executing buffer previous command\n");
                if (g_irc_state->api && g_irc_state->api->execute_command) {
                    int result = g_irc_state->api->execute_command(editor, "bp");
                    printf("[IRC] Buffer previous command result: %d\n", result);
                } else {
                    printf("[IRC] ERROR: execute_command API not available\n");
                }
            } else {
                /* For other commands, try to execute them generically */
                printf("[IRC] Executing generic vi command: %s\n", command);
                if (g_irc_state->api && g_irc_state->api->execute_command) {
                    int result = g_irc_state->api->execute_command(editor, command);
                    printf("[IRC] Generic command result: %d\n", result);
                } else {
                    printf("[IRC] ERROR: execute_command API not available\n");
                }
            }
            
            /* Reset command mode */
            g_irc_state->in_vi_command = false;
            g_irc_state->vi_command_buffer[0] = '\0';
            return 1; /* Consumed */
        } else if (key == 27) { /* Escape - command cancelled */
            printf("[IRC] Vi command cancelled - letting editor handle escape\n");
            g_irc_state->in_vi_command = false;
            g_irc_state->vi_command_buffer[0] = '\0';
            return 0; /* Let editor handle escape for proper mode transition */
        } else if (key >= 32 && key <= 126) { /* Collect printable characters */
            size_t len = strlen(g_irc_state->vi_command_buffer);
            if (len < sizeof(g_irc_state->vi_command_buffer) - 1) {
                g_irc_state->vi_command_buffer[len] = (char)key;
                g_irc_state->vi_command_buffer[len + 1] = '\0';
                printf("[IRC] Vi command so far: '%s'\n", g_irc_state->vi_command_buffer);
            }
            return 1; /* Consumed */
        }
        /* For other keys (arrows, etc.) in vi command mode, let them pass through */
        return 0;
    }
    
    /* ONLY intercept the initial colon when in IRC buffer AND in full-window mode */
    if (key == ':' && !g_in_command_mode && g_irc_state->irc_buffer && g_irc_state->wants_full_window) {
        /* Check if we're currently in the IRC buffer */
        if (g_irc_state->api && g_irc_state->api->get_current_buffer) {
            vizero_buffer_t* current_buffer = g_irc_state->api->get_current_buffer(editor);
            if (current_buffer == g_irc_state->irc_buffer) {
                printf("[IRC] Initial colon detected in IRC buffer - making readonly to prevent text bleeding\n");
                g_in_command_mode = 1;
                
                /* Make IRC buffer readonly immediately */
                if (g_irc_state->api->set_buffer_readonly) {
                    printf("[IRC] Setting IRC buffer readonly for command mode protection\n");
                    g_irc_state->api->set_buffer_readonly(current_buffer, 1);
                }
                
                /* Let colon pass through to enter command mode */
                return 0;
            }
        }
    }
    
    /* When in command mode and IRC is inactive, DON'T intercept anything - let vim handle command building */
    if (g_in_command_mode && g_irc_state->buffer_count == 0) {
        /* Check for Enter to detect command completion */
        if (key == 13) { /* Enter key */
            printf("[IRC] Command completed - IRC will check if it's an IRC command\n");
            g_in_command_mode = 0;
            /* Don't restore buffer writability yet - let the command handler decide */
            return 0; /* Let Enter pass through */
        }
        
        /* Check for Escape to cancel command mode */
        if (key == 27) { /* Escape */
            printf("[IRC] Command mode cancelled - restoring buffer writability\n");
            g_in_command_mode = 0;
            
            /* Restore IRC buffer writability since command was cancelled */
            if (g_irc_state && g_irc_state->irc_buffer && g_irc_state->api && g_irc_state->api->get_current_buffer && g_irc_state->api->set_buffer_readonly) {
                vizero_buffer_t* current_buffer = g_irc_state->api->get_current_buffer(editor);
                if (current_buffer == g_irc_state->irc_buffer) {
                    g_irc_state->api->set_buffer_readonly(current_buffer, 0);
                }
            }
            return 0; /* Let Escape pass through */
        }
        
        /* For all other keys in command mode when IRC inactive, let vim handle it */
        return 0;
    }
    
    /* CRITICAL: If IRC is completely inactive and not in command mode, let vim handle everything */
    if (g_irc_state->buffer_count == 0 && !g_in_command_mode) {
        /* Let ALL keys pass through to vim - no interference when IRC is inactive */
        return 0; /* Not consumed - let vim handle normally */
    }
    
    /* Process incoming IRC messages on every input event */
    irc_process_incoming();
    
    printf("[IRC] Handling key %d ('%c') in IRC mode\n", key, (char)key);
    
    /* Check first if we're in an IRC buffer - if not, don't process at all */
    bool currently_in_irc_buffer = irc_is_in_irc_buffer(editor);
    if (!currently_in_irc_buffer) {
        /* Not in IRC buffer - let editor handle all keys normally without any IRC processing */
        return 0;
    }
    
    /* Track buffer switching to clear escape flag when returning to IRC */
    static bool was_in_irc_buffer_last_time = false;
    
    /* We're in an IRC buffer - proceed with IRC key handling */
    
    /* If we switched back to IRC buffer from another buffer, clear the just_escaped flag and enable full-screen */
    if (!was_in_irc_buffer_last_time) {
        printf("[IRC] Detected switch back to IRC buffer - enabling full-screen mode\n");
        g_irc_state->just_escaped = false;
        g_irc_state->wants_full_window = true; /* Force full-screen when entering *IRC* buffer */
    }
    
    /* FORCE IRC full-screen mode whenever we're in IRC buffer and not escaped recently */
    /* This ensures that switching to *IRC* buffer always shows the IRC interface */
    if (!g_irc_state->wants_full_window && !g_irc_state->just_escaped) {
        g_irc_state->wants_full_window = true;
    }
    
    was_in_irc_buffer_last_time = true;
        
        /* Handle Escape - toggle between IRC full-screen and normal editor mode */
        if (key == 27) { /* Escape */
            printf("[IRC] Escape detected - toggling IRC display mode\n");
            
            /* If we're currently in full-screen mode, switch to normal mode */
            if (g_irc_state->wants_full_window) {
                g_irc_state->wants_full_window = false;
                g_irc_state->just_escaped = true; /* Prevent immediate auto-triggering */
                printf("[IRC] IRC full-screen mode: OFF\n");
                printf("[IRC] Switching to normal editor mode - letting editor handle escape for proper mode transition\n");
                return 0; /* Let editor handle escape to switch to NORMAL mode */
            } else {
                /* If we're already in normal mode, switch back to full-screen */
                g_irc_state->wants_full_window = true;
                g_irc_state->just_escaped = false; /* Clear flag when returning to full-screen */
                printf("[IRC] IRC full-screen mode: ON\n");
                return 1; /* Consumed - we handled the toggle to full-screen */
            }
        }
        
        /* Auto-detect: if user types printable characters in IRC buffer, enable full-screen mode */
        /* But don't auto-trigger immediately after an escape */
        if (!g_irc_state->wants_full_window && !g_irc_state->just_escaped && key >= 32 && key <= 126) {
            printf("[IRC] User typing in IRC buffer - automatically enabling full-screen mode\n");
            g_irc_state->wants_full_window = true;
            g_irc_state->just_escaped = false; /* Clear flag when user intentionally returns to IRC */
            /* Continue processing the key for IRC input below */
        }
        
        /* Don't clear just_escaped flag automatically - let it persist until user switches buffers or explicitly returns to IRC */
        
        /* If we're not in full-window mode and it's not a printable character, let the editor handle it */
        if (!g_irc_state->wants_full_window) {
            printf("[IRC] Not in full-window mode - letting editor handle key %d normally\n", key);
            return 0; /* Let editor handle it */
        }
        
        /* Handle colon - starts vi command mode (only in full-window mode) */
        if (g_irc_state->wants_full_window && (key == ':' || (key == ';' && (modifiers & KMOD_SHIFT)))) {
            printf("[IRC] Colon detected (key=%d, shift=%s) - entering vi command mode\n", 
                   key, (modifiers & KMOD_SHIFT) ? "yes" : "no");
            g_irc_state->in_vi_command = true;
            return 0; /* Let vi handle the command */
        }
        
        /* For all other keys when IRC is active and in full-window mode, consume them for IRC input */
        printf("[IRC] IRC active - processing key %d for IRC input\n", key);
        
        /* Handle other IRC-specific keys - regular text input for chat */
        printf("[IRC] In IRC buffer - processing key %d for IRC input\n", key);
    
    /* === IRC KEY PROCESSING (only runs when in an IRC buffer) === */
    
    /* Handle regular character input - but NOT when in vi command mode */
    if (key >= 32 && key <= 126 && !g_irc_state->in_vi_command) { /* Printable ASCII and not in vi command mode */
        char input_char = (char)key;
        
        /* DEBUG: Print key and modifiers for testing */
        static int debug_key_count = 0;
        if (debug_key_count < 5) {
            printf("[IRC] Key: %d ('%c'), Modifiers: 0x%x, KMOD_SHIFT: 0x%x\n", 
                   key, (char)key, modifiers, KMOD_SHIFT);
            debug_key_count++;
        }
        
        /* Handle Shift key for uppercase and symbols */
        if (modifiers & KMOD_SHIFT) { /* Shift key pressed */
            if (key >= 'a' && key <= 'z') {
                /* Convert to uppercase */
                input_char = (char)(key - 32);
            } else {
                /* Handle shifted symbols */
                switch (key) {
                    case '1': input_char = '!'; break;
                    case '2': input_char = '@'; break;
                    case '3': input_char = '#'; break;
                    case '4': input_char = '$'; break;
                    case '5': input_char = '%'; break;
                    case '6': input_char = '^'; break;
                    case '7': input_char = '&'; break;
                    case '8': input_char = '*'; break;
                    case '9': input_char = '('; break;
                    case '0': input_char = ')'; break;
                    case '-': input_char = '_'; break;
                    case '=': input_char = '+'; break;
                    case '[': input_char = '{'; break;
                    case ']': input_char = '}'; break;
                    case '\\': input_char = '|'; break;
                    case ';': input_char = ':'; break;
                    case '\'': input_char = '"'; break;
                    case ',': input_char = '<'; break;
                    case '.': input_char = '>'; break;
                    case '/': input_char = '?'; break;
                    case '`': input_char = '~'; break;
                    /* Keep original character for others */
                }
            }
        }
        
        /* Add character to input buffer */
        size_t len = strlen(g_irc_state->input_buffer);
        if (len < sizeof(g_irc_state->input_buffer) - 1) {
            g_irc_state->input_buffer[len] = input_char;
            g_irc_state->input_buffer[len + 1] = '\0';
        }
        return 1; /* Consumed */
    }
    
    /* Handle special keys - but NOT when in vi command mode */
    if (!g_irc_state->in_vi_command) {
        switch (key) {
            case 13: /* Enter */
                if (strlen(g_irc_state->input_buffer) > 0) {
                printf("[IRC] Input received: '%s'\n", g_irc_state->input_buffer);
                
                /* Check if input is a command (starts with /) */
                if (g_irc_state->input_buffer[0] == '/') {
                    /* Parse and execute IRC command */
                    char* command = g_irc_state->input_buffer + 1; /* Skip the '/' */
                    char* args = strchr(command, ' ');
                    if (args) {
                        *args = '\0'; /* Terminate command */
                        args++; /* Move to arguments */
                    }
                    
                    printf("[IRC] Executing command: '%s' with args: '%s'\n", command, args ? args : "");
                    
                    /* Execute the command */
                    if (strcmp(command, "join") == 0) {
                        irc_cmd_join(g_irc_state->editor, args);
                    } else if (strcmp(command, "part") == 0) {
                        irc_cmd_part(g_irc_state->editor, args);
                    } else if (strcmp(command, "msg") == 0) {
                        irc_cmd_msg(g_irc_state->editor, args);
                    } else if (strcmp(command, "quit") == 0) {
                        irc_disconnect();
                    } else {
                        printf("[IRC] Unknown command: %s\n", command);
                    }
                } else {
                    /* Check if input looks like a command without / prefix */
                    char input_copy[512];
                    strncpy(input_copy, g_irc_state->input_buffer, sizeof(input_copy) - 1);
                    input_copy[sizeof(input_copy) - 1] = '\0';
                    
                    char* first_word = strtok(input_copy, " ");
                    char* rest = strtok(NULL, "");
                    
                    if (first_word && (strcmp(first_word, "join") == 0 || 
                                      strcmp(first_word, "part") == 0 ||
                                      strcmp(first_word, "msg") == 0 ||
                                      strcmp(first_word, "quit") == 0)) {
                        printf("[IRC] Executing command without /: '%s' with args: '%s'\n", first_word, rest ? rest : "");
                        
                        if (strcmp(first_word, "join") == 0) {
                            irc_cmd_join(g_irc_state->editor, rest);
                        } else if (strcmp(first_word, "part") == 0) {
                            irc_cmd_part(g_irc_state->editor, rest);
                        } else if (strcmp(first_word, "msg") == 0) {
                            irc_cmd_msg(g_irc_state->editor, rest);
                        } else if (strcmp(first_word, "quit") == 0) {
                            irc_disconnect();
                        }
                    } else {
                        /* Regular message - send to current buffer */
                        if (strlen(g_irc_state->current_buffer) > 0) {
                            printf("[IRC] Current buffer: %s\n", g_irc_state->current_buffer);
                            irc_send_message(g_irc_state->current_buffer, g_irc_state->input_buffer);
                        } else {
                            printf("[IRC] No current buffer to send to\n");
                        }
                    }
                }
                
                /* Clear input buffer */
                g_irc_state->input_buffer[0] = '\0';
            }
            return 1; /* Consumed */
            
        case 8: /* Backspace */
            {
                size_t len = strlen(g_irc_state->input_buffer);
                if (len > 0) {
                    g_irc_state->input_buffer[len - 1] = '\0';
                }
            }
            return 1; /* Consumed */
        }
    }
    
    return 0; /* Let Vizero handle special keys (arrows, F-keys, etc.) */
}

/* Command registration array */
static vizero_plugin_command_t irc_commands[] = {
    {
        .command = "connect",
        .description = "Connect to IRC server: /connect <server[:port]> [nick]",
        .handler = irc_cmd_connect,
        .user_data = NULL
    },
    {
        .command = "disconnect",
        .description = "Disconnect from IRC server",
        .handler = irc_cmd_disconnect,
        .user_data = NULL
    },
    {
        .command = "join",
        .description = "Join IRC channel: /join <#channel>",
        .handler = irc_cmd_join,
        .user_data = NULL
    },
    {
        .command = "part",
        .description = "Leave IRC channel: /part [#channel] [reason]",
        .handler = irc_cmd_part,
        .user_data = NULL
    },
    {
        .command = "msg",
        .description = "Send private message: /msg <target> <message>",
        .handler = irc_cmd_msg,
        .user_data = NULL
    },
    {
        .command = "next",
        .description = "Switch to next IRC buffer",
        .handler = irc_cmd_next,
        .user_data = NULL
    },
    {
        .command = "prev",
        .description = "Switch to previous IRC buffer",
        .handler = irc_cmd_prev,
        .user_data = NULL
    },
    {
        .command = "buffer",
        .description = "Switch to specific IRC buffer: /buffer <name>",
        .handler = irc_cmd_buffer,
        .user_data = NULL
    },
    {
        .command = "disable",
        .description = "Disable IRC plugin and restore buffer writability",
        .handler = irc_cmd_disable,
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
)

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

#ifdef HAVE_SDL2_TTF
    /* Initialize SDL_ttf */
    if (TTF_Init() != 0) {
        printf("[IRC] Failed to initialize SDL_ttf: %s\n", TTF_GetError());
        return -1;
    }
#endif
    
    /* Create global IRC state */
    g_irc_state = calloc(1, sizeof(irc_state_t));
    if (!g_irc_state) {
        return -1;
    }
    
    /* Store editor and API references */
    g_irc_state->editor = editor;
    g_irc_state->api = api;
    
    /* Store reference to original buffer for later restoration */
    g_irc_state->original_buffer = NULL;
    g_irc_state->irc_buffer = NULL;
    g_irc_state->in_vi_command = false;
    g_irc_state->vi_command_buffer[0] = '\0';
    g_irc_state->just_escaped = false;

    
    if (g_irc_state->api && g_irc_state->api->get_current_buffer) {
        g_irc_state->original_buffer = g_irc_state->api->get_current_buffer(editor);
        printf("[IRC] Original buffer stored - will remain writable\n");
    }
    
    printf("[IRC] IRC plugin loaded - will create dedicated buffer on connect\n");
    
    /* Initialize connection state */
    g_irc_state->connection.socket = INVALID_SOCKET_VALUE;
    g_irc_state->connection.connected = false;
    g_irc_state->connection.registered = false;
    g_irc_state->buffer_count = 0;
    strcpy(g_irc_state->current_buffer, "");
    strcpy(g_irc_state->input_buffer, "");
    
    /* Initialize SDL rendering context */
    g_irc_state->sdl_renderer = NULL;
    g_irc_state->render_texture = NULL;
#ifdef HAVE_SDL2_TTF
    g_irc_state->font = NULL;
#endif
    g_irc_state->texture_width = 0;
    g_irc_state->texture_height = 0;
    g_irc_state->wants_full_window = true; /* Start in full-screen IRC mode */
    
#ifdef HAVE_SDL2_TTF
    /* Try to load a default font - check common locations */
    const char* font_paths[] = {
        "fonts/DejaVuSansMono.ttf",
        "C:/Windows/Fonts/consola.ttf",      /* Windows Consolas */
        "C:/Windows/Fonts/cour.ttf",         /* Windows Courier New */
        "/usr/share/fonts/truetype/dejavu/DejaVuSansMono.ttf",  /* Linux */
        "/System/Library/Fonts/Monaco.ttf",  /* macOS */
        NULL
    };
    
    for (int i = 0; font_paths[i] != NULL; i++) {
        g_irc_state->font = TTF_OpenFont(font_paths[i], 12);
        if (g_irc_state->font) {
            printf("[IRC] Loaded font: %s\n", font_paths[i]);
            break;
        }
    }
    
    if (!g_irc_state->font) {
        printf("[IRC] Warning: Could not load any font, text rendering will be disabled\n");
    }
#else
    printf("[IRC] Built without SDL2_ttf, using simple text rendering\n");
#endif
    
    /* Store state in plugin */
    plugin->user_data = g_irc_state;
    
    /* Register commands */
    plugin->callbacks.commands = irc_commands;
    plugin->callbacks.command_count = sizeof(irc_commands) / sizeof(irc_commands[0]);
    
    /* Register custom rendering callbacks */
    plugin->callbacks.render_full_window = irc_render_full_window;
    plugin->callbacks.wants_full_window = irc_wants_full_window;
    plugin->callbacks.on_key_input = irc_on_key_input;
    plugin->callbacks.on_command = irc_on_any_command;
    
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
    (void)plugin; /* Required by plugin API */
    if (g_irc_state) {
        /* Disconnect if connected */
        irc_disconnect();
        
        /* Clean up SDL resources */
        if (g_irc_state->render_texture) {
            SDL_DestroyTexture(g_irc_state->render_texture);
        }
        
#ifdef HAVE_SDL2_TTF
        if (g_irc_state->font) {
            TTF_CloseFont(g_irc_state->font);
        }
#endif
        
        /* Clean up buffers */
        for (size_t i = 0; i < g_irc_state->buffer_count; i++) {
            if (g_irc_state->buffers[i]) {
                irc_destroy_buffer(g_irc_state->buffers[i]);
            }
        }
        
        /* Free global state */
        free(g_irc_state);
        g_irc_state = NULL;
    }
    
#ifdef HAVE_SDL2_TTF
    /* Clean up SDL_ttf */
    TTF_Quit();
#endif
    
#ifdef _WIN32
    WSACleanup();
#endif
    
    printf("[IRC] IRC client plugin cleaned up\n");
}