/* IRC Client Plugin for Vizero - Command Registration Demo
 * This is a simplified implementation to demonstrate the command registration system
 * Full IRC functionality with SDL_net will be implemented in a future version
 */

#include "vizero/plugin_interface.h"
#include <stdio.h>
#include <stdlib.h>
#include <string.h>

/* Plugin state */
typedef struct {
    vizero_editor_t* editor;
    const vizero_editor_api_t* api;
} irc_plugin_state_t;

/* Forward declaration for API access */
static const vizero_editor_api_t* g_api = NULL;

/* Command handlers */
static int irc_command_handler(vizero_editor_t* editor, const char* args) {
    if (!args || strlen(args) == 0) {
        /* Show usage */
        if (g_api && g_api->set_status_message) {
            g_api->set_status_message(editor, "Usage: :irc <server> [channel] - Connect to IRC server");
        }
        return 0;
    }
    
    /* Parse arguments */
    char server[256] = {0};
    char channel[64] = {0};
    int parsed = sscanf(args, "%255s %63s", server, channel);
    
    if (parsed < 1) {
        if (g_api && g_api->set_status_message) {
            g_api->set_status_message(editor, "Invalid IRC command syntax");
        }
        return -1;
    }
    
    /* Create status message */
    char msg[512];
    if (strlen(channel) > 0) {
        snprintf(msg, sizeof(msg), "IRC: Would connect to %s and join %s (demo)", server, channel);
    } else {
        snprintf(msg, sizeof(msg), "IRC: Would connect to %s (demo)", server);
    }
    
    if (g_api && g_api->set_status_message) {
        g_api->set_status_message(editor, msg);
    }
    return 0;
}

static int irc_join_handler(vizero_editor_t* editor, const char* args) {
    if (!args || strlen(args) == 0) {
        if (g_api && g_api->set_status_message) {
            g_api->set_status_message(editor, "Usage: :ircjoin <channel> - Join IRC channel");
        }
        return 0;
    }
    
    char msg[256];
    snprintf(msg, sizeof(msg), "IRC: Would join channel %s (demo)", args);
    
    if (g_api && g_api->set_status_message) {
        g_api->set_status_message(editor, msg);
    }
    return 0;
}

static int irc_part_handler(vizero_editor_t* editor, const char* args) {
    const char* reason = (args && strlen(args) > 0) ? args : "Leaving";
    
    char msg[256];
    snprintf(msg, sizeof(msg), "IRC: Would leave channel with reason: %s (demo)", reason);
    
    if (g_api && g_api->set_status_message) {
        g_api->set_status_message(editor, msg);
    }
    return 0;
}

static int irc_msg_handler(vizero_editor_t* editor, const char* args) {
    if (!args || strlen(args) == 0) {
        if (g_api && g_api->set_status_message) {
            g_api->set_status_message(editor, "Usage: :ircmsg <target> <message> - Send private message");
        }
        return 0;
    }
    
    char target[64] = {0};
    const char* message = "";
    
    const char* space = strchr(args, ' ');
    if (space) {
        size_t target_len = space - args;
        if (target_len < sizeof(target)) {
            strncpy(target, args, target_len);
            target[target_len] = '\0';
            message = space + 1;
        }
    } else {
        strncpy(target, args, sizeof(target) - 1);
    }
    
    char msg[512];
    snprintf(msg, sizeof(msg), "IRC: Would send to %s: %s (demo)", target, message);
    
    if (g_api && g_api->set_status_message) {
        g_api->set_status_message(editor, msg);
    }
    return 0;
}

static int irc_help_handler(vizero_editor_t* editor, const char* args) {
    const char* help_msg = "IRC Commands: :irc <server> [channel], :ircjoin <channel>, :ircpart [reason], :ircmsg <target> <message>";
    
    if (g_api && g_api->set_status_message) {
        g_api->set_status_message(editor, help_msg);
    }
    return 0;
}

/* Command registration array */
static vizero_plugin_command_t irc_commands[] = {
    {
        .command = "irc",
        .description = "Connect to IRC server: :irc <server> [channel]",
        .handler = irc_command_handler,
        .user_data = NULL
    },
    {
        .command = "ircjoin",
        .description = "Join IRC channel: :ircjoin <channel>",
        .handler = irc_join_handler,
        .user_data = NULL
    },
    {
        .command = "ircpart",
        .description = "Leave IRC channel: :ircpart [reason]",
        .handler = irc_part_handler,
        .user_data = NULL
    },
    {
        .command = "ircmsg",
        .description = "Send private message: :ircmsg <target> <message>",
        .handler = irc_msg_handler,
        .user_data = NULL
    },
    {
        .command = "irchelp",
        .description = "Show IRC plugin help",
        .handler = irc_help_handler,
        .user_data = NULL
    }
};

/* Plugin information */
VIZERO_PLUGIN_DEFINE_INFO(
    "irc_client",                    /* name */
    "0.1.0",                        /* version */
    "Vizero Team",                  /* author */
    "IRC Client Plugin (Demo)",     /* description */
    VIZERO_PLUGIN_TYPE_GENERIC      /* type */
);

/* Plugin entry points */
int vizero_plugin_init(vizero_plugin_t* plugin, vizero_editor_t* editor, const vizero_editor_api_t* api) {
    /* Store global API reference */
    g_api = api;
    
    /* Create plugin state */
    irc_plugin_state_t* state = calloc(1, sizeof(irc_plugin_state_t));
    if (!state) {
        return -1;
    }
    
    state->editor = editor;
    state->api = api;
    
    /* Store state in plugin */
    plugin->user_data = state;
    
    /* Register commands */
    plugin->callbacks.commands = irc_commands;
    plugin->callbacks.command_count = sizeof(irc_commands) / sizeof(irc_commands[0]);
    
    printf("[IRC] Plugin initialized with %zu commands\n", plugin->callbacks.command_count);
    
    /* Log registered commands */
    for (size_t i = 0; i < plugin->callbacks.command_count; i++) {
        printf("[IRC] Registered command: %s - %s\n", 
               irc_commands[i].command, 
               irc_commands[i].description);
    }
    
    return 0;
}

void vizero_plugin_cleanup(vizero_plugin_t* plugin) {
    if (plugin && plugin->user_data) {
        free(plugin->user_data);
        plugin->user_data = NULL;
    }
    printf("[IRC] Plugin cleaned up\n");
}