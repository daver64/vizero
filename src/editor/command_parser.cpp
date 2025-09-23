/* Command parser implementation */
#include "vizero/command_parser.h"
#include "vizero/editor_state.h"
#include "vizero/application.h"
#include <stdlib.h>
#include <string.h>
#include <stdio.h>

struct vizero_command_parser_t { 
    char command_buffer[256];
    char args_buffer[256];
};

vizero_command_parser_t* vizero_command_parser_create(void) {
    vizero_command_parser_t* parser = (vizero_command_parser_t*)calloc(1, sizeof(vizero_command_parser_t));
    return parser;
}

void vizero_command_parser_destroy(vizero_command_parser_t* parser) {
    free(parser);
}

int vizero_command_parser_parse(vizero_command_parser_t* parser, const char* input, vizero_command_t* command) {
    if (!parser || !input || !command) return -1;
    
    /* Skip leading colon if present */
    const char* cmd_start = input;
    if (cmd_start[0] == ':') {
        cmd_start++;
    }
    
    /* Parse command name */
    const char* space = strchr(cmd_start, ' ');
    size_t cmd_len = space ? (size_t)(space - cmd_start) : strlen(cmd_start);
    
    if (cmd_len >= sizeof(parser->command_buffer)) return -1;
    
    memcpy(parser->command_buffer, cmd_start, cmd_len);
    parser->command_buffer[cmd_len] = '\0';
    
    /* Parse arguments */
    if (space) {
        strncpy(parser->args_buffer, space + 1, sizeof(parser->args_buffer) - 1);
        parser->args_buffer[sizeof(parser->args_buffer) - 1] = '\0';
    } else {
        parser->args_buffer[0] = '\0';
    }
    
    command->name = parser->command_buffer;
    command->args = parser->args_buffer;
    command->arg_count = space ? 1 : 0;
    
    return 0;
}

int vizero_command_parser_execute(vizero_command_parser_t* parser, vizero_editor_state_t* state, const vizero_command_t* command) {
    if (!parser || !state || !command || !command->name) return -1;
    
    /* Handle quit command */
    if (strcmp(command->name, "q") == 0 || strcmp(command->name, "quit") == 0) {
        /* Use the editor's existing execute command function */
        return vizero_editor_execute_command(state, "q");
    }
    
    /* Handle write command */
    if (strcmp(command->name, "w") == 0 || strcmp(command->name, "write") == 0) {
        return vizero_editor_execute_command(state, "w");
    }
    
    /* Handle write and quit */
    if (strcmp(command->name, "wq") == 0) {
        return vizero_editor_execute_command(state, "wq");
    }
    
    /* Unknown command */
    return -1;
}