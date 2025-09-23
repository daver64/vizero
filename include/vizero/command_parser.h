#ifndef VIZERO_COMMAND_PARSER_H
#define VIZERO_COMMAND_PARSER_H

#ifdef __cplusplus
extern "C" {
#endif

/* Forward declarations */
typedef struct vizero_command_parser_t vizero_command_parser_t;
typedef struct vizero_editor_state_t vizero_editor_state_t;

/* Command structure */
typedef struct {
    const char* name;
    const char* args;
    int arg_count;
} vizero_command_t;

/* Command parser creation and destruction */
vizero_command_parser_t* vizero_command_parser_create(void);
void vizero_command_parser_destroy(vizero_command_parser_t* parser);

/* Command parsing */
int vizero_command_parser_parse(vizero_command_parser_t* parser, const char* input, vizero_command_t* command);
int vizero_command_parser_execute(vizero_command_parser_t* parser, vizero_editor_state_t* state, const vizero_command_t* command);

#ifdef __cplusplus
}
#endif

#endif /* VIZERO_COMMAND_PARSER_H */