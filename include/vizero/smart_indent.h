#ifndef VIZERO_SMART_INDENT_H
#define VIZERO_SMART_INDENT_H

#include <stddef.h>
#include <stdbool.h>

#ifdef __cplusplus
extern "C" {
#endif

/* Forward declarations */
typedef struct vizero_buffer_t vizero_buffer_t;
typedef struct vizero_cursor_t vizero_cursor_t;
typedef struct vizero_editor_state_t vizero_editor_state_t;

/* Indentation style */
typedef enum {
    VIZERO_INDENT_TABS,         /* Use tabs for indentation */
    VIZERO_INDENT_SPACES,       /* Use spaces for indentation */
    VIZERO_INDENT_MIXED         /* Allow mixed tabs and spaces */
} vizero_indent_style_t;

/* Language-specific indentation rules */
typedef enum {
    VIZERO_LANG_C,
    VIZERO_LANG_CPP,
    VIZERO_LANG_PYTHON,
    VIZERO_LANG_JAVASCRIPT,
    VIZERO_LANG_JAVA,
    VIZERO_LANG_CSHARP,
    VIZERO_LANG_PHP,
    VIZERO_LANG_XML,
    VIZERO_LANG_MARKDOWN,
    VIZERO_LANG_LISP,
    VIZERO_LANG_GENERIC
} vizero_language_type_t;

/* Smart indentation configuration */
typedef struct vizero_smart_indent_config_t {
    vizero_indent_style_t style;        /* Indentation style */
    size_t tab_width;                   /* Tab width in characters */
    size_t indent_size;                 /* Indentation size for spaces */
    
    /* Auto-indentation options */
    bool auto_indent;                   /* Enable automatic indentation */
    bool smart_indent;                  /* Enable smart/language-aware indentation */
    bool indent_on_paste;               /* Auto-indent pasted content */
    bool trim_trailing_whitespace;     /* Remove trailing whitespace */
    
    /* Language-specific settings */
    vizero_language_type_t language;    /* Language type for smart indentation */
    bool align_function_parameters;     /* Align function parameters */
    bool align_assignment_operators;   /* Align assignment operators */
    bool indent_case_labels;            /* Indent switch case labels */
    bool indent_namespace_contents;     /* Indent namespace contents */
} vizero_smart_indent_config_t;

/* Smart indentation context */
typedef struct vizero_smart_indent_t {
    vizero_smart_indent_config_t config;
    vizero_buffer_t* buffer;
    
    /* State tracking */
    size_t* indent_levels;              /* Cached indentation levels per line */
    size_t line_count;                  /* Number of lines cached */
    bool needs_update;                  /* Whether cache needs refresh */
} vizero_smart_indent_t;

/* Smart indentation lifecycle */
vizero_smart_indent_t* vizero_smart_indent_create(vizero_buffer_t* buffer);
void vizero_smart_indent_destroy(vizero_smart_indent_t* indent);

/* Configuration */
void vizero_smart_indent_set_config(vizero_smart_indent_t* indent, const vizero_smart_indent_config_t* config);
const vizero_smart_indent_config_t* vizero_smart_indent_get_config(const vizero_smart_indent_t* indent);

/* Language detection */
vizero_language_type_t vizero_smart_indent_detect_language(const char* filename);
void vizero_smart_indent_set_language(vizero_smart_indent_t* indent, vizero_language_type_t language);

/* Indentation operations */
int vizero_smart_indent_line(vizero_smart_indent_t* indent, size_t line_number);
int vizero_smart_indent_selection(vizero_smart_indent_t* indent, size_t start_line, size_t end_line);
int vizero_smart_indent_buffer(vizero_smart_indent_t* indent);

/* Auto-indentation on text insertion */
int vizero_smart_indent_on_newline(vizero_smart_indent_t* indent, vizero_cursor_t* cursor);
int vizero_smart_indent_on_character(vizero_smart_indent_t* indent, vizero_cursor_t* cursor, char character);
int vizero_smart_indent_on_paste(vizero_smart_indent_t* indent, size_t start_line, size_t end_line);

/* Indentation calculation */
size_t vizero_smart_indent_calculate_indent(const vizero_smart_indent_t* indent, size_t line_number);
size_t vizero_smart_indent_get_line_indent(const vizero_smart_indent_t* indent, size_t line_number);
size_t vizero_smart_indent_get_visual_column(const vizero_smart_indent_t* indent, size_t line_number, size_t column);

/* Whitespace management */
int vizero_smart_indent_convert_to_tabs(vizero_smart_indent_t* indent);
int vizero_smart_indent_convert_to_spaces(vizero_smart_indent_t* indent);
int vizero_smart_indent_trim_trailing_whitespace(vizero_smart_indent_t* indent);

/* Bracket/brace matching for indentation */
bool vizero_smart_indent_is_opening_bracket(char c);
bool vizero_smart_indent_is_closing_bracket(char c);
char vizero_smart_indent_get_matching_bracket(char c);

/* Utility functions */
bool vizero_smart_indent_is_blank_line(const vizero_smart_indent_t* indent, size_t line_number);
bool vizero_smart_indent_is_comment_line(const vizero_smart_indent_t* indent, size_t line_number);
bool vizero_smart_indent_is_string_line(const vizero_smart_indent_t* indent, size_t line_number);

/* Integration with editor state */
int vizero_smart_indent_integrate_with_editor(vizero_editor_state_t* state, vizero_buffer_t* buffer);
void vizero_smart_indent_update_after_edit(vizero_smart_indent_t* indent, size_t line, int lines_added);

#ifdef __cplusplus
}
#endif

#endif /* VIZERO_SMART_INDENT_H */