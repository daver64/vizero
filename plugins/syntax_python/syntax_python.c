/* Python syntax highlighting plugin */
#include "vizero/plugin_interface.h"
#include <string.h>
#include <stdlib.h>
#include <ctype.h>

/* Cross-platform unused attribute */
#ifdef _MSC_VER
    #define UNUSED_PARAM
#else
    #define UNUSED_PARAM __attribute__((unused))
#endif

/* Syntax highlight flags */
#define SYNTAX_BOLD       0x01
#define SYNTAX_ITALIC     0x02
#define SYNTAX_UNDERLINE  0x04

/* Token types and their colours */
typedef enum {
    TOKEN_NORMAL,
    TOKEN_KEYWORD,
    TOKEN_STRING,
    TOKEN_COMMENT,
    TOKEN_NUMBER,
    TOKEN_OPERATOR,
    TOKEN_BUILTIN,
    TOKEN_DECORATOR,
    TOKEN_FUNCTION_DEF,
    TOKEN_CLASS_DEF,
    TOKEN_IMPORT,
    TOKEN_EXCEPTION,
    TOKEN_LITERAL
} token_type_t;

/* Color definitions - Python themed colors */
static vizero_plugin_colour_t colours[] = {
    {255, 255, 255, 255}, /* TOKEN_NORMAL - white */
    {86, 156, 214, 255},  /* TOKEN_KEYWORD - blue */
    {206, 145, 120, 255}, /* TOKEN_STRING - orange */
    {106, 153, 85, 255},  /* TOKEN_COMMENT - green */
    {181, 206, 168, 255}, /* TOKEN_NUMBER - light green */
    {212, 212, 212, 255}, /* TOKEN_OPERATOR - light gray */
    {78, 201, 176, 255},  /* TOKEN_BUILTIN - cyan */
    {255, 215, 0, 255},   /* TOKEN_DECORATOR - gold */
    {220, 120, 255, 255}, /* TOKEN_FUNCTION_DEF - purple */
    {255, 140, 0, 255},   /* TOKEN_CLASS_DEF - orange */
    {86, 156, 214, 255},  /* TOKEN_IMPORT - blue */
    {255, 100, 100, 255}, /* TOKEN_EXCEPTION - red */
    {86, 156, 214, 255}   /* TOKEN_LITERAL - blue */
};

/* Python keywords */
static const char* python_keywords[] = {
    "and", "as", "assert", "break", "class", "continue", "def", "del", "elif", "else",
    "except", "finally", "for", "from", "global", "if", "import", "in", "is", "lambda",
    "nonlocal", "not", "or", "pass", "raise", "return", "try", "while", "with", "yield",
    "async", "await", NULL
};

/* Python built-in functions */
static const char* python_builtins[] = {
    "abs", "all", "any", "ascii", "bin", "bool", "bytearray", "bytes", "callable", "chr",
    "classmethod", "compile", "complex", "delattr", "dict", "dir", "divmod", "enumerate",
    "eval", "exec", "filter", "float", "format", "frozenset", "getattr", "globals",
    "hasattr", "hash", "help", "hex", "id", "input", "int", "isinstance", "issubclass",
    "iter", "len", "list", "locals", "map", "max", "memoryview", "min", "next", "object",
    "oct", "open", "ord", "pow", "print", "property", "range", "repr", "reversed",
    "round", "set", "setattr", "slice", "sorted", "staticmethod", "str", "sum", "super",
    "tuple", "type", "vars", "zip", "__import__", NULL
};

/* Python exceptions */
static const char* python_exceptions[] = {
    "Exception", "ArithmeticError", "BufferError", "LookupError", "AssertionError",
    "AttributeError", "EOFError", "FloatingPointError", "GeneratorExit", "ImportError",
    "ModuleNotFoundError", "IndexError", "KeyError", "KeyboardInterrupt", "MemoryError",
    "NameError", "NotImplementedError", "OSError", "OverflowError", "RecursionError",
    "ReferenceError", "RuntimeError", "StopIteration", "StopAsyncIteration", "SyntaxError",
    "IndentationError", "TabError", "SystemError", "SystemExit", "TypeError",
    "UnboundLocalError", "UnicodeError", "UnicodeDecodeError", "UnicodeEncodeError",
    "UnicodeTranslateError", "ValueError", "ZeroDivisionError", NULL
};

/* Python literals */
static const char* python_literals[] = {
    "True", "False", "None", "Ellipsis", "NotImplemented", NULL
};

static const vizero_editor_api_t* editor_api = NULL;

/* Simple keyword detection helper - case sensitive for Python */
static int is_keyword(const char* text, size_t len, const char** keywords) {
    for (int i = 0; keywords[i] != NULL; i++) {
        size_t keyword_len = strlen(keywords[i]);
        if (len == keyword_len && strncmp(text, keywords[i], len) == 0) {
            return 1;
        }
    }
    return 0;
}

/* Helper function to determine if file is Python or REPL buffer */
static int is_python_file(const char* filename) {
    if (!filename) return 0;
    
    /* Handle REPL buffers */
    if (strstr(filename, "*Python*") || strstr(filename, "*REPL*") || 
        strstr(filename, "python-repl") || strstr(filename, ".py-repl")) {
        return 1;
    }
    
    const char* ext = strrchr(filename, '.');
    if (!ext) return 0;
    
    return (strcmp(ext, ".py") == 0 || strcmp(ext, ".pyw") == 0 || 
            strcmp(ext, ".pyi") == 0 || strcmp(ext, ".pyx") == 0);
}

/* Python syntax highlighting */
static int UNUSED_PARAM highlight_python_line(const char* line, size_t line_num, 
                                             vizero_syntax_token_t* tokens, size_t max_tokens) {
    if (!line || !tokens) return 0;
    size_t line_len = strlen(line);
    if (line_len == 0) return 0;
    size_t i = 0;
    size_t count = 0;
    
    while (i < line_len && count < max_tokens) {
        /* Skip whitespace */
        if (isspace(line[i])) {
            i++;
            continue;
        }
        
        /* Comments */
        if (line[i] == '#') {
            vizero_syntax_token_t* token = &tokens[count++];
            token->range.start.line = line_num;
            token->range.start.column = i;
            token->range.end.line = line_num;
            token->range.end.column = line_len;
            token->colour = colours[TOKEN_COMMENT];
            token->flags = SYNTAX_ITALIC;
            break; /* Rest of line is comment */
        }
        
        /* Decorators */
        if (line[i] == '@') {
            size_t start = i;
            i++;
            while (i < line_len && !isspace(line[i])) i++;
            
            vizero_syntax_token_t* token = &tokens[count++];
            token->range.start.line = line_num;
            token->range.start.column = start;
            token->range.end.line = line_num;
            token->range.end.column = i;
            token->colour = colours[TOKEN_DECORATOR];
            token->flags = SYNTAX_BOLD;
            continue;
        }
        
        /* Triple-quoted strings */
        if (i + 3 <= line_len && (strncmp(line + i, "\"\"\"", 3) == 0 || strncmp(line + i, "'''", 3) == 0)) {
            char quote_type = line[i];
            size_t start = i;
            i += 3;
            /* Find end of triple quote or end of line */
            while (i + 3 <= line_len) {
                if (line[i] == quote_type && line[i+1] == quote_type && line[i+2] == quote_type) {
                    i += 3;
                    break;
                }
                i++;
            }
            /* If we didn't find the end, go to end of line */
            if (i + 3 > line_len) i = line_len;
            
            vizero_syntax_token_t* token = &tokens[count++];
            token->range.start.line = line_num;
            token->range.start.column = start;
            token->range.end.line = line_num;
            token->range.end.column = i;
            token->colour = colours[TOKEN_STRING];
            token->flags = 0;
            continue;
        }
        
        /* String literals */
        if (line[i] == '"' || line[i] == '\'') {
            char quote = line[i];
            size_t start = i++;
            while (i < line_len && line[i] != quote) {
                if (line[i] == '\\' && i + 1 < line_len) i++; /* Skip escaped chars */
                i++;
            }
            if (i < line_len) i++; /* Include closing quote */
            
            vizero_syntax_token_t* token = &tokens[count++];
            token->range.start.line = line_num;
            token->range.start.column = start;
            token->range.end.line = line_num;
            token->range.end.column = i;
            token->colour = colours[TOKEN_STRING];
            token->flags = 0;
            continue;
        }
        
        /* Raw strings */
        if ((line[i] == 'r' || line[i] == 'R') && i + 1 < line_len && 
            (line[i + 1] == '"' || line[i + 1] == '\'')) {
            char quote = line[i + 1];
            size_t start = i;
            i += 2; /* Skip r" or r' */
            while (i < line_len && line[i] != quote) i++;
            if (i < line_len) i++; /* Include closing quote */
            
            vizero_syntax_token_t* token = &tokens[count++];
            token->range.start.line = line_num;
            token->range.start.column = start;
            token->range.end.line = line_num;
            token->range.end.column = i;
            token->colour = colours[TOKEN_STRING];
            token->flags = 0;
            continue;
        }
        
        /* Numbers */
        if (isdigit(line[i]) || (line[i] == '.' && i + 1 < line_len && isdigit(line[i + 1]))) {
            size_t start = i;
            
            /* Handle hex, oct, bin */
            if (line[i] == '0' && i + 1 < line_len) {
                if (line[i + 1] == 'x' || line[i + 1] == 'X') {
                    i += 2;
                    while (i < line_len && isxdigit(line[i])) i++;
                } else if (line[i + 1] == 'o' || line[i + 1] == 'O') {
                    i += 2;
                    while (i < line_len && line[i] >= '0' && line[i] <= '7') i++;
                } else if (line[i + 1] == 'b' || line[i + 1] == 'B') {
                    i += 2;
                    while (i < line_len && (line[i] == '0' || line[i] == '1')) i++;
                } else {
                    /* Regular number */
                    while (i < line_len && (isdigit(line[i]) || line[i] == '.' || 
                                          line[i] == 'e' || line[i] == 'E' || 
                                          line[i] == '+' || line[i] == '-')) i++;
                }
            } else {
                /* Regular number */
                while (i < line_len && (isdigit(line[i]) || line[i] == '.' || 
                                      line[i] == 'e' || line[i] == 'E' || 
                                      line[i] == '+' || line[i] == '-')) i++;
            }
            
            vizero_syntax_token_t* token = &tokens[count++];
            token->range.start.line = line_num;
            token->range.start.column = start;
            token->range.end.line = line_num;
            token->range.end.column = i;
            token->colour = colours[TOKEN_NUMBER];
            token->flags = 0;
            continue;
        }
        
        /* Identifiers and keywords */
        if (isalpha(line[i]) || line[i] == '_') {
            size_t start = i;
            while (i < line_len && (isalnum(line[i]) || line[i] == '_')) i++;
            size_t len = i - start;
            
            token_type_t type = TOKEN_NORMAL;
            if (is_keyword(line + start, len, python_keywords)) {
                type = TOKEN_KEYWORD;
            } else if (is_keyword(line + start, len, python_builtins)) {
                type = TOKEN_BUILTIN;
            } else if (is_keyword(line + start, len, python_exceptions)) {
                type = TOKEN_EXCEPTION;
            } else if (is_keyword(line + start, len, python_literals)) {
                type = TOKEN_LITERAL;
            }
            
            /* Check for function/class definitions */
            if (type == TOKEN_KEYWORD) {
                if (strncmp(line + start, "def", 3) == 0 || strncmp(line + start, "class", 5) == 0) {
                    type = (strncmp(line + start, "def", 3) == 0) ? TOKEN_FUNCTION_DEF : TOKEN_CLASS_DEF;
                } else if (strncmp(line + start, "import", 6) == 0 || strncmp(line + start, "from", 4) == 0) {
                    type = TOKEN_IMPORT;
                }
            }
            
            if (type != TOKEN_NORMAL) {
                vizero_syntax_token_t* token = &tokens[count++];
                token->range.start.line = line_num;
                token->range.start.column = start;
                token->range.end.line = line_num;
                token->range.end.column = i;
                token->colour = colours[type];
                token->flags = (type == TOKEN_KEYWORD || type == TOKEN_FUNCTION_DEF || 
                               type == TOKEN_CLASS_DEF) ? SYNTAX_BOLD : 0;
            }
            continue;
        }
        
        /* Operators */
        if (strchr("+-*/%=<>!&|^~?:;,.(){}[]", line[i])) {
            vizero_syntax_token_t* token = &tokens[count++];
            token->range.start.line = line_num;
            token->range.start.column = i;
            token->range.end.line = line_num;
            token->range.end.column = i + 1;
            token->colour = colours[TOKEN_OPERATOR];
            token->flags = 0;
        }
        
        i++;
    }
    return (int)count;
}

/* Main syntax highlighting function */
static int highlight_syntax(vizero_buffer_t* buffer, size_t start_line, size_t end_line,
                           vizero_syntax_token_t* tokens, size_t max_tokens, size_t* token_count) {
    if (!buffer || !tokens || !token_count || !editor_api) return 0;
    if (!editor_api->get_buffer_line || !editor_api->get_buffer_filename) return 0;

    const char* filename = editor_api->get_buffer_filename(buffer);
    
    /* Only handle Python files and REPL buffers */
    if (!is_python_file(filename)) return 0;
    
    size_t count = 0;
    for (size_t line = start_line; line <= end_line && count < max_tokens; line++) {
        const char* line_text = editor_api->get_buffer_line(buffer, line);
        if (!line_text) continue;
        
        int n = highlight_python_line(line_text, line, tokens + count, max_tokens - count);
        if (n > 0) count += n;
    }
    *token_count = count;
    return 0;
}

VIZERO_PLUGIN_DEFINE_INFO(
    "Python Syntax Highlighter",
    "1.0.0",
    "Vizero Team", 
    "Syntax highlighting plugin for Python files and REPL buffers",
    VIZERO_PLUGIN_TYPE_SYNTAX_HIGHLIGHTER
)

int vizero_plugin_init(vizero_plugin_t* plugin, vizero_editor_t* editor, const vizero_editor_api_t* api) {
    if (!plugin || !editor || !api) return -1;
    editor_api = api;
    plugin->callbacks.highlight_syntax = highlight_syntax;
    return 0;
}

void vizero_plugin_cleanup(vizero_plugin_t* plugin) {
    (void)plugin;
    editor_api = NULL;
}