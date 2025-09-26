/* JavaScript syntax highlighting plugin */
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
    TOKEN_FUNCTION,
    TOKEN_CLASS,
    TOKEN_IMPORT,
    TOKEN_REGEX,
    TOKEN_TEMPLATE,
    TOKEN_PROPERTY,
    TOKEN_CONSTANT,
    TOKEN_TYPE
} token_type_t;

/* Color definitions - JavaScript themed colors */
static vizero_plugin_colour_t colours[] = {
    {255, 255, 255, 255}, /* TOKEN_NORMAL - white */
    {86, 156, 214, 255},  /* TOKEN_KEYWORD - blue */
    {206, 145, 120, 255}, /* TOKEN_STRING - orange */
    {106, 153, 85, 255},  /* TOKEN_COMMENT - green */
    {181, 206, 168, 255}, /* TOKEN_NUMBER - light green */
    {212, 212, 212, 255}, /* TOKEN_OPERATOR - light gray */
    {78, 201, 176, 255},  /* TOKEN_BUILTIN - cyan */
    {220, 220, 170, 255}, /* TOKEN_FUNCTION - yellow */
    {78, 201, 176, 255},  /* TOKEN_CLASS - cyan */
    {199, 146, 234, 255}, /* TOKEN_IMPORT - purple */
    {214, 157, 133, 255}, /* TOKEN_REGEX - brown */
    {255, 198, 109, 255}, /* TOKEN_TEMPLATE - gold */
    {156, 220, 254, 255}, /* TOKEN_PROPERTY - light blue */
    {100, 149, 237, 255}, /* TOKEN_CONSTANT - cornflower blue */
    {86, 156, 214, 255}   /* TOKEN_TYPE - blue */
};

/* JavaScript keywords */
static const char* js_keywords[] = {
    "async", "await", "break", "case", "catch", "class", "const", "continue", "debugger",
    "default", "delete", "do", "else", "export", "extends", "finally", "for", "function",
    "if", "import", "in", "instanceof", "let", "new", "return", "super", "switch", "this",
    "throw", "try", "typeof", "var", "void", "while", "with", "yield", NULL
};

/* JavaScript built-in objects and functions */
static const char* js_builtins[] = {
    "Array", "ArrayBuffer", "BigInt", "Boolean", "DataView", "Date", "Error", "EvalError",
    "Float32Array", "Float64Array", "Function", "Generator", "GeneratorFunction", "Infinity",
    "Int8Array", "Int16Array", "Int32Array", "Intl", "JSON", "Map", "Math", "NaN", "Number",
    "Object", "Promise", "Proxy", "RangeError", "ReferenceError", "Reflect", "RegExp", "Set",
    "String", "Symbol", "SyntaxError", "TypeError", "Uint8Array", "Uint8ClampedArray",
    "Uint16Array", "Uint32Array", "URIError", "WeakMap", "WeakSet", "console", "decodeURI",
    "decodeURIComponent", "encodeURI", "encodeURIComponent", "escape", "eval", "isFinite",
    "isNaN", "parseFloat", "parseInt", "unescape", "undefined", NULL
};

/* JavaScript constants */
static const char* js_constants[] = {
    "true", "false", "null", "undefined", "Infinity", "NaN", NULL
};

/* DOM/Browser APIs (common ones) */
static const char* js_dom_apis[] = {
    "document", "window", "navigator", "location", "history", "screen", "localStorage",
    "sessionStorage", "XMLHttpRequest", "fetch", "setTimeout", "setInterval", "clearTimeout",
    "clearInterval", "alert", "confirm", "prompt", "console", NULL
};

static const vizero_editor_api_t* editor_api = NULL;

/* Simple keyword detection helper - case sensitive for JavaScript */
static int is_keyword(const char* text, size_t len, const char** keywords) {
    for (int i = 0; keywords[i] != NULL; i++) {
        size_t keyword_len = strlen(keywords[i]);
        if (len == keyword_len && strncmp(text, keywords[i], len) == 0) {
            return 1;
        }
    }
    return 0;
}

/* Helper function to determine if file is JavaScript */
static int is_javascript_file(const char* filename) {
    if (!filename) return 0;
    
    /* Handle REPL buffers and special buffers */
    if (strstr(filename, "*JavaScript*") || strstr(filename, "*JS*") || 
        strstr(filename, "javascript-repl") || strstr(filename, ".js-repl")) {
        return 1;
    }
    
    const char* ext = strrchr(filename, '.');
    if (!ext) return 0;
    
    return (strcmp(ext, ".js") == 0 || strcmp(ext, ".jsx") == 0 || 
            strcmp(ext, ".mjs") == 0 || strcmp(ext, ".cjs") == 0 ||
            strcmp(ext, ".ts") == 0 || strcmp(ext, ".tsx") == 0);
}

/* Check if character can be part of identifier */
static int is_identifier_char(char c) {
    return isalnum(c) || c == '_' || c == '$';
}

/* JavaScript syntax highlighting */
static int UNUSED_PARAM highlight_javascript_line(const char* line, size_t line_num, 
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
        
        /* Single line comments */
        if (i + 1 < line_len && line[i] == '/' && line[i + 1] == '/') {
            vizero_syntax_token_t* token = &tokens[count++];
            token->range.start.line = line_num;
            token->range.start.column = i;
            token->range.end.line = line_num;
            token->range.end.column = line_len;
            token->colour = colours[TOKEN_COMMENT];
            token->flags = SYNTAX_ITALIC;
            break; /* Rest of line is comment */
        }
        
        /* Multi-line comments start */
        if (i + 1 < line_len && line[i] == '/' && line[i + 1] == '*') {
            size_t start = i;
            i += 2;
            /* Find end of comment or end of line */
            while (i + 1 < line_len) {
                if (line[i] == '*' && line[i + 1] == '/') {
                    i += 2;
                    break;
                }
                i++;
            }
            if (i + 1 >= line_len) i = line_len; /* Go to end if not found */
            
            vizero_syntax_token_t* token = &tokens[count++];
            token->range.start.line = line_num;
            token->range.start.column = start;
            token->range.end.line = line_num;
            token->range.end.column = i;
            token->colour = colours[TOKEN_COMMENT];
            token->flags = SYNTAX_ITALIC;
            continue;
        }
        
        /* Template literals */
        if (line[i] == '`') {
            size_t start = i++;
            while (i < line_len && line[i] != '`') {
                if (line[i] == '\\' && i + 1 < line_len) i++; /* Skip escaped chars */
                i++;
            }
            if (i < line_len) i++; /* Include closing backtick */
            
            vizero_syntax_token_t* token = &tokens[count++];
            token->range.start.line = line_num;
            token->range.start.column = start;
            token->range.end.line = line_num;
            token->range.end.column = i;
            token->colour = colours[TOKEN_TEMPLATE];
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
        
        /* Regular expressions - basic detection */
        if (line[i] == '/' && i > 0 && (line[i-1] == '=' || line[i-1] == '(' || 
                                       line[i-1] == '[' || line[i-1] == ',' || 
                                       line[i-1] == ':' || line[i-1] == ';' ||
                                       line[i-1] == '!' || line[i-1] == '&' ||
                                       line[i-1] == '|' || line[i-1] == '?' ||
                                       line[i-1] == '{' || line[i-1] == '}' ||
                                       line[i-1] == ' ' || line[i-1] == '\t')) {
            size_t start = i++;
            while (i < line_len && line[i] != '/') {
                if (line[i] == '\\' && i + 1 < line_len) i++; /* Skip escaped chars */
                i++;
            }
            if (i < line_len) {
                i++; /* Include closing slash */
                /* Include regex flags */
                while (i < line_len && (line[i] == 'g' || line[i] == 'i' || line[i] == 'm' ||
                                       line[i] == 's' || line[i] == 'u' || line[i] == 'y')) {
                    i++;
                }
            }
            
            vizero_syntax_token_t* token = &tokens[count++];
            token->range.start.line = line_num;
            token->range.start.column = start;
            token->range.end.line = line_num;
            token->range.end.column = i;
            token->colour = colours[TOKEN_REGEX];
            token->flags = 0;
            continue;
        }
        
        /* Numbers */
        if (isdigit(line[i]) || (line[i] == '.' && i + 1 < line_len && isdigit(line[i + 1]))) {
            size_t start = i;
            
            /* Handle hex, octal, binary */
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
                    /* Regular number or float */
                    while (i < line_len && (isdigit(line[i]) || line[i] == '.' || 
                                          line[i] == 'e' || line[i] == 'E' || 
                                          line[i] == '+' || line[i] == '-')) i++;
                }
            } else {
                /* Regular number or float */
                while (i < line_len && (isdigit(line[i]) || line[i] == '.' || 
                                      line[i] == 'e' || line[i] == 'E' || 
                                      line[i] == '+' || line[i] == '-')) i++;
            }
            
            /* Check for number suffixes (BigInt) */
            if (i < line_len && line[i] == 'n') i++;
            
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
        if (isalpha(line[i]) || line[i] == '_' || line[i] == '$') {
            size_t start = i;
            while (i < line_len && is_identifier_char(line[i])) i++;
            size_t len = i - start;
            
            token_type_t type = TOKEN_NORMAL;
            if (is_keyword(line + start, len, js_keywords)) {
                type = TOKEN_KEYWORD;
            } else if (is_keyword(line + start, len, js_builtins)) {
                type = TOKEN_BUILTIN;
            } else if (is_keyword(line + start, len, js_constants)) {
                type = TOKEN_CONSTANT;
            } else if (is_keyword(line + start, len, js_dom_apis)) {
                type = TOKEN_BUILTIN;
            }
            
            /* Check for function/class definitions */
            if (type == TOKEN_KEYWORD) {
                if (strncmp(line + start, "function", 8) == 0) {
                    type = TOKEN_FUNCTION;
                } else if (strncmp(line + start, "class", 5) == 0) {
                    type = TOKEN_CLASS;
                } else if (strncmp(line + start, "import", 6) == 0 || 
                          strncmp(line + start, "export", 6) == 0) {
                    type = TOKEN_IMPORT;
                }
            }
            
            /* Check if followed by parentheses (function call) */
            if (type == TOKEN_NORMAL) {
                size_t j = i;
                while (j < line_len && isspace(line[j])) j++;
                if (j < line_len && line[j] == '(') {
                    type = TOKEN_FUNCTION;
                }
            }
            
            /* Check if preceded by dot (property access) */
            if (type == TOKEN_NORMAL && start > 0) {
                size_t j = start - 1;
                while (j > 0 && isspace(line[j])) j--;
                if (line[j] == '.') {
                    type = TOKEN_PROPERTY;
                }
            }
            
            if (type != TOKEN_NORMAL) {
                vizero_syntax_token_t* token = &tokens[count++];
                token->range.start.line = line_num;
                token->range.start.column = start;
                token->range.end.line = line_num;
                token->range.end.column = i;
                token->colour = colours[type];
                token->flags = (type == TOKEN_KEYWORD || type == TOKEN_FUNCTION || 
                               type == TOKEN_CLASS) ? SYNTAX_BOLD : 0;
            }
            continue;
        }
        
        /* Operators and punctuation */
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
    if (!buffer || !tokens || !editor_api) return 0;
    if (!editor_api->get_buffer_line || !editor_api->get_buffer_filename) return 0;
    
    const char* filename = editor_api->get_buffer_filename(buffer);
    
    /* Only handle JavaScript files and REPL buffers */
    if (!is_javascript_file(filename)) return 0;
    
    size_t tokens_used = 0;
    for (size_t line = start_line; line <= end_line && tokens_used < max_tokens; line++) {
        const char* line_text = editor_api->get_buffer_line(buffer, line);
        if (!line_text) continue;
        
        int n = highlight_javascript_line(line_text, line, tokens + tokens_used, max_tokens - tokens_used);
        if (n > 0) tokens_used += n;
    }
    
    if (token_count) *token_count = tokens_used;
    return 0; /* Return 0 for success */
}

VIZERO_PLUGIN_DEFINE_INFO(
    "JavaScript Syntax Highlighter",
    "1.0.0",
    "Vizero Team", 
    "Syntax highlighting plugin for JavaScript, TypeScript, and JSX files",
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