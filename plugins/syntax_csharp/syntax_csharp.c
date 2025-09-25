/* C# syntax highlighting plugin */
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
    TOKEN_PREPROCESSOR,
    TOKEN_TYPE,
    TOKEN_MODIFIER,
    TOKEN_ATTRIBUTE,
    TOKEN_LITERAL
} token_type_t;

/* Simple keyword detection helper - case sensitive for C# */
static int is_keyword(const char* text, size_t len, const char** keywords) {
    for (int i = 0; keywords[i] != NULL; i++) {
        size_t keyword_len = strlen(keywords[i]);
        if (len == keyword_len && strncmp(text, keywords[i], len) == 0) {
            return 1;
        }
    }
    return 0;
}

/* Color definitions - C# themed colors */
static vizero_plugin_colour_t colours[] = {
    {255, 255, 255, 255}, /* TOKEN_NORMAL - white */
    {86, 156, 214, 255},  /* TOKEN_KEYWORD - blue */
    {214, 157, 133, 255}, /* TOKEN_STRING - orange */
    {106, 153, 85, 255},  /* TOKEN_COMMENT - green */
    {181, 206, 168, 255}, /* TOKEN_NUMBER - light green */
    {212, 212, 212, 255}, /* TOKEN_OPERATOR - light gray */
    {155, 155, 155, 255}, /* TOKEN_PREPROCESSOR - gray */
    {78, 201, 176, 255},  /* TOKEN_TYPE - cyan */
    {86, 156, 214, 255},  /* TOKEN_MODIFIER - blue */
    {255, 215, 0, 255},   /* TOKEN_ATTRIBUTE - gold */
    {86, 156, 214, 255}   /* TOKEN_LITERAL - blue */
};

/* C# keywords */
static const char* csharp_keywords[] = {
    "abstract", "as", "base", "break", "case", "catch", "checked", "class",
    "const", "continue", "default", "delegate", "do", "else", "enum", "event",
    "explicit", "extern", "finally", "fixed", "for", "foreach", "goto", "if",
    "implicit", "in", "interface", "internal", "is", "lock", "namespace", "new",
    "operator", "out", "override", "params", "readonly", "ref", "return",
    "sealed", "sizeof", "stackalloc", "static", "struct", "switch", "this",
    "throw", "try", "typeof", "unchecked", "unsafe", "using", "virtual",
    "volatile", "while", "yield",
    NULL
};

/* C# access modifiers */
static const char* csharp_modifiers[] = {
    "public", "private", "protected", "internal", "partial", "async", "await",
    NULL
};

/* C# built-in types */
static const char* csharp_types[] = {
    "bool", "byte", "char", "decimal", "double", "float", "int", "long",
    "object", "sbyte", "short", "string", "uint", "ulong", "ushort", "void",
    "var", "dynamic", "nint", "nuint",
    NULL
};

/* C# literals */
static const char* csharp_literals[] = {
    "true", "false", "null",
    NULL
};

static const vizero_editor_api_t* editor_api = NULL;

/* Helper function to determine if file is C# */
static int is_csharp_file(const char* filename) {
    if (!filename) return 0;
    
    const char* ext = strrchr(filename, '.');
    if (!ext) return 0;
    
    return (strcmp(ext, ".cs") == 0);
}

/* C# syntax highlighting */
static int UNUSED_PARAM highlight_csharp_line(const char* line, size_t line_num, 
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
        
        /* Single-line comments */
        if (i < line_len - 1 && line[i] == '/' && line[i + 1] == '/') {
            vizero_syntax_token_t* token = &tokens[count++];
            token->range.start.line = line_num;
            token->range.start.column = i;
            token->range.end.line = line_num;
            token->range.end.column = line_len;
            token->colour = colours[TOKEN_COMMENT];
            token->flags = SYNTAX_ITALIC;
            break; /* Rest of line is comment */
        }
        
        /* Multi-line comments */
        if (i < line_len - 1 && line[i] == '/' && line[i + 1] == '*') {
            vizero_syntax_token_t* token = &tokens[count++];
            token->range.start.line = line_num;
            token->range.start.column = i;
            token->range.end.line = line_num;
            token->range.end.column = line_len;
            token->colour = colours[TOKEN_COMMENT];
            token->flags = SYNTAX_ITALIC;
            break; /* Rest of line is comment */
        }
        
        /* Preprocessor directives */
        if (line[i] == '#') {
            vizero_syntax_token_t* token = &tokens[count++];
            token->range.start.line = line_num;
            token->range.start.column = i;
            token->range.end.line = line_num;
            token->range.end.column = line_len;
            token->colour = colours[TOKEN_PREPROCESSOR];
            token->flags = 0;
            break; /* Rest of line is preprocessor */
        }
        
        /* Attributes */
        if (line[i] == '[') {
            size_t start = i;
            i++; 
            while (i < line_len && line[i] != ']') i++;
            if (i < line_len) i++; /* Include closing bracket */
            
            vizero_syntax_token_t* token = &tokens[count++];
            token->range.start.line = line_num;
            token->range.start.column = start;
            token->range.end.line = line_num;
            token->range.end.column = i;
            token->colour = colours[TOKEN_ATTRIBUTE];
            token->flags = 0;
            continue;
        }
        
        /* String literals */
        if (line[i] == '"') {
            size_t start = i++;
            while (i < line_len && line[i] != '"') {
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
        
        /* Verbatim string literals */
        if (line[i] == '@' && i + 1 < line_len && line[i + 1] == '"') {
            size_t start = i;
            i += 2; /* Skip @" */
            while (i < line_len && line[i] != '"') i++;
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
        
        /* Character literals */
        if (line[i] == '\'') {
            size_t start = i++;
            while (i < line_len && line[i] != '\'') {
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
        
        /* Numbers */
        if (isdigit(line[i]) || (line[i] == '0' && i + 1 < line_len && 
                                (line[i + 1] == 'x' || line[i + 1] == 'X'))) {
            size_t start = i;
            if (line[i] == '0' && i + 1 < line_len && (line[i + 1] == 'x' || line[i + 1] == 'X')) {
                i += 2; /* Skip 0x */
                while (i < line_len && isxdigit(line[i])) i++;
            } else {
                while (i < line_len && (isdigit(line[i]) || line[i] == '.' || line[i] == 'f' || line[i] == 'F' || 
                                       line[i] == 'd' || line[i] == 'D' || line[i] == 'm' || line[i] == 'M')) i++;
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
            if (is_keyword(line + start, len, csharp_keywords)) {
                type = TOKEN_KEYWORD;
            } else if (is_keyword(line + start, len, csharp_modifiers)) {
                type = TOKEN_MODIFIER;
            } else if (is_keyword(line + start, len, csharp_types)) {
                type = TOKEN_TYPE;
            } else if (is_keyword(line + start, len, csharp_literals)) {
                type = TOKEN_LITERAL;
            }
            
            if (type != TOKEN_NORMAL) {
                vizero_syntax_token_t* token = &tokens[count++];
                token->range.start.line = line_num;
                token->range.start.column = start;
                token->range.end.line = line_num;
                token->range.end.column = i;
                token->colour = colours[type];
                token->flags = (type == TOKEN_KEYWORD || type == TOKEN_MODIFIER) ? SYNTAX_BOLD : 0;
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
                           vizero_syntax_token_t* tokens, size_t max_tokens) {
    if (!buffer || !tokens || !editor_api) return 0;
    if (!editor_api->get_buffer_line || !editor_api->get_buffer_filename) return 0;
    
    const char* filename = editor_api->get_buffer_filename(buffer);
    
    /* Only handle C# files */
    if (!is_csharp_file(filename)) return 0;
    
    size_t token_count = 0;
    for (size_t line = start_line; line <= end_line && token_count < max_tokens; line++) {
        const char* line_text = editor_api->get_buffer_line(buffer, line);
        if (!line_text) continue;
        
        int n = highlight_csharp_line(line_text, line, tokens + token_count, max_tokens - token_count);
        if (n > 0) token_count += n;
    }
    return (int)token_count;
}

VIZERO_PLUGIN_DEFINE_INFO(
    "C# Syntax Highlighter",
    "1.0.0",
    "Vizero Team", 
    "Syntax highlighting plugin for C# files",
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