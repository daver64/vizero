/* PHP syntax highlighting plugin */
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
    TOKEN_VARIABLE,
    TOKEN_FUNCTION,
    TOKEN_CLASS,
    TOKEN_CONSTANT,
    TOKEN_BUILTIN,
    TOKEN_SUPERGLOBAL,
    TOKEN_TAG,
    TOKEN_DOCBLOCK,
    TOKEN_TYPE_HINT,
    TOKEN_NAMESPACE
} token_type_t;

/* Color definitions - PHP themed colors */
static vizero_plugin_colour_t colours[] = {
    {255, 255, 255, 255}, /* TOKEN_NORMAL - white */
    {86, 156, 214, 255},  /* TOKEN_KEYWORD - blue */
    {206, 145, 120, 255}, /* TOKEN_STRING - orange */
    {106, 153, 85, 255},  /* TOKEN_COMMENT - green */
    {181, 206, 168, 255}, /* TOKEN_NUMBER - light green */
    {212, 212, 212, 255}, /* TOKEN_OPERATOR - light gray */
    {156, 220, 254, 255}, /* TOKEN_VARIABLE - light blue */
    {220, 120, 255, 255}, /* TOKEN_FUNCTION - purple */
    {255, 140, 0, 255},   /* TOKEN_CLASS - orange */
    {78, 201, 176, 255},  /* TOKEN_CONSTANT - cyan */
    {255, 215, 0, 255},   /* TOKEN_BUILTIN - gold */
    {255, 100, 100, 255}, /* TOKEN_SUPERGLOBAL - red */
    {128, 128, 128, 255}, /* TOKEN_TAG - gray */
    {106, 153, 85, 255},  /* TOKEN_DOCBLOCK - green (same as comment) */
    {86, 156, 214, 255},  /* TOKEN_TYPE_HINT - blue */
    {220, 220, 170, 255}  /* TOKEN_NAMESPACE - light yellow */
};

/* PHP keywords */
static const char* php_keywords[] = {
    "abstract", "and", "array", "as", "break", "callable", "case", "catch", "class", "clone",
    "const", "continue", "declare", "default", "die", "do", "echo", "else", "elseif", "empty",
    "enddeclare", "endfor", "endforeach", "endif", "endswitch", "endwhile", "eval", "exit",
    "extends", "final", "finally", "for", "foreach", "function", "global", "goto", "if",
    "implements", "include", "include_once", "instanceof", "insteadof", "interface", "isset",
    "list", "namespace", "new", "or", "print", "private", "protected", "public", "require",
    "require_once", "return", "static", "switch", "throw", "trait", "try", "unset", "use",
    "var", "while", "xor", "yield", "yield from", "__halt_compiler", NULL
};

/* PHP built-in functions (common ones) */
static const char* php_builtins[] = {
    "strlen", "strpos", "substr", "str_replace", "explode", "implode", "trim", "ltrim", "rtrim",
    "strtolower", "strtoupper", "ucfirst", "ucwords", "sprintf", "printf", "number_format",
    "count", "array_push", "array_pop", "array_shift", "array_unshift", "array_merge",
    "array_keys", "array_values", "array_slice", "array_splice", "sort", "asort", "ksort",
    "in_array", "array_search", "array_key_exists", "is_array", "is_string", "is_int",
    "is_float", "is_bool", "is_null", "is_numeric", "is_object", "is_callable", "is_resource",
    "isset", "empty", "unset", "var_dump", "print_r", "serialize", "unserialize",
    "file_get_contents", "file_put_contents", "fopen", "fclose", "fread", "fwrite",
    "dirname", "basename", "pathinfo", "realpath", "file_exists", "is_file", "is_dir",
    "mkdir", "rmdir", "unlink", "copy", "rename", "chmod", "time", "date", "strtotime",
    "microtime", "sleep", "usleep", "rand", "mt_rand", "md5", "sha1", "hash", "base64_encode",
    "base64_decode", "urlencode", "urldecode", "htmlspecialchars", "htmlentities",
    "strip_tags", "nl2br", "json_encode", "json_decode", "preg_match", "preg_replace",
    "preg_split", "mysql_connect", "mysqli_connect", "pdo", NULL
};

/* PHP superglobals */
static const char* php_superglobals[] = {
    "$_GET", "$_POST", "$_REQUEST", "$_SESSION", "$_COOKIE", "$_SERVER", "$_ENV", "$_FILES",
    "$GLOBALS", "$argc", "$argv", NULL
};

/* PHP constants */
static const char* php_constants[] = {
    "TRUE", "FALSE", "NULL", "__FILE__", "__LINE__", "__DIR__", "__FUNCTION__", "__CLASS__",
    "__TRAIT__", "__METHOD__", "__NAMESPACE__", "PHP_VERSION", "PHP_OS", "PHP_SAPI",
    "PHP_EOL", "PHP_INT_MAX", "PHP_INT_MIN", "PHP_FLOAT_MAX", "PHP_FLOAT_MIN", NULL
};

/* PHP type hints */
static const char* php_type_hints[] = {
    "string", "int", "float", "bool", "array", "object", "callable", "iterable", "void",
    "mixed", "never", "self", "parent", "static", NULL
};

static const vizero_editor_api_t* editor_api = NULL;

/* Cross-platform case-insensitive string comparison */
#ifdef _WIN32
#define strncasecmp _strnicmp
#define strcasecmp _stricmp
#endif

/* Simple keyword detection helper - case insensitive for PHP */
static int is_keyword(const char* text, size_t len, const char** keywords) {
    for (int i = 0; keywords[i] != NULL; i++) {
        size_t keyword_len = strlen(keywords[i]);
        if (len == keyword_len && strncasecmp(text, keywords[i], len) == 0) {
            return 1;
        }
    }
    return 0;
}

/* Case sensitive keyword detection for constants and superglobals */
static int is_keyword_case_sensitive(const char* text, size_t len, const char** keywords) {
    for (int i = 0; keywords[i] != NULL; i++) {
        size_t keyword_len = strlen(keywords[i]);
        if (len == keyword_len && strncmp(text, keywords[i], len) == 0) {
            return 1;
        }
    }
    return 0;
}

/* Helper function to determine if file is PHP */
static int is_php_file(const char* filename) {
    if (!filename) return 0;
    
    /* Handle PHP REPL buffers */
    if (strstr(filename, "*PHP*") || strstr(filename, "*php*") || 
        strstr(filename, "php-repl") || strstr(filename, ".php-repl")) {
        return 1;
    }
    
    const char* ext = strrchr(filename, '.');
    if (!ext) return 0;
    
    return (strcasecmp(ext, ".php") == 0 || strcasecmp(ext, ".phtml") == 0 || 
            strcasecmp(ext, ".php3") == 0 || strcasecmp(ext, ".php4") == 0 ||
            strcasecmp(ext, ".php5") == 0 || strcasecmp(ext, ".phps") == 0);
}

/* PHP syntax highlighting */
static int UNUSED_PARAM highlight_php_line(const char* line, size_t line_num, 
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
        
        /* PHP opening/closing tags */
        if (i + 5 <= line_len && strncmp(line + i, "<?php", 5) == 0) {
            size_t start = i;
            i += 5;
            
            vizero_syntax_token_t* token = &tokens[count++];
            token->range.start.line = line_num;
            token->range.start.column = start;
            token->range.end.line = line_num;
            token->range.end.column = i;
            token->colour = colours[TOKEN_TAG];
            token->flags = SYNTAX_BOLD;
            continue;
        }
        
        if (i + 2 <= line_len && strncmp(line + i, "<?", 2) == 0) {
            size_t start = i;
            i += 2;
            
            vizero_syntax_token_t* token = &tokens[count++];
            token->range.start.line = line_num;
            token->range.start.column = start;
            token->range.end.line = line_num;
            token->range.end.column = i;
            token->colour = colours[TOKEN_TAG];
            token->flags = SYNTAX_BOLD;
            continue;
        }
        
        if (i + 2 <= line_len && strncmp(line + i, "?>", 2) == 0) {
            size_t start = i;
            i += 2;
            
            vizero_syntax_token_t* token = &tokens[count++];
            token->range.start.line = line_num;
            token->range.start.column = start;
            token->range.end.line = line_num;
            token->range.end.column = i;
            token->colour = colours[TOKEN_TAG];
            token->flags = SYNTAX_BOLD;
            continue;
        }
        
        /* Multi-line comments */
        if (i + 2 <= line_len && strncmp(line + i, "/*", 2) == 0) {
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
            /* If we didn't find the end, go to end of line */
            if (i + 1 >= line_len) i = line_len;
            
            token_type_t comment_type = TOKEN_COMMENT;
            /* Check if it's a DocBlock comment */
            if (start + 3 < line_len && line[start + 2] == '*') {
                comment_type = TOKEN_DOCBLOCK;
            }
            
            vizero_syntax_token_t* token = &tokens[count++];
            token->range.start.line = line_num;
            token->range.start.column = start;
            token->range.end.line = line_num;
            token->range.end.column = i;
            token->colour = colours[comment_type];
            token->flags = SYNTAX_ITALIC;
            continue;
        }
        
        /* Single-line comments */
        if (line[i] == '#' || (i + 1 < line_len && strncmp(line + i, "//", 2) == 0)) {
            size_t start = i;
            
            vizero_syntax_token_t* token = &tokens[count++];
            token->range.start.line = line_num;
            token->range.start.column = start;
            token->range.end.line = line_num;
            token->range.end.column = line_len;
            token->colour = colours[TOKEN_COMMENT];
            token->flags = SYNTAX_ITALIC;
            break; /* Rest of line is comment */
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
        
        /* Heredoc/Nowdoc strings */
        if (i + 3 <= line_len && strncmp(line + i, "<<<", 3) == 0) {
            size_t start = i;
            i += 3;
            /* Skip to end of line for simplicity - proper heredoc parsing is complex */
            i = line_len;
            
            vizero_syntax_token_t* token = &tokens[count++];
            token->range.start.line = line_num;
            token->range.start.column = start;
            token->range.end.line = line_num;
            token->range.end.column = i;
            token->colour = colours[TOKEN_STRING];
            token->flags = 0;
            continue;
        }
        
        /* Variables */
        if (line[i] == '$') {
            size_t start = i++;
            while (i < line_len && (isalnum(line[i]) || line[i] == '_')) i++;
            size_t len = i - start;
            
            token_type_t type = TOKEN_VARIABLE;
            /* Check if it's a superglobal */
            if (is_keyword_case_sensitive(line + start, len, php_superglobals)) {
                type = TOKEN_SUPERGLOBAL;
            }
            
            vizero_syntax_token_t* token = &tokens[count++];
            token->range.start.line = line_num;
            token->range.start.column = start;
            token->range.end.line = line_num;
            token->range.end.column = i;
            token->colour = colours[type];
            token->flags = (type == TOKEN_SUPERGLOBAL) ? SYNTAX_BOLD : 0;
            continue;
        }
        
        /* Numbers */
        if (isdigit(line[i]) || (line[i] == '.' && i + 1 < line_len && isdigit(line[i + 1]))) {
            size_t start = i;
            
            /* Handle hex */
            if (line[i] == '0' && i + 1 < line_len && (line[i + 1] == 'x' || line[i + 1] == 'X')) {
                i += 2;
                while (i < line_len && isxdigit(line[i])) i++;
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
            if (is_keyword(line + start, len, php_keywords)) {
                type = TOKEN_KEYWORD;
            } else if (is_keyword(line + start, len, php_builtins)) {
                type = TOKEN_BUILTIN;
            } else if (is_keyword_case_sensitive(line + start, len, php_constants)) {
                type = TOKEN_CONSTANT;
            } else if (is_keyword(line + start, len, php_type_hints)) {
                type = TOKEN_TYPE_HINT;
            } else {
                /* Check for function call (followed by '(') */
                size_t j = i;
                while (j < line_len && isspace(line[j])) j++;
                if (j < line_len && line[j] == '(') {
                    type = TOKEN_FUNCTION;
                }
                /* Check for class name (after 'new' keyword or '::') */
                if (start > 4 && strncasecmp(line + start - 4, "new ", 4) == 0) {
                    type = TOKEN_CLASS;
                }
            }
            
            /* Special handling for specific keywords */
            if (type == TOKEN_KEYWORD) {
                if (strncasecmp(line + start, "class", 5) == 0 || 
                    strncasecmp(line + start, "interface", 9) == 0 ||
                    strncasecmp(line + start, "trait", 5) == 0) {
                    type = TOKEN_CLASS;
                } else if (strncasecmp(line + start, "function", 8) == 0) {
                    type = TOKEN_FUNCTION;
                } else if (strncasecmp(line + start, "namespace", 9) == 0) {
                    type = TOKEN_NAMESPACE;
                }
            }
            
            if (type != TOKEN_NORMAL) {
                vizero_syntax_token_t* token = &tokens[count++];
                token->range.start.line = line_num;
                token->range.start.column = start;
                token->range.end.line = line_num;
                token->range.end.column = i;
                token->colour = colours[type];
                token->flags = (type == TOKEN_KEYWORD || type == TOKEN_CLASS || 
                               type == TOKEN_FUNCTION || type == TOKEN_CONSTANT) ? SYNTAX_BOLD : 0;
            }
            continue;
        }
        
        /* Operators and punctuation */
        if (strchr("+-*/%=<>!&|^~?:;,.(){}[]", line[i])) {
            /* Handle multi-character operators */
            size_t start = i;
            size_t op_len = 1;
            
            if (i + 1 < line_len) {
                char op[3] = {line[i], line[i + 1], '\0'};
                if (strcmp(op, "==") == 0 || strcmp(op, "!=") == 0 || strcmp(op, "<=") == 0 ||
                    strcmp(op, ">=") == 0 || strcmp(op, "&&") == 0 || strcmp(op, "||") == 0 ||
                    strcmp(op, "++") == 0 || strcmp(op, "--") == 0 || strcmp(op, "+=") == 0 ||
                    strcmp(op, "-=") == 0 || strcmp(op, "*=") == 0 || strcmp(op, "/=") == 0 ||
                    strcmp(op, "%=") == 0 || strcmp(op, ".=") == 0 || strcmp(op, "=>") == 0 ||
                    strcmp(op, "->") == 0 || strcmp(op, "::") == 0 || strcmp(op, "<<") == 0 ||
                    strcmp(op, ">>") == 0) {
                    op_len = 2;
                }
                
                /* Check for three-character operators */
                if (i + 2 < line_len && strcmp(op, "===") == 0 || strcmp(op, "!==") == 0) {
                    op_len = 3;
                }
            }
            
            vizero_syntax_token_t* token = &tokens[count++];
            token->range.start.line = line_num;
            token->range.start.column = start;
            token->range.end.line = line_num;
            token->range.end.column = start + op_len;
            token->colour = colours[TOKEN_OPERATOR];
            token->flags = 0;
            
            i += op_len;
            continue;
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
    
    /* Only handle PHP files and REPL buffers */
    if (!is_php_file(filename)) return 0;
    
    size_t count = 0;
    for (size_t line = start_line; line <= end_line && count < max_tokens; line++) {
        const char* line_text = editor_api->get_buffer_line(buffer, line);
        if (!line_text) continue;
        
        int n = highlight_php_line(line_text, line, tokens + count, max_tokens - count);
        if (n > 0) count += n;
    }
    *token_count = count;
    return 0;
}

VIZERO_PLUGIN_DEFINE_INFO(
    "PHP Syntax Highlighter",
    "1.0.0",
    "Vizero Team", 
    "Syntax highlighting plugin for PHP files and REPL buffers",
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