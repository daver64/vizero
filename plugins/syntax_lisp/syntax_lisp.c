/* Common Lisp syntax highlighting plugin */
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
    TOKEN_SYMBOL,
    TOKEN_FUNCTION,
    TOKEN_MACRO,
    TOKEN_SPECIAL_FORM,
    TOKEN_OPERATOR,
    TOKEN_LITERAL,
    TOKEN_PAREN,
    TOKEN_QUOTE,
    TOKEN_PACKAGE
} token_type_t;

/* Color definitions - Lisp themed colors with good contrast */
static vizero_plugin_colour_t colours[] = {
    {255, 255, 255, 255}, /* TOKEN_NORMAL - white */
    {147, 112, 219, 255}, /* TOKEN_KEYWORD - medium slate blue */
    {255, 182, 193, 255}, /* TOKEN_STRING - light pink */
    {105, 105, 105, 255}, /* TOKEN_COMMENT - dim gray */
    {152, 251, 152, 255}, /* TOKEN_NUMBER - pale green */
    {255, 215, 0, 255},   /* TOKEN_SYMBOL - gold */
    {70, 130, 180, 255},  /* TOKEN_FUNCTION - steel blue */
    {255, 69, 0, 255},    /* TOKEN_MACRO - orange red */
    {138, 43, 226, 255},  /* TOKEN_SPECIAL_FORM - blue violet */
    {220, 220, 220, 255}, /* TOKEN_OPERATOR - gainsboro */
    {173, 216, 230, 255}, /* TOKEN_LITERAL - light blue */
    {255, 255, 255, 255}, /* TOKEN_PAREN - white */
    {255, 140, 0, 255},   /* TOKEN_QUOTE - dark orange */
    {72, 209, 204, 255}   /* TOKEN_PACKAGE - medium turquoise */
};

/* Common Lisp special forms */
static const char* lisp_special_forms[] = {
    "block", "catch", "declare", "eval-when", "flet", "function", "go", "if",
    "labels", "let", "let*", "load-time-value", "locally", "macrolet",
    "multiple-value-call", "multiple-value-prog1", "progn", "progv", "quote",
    "return-from", "setq", "symbol-macrolet", "tagbody", "the", "throw",
    "unwind-protect", NULL
};

/* Common Lisp macros */
static const char* lisp_macros[] = {
    "and", "case", "cond", "decf", "defclass", "defconstant", "defgeneric",
    "defmacro", "defmethod", "defpackage", "defparameter", "defsetf", "defstruct",
    "deftype", "defun", "defvar", "do", "do*", "dolist", "dotimes", "ecase",
    "etypecase", "format", "handler-bind", "handler-case", "ignore-errors",
    "incf", "lambda", "loop", "multiple-value-bind", "multiple-value-setq",
    "or", "pop", "prog", "prog*", "prog1", "prog2", "psetf", "psetq", "push",
    "pushnew", "restart-bind", "restart-case", "rotatef", "setf", "shiftf",
    "step", "time", "trace", "typecase", "unless", "untrace", "when",
    "with-open-file", "with-open-stream", "with-output-to-string", NULL
};

/* Common Lisp functions */
static const char* lisp_functions[] = {
    "abs", "acons", "acos", "acosh", "add-method", "adjoin", "adjustable-array-p",
    "adjust-array", "alpha-char-p", "alphanumericp", "append", "apply", "apropos",
    "aref", "array-dimension", "array-dimensions", "array-element-type",
    "array-has-fill-pointer-p", "array-in-bounds-p", "array-rank", "array-row-major-index",
    "array-total-size", "arrayp", "ash", "asin", "asinh", "assoc", "assoc-if",
    "assoc-if-not", "atan", "atanh", "atom", "car", "cdr", "ceiling", "char",
    "char-code", "char-downcase", "char-equal", "char-greaterp", "char-lessp",
    "char-name", "char-not-equal", "char-not-greaterp", "char-not-lessp",
    "char-upcase", "characterp", "code-char", "coerce", "compile", "complement",
    "complex", "complexp", "concatenate", "cons", "consp", "constantly",
    "copy-list", "copy-seq", "copy-structure", "copy-symbol", "copy-tree",
    "cos", "cosh", "count", "count-if", "count-if-not", "delete", "delete-if",
    "delete-if-not", "denominator", "digit-char", "digit-char-p", "endp",
    "eq", "eql", "equal", "equalp", "error", "evenp", "every", "exp",
    "expt", "fboundp", "find", "find-if", "find-if-not", "first", "float",
    "floatp", "floor", "fmakunbound", "funcall", "functionp", "gcd", "gensym",
    "get", "getf", "get-properties", "hash-table-count", "hash-table-p",
    "identity", "imagpart", "intersection", "integerp", "isqrt", "keywordp",
    "last", "lcm", "length", "list", "list*", "listp", "log", "logand",
    "logandc1", "logandc2", "logbitp", "logcount", "logeqv", "logior",
    "lognand", "lognor", "lognot", "logorc1", "logorc2", "logtest", "logxor",
    "make-array", "make-hash-table", "make-list", "make-string", "make-symbol",
    "mapcar", "mapcan", "mapc", "mapcon", "maphash", "maplist", "max", "member",
    "member-if", "member-if-not", "merge", "min", "minusp", "mod", "nconc",
    "nreverse", "nsubstitute", "nsubstitute-if", "nsubstitute-if-not", "nth",
    "nthcdr", "null", "numberp", "numerator", "oddp", "package-name",
    "package-nicknames", "package-use-list", "package-used-by-list", "packagep",
    "pairlis", "parse-integer", "plusp", "position", "position-if",
    "position-if-not", "print", "princ", "rational", "rationalp", "read",
    "readtablep", "realp", "rem", "remove", "remove-if", "remove-if-not",
    "replace", "rest", "reverse", "round", "search", "second", "sin", "sinh",
    "sort", "sqrt", "stable-sort", "string", "string=", "string/=", "string<",
    "string<=", "string>", "string>=", "string-capitalize", "string-downcase",
    "string-equal", "string-greaterp", "string-left-trim", "string-lessp",
    "string-not-equal", "string-not-greaterp", "string-not-lessp",
    "string-right-trim", "string-trim", "string-upcase", "stringp", "subseq",
    "substitute", "substitute-if", "substitute-if-not", "symbolp", "tan", "tanh",
    "third", "tree-equal", "truncate", "type-of", "typep", "union", "values",
    "vector", "vectorp", "write", "write-string", "zerop", NULL
};

/* Common Lisp literals and constants */
static const char* lisp_literals[] = {
    "t", "nil", "pi", NULL
};

static const vizero_editor_api_t* editor_api = NULL;

/* Simple keyword detection helper - case insensitive for Lisp */
static int is_keyword(const char* text, size_t len, const char** keywords) {
    for (int i = 0; keywords[i] != NULL; i++) {
        size_t keyword_len = strlen(keywords[i]);
        if (len == keyword_len) {
            int match = 1;
            for (size_t j = 0; j < len; j++) {
                if (tolower(text[j]) != tolower(keywords[i][j])) {
                    match = 0;
                    break;
                }
            }
            if (match) return 1;
        }
    }
    return 0;
}

/* Helper function to determine if file is Lisp or REPL buffer */
static int is_lisp_file(const char* filename) {
    if (!filename) return 0;
    
    /* Handle REPL buffers */
    if (strstr(filename, "*SLIME*") || strstr(filename, "*Lisp*") || 
        strstr(filename, "*REPL*") || strstr(filename, "lisp-repl") ||
        strstr(filename, ".lisp-repl") || strstr(filename, "*sbcl*") ||
        strstr(filename, "*ccl*") || strstr(filename, "*clisp*")) {
        return 1;
    }
    
    const char* ext = strrchr(filename, '.');
    if (!ext) return 0;
    
    return (strcmp(ext, ".lisp") == 0 || strcmp(ext, ".lsp") == 0 || 
            strcmp(ext, ".cl") == 0 || strcmp(ext, ".asd") == 0 ||
            strcmp(ext, ".el") == 0); /* Include Emacs Lisp */
}

/* Skip whitespace */
static size_t skip_whitespace(const char* line, size_t i, size_t line_len) {
    while (i < line_len && isspace(line[i])) i++;
    return i;
}

/* Parse a Lisp symbol/identifier */
static size_t parse_symbol(const char* line, size_t start, size_t line_len) {
    size_t i = start;
    while (i < line_len && !isspace(line[i]) && line[i] != '(' && line[i] != ')' &&
           line[i] != ';' && line[i] != '"' && line[i] != '\'' && line[i] != '`' &&
           line[i] != ',' && line[i] != '#') {
        i++;
    }
    return i;
}

/* Check if a symbol is a package qualifier (contains ::) */
static int has_package_qualifier(const char* text, size_t len) {
    for (size_t i = 0; i + 1 < len; i++) {
        if (text[i] == ':' && text[i + 1] == ':') return 1;
    }
    return 0;
}

/* Common Lisp syntax highlighting */
static int UNUSED_PARAM highlight_lisp_line(const char* line, size_t line_num, 
                                           vizero_syntax_token_t* tokens, size_t max_tokens) {
    if (!line || !tokens) return 0;
    size_t line_len = strlen(line);
    if (line_len == 0) return 0;
    
    size_t i = 0;
    size_t count = 0;
    
    while (i < line_len && count < max_tokens) {
        i = skip_whitespace(line, i, line_len);
        if (i >= line_len) break;
        
        /* Comments */
        if (line[i] == ';') {
            vizero_syntax_token_t* token = &tokens[count++];
            token->range.start.line = line_num;
            token->range.start.column = i;
            token->range.end.line = line_num;
            token->range.end.column = line_len;
            token->colour = colours[TOKEN_COMMENT];
            token->flags = SYNTAX_ITALIC;
            break; /* Rest of line is comment */
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
        
        /* Parentheses */
        if (line[i] == '(' || line[i] == ')') {
            vizero_syntax_token_t* token = &tokens[count++];
            token->range.start.line = line_num;
            token->range.start.column = i;
            token->range.end.line = line_num;
            token->range.end.column = i + 1;
            token->colour = colours[TOKEN_PAREN];
            token->flags = SYNTAX_BOLD;
            i++;
            continue;
        }
        
        /* Quote characters */
        if (line[i] == '\'' || line[i] == '`' || line[i] == ',') {
            vizero_syntax_token_t* token = &tokens[count++];
            token->range.start.line = line_num;
            token->range.start.column = i;
            token->range.end.line = line_num;
            token->range.end.column = i + 1;
            token->colour = colours[TOKEN_QUOTE];
            token->flags = SYNTAX_BOLD;
            i++;
            continue;
        }
        
        /* Sharp reader macros */
        if (line[i] == '#') {
            size_t start = i++;
            /* Handle various sharp macros like #\, #', #(, etc. */
            if (i < line_len) {
                if (line[i] == '\\') {
                    /* Character literal #\space, #\newline, etc. */
                    i++;
                    while (i < line_len && isalpha(line[i])) i++;
                } else if (line[i] == '\'' || line[i] == '(' || line[i] == '*') {
                    i++;
                } else if (isdigit(line[i])) {
                    /* Radix notation like #16rFF */
                    while (i < line_len && (isdigit(line[i]) || line[i] == 'r' || 
                                          line[i] == 'R' || isxdigit(line[i]))) i++;
                }
            }
            
            vizero_syntax_token_t* token = &tokens[count++];
            token->range.start.line = line_num;
            token->range.start.column = start;
            token->range.end.line = line_num;
            token->range.end.column = i;
            token->colour = colours[TOKEN_LITERAL];
            token->flags = 0;
            continue;
        }
        
        /* Numbers */
        if (isdigit(line[i]) || (line[i] == '.' && i + 1 < line_len && isdigit(line[i + 1])) ||
            ((line[i] == '+' || line[i] == '-') && i + 1 < line_len && isdigit(line[i + 1]))) {
            size_t start = i;
            
            /* Skip sign */
            if (line[i] == '+' || line[i] == '-') i++;
            
            /* Parse number (integer or float) */
            while (i < line_len && (isdigit(line[i]) || line[i] == '.' || 
                                  line[i] == 'e' || line[i] == 'E' || line[i] == 'd' || line[i] == 'D' ||
                                  line[i] == 'f' || line[i] == 'F' || line[i] == 'l' || line[i] == 'L' ||
                                  line[i] == 's' || line[i] == 'S' || line[i] == '+' || line[i] == '-' ||
                                  line[i] == '/')) { /* Include ratio notation */
                i++;
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
        
        /* Keywords (symbols starting with :) */
        if (line[i] == ':') {
            size_t start = i++;
            i = parse_symbol(line, i, line_len);
            
            vizero_syntax_token_t* token = &tokens[count++];
            token->range.start.line = line_num;
            token->range.start.column = start;
            token->range.end.line = line_num;
            token->range.end.column = i;
            token->colour = colours[TOKEN_KEYWORD];
            token->flags = SYNTAX_BOLD;
            continue;
        }
        
        /* Symbols and identifiers */
        if (isalpha(line[i]) || strchr("+-*/<>=!&%$_?^~@", line[i])) {
            size_t start = i;
            i = parse_symbol(line, i, line_len);
            size_t len = i - start;
            
            if (len == 0) {
                i++;
                continue;
            }
            
            token_type_t type = TOKEN_SYMBOL;
            
            /* Check for special forms */
            if (is_keyword(line + start, len, lisp_special_forms)) {
                type = TOKEN_SPECIAL_FORM;
            }
            /* Check for macros */
            else if (is_keyword(line + start, len, lisp_macros)) {
                type = TOKEN_MACRO;
            }
            /* Check for functions */
            else if (is_keyword(line + start, len, lisp_functions)) {
                type = TOKEN_FUNCTION;
            }
            /* Check for literals */
            else if (is_keyword(line + start, len, lisp_literals)) {
                type = TOKEN_LITERAL;
            }
            /* Check for package qualifiers */
            else if (has_package_qualifier(line + start, len)) {
                type = TOKEN_PACKAGE;
            }
            
            if (type != TOKEN_SYMBOL) {
                vizero_syntax_token_t* token = &tokens[count++];
                token->range.start.line = line_num;
                token->range.start.column = start;
                token->range.end.line = line_num;
                token->range.end.column = i;
                token->colour = colours[type];
                token->flags = (type == TOKEN_SPECIAL_FORM || type == TOKEN_MACRO) ? SYNTAX_BOLD : 0;
            }
            continue;
        }
        
        /* Skip other characters */
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
    
    /* Only handle Lisp files and REPL buffers */
    if (!is_lisp_file(filename)) return 0;
    
    size_t token_count = 0;
    for (size_t line = start_line; line <= end_line && token_count < max_tokens; line++) {
        const char* line_text = editor_api->get_buffer_line(buffer, line);
        if (!line_text) continue;
        
        int n = highlight_lisp_line(line_text, line, tokens + token_count, max_tokens - token_count);
        if (n > 0) token_count += n;
    }
    return (int)token_count;
}

VIZERO_PLUGIN_DEFINE_INFO(
    "Common Lisp Syntax Highlighter",
    "1.0.0",
    "Vizero Team", 
    "Syntax highlighting plugin for Common Lisp and REPL buffers",
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