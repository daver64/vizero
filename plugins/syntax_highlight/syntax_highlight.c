/* Syntax highlighting plugin for C and Assembly */
#include "vizero/plugin_interface.h"
#include <string.h>
#include <stdlib.h>
#include "markdown_syntax.h"
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

/* Token types and their colors */
typedef enum {
    TOKEN_NORMAL,
    TOKEN_KEYWORD,
    TOKEN_STRING,
    TOKEN_COMMENT,
    TOKEN_NUMBER,
    TOKEN_OPERATOR,
    TOKEN_PREPROCESSOR,
    TOKEN_TYPE,
    TOKEN_REGISTER,      /* For assembly */
    TOKEN_INSTRUCTION,   /* For assembly */
    TOKEN_LABEL          /* For assembly */
} token_type_t;

/* Simple keyword detection helper - case insensitive */
static int is_keyword(const char* text, size_t len, const char** keywords) {
    for (int i = 0; keywords[i] != NULL; i++) {
        size_t keyword_len = strlen(keywords[i]);
        if (len == keyword_len) {
            int match = 1;
            for (size_t j = 0; j < len; j++) {
                char c1 = (text[j] >= 'A' && text[j] <= 'Z') ? text[j] + 32 : text[j];
                char c2 = keywords[i][j];
                if (c1 != c2) {
                    match = 0;
                    break;
                }
            }
            if (match) return 1;
        }
    }
    return 0;
}

/* Color definitions */
static vizero_plugin_color_t colors[] = {
    {255, 255, 255, 255}, /* TOKEN_NORMAL - white */
    {86, 156, 214, 255},  /* TOKEN_KEYWORD - blue */
    {206, 145, 120, 255}, /* TOKEN_STRING - orange */
    {106, 153, 85, 255},  /* TOKEN_COMMENT - green */
    {181, 206, 168, 255}, /* TOKEN_NUMBER - light green */
    {212, 212, 212, 255}, /* TOKEN_OPERATOR - light gray */
    {155, 155, 155, 255}, /* TOKEN_PREPROCESSOR - gray */
    {78, 201, 176, 255},  /* TOKEN_TYPE - cyan */
    {255, 215, 0, 255},   /* TOKEN_REGISTER - gold */
    {100, 200, 255, 255}, /* TOKEN_INSTRUCTION - light blue */
    {255, 140, 0, 255}    /* TOKEN_LABEL - orange */
};

/* C keywords */
static const char* c_keywords[] = {
    "auto", "break", "case", "char", "const", "continue", "default", "do",
    "double", "else", "enum", "extern", "float", "for", "goto", "if",
    "inline", "int", "long", "register", "restrict", "return", "short",
    "signed", "sizeof", "static", "struct", "switch", "typedef", "union",
    "unsigned", "void", "volatile", "while", "_Bool", "_Complex", "_Imaginary",
    NULL
};

/* C types */
static const char* c_types[] = {
    "size_t", "ssize_t", "ptrdiff_t", "intptr_t", "uintptr_t",
    "int8_t", "int16_t", "int32_t", "int64_t",
    "uint8_t", "uint16_t", "uint32_t", "uint64_t",
    "FILE", "NULL",
    NULL
};

/* Assembly instructions (x86/x64) */
static const char* asm_instructions[] = {
    "mov", "add", "sub", "mul", "div", "inc", "dec", "cmp", "test",
    "jmp", "je", "jne", "jz", "jnz", "jl", "jle", "jg", "jge",
    "call", "ret", "push", "pop", "nop", "int", "lea", "xor",
    "and", "or", "not", "shl", "shr", "sal", "sar", "rol", "ror",
    "loop", "loope", "loopne", "rep", "repz", "repnz",
    "insb", "insw", "insd", "outsb", "outsw", "outsd",
    "movsb", "movsw", "movsd", "cmpsb", "cmpsw", "cmpsd",
    "scasb", "scasw", "scasd", "stosb", "stosw", "stosd",
    "lodsb", "lodsw", "lodsd", "in", "out", "cli", "sti",
    "hlt", "wait", "lock", "lahf", "sahf", "pushf", "popf",
    "adc", "sbb", "imul", "idiv", "neg", "cbw", "cwd", "cdq",
    "movsx", "movzx", "setne", "sete", "setl", "setg", "setle", "setge",
    NULL
};

/* Assembly registers */
static const char* asm_registers[] = {
    "eax", "ebx", "ecx", "edx", "esi", "edi", "esp", "ebp",
    "rax", "rbx", "rcx", "rdx", "rsi", "rdi", "rsp", "rbp",
    "r8", "r9", "r10", "r11", "r12", "r13", "r14", "r15",
    "ax", "bx", "cx", "dx", "si", "di", "sp", "bp",
    "al", "bl", "cl", "dl", "ah", "bh", "ch", "dh",
    "cs", "ds", "es", "fs", "gs", "ss",
    NULL
};

static const vizero_editor_api_t* editor_api = NULL;

/* Helper function to determine file type based on filename */
static int get_file_type(const char* filename) {
    if (!filename) return 0; /* Unknown */
    
    const char* ext = strrchr(filename, '.');
    if (!ext) return 0; /* No extension */
    
    /* C/C++ files */
    if (strcmp(ext, ".c") == 0 || strcmp(ext, ".h") == 0 ||
        strcmp(ext, ".cpp") == 0 || strcmp(ext, ".cxx") == 0 ||
        strcmp(ext, ".cc") == 0 || strcmp(ext, ".hpp") == 0) {
        return 1; /* C/C++ */
    }
    
    /* Assembly files */
    if (strcmp(ext, ".s") == 0 || strcmp(ext, ".S") == 0 ||
        strcmp(ext, ".asm") == 0 || strcmp(ext, ".inc") == 0 ||
        strcmp(ext, ".x86") == 0) {
        return 2; /* Assembly */
    }
    
    /* Markdown files */
    if (strcmp(ext, ".md") == 0 || strcmp(ext, ".markdown") == 0) {
        return 3; /* Markdown */
    }
    return 0; /* Unknown */
}

/* C syntax highlighting */
static int UNUSED_PARAM highlight_c_line(const char* line, size_t line_num, 
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
        if (i < line_len - 1 && line[i] == '/' && line[i + 1] == '/') {
            vizero_syntax_token_t* token = &tokens[count++];
            token->range.start.line = line_num;
            token->range.start.column = i;
            token->range.end.line = line_num;
            token->range.end.column = line_len;
            token->color = colors[TOKEN_COMMENT];
            token->flags = SYNTAX_ITALIC;
            break; /* Rest of line is comment */
        }
        
        if (i < line_len - 1 && line[i] == '/' && line[i + 1] == '*') {
            vizero_syntax_token_t* token = &tokens[count++];
            token->range.start.line = line_num;
            token->range.start.column = i;
            token->range.end.line = line_num;
            token->range.end.column = line_len;
            token->color = colors[TOKEN_COMMENT];
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
            token->color = colors[TOKEN_PREPROCESSOR];
            token->flags = 0;
            break; /* Rest of line is preprocessor */
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
            token->color = colors[TOKEN_STRING];
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
            token->color = colors[TOKEN_STRING];
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
                while (i < line_len && (isdigit(line[i]) || line[i] == '.')) i++;
            }
            
            vizero_syntax_token_t* token = &tokens[count++];
            token->range.start.line = line_num;
            token->range.start.column = start;
            token->range.end.line = line_num;
            token->range.end.column = i;
            token->color = colors[TOKEN_NUMBER];
            token->flags = 0;
            continue;
        }
        
        /* Identifiers and keywords */
        if (isalpha(line[i]) || line[i] == '_') {
            size_t start = i;
            while (i < line_len && (isalnum(line[i]) || line[i] == '_')) i++;
            size_t len = i - start;
            
            token_type_t type = TOKEN_NORMAL;
            if (is_keyword(line + start, len, c_keywords)) {
                type = TOKEN_KEYWORD;
            } else if (is_keyword(line + start, len, c_types)) {
                type = TOKEN_TYPE;
            }
            
            if (type != TOKEN_NORMAL) {
                vizero_syntax_token_t* token = &tokens[count++];
                token->range.start.line = line_num;
                token->range.start.column = start;
                token->range.end.line = line_num;
                token->range.end.column = i;
                token->color = colors[type];
                token->flags = (type == TOKEN_KEYWORD) ? SYNTAX_BOLD : 0;
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
            token->color = colors[TOKEN_OPERATOR];
            token->flags = 0;
        }
        
        i++;
    }
    return (int)count;
}

/* Assembly syntax highlighting */
static int UNUSED_PARAM highlight_asm_line(const char* line, size_t line_num,
                             vizero_syntax_token_t* tokens, size_t max_tokens) {
    if (!line || !tokens) return 0;
    size_t line_len = strlen(line);
    if (line_len == 0) return 0;
    size_t i = 0;
    size_t count = 0;
    while (i < line_len && count < max_tokens) {
        if (isspace(line[i])) { i++; continue; }
        if (line[i] == ';' || (line[i] == '/' && i + 1 < line_len && line[i + 1] == '/')) {
            vizero_syntax_token_t* token = &tokens[count++];
            token->range.start.line = line_num;
            token->range.start.column = i;
            token->range.end.line = line_num;
            token->range.end.column = line_len;
            token->color = colors[TOKEN_COMMENT];
            token->flags = SYNTAX_ITALIC;
            break;
        }
        if (isalpha(line[i]) || line[i] == '_') {
            size_t start = i;
            while (i < line_len && (isalnum(line[i]) || line[i] == '_')) i++;
            size_t temp_i = i;
            while (temp_i < line_len && isspace(line[temp_i])) temp_i++;
            if (temp_i < line_len && line[temp_i] == ':') {
                vizero_syntax_token_t* token = &tokens[count++];
                token->range.start.line = line_num;
                token->range.start.column = start;
                token->range.end.line = line_num;
                token->range.end.column = i;
                token->color = colors[TOKEN_LABEL];
                token->flags = SYNTAX_BOLD;
                continue;
            }
            size_t len = i - start;
            token_type_t type = TOKEN_NORMAL;
            if (is_keyword(line + start, len, asm_instructions)) type = TOKEN_INSTRUCTION;
            else if (is_keyword(line + start, len, asm_registers)) type = TOKEN_REGISTER;
            if (type != TOKEN_NORMAL) {
                vizero_syntax_token_t* token = &tokens[count++];
                token->range.start.line = line_num;
                token->range.start.column = start;
                token->range.end.line = line_num;
                token->range.end.column = i;
                token->color = colors[type];
                token->flags = (type == TOKEN_INSTRUCTION) ? SYNTAX_BOLD : 0;
            }
            continue;
        }
        if (isdigit(line[i]) || (line[i] == '0' && i + 1 < line_len && (line[i + 1] == 'x' || line[i + 1] == 'X'))) {
            size_t start = i;
            if (line[i] == '0' && i + 1 < line_len && (line[i + 1] == 'x' || line[i + 1] == 'X')) {
                i += 2;
                while (i < line_len && isxdigit(line[i])) i++;
            } else {
                while (i < line_len && isdigit(line[i])) i++;
            }
            
            vizero_syntax_token_t* token = &tokens[count++];
            token->range.start.line = line_num;
            token->range.start.column = start;
            token->range.end.line = line_num;
            token->range.end.column = i;
            token->color = colors[TOKEN_NUMBER];
            token->flags = 0;
            continue;
        }
        
        i++;
    }
    return (int)count;
}

/* Update plugin API: highlight_syntax takes caller-allocated buffer */
static int highlight_syntax(vizero_buffer_t* buffer, size_t start_line, size_t end_line,
                           vizero_syntax_token_t* tokens, size_t max_tokens) {
    if (!buffer || !tokens || !editor_api) return 0;
    if (!editor_api->get_buffer_line || !editor_api->get_buffer_filename) return 0;
    const char* filename = editor_api->get_buffer_filename(buffer);
    int file_type = get_file_type(filename);
    if (file_type == 3) {
        /* Markdown file - use markdown_highlight */
        int result = markdown_highlight(buffer, start_line, end_line, &tokens, &max_tokens, editor_api);
        return result;
    }
    size_t token_count = 0;
    for (size_t line = start_line; line <= end_line && token_count < max_tokens; line++) {
        const char* line_text = editor_api->get_buffer_line(buffer, line);
        if (!line_text) continue;
        int n = 0;
        if (file_type == 2) {
            n = highlight_asm_line(line_text, line, tokens + token_count, max_tokens - token_count);
        } else if (file_type == 1) {
            n = highlight_c_line(line_text, line, tokens + token_count, max_tokens - token_count);
        }
        if (n > 0) token_count += n;
    }
    return (int)token_count;
}

VIZERO_PLUGIN_DEFINE_INFO(
    "Syntax Highlighter",
    "1.0.0",
    "Vizero Team", 
    "Syntax highlighting plugin for C and Assembly",
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