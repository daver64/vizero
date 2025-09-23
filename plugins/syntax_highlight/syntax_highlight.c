/* Syntax highlighting plugin for C and Assembly */
#include "vizero/plugin_interface.h"
#include <string.h>
#include <stdlib.h>
#include <ctype.h>

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
    
    return 0; /* Unknown */
}

/* C syntax highlighting */
static int __attribute__((unused)) highlight_c_line(const char* line, size_t line_num, 
                           vizero_syntax_token_t** tokens, size_t* token_count) {
    if (!line || !tokens || !token_count) return -1;
    
    size_t line_len = strlen(line);
    if (line_len == 0) {
        *token_count = 0;
        return 0;
    }
    
    /* Allocate maximum possible tokens (one per character is overkill but safe) */
    *tokens = malloc(line_len * sizeof(vizero_syntax_token_t));
    if (!*tokens) return -1;
    
    *token_count = 0;
    size_t i = 0;
    
    while (i < line_len) {
        /* Skip whitespace */
        if (isspace(line[i])) {
            i++;
            continue;
        }
        
        /* Comments */
        if (i < line_len - 1 && line[i] == '/' && line[i + 1] == '/') {
            /* Single line comment */
            vizero_syntax_token_t* token = &(*tokens)[(*token_count)++];
            token->range.start.line = line_num;
            token->range.start.column = i;
            token->range.end.line = line_num;
            token->range.end.column = line_len;
            token->color = colors[TOKEN_COMMENT];
            token->flags = SYNTAX_ITALIC;
            break; /* Rest of line is comment */
        }
        
        if (i < line_len - 1 && line[i] == '/' && line[i + 1] == '*') {
            /* Multi-line comment start - for now just highlight this line */
            vizero_syntax_token_t* token = &(*tokens)[(*token_count)++];
            token->range.start.line = line_num;
            token->range.start.column = i;
            token->range.end.line = line_num;
            token->range.end.column = line_len;
            token->color = colors[TOKEN_COMMENT];
            token->flags = SYNTAX_ITALIC;
            break;
        }
        
        /* Preprocessor directives */
        if (line[i] == '#') {
            vizero_syntax_token_t* token = &(*tokens)[(*token_count)++];
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
            
            vizero_syntax_token_t* token = &(*tokens)[(*token_count)++];
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
            
            vizero_syntax_token_t* token = &(*tokens)[(*token_count)++];
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
            
            vizero_syntax_token_t* token = &(*tokens)[(*token_count)++];
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
                vizero_syntax_token_t* token = &(*tokens)[(*token_count)++];
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
            vizero_syntax_token_t* token = &(*tokens)[(*token_count)++];
            token->range.start.line = line_num;
            token->range.start.column = i;
            token->range.end.line = line_num;
            token->range.end.column = i + 1;
            token->color = colors[TOKEN_OPERATOR];
            token->flags = 0;
        }
        
        i++;
    }
    
    return 0;
}

/* Assembly syntax highlighting */
static int __attribute__((unused)) highlight_asm_line(const char* line, size_t line_num,
                             vizero_syntax_token_t** tokens, size_t* token_count) {
    if (!line || !tokens || !token_count) return -1;
    
    size_t line_len = strlen(line);
    if (line_len == 0) {
        *token_count = 0;
        return 0;
    }
    
    *tokens = malloc(line_len * sizeof(vizero_syntax_token_t));
    if (!*tokens) return -1;
    
    *token_count = 0;
    size_t i = 0;
    
    while (i < line_len) {
        if (isspace(line[i])) {
            i++;
            continue;
        }
        
        /* Comments */
        if (line[i] == ';' || (line[i] == '/' && i + 1 < line_len && line[i + 1] == '/')) {
            vizero_syntax_token_t* token = &(*tokens)[(*token_count)++];
            token->range.start.line = line_num;
            token->range.start.column = i;
            token->range.end.line = line_num;
            token->range.end.column = line_len;
            token->color = colors[TOKEN_COMMENT];
            token->flags = SYNTAX_ITALIC;
            break;
        }
        
        /* Labels (word followed by colon) */
        if (isalpha(line[i]) || line[i] == '_') {
            size_t start = i;
            while (i < line_len && (isalnum(line[i]) || line[i] == '_')) i++;
            
            /* Check if followed by colon */
            size_t temp_i = i;
            while (temp_i < line_len && isspace(line[temp_i])) temp_i++;
            if (temp_i < line_len && line[temp_i] == ':') {
                vizero_syntax_token_t* token = &(*tokens)[(*token_count)++];
                token->range.start.line = line_num;
                token->range.start.column = start;
                token->range.end.line = line_num;
                token->range.end.column = i;
                token->color = colors[TOKEN_LABEL];
                token->flags = SYNTAX_BOLD;
                continue;
            }
            
            /* Check for instructions or registers */
            size_t len = i - start;
            token_type_t type = TOKEN_NORMAL;
            if (is_keyword(line + start, len, asm_instructions)) {
                type = TOKEN_INSTRUCTION;
            } else if (is_keyword(line + start, len, asm_registers)) {
                type = TOKEN_REGISTER;
            }
            
            if (type != TOKEN_NORMAL) {
                vizero_syntax_token_t* token = &(*tokens)[(*token_count)++];
                token->range.start.line = line_num;
                token->range.start.column = start;
                token->range.end.line = line_num;
                token->range.end.column = i;
                token->color = colors[type];
                token->flags = (type == TOKEN_INSTRUCTION) ? SYNTAX_BOLD : 0;
            }
            continue;
        }
        
        /* Numbers (including hex) */
        if (isdigit(line[i]) || (line[i] == '0' && i + 1 < line_len && 
                                (line[i + 1] == 'x' || line[i + 1] == 'X'))) {
            size_t start = i;
            if (line[i] == '0' && i + 1 < line_len && (line[i + 1] == 'x' || line[i + 1] == 'X')) {
                i += 2;
                while (i < line_len && isxdigit(line[i])) i++;
            } else {
                while (i < line_len && isdigit(line[i])) i++;
            }
            
            vizero_syntax_token_t* token = &(*tokens)[(*token_count)++];
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
    
    return 0;
}

/* Main syntax highlighting function */
static int highlight_syntax(vizero_buffer_t* buffer, size_t start_line, size_t end_line,
                           vizero_syntax_token_t** tokens, size_t* token_count) {
    if (!buffer || !tokens || !token_count || !editor_api) return -1;
    
    if (!editor_api->get_buffer_line || !editor_api->get_buffer_filename) {
        return -1;
    }
    
    /* Allocate tokens - estimate max 10 tokens per line for word-level highlighting */
    size_t line_count = end_line - start_line + 1;
    size_t max_tokens = line_count * 10;
    *tokens = malloc(max_tokens * sizeof(vizero_syntax_token_t));
    if (!*tokens) {
        *token_count = 0;
        return -1;
    }
    
    *token_count = 0;
    
    /* Get filename to determine file type */
    const char* filename = editor_api->get_buffer_filename(buffer);
    int file_type = get_file_type(filename);
    
    /* Create word-level tokens */
    for (size_t line = start_line; line <= end_line; line++) {
        const char* line_text = editor_api->get_buffer_line(buffer, line);
        if (!line_text) continue;
        
        size_t line_len = strlen(line_text);
        if (line_len == 0) continue;
        
        if (file_type == 2) {
            /* Assembly file - word-by-word tokenization */
            
            /* Check for comment first */
            if (strchr(line_text, ';') == line_text) {
                /* Entire line is comment */
                if (*token_count >= max_tokens) break;
                vizero_syntax_token_t* token = &(*tokens)[*token_count];
                token->range.start.line = line;
                token->range.start.column = 0;
                token->range.end.line = line;
                token->range.end.column = line_len;
                token->color = colors[TOKEN_COMMENT];
                token->flags = SYNTAX_ITALIC;
                (*token_count)++;
                continue;
            }
            
            /* Check for labels */
            if (line_len > 0 && line_text[line_len - 1] == ':') {
                if (*token_count >= max_tokens) break;
                vizero_syntax_token_t* token = &(*tokens)[*token_count];
                token->range.start.line = line;
                token->range.start.column = 0;
                token->range.end.line = line;
                token->range.end.column = line_len - 1;
                token->color = colors[TOKEN_LABEL];
                token->flags = SYNTAX_BOLD;
                (*token_count)++;
                continue;
            }
            
            /* Parse words in the line */
            const char* pos = line_text;
            while (pos < line_text + line_len && *token_count < max_tokens) {
                /* Skip whitespace */
                while (pos < line_text + line_len && (*pos == ' ' || *pos == '\t')) {
                    pos++;
                }
                if (pos >= line_text + line_len) break;
                
                /* Handle comment in middle of line */
                if (*pos == ';') {
                    vizero_syntax_token_t* token = &(*tokens)[*token_count];
                    token->range.start.line = line;
                    token->range.start.column = pos - line_text;
                    token->range.end.line = line;
                    token->range.end.column = line_len;
                    token->color = colors[TOKEN_COMMENT];
                    token->flags = SYNTAX_ITALIC;
                    (*token_count)++;
                    break;
                }
                
                /* Find end of current token */
                const char* token_start = pos;
                
                /* Handle bracketed expressions like [ebx+9] */
                if (*pos == '[') {
                    /* Find closing bracket */
                    while (pos < line_text + line_len && *pos != ']') {
                        pos++;
                    }
                    if (pos < line_text + line_len && *pos == ']') {
                        pos++; /* Include the closing bracket */
                    }
                } else {
                    /* Regular word - stop at whitespace or punctuation */
                    while (pos < line_text + line_len && 
                           *pos != ' ' && *pos != '\t' && *pos != ',' && 
                           *pos != ';' && *pos != '[' && *pos != ']') {
                        pos++;
                    }
                }
                
                if (pos > token_start) {
                    size_t token_len = pos - token_start;
                    vizero_syntax_token_t* token = &(*tokens)[*token_count];
                    token->range.start.line = line;
                    token->range.start.column = token_start - line_text;
                    token->range.end.line = line;
                    token->range.end.column = pos - line_text;
                    
                    /* Determine token color */
                    if (*token_start == '[') {
                        /* Bracketed expression - normal color */
                        token->color = colors[TOKEN_NORMAL];
                        token->flags = 0;
                    } else if (is_keyword(token_start, token_len, asm_instructions)) {
                        token->color = colors[TOKEN_INSTRUCTION];
                        token->flags = SYNTAX_BOLD;
                    } else if (is_keyword(token_start, token_len, asm_registers)) {
                        token->color = colors[TOKEN_REGISTER];
                        token->flags = 0;
                    } else if (*token_start >= '0' && *token_start <= '9') {
                        token->color = colors[TOKEN_NUMBER];
                        token->flags = 0;
                    } else {
                        token->color = colors[TOKEN_NORMAL];
                        token->flags = 0;
                    }
                    (*token_count)++;
                }
                
                /* Skip comma or other punctuation */
                if (pos < line_text + line_len && (*pos == ',' || *pos == ':')) {
                    pos++;
                }
            }
            
        } else if (file_type == 1) {
            /* C file - word-by-word tokenization */
            
            /* Check for preprocessor lines first */
            if (line_text[0] == '#') {
                if (*token_count >= max_tokens) break;
                vizero_syntax_token_t* token = &(*tokens)[*token_count];
                token->range.start.line = line;
                token->range.start.column = 0;
                token->range.end.line = line;
                token->range.end.column = line_len;
                token->color = colors[TOKEN_PREPROCESSOR];
                token->flags = 0;
                (*token_count)++;
                continue;
            }
            
            /* Check for comment lines */
            if (strstr(line_text, "//") == line_text || strstr(line_text, "/*") == line_text) {
                if (*token_count >= max_tokens) break;
                vizero_syntax_token_t* token = &(*tokens)[*token_count];
                token->range.start.line = line;
                token->range.start.column = 0;
                token->range.end.line = line;
                token->range.end.column = line_len;
                token->color = colors[TOKEN_COMMENT];
                token->flags = SYNTAX_ITALIC;
                (*token_count)++;
                continue;
            }
            
            /* Parse words in the line */
            const char* pos = line_text;
            while (pos < line_text + line_len && *token_count < max_tokens) {
                /* Skip whitespace */
                while (pos < line_text + line_len && (*pos == ' ' || *pos == '\t')) {
                    pos++;
                }
                if (pos >= line_text + line_len) break;
                
                /* Handle strings */
                if (*pos == '"') {
                    const char* token_start = pos;
                    pos++; /* Skip opening quote */
                    /* Find closing quote */
                    while (pos < line_text + line_len && *pos != '"') {
                        if (*pos == '\\' && pos + 1 < line_text + line_len) {
                            pos += 2; /* Skip escaped character */
                        } else {
                            pos++;
                        }
                    }
                    if (pos < line_text + line_len && *pos == '"') {
                        pos++; /* Include closing quote */
                    }
                    
                    vizero_syntax_token_t* token = &(*tokens)[*token_count];
                    token->range.start.line = line;
                    token->range.start.column = token_start - line_text;
                    token->range.end.line = line;
                    token->range.end.column = pos - line_text;
                    token->color = colors[TOKEN_STRING];
                    token->flags = 0;
                    (*token_count)++;
                    continue;
                }
                
                /* Handle single-line comments */
                if (pos + 1 < line_text + line_len && pos[0] == '/' && pos[1] == '/') {
                    vizero_syntax_token_t* token = &(*tokens)[*token_count];
                    token->range.start.line = line;
                    token->range.start.column = pos - line_text;
                    token->range.end.line = line;
                    token->range.end.column = line_len;
                    token->color = colors[TOKEN_COMMENT];
                    token->flags = SYNTAX_ITALIC;
                    (*token_count)++;
                    break;
                }
                
                /* Handle block comments */
                if (pos + 1 < line_text + line_len && pos[0] == '/' && pos[1] == '*') {
                    const char* token_start = pos;
                    pos += 2;
                    /* Find closing */
                    while (pos + 1 < line_text + line_len && !(pos[0] == '*' && pos[1] == '/')) {
                        pos++;
                    }
                    if (pos + 1 < line_text + line_len) {
                        pos += 2; /* Include closing */
                    }
                    
                    vizero_syntax_token_t* token = &(*tokens)[*token_count];
                    token->range.start.line = line;
                    token->range.start.column = token_start - line_text;
                    token->range.end.line = line;
                    token->range.end.column = pos - line_text;
                    token->color = colors[TOKEN_COMMENT];
                    token->flags = SYNTAX_ITALIC;
                    (*token_count)++;
                    continue;
                }
                
                /* Handle regular words/identifiers */
                const char* token_start = pos;
                if ((*pos >= 'a' && *pos <= 'z') || (*pos >= 'A' && *pos <= 'Z') || *pos == '_') {
                    /* Identifier or keyword */
                    while (pos < line_text + line_len && 
                           ((*pos >= 'a' && *pos <= 'z') || (*pos >= 'A' && *pos <= 'Z') || 
                            (*pos >= '0' && *pos <= '9') || *pos == '_')) {
                        pos++;
                    }
                    
                    size_t token_len = pos - token_start;
                    vizero_syntax_token_t* token = &(*tokens)[*token_count];
                    token->range.start.line = line;
                    token->range.start.column = token_start - line_text;
                    token->range.end.line = line;
                    token->range.end.column = pos - line_text;
                    
                    /* Check if it's a keyword or type */
                    if (is_keyword(token_start, token_len, c_keywords)) {
                        token->color = colors[TOKEN_KEYWORD];
                        token->flags = SYNTAX_BOLD;
                    } else if (is_keyword(token_start, token_len, c_types)) {
                        token->color = colors[TOKEN_TYPE];
                        token->flags = 0;
                    } else {
                        token->color = colors[TOKEN_NORMAL];
                        token->flags = 0;
                    }
                    (*token_count)++;
                } else if (*pos >= '0' && *pos <= '9') {
                    /* Number */
                    while (pos < line_text + line_len && 
                           ((*pos >= '0' && *pos <= '9') || *pos == '.' || 
                            *pos == 'x' || *pos == 'X' || (*pos >= 'a' && *pos <= 'f') || 
                            (*pos >= 'A' && *pos <= 'F'))) {
                        pos++;
                    }
                    
                    vizero_syntax_token_t* token = &(*tokens)[*token_count];
                    token->range.start.line = line;
                    token->range.start.column = token_start - line_text;
                    token->range.end.line = line;
                    token->range.end.column = pos - line_text;
                    token->color = colors[TOKEN_NUMBER];
                    token->flags = 0;
                    (*token_count)++;
                } else {
                    /* Single character operator or punctuation */
                    pos++;
                    
                    vizero_syntax_token_t* token = &(*tokens)[*token_count];
                    token->range.start.line = line;
                    token->range.start.column = token_start - line_text;
                    token->range.end.line = line;
                    token->range.end.column = pos - line_text;
                    token->color = colors[TOKEN_OPERATOR];
                    token->flags = 0;
                    (*token_count)++;
                }
            }
        } else {
            /* Unknown file type */
            if (*token_count >= max_tokens) break;
            vizero_syntax_token_t* token = &(*tokens)[*token_count];
            token->range.start.line = line;
            token->range.start.column = 0;
            token->range.end.line = line;
            token->range.end.column = line_len;
            token->color = colors[TOKEN_NORMAL];
            token->flags = 0;
            (*token_count)++;
        }
    }
    
    return (*token_count > 0) ? 1 : 0;
}

VIZERO_PLUGIN_DEFINE_INFO(
    "Syntax Highlighter",
    "1.0.0",
    "Vizero Team", 
    "Syntax highlighting plugin for C and Assembly",
    VIZERO_PLUGIN_TYPE_SYNTAX_HIGHLIGHTER
)

VIZERO_PLUGIN_API int vizero_plugin_init(vizero_plugin_t* plugin, vizero_editor_t* editor, const vizero_editor_api_t* api) {
    if (!plugin || !editor || !api) return -1;
    
    editor_api = api;
    
    /* Register syntax highlighting callback */
    plugin->callbacks.highlight_syntax = highlight_syntax;
    
    return 0;
}

VIZERO_PLUGIN_API void vizero_plugin_cleanup(vizero_plugin_t* plugin) {
    (void)plugin;
    editor_api = NULL;
}