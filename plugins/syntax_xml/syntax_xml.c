/* XML syntax highlighting plugin */
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
    TOKEN_TAG,
    TOKEN_ATTRIBUTE_NAME,
    TOKEN_ATTRIBUTE_VALUE,
    TOKEN_COMMENT,
    TOKEN_CDATA,
    TOKEN_DOCTYPE,
    TOKEN_PROCESSING_INSTRUCTION,
    TOKEN_TEXT_CONTENT
} token_type_t;

/* Color definitions - XML themed colors */
static vizero_plugin_colour_t colours[] = {
    {255, 255, 255, 255}, /* TOKEN_NORMAL - white */
    {86, 156, 214, 255},  /* TOKEN_TAG - blue */
    {156, 220, 254, 255}, /* TOKEN_ATTRIBUTE_NAME - light blue */
    {206, 145, 120, 255}, /* TOKEN_ATTRIBUTE_VALUE - orange */
    {106, 153, 85, 255},  /* TOKEN_COMMENT - green */
    {255, 215, 0, 255},   /* TOKEN_CDATA - gold */
    {155, 155, 155, 255}, /* TOKEN_DOCTYPE - gray */
    {255, 140, 0, 255},   /* TOKEN_PROCESSING_INSTRUCTION - orange */
    {212, 212, 212, 255}  /* TOKEN_TEXT_CONTENT - light gray */
};

static const vizero_editor_api_t* editor_api = NULL;

/* Helper function to determine if file is XML */
static int is_xml_file(const char* filename) {
    if (!filename) return 0;
    
    const char* ext = strrchr(filename, '.');
    if (!ext) return 0;
    
    return (strcmp(ext, ".xml") == 0 || strcmp(ext, ".xsd") == 0 || 
            strcmp(ext, ".xsl") == 0 || strcmp(ext, ".xslt") == 0 ||
            strcmp(ext, ".xaml") == 0 || strcmp(ext, ".config") == 0 ||
            strcmp(ext, ".plist") == 0 || strcmp(ext, ".svg") == 0);
}

/* XML syntax highlighting */
static int UNUSED_PARAM highlight_xml_line(const char* line, size_t line_num, 
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
        
        /* XML Comments <!-- --> */
        if (i + 4 <= line_len && strncmp(line + i, "<!--", 4) == 0) {
            size_t start = i;
            i += 4;
            /* Find end of comment or end of line */
            while (i + 3 <= line_len) {
                if (strncmp(line + i, "-->", 3) == 0) {
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
            token->colour = colours[TOKEN_COMMENT];
            token->flags = SYNTAX_ITALIC;
            continue;
        }
        
        /* CDATA sections <![CDATA[ ]]> */
        if (i + 9 <= line_len && strncmp(line + i, "<![CDATA[", 9) == 0) {
            size_t start = i;
            i += 9;
            /* Find end of CDATA or end of line */
            while (i + 3 <= line_len) {
                if (strncmp(line + i, "]]>", 3) == 0) {
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
            token->colour = colours[TOKEN_CDATA];
            token->flags = 0;
            continue;
        }
        
        /* DOCTYPE declarations */
        if (i + 9 <= line_len && strncmp(line + i, "<!DOCTYPE", 9) == 0) {
            size_t start = i;
            i += 9;
            /* Find end of DOCTYPE or end of line */
            while (i < line_len && line[i] != '>') i++;
            if (i < line_len) i++; /* Include closing > */
            
            vizero_syntax_token_t* token = &tokens[count++];
            token->range.start.line = line_num;
            token->range.start.column = start;
            token->range.end.line = line_num;
            token->range.end.column = i;
            token->colour = colours[TOKEN_DOCTYPE];
            token->flags = SYNTAX_BOLD;
            continue;
        }
        
        /* Processing Instructions <?xml ?> */
        if (i + 1 < line_len && line[i] == '<' && line[i + 1] == '?') {
            size_t start = i;
            i += 2;
            /* Find end of PI or end of line */
            while (i + 1 < line_len) {
                if (line[i] == '?' && line[i + 1] == '>') {
                    i += 2;
                    break;
                }
                i++;
            }
            /* If we didn't find the end, go to end of line */
            if (i + 1 >= line_len) i = line_len;
            
            vizero_syntax_token_t* token = &tokens[count++];
            token->range.start.line = line_num;
            token->range.start.column = start;
            token->range.end.line = line_num;
            token->range.end.column = i;
            token->colour = colours[TOKEN_PROCESSING_INSTRUCTION];
            token->flags = 0;
            continue;
        }
        
        /* XML Tags */
        if (line[i] == '<') {
            size_t start = i;
            i++;
            
            /* Skip closing tag marker */
            if (i < line_len && line[i] == '/') i++;
            
            /* Parse tag name */
            while (i < line_len && (isalnum(line[i]) || line[i] == '_' || line[i] == ':' || line[i] == '-')) i++;
            
            /* Highlight tag name */
            if (i > start + 1) {
                vizero_syntax_token_t* token = &tokens[count++];
                token->range.start.line = line_num;
                token->range.start.column = start;
                token->range.end.line = line_num;
                token->range.end.column = i;
                token->colour = colours[TOKEN_TAG];
                token->flags = SYNTAX_BOLD;
            }
            
            /* Parse attributes */
            while (i < line_len && line[i] != '>' && line[i] != '/' && count < max_tokens) {
                size_t start_pos = i; /* Safety check to prevent infinite loops */
                
                /* Skip whitespace */
                while (i < line_len && isspace(line[i])) i++;
                if (i >= line_len || line[i] == '>' || line[i] == '/') break;
                
                /* Attribute name */
                size_t attr_start = i;
                while (i < line_len && (isalnum(line[i]) || line[i] == '_' || line[i] == ':' || line[i] == '-')) i++;
                
                if (i > attr_start) {
                    if (count < max_tokens) {
                        vizero_syntax_token_t* token = &tokens[count++];
                        token->range.start.line = line_num;
                        token->range.start.column = attr_start;
                        token->range.end.line = line_num;
                        token->range.end.column = i;
                        token->colour = colours[TOKEN_ATTRIBUTE_NAME];
                        token->flags = 0;
                    }
                }
                
                /* Skip whitespace and = */
                while (i < line_len && (isspace(line[i]) || line[i] == '=')) i++;
                
                /* Attribute value */
                if (i < line_len && (line[i] == '"' || line[i] == '\'')) {
                    char quote = line[i];
                    size_t value_start = i++;
                    while (i < line_len && line[i] != quote) i++;
                    if (i < line_len) i++; /* Include closing quote */
                    
                    if (count < max_tokens) {
                        vizero_syntax_token_t* token = &tokens[count++];
                        token->range.start.line = line_num;
                        token->range.start.column = value_start;
                        token->range.end.line = line_num;
                        token->range.end.column = i;
                        token->colour = colours[TOKEN_ATTRIBUTE_VALUE];
                        token->flags = 0;
                    }
                }
                
                /* Safety check: if we haven't advanced, break to prevent infinite loop */
                if (i == start_pos) {
                    i++; /* Force advance by one character */
                }
            }
            
            /* Skip to end of tag */
            while (i < line_len && line[i] != '>') i++;
            if (i < line_len) i++; /* Include closing > */
            continue;
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
    
    /* Only handle XML files */
    if (!is_xml_file(filename)) return 0;
    
    size_t token_count = 0;
    for (size_t line = start_line; line <= end_line && token_count < max_tokens; line++) {
        const char* line_text = editor_api->get_buffer_line(buffer, line);
        if (!line_text) continue;
        
        int n = highlight_xml_line(line_text, line, tokens + token_count, max_tokens - token_count);
        if (n > 0) token_count += n;
    }
    return (int)token_count;
}

VIZERO_PLUGIN_DEFINE_INFO(
    "XML Syntax Highlighter",
    "1.0.0",
    "Vizero Team", 
    "Syntax highlighting plugin for XML, XSD, XSLT, XAML, and other XML-based files",
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