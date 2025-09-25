/* Markdown syntax highlighting plugin */
#include "vizero/plugin_interface.h"
#include <string.h>
#include <stdlib.h>

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

// Pale, high-contrast colors for Markdown highlighting
static vizero_plugin_colour_t header_colour = { 255, 220, 120, 255 };   // Pale gold
static vizero_plugin_colour_t bold_colour   = { 240, 255, 240, 255 };   // Brighter pale green
static vizero_plugin_colour_t italic_colour = { 220, 220, 255, 255 };   // Pale blue
static vizero_plugin_colour_t code_colour   = { 255, 240, 200, 255 };   // Pale cream
static vizero_plugin_colour_t link_colour   = { 200, 240, 255, 255 };   // Pale cyan

static const vizero_editor_api_t* editor_api = NULL;

/* Helper function to determine if file is markdown */
static int is_markdown_file(const char* filename) {
    if (!filename) return 0;
    
    const char* ext = strrchr(filename, '.');
    if (!ext) return 0;
    
    return (strcmp(ext, ".md") == 0 || strcmp(ext, ".markdown") == 0);
}

static void add_token(vizero_syntax_token_t* tokens, size_t* count, size_t max_tokens, 
                     vizero_range_t range, vizero_plugin_colour_t color, uint32_t flags) {
    if (*count < max_tokens) {
        tokens[*count].range = range;
        tokens[*count].colour = color;
        tokens[*count].flags = flags;
        (*count)++;
    }
}

static int highlight_markdown(vizero_buffer_t* buffer, size_t start_line, size_t end_line,
                             vizero_syntax_token_t* tokens, size_t max_tokens) {
    size_t token_count = 0;
    
    for (size_t line = start_line; line < end_line; ++line) {
        const char* text = NULL;
        size_t len = 0;
        if (editor_api && editor_api->get_buffer_line) {
            text = editor_api->get_buffer_line(buffer, line);
            len = editor_api->get_buffer_line_length(buffer, line);
        }
        if (!text) continue;
        
        /* Headers - lines starting with # */
        if (text[0] == '#') {
            size_t i = 0;
            while (text[i] == '#') ++i;
            while (text[i] == ' ') ++i;
            vizero_range_t r = { {line, 0}, {line, len} };
            add_token(tokens, &token_count, max_tokens, r, header_colour, 0);
            continue;
        }
        
        /* Process inline formatting */
        for (size_t i = 0; i < len;) {
            /* Bold text (**text** or __text__) */
            if ((text[i] == '*' && text[i+1] == '*') || (text[i] == '_' && text[i+1] == '_')) {
                char delim = text[i];
                size_t start = i;
                i += 2;
                while (i+1 < len && !(text[i] == delim && text[i+1] == delim)) ++i;
                if (i+1 < len) {
                    vizero_range_t r = { {line, start}, {line, i+2} };
                    add_token(tokens, &token_count, max_tokens, r, bold_colour, SYNTAX_BOLD);
                    i += 2;
                    continue;
                }
            }
            
            /* Italic text (*text* or _text_) */
            if (text[i] == '*' || text[i] == '_') {
                char delim = text[i];
                size_t start = i;
                i++;
                while (i < len && text[i] != delim) ++i;
                if (i < len) {
                    vizero_range_t r = { {line, start}, {line, i+1} };
                    add_token(tokens, &token_count, max_tokens, r, italic_colour, SYNTAX_ITALIC);
                    i++;
                    continue;
                }
            }
            
            /* Inline code (`code`) */
            if (text[i] == '`') {
                size_t start = i;
                i++;
                while (i < len && text[i] != '`') ++i;
                if (i < len) {
                    vizero_range_t r = { {line, start}, {line, i+1} };
                    add_token(tokens, &token_count, max_tokens, r, code_colour, 0);
                    i++;
                    continue;
                }
            }
            
            /* Links [text](url) */
            if (text[i] == '[') {
                size_t start = i;
                size_t link_end = 0;
                while (i < len && text[i] != ']') ++i;
                if (i < len && text[i] == ']' && text[i+1] == '(') {
                    i += 2;
                    while (i < len && text[i] != ')') ++i;
                    if (i < len && text[i] == ')') {
                        link_end = i+1;
                        vizero_range_t r = { {line, start}, {line, link_end} };
                        add_token(tokens, &token_count, max_tokens, r, link_colour, 0);
                        i++;
                        continue;
                    }
                }
            }
            i++;
        }
    }
    return (int)token_count;
}

/* Main syntax highlighting function */
static int highlight_syntax(vizero_buffer_t* buffer, size_t start_line, size_t end_line,
                           vizero_syntax_token_t* tokens, size_t max_tokens) {
    if (!buffer || !tokens || !editor_api) return 0;
    if (!editor_api->get_buffer_line || !editor_api->get_buffer_filename) return 0;
    
    const char* filename = editor_api->get_buffer_filename(buffer);
    
    /* Only handle Markdown files */
    if (!is_markdown_file(filename)) return 0;
    
    return highlight_markdown(buffer, start_line, end_line, tokens, max_tokens);
}

VIZERO_PLUGIN_DEFINE_INFO(
    "Markdown Syntax Highlighter",
    "1.0.0",
    "Vizero Team", 
    "Syntax highlighting plugin for Markdown files",
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