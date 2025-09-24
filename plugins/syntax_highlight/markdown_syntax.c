#include "markdown_syntax.h"
#include <stdio.h>
#include <string.h>
#include <stdlib.h>

#define STYLE_HEADER 1
#define STYLE_BOLD   2
#define STYLE_ITALIC 3
#define STYLE_CODE   4
#define STYLE_LINK   5

// Pale, high-contrast colors for Markdown highlighting
static vizero_plugin_colour_t header_colour = { 255, 220, 120, 255 };   // Pale gold
static vizero_plugin_colour_t bold_colour   = { 240, 255, 240, 255 };   // Brighter pale green
static vizero_plugin_colour_t italic_colour = { 220, 220, 255, 255 };   // Pale blue
static vizero_plugin_colour_t code_colour   = { 255, 240, 200, 255 };   // Pale cream
static vizero_plugin_colour_t link_colour   = { 200, 240, 255, 255 };   // Pale cyan

static void add_token(vizero_syntax_token_t* tokens, size_t* count, size_t max_tokens, vizero_range_t range, vizero_plugin_colour_t color, uint32_t flags) {
    if (*count < max_tokens) {
        tokens[*count].range = range;
        tokens[*count].colour = color;
        tokens[*count].flags = flags;
        (*count)++;
    }
}

int markdown_highlight(
    vizero_buffer_t* buffer, size_t start_line, size_t end_line,
    vizero_syntax_token_t* tokens, size_t max_tokens,
    const vizero_editor_api_t* api)
{
    size_t token_count = 0;
    // int debug_token_count = 0;
    for (size_t line = start_line; line < end_line; ++line) {
        const char* text = NULL;
        size_t len = 0;
        if (api && api->get_buffer_line) {
            text = api->get_buffer_line(buffer, line);
            len = api->get_buffer_line_length(buffer, line);
        }
        if (!text) continue;
        if (text[0] == '#') {
            size_t i = 0;
            while (text[i] == '#') ++i;
            while (text[i] == ' ') ++i;
            vizero_range_t r = { {line, 0}, {line, len} };
            add_token(tokens, &token_count, max_tokens, r, header_colour, 0);
            // debug_token_count++;
            continue;
        }
        for (size_t i = 0; i < len;) {
            if ((text[i] == '*' && text[i+1] == '*') || (text[i] == '_' && text[i+1] == '_')) {
                char delim = text[i];
                size_t start = i;
                i += 2;
                while (i+1 < len && !(text[i] == delim && text[i+1] == delim)) ++i;
                if (i+1 < len) {
                    vizero_range_t r = { {line, start}, {line, i+2} };
                    add_token(tokens, &token_count, max_tokens, r, bold_colour, 1);
                    // debug_token_count++;
                    i += 2;
                    continue;
                }
            }
            if (text[i] == '*' || text[i] == '_') {
                char delim = text[i];
                size_t start = i;
                i++;
                while (i < len && text[i] != delim) ++i;
                if (i < len) {
                    vizero_range_t r = { {line, start}, {line, i+1} };
                    add_token(tokens, &token_count, max_tokens, r, italic_colour, 2);
                    // debug_token_count++;
                    i++;
                    continue;
                }
            }
            if (text[i] == '`') {
                size_t start = i;
                i++;
                while (i < len && text[i] != '`') ++i;
                if (i < len) {
                    vizero_range_t r = { {line, start}, {line, i+1} };
                    add_token(tokens, &token_count, max_tokens, r, code_colour, 3);
                    // debug_token_count++;
                    i++;
                    continue;
                }
            }
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
                        add_token(tokens, &token_count, max_tokens, r, link_colour, 4);
                        // debug_token_count++;
                        i++;
                        continue;
                    }
                }
            }
            i++;
        }
    }
    // printf("[markdown_highlight] produced %d tokens\n", debug_token_count);
    return (int)token_count;
}
