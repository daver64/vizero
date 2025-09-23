#ifndef VIZERO_MARKDOWN_SYNTAX_H
#define VIZERO_MARKDOWN_SYNTAX_H

#include "vizero/plugin_interface.h"

#ifdef __cplusplus
extern "C" {
#endif

VIZERO_PLUGIN_API int markdown_highlight(
    vizero_buffer_t* buffer, size_t start_line, size_t end_line,
    vizero_syntax_token_t** tokens, size_t* token_count,
    const vizero_editor_api_t* api);

#ifdef __cplusplus
}
#endif

#endif // VIZERO_MARKDOWN_SYNTAX_H
