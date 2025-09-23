#ifndef VIZERO_LINE_H
#define VIZERO_LINE_H

#ifdef __cplusplus
extern "C" {
#endif

#include <stddef.h>

/* Forward declarations */
typedef struct vizero_line_t vizero_line_t;

/* Line structure representing a single line of text */
struct vizero_line_t {
    char* text;
    size_t length;
    size_t capacity;
    int dirty;
};

/* Line operations */
vizero_line_t* vizero_line_create(const char* text);
void vizero_line_destroy(vizero_line_t* line);
int vizero_line_insert(vizero_line_t* line, size_t pos, const char* text);
int vizero_line_delete(vizero_line_t* line, size_t start, size_t end);
const char* vizero_line_get_text(vizero_line_t* line);
size_t vizero_line_get_length(vizero_line_t* line);

#ifdef __cplusplus
}
#endif

#endif /* VIZERO_LINE_H */