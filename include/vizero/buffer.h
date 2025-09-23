
#ifndef VIZERO_BUFFER_H
#define VIZERO_BUFFER_H

#include <stddef.h>
#include <stdint.h>

#ifdef __cplusplus
extern "C" {
#endif

typedef struct vizero_buffer_t vizero_buffer_t;
typedef struct vizero_line_t vizero_line_t;

typedef struct {
    size_t line;
    size_t column;
    size_t length;
} vizero_search_result_t;

typedef struct {
    size_t line_count;
    size_t character_count;
    size_t word_count;
    size_t byte_count;
} vizero_buffer_stats_t;

/* Buffer creation and destruction */
vizero_buffer_t* vizero_buffer_create(void);
vizero_buffer_t* vizero_buffer_create_from_file(const char* filename);
void vizero_buffer_destroy(vizero_buffer_t* buffer);

/* Buffer properties */
const char* vizero_buffer_get_filename(vizero_buffer_t* buffer);
int vizero_buffer_set_filename(vizero_buffer_t* buffer, const char* filename);
int vizero_buffer_is_modified(vizero_buffer_t* buffer);
int vizero_buffer_is_readonly(vizero_buffer_t* buffer);
void vizero_buffer_set_readonly(vizero_buffer_t* buffer, int readonly);

/* Buffer content operations */
size_t vizero_buffer_get_line_count(vizero_buffer_t* buffer);
vizero_line_t* vizero_buffer_get_line(vizero_buffer_t* buffer, size_t line_num);
const char* vizero_buffer_get_line_text(vizero_buffer_t* buffer, size_t line_num);
size_t vizero_buffer_get_line_length(vizero_buffer_t* buffer, size_t line_num);
const char* vizero_buffer_get_text(vizero_buffer_t* buffer);  /* Get entire buffer as string */

/* Text modification */
int vizero_buffer_insert_char(vizero_buffer_t* buffer, size_t line, size_t col, char c);
int vizero_buffer_insert_text(vizero_buffer_t* buffer, size_t line, size_t col, const char* text);
int vizero_buffer_delete_char(vizero_buffer_t* buffer, size_t line, size_t col);
int vizero_buffer_delete_range(vizero_buffer_t* buffer, size_t start_line, size_t start_col, 
                              size_t end_line, size_t end_col);

/* Line operations */
int vizero_buffer_insert_line(vizero_buffer_t* buffer, size_t line_num, const char* text);
int vizero_buffer_delete_line(vizero_buffer_t* buffer, size_t line_num);
int vizero_buffer_split_line(vizero_buffer_t* buffer, size_t line_num, size_t col);
int vizero_buffer_join_lines(vizero_buffer_t* buffer, size_t line_num);

/* File I/O */
int vizero_buffer_load_from_file(vizero_buffer_t* buffer, const char* filename);
int vizero_buffer_save_to_file(vizero_buffer_t* buffer, const char* filename);
int vizero_buffer_save(vizero_buffer_t* buffer);

/* Search operations */
int vizero_buffer_search(vizero_buffer_t* buffer, const char* pattern, int use_regex,
                        vizero_search_result_t* results, size_t max_results, size_t* result_count);

/* File modification time polling for auto-reload */
uint64_t vizero_buffer_get_last_disk_mtime(vizero_buffer_t* buffer);
void vizero_buffer_set_last_disk_mtime(vizero_buffer_t* buffer, uint64_t mtime);

/* Undo/Redo support */
int vizero_buffer_undo(vizero_buffer_t* buffer);
int vizero_buffer_redo(vizero_buffer_t* buffer);
int vizero_buffer_can_undo(vizero_buffer_t* buffer);
int vizero_buffer_can_redo(vizero_buffer_t* buffer);

void vizero_buffer_get_stats(vizero_buffer_t* buffer, vizero_buffer_stats_t* stats);

#ifdef __cplusplus
}
#endif

#endif /* VIZERO_BUFFER_H */