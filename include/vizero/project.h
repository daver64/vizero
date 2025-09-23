#ifndef VIZERO_PROJECT_H
#define VIZERO_PROJECT_H

#ifdef __cplusplus
extern "C" {
#endif

#include <stddef.h>

/* Forward declarations */
typedef struct vizero_project_t vizero_project_t;
typedef struct vizero_buffer_t vizero_buffer_t;

/* Project creation and destruction */
vizero_project_t* vizero_project_create(const char* root_directory);
vizero_project_t* vizero_project_create_empty(void);
void vizero_project_destroy(vizero_project_t* project);

/* Project properties */
const char* vizero_project_get_root_directory(vizero_project_t* project);
int vizero_project_set_root_directory(vizero_project_t* project, const char* directory);
const char* vizero_project_get_name(vizero_project_t* project);
int vizero_project_set_name(vizero_project_t* project, const char* name);

/* Buffer management */
size_t vizero_project_get_buffer_count(vizero_project_t* project);
vizero_buffer_t* vizero_project_get_buffer(vizero_project_t* project, size_t index);
vizero_buffer_t* vizero_project_find_buffer_by_filename(vizero_project_t* project, const char* filename);
int vizero_project_add_buffer(vizero_project_t* project, vizero_buffer_t* buffer);
int vizero_project_remove_buffer(vizero_project_t* project, vizero_buffer_t* buffer);
int vizero_project_close_buffer(vizero_project_t* project, size_t buffer_index);

/* File operations */
vizero_buffer_t* vizero_project_open_file(vizero_project_t* project, const char* filename);
int vizero_project_create_new_buffer(vizero_project_t* project, const char* name);
int vizero_project_save_all_buffers(vizero_project_t* project);
int vizero_project_has_unsaved_changes(vizero_project_t* project);

/* Project file discovery */
typedef struct {
    char** files;
    size_t count;
    size_t capacity;
} vizero_file_list_t;

void vizero_file_list_init(vizero_file_list_t* list);
void vizero_file_list_destroy(vizero_file_list_t* list);
int vizero_project_scan_files(vizero_project_t* project, vizero_file_list_t* files, 
                             const char* pattern, int recursive);

/* Project workspace file */
int vizero_project_save_workspace(vizero_project_t* project, const char* filename);
int vizero_project_load_workspace(vizero_project_t* project, const char* filename);

#ifdef __cplusplus
}
#endif

#endif /* VIZERO_PROJECT_H */