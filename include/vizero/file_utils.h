#ifndef VIZERO_FILE_UTILS_H
#define VIZERO_FILE_UTILS_H

#ifdef __cplusplus
extern "C" {
#endif

#include <stddef.h>

/* File operations */
char* vizero_file_read_all(const char* filename, size_t* size);
int vizero_file_write_all(const char* filename, const char* data, size_t size);
int vizero_file_exists(const char* filename);
int vizero_file_is_directory(const char* filename);

/* Path operations */
char* vizero_path_get_directory(const char* path);
char* vizero_path_get_filename(const char* path);
char* vizero_path_get_extension(const char* path);
char* vizero_path_join(const char* dir, const char* filename);

/* Directory operations */
int vizero_directory_create(const char* path);
int vizero_directory_exists(const char* path);

/* Executable path operations */
char* vizero_get_executable_path(void);
char* vizero_get_executable_directory(void);
char* vizero_get_resource_path(const char* relative_path);

/* File encoding detection */
int vizero_file_is_utf16(const char* filename);

#ifdef __cplusplus
}
#endif

#endif /* VIZERO_FILE_UTILS_H */