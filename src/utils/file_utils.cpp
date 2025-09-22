/* Stub implementations */
#include "vizero/file_utils.h"
#include <stdlib.h>
#include <string.h>

char* vizero_file_read_all(const char* filename, size_t* size) {
    (void)filename; if (size) *size = 0; return NULL;
}

int vizero_file_write_all(const char* filename, const char* data, size_t size) {
    (void)filename; (void)data; (void)size; return 0;
}

int vizero_file_exists(const char* filename) {
    (void)filename; return 0;
}

int vizero_file_is_directory(const char* filename) {
    (void)filename; return 0;
}

char* vizero_path_get_directory(const char* path) {
    (void)path; return NULL;
}

char* vizero_path_get_filename(const char* path) {
    (void)path; return NULL;
}

char* vizero_path_get_extension(const char* path) {
    (void)path; return NULL;
}

char* vizero_path_join(const char* dir, const char* filename) {
    (void)dir; (void)filename; return NULL;
}

int vizero_directory_create(const char* path) {
    (void)path; return 0;
}

int vizero_directory_exists(const char* path) {
    (void)path; return 0;
}