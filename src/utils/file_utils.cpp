/* Complete file utilities implementation */
#include "vizero/file_utils.h"
#include <stdlib.h>
#include <string.h>
#include <stdio.h>

#ifdef _WIN32
#include <windows.h>
#include <direct.h>
#include <sys/stat.h>
#include <io.h>
#define mkdir _mkdir
#define access _access
#define F_OK 0
#else
#include <unistd.h>
#include <sys/stat.h>
#include <sys/types.h>
#include <dirent.h>
#endif

char* vizero_file_read_all(const char* filename, size_t* size) {
    if (!filename) {
        if (size) *size = 0;
        return NULL;
    }
    
    FILE* file = fopen(filename, "rb");
    if (!file) {
        if (size) *size = 0;
        return NULL;
    }
    
    /* Get file size */
    fseek(file, 0, SEEK_END);
    long file_size = ftell(file);
    fseek(file, 0, SEEK_SET);
    
    if (file_size < 0) {
        fclose(file);
        if (size) *size = 0;
        return NULL;
    }
    
    /* Allocate buffer */
    char* buffer = (char*)malloc(file_size + 1);
    if (!buffer) {
        fclose(file);
        if (size) *size = 0;
        return NULL;
    }
    
    /* Read file */
    size_t bytes_read = fread(buffer, 1, file_size, file);
    fclose(file);
    
    buffer[bytes_read] = '\0';
    
    /* Normalize line endings - convert \r\n to \n */
    size_t write_pos = 0;
    for (size_t read_pos = 0; read_pos < bytes_read; read_pos++) {
        if (buffer[read_pos] == '\r' && read_pos + 1 < bytes_read && buffer[read_pos + 1] == '\n') {
            /* Skip \r in \r\n sequence */
            continue;
        } else if (buffer[read_pos] == '\r') {
            /* Convert standalone \r to \n */
            buffer[write_pos++] = '\n';
        } else {
            /* Copy other characters as-is */
            buffer[write_pos++] = buffer[read_pos];
        }
    }
    
    buffer[write_pos] = '\0';
    
    if (size) *size = write_pos;
    return buffer;
}

int vizero_file_write_all(const char* filename, const char* data, size_t size) {
    if (!filename || !data) return -1;
    
    FILE* file = fopen(filename, "wb");
    if (!file) return -1;
    
    size_t bytes_written = fwrite(data, 1, size, file);
    fclose(file);
    
    return (bytes_written == size) ? 0 : -1;
}

int vizero_file_exists(const char* filename) {
    if (!filename) return 0;
    
#ifdef _WIN32
    return (_access(filename, F_OK) == 0) ? 1 : 0;
#else
    return (access(filename, F_OK) == 0) ? 1 : 0;
#endif
}

int vizero_file_is_directory(const char* filename) {
    if (!filename) return 0;
    
    struct stat st;
    if (stat(filename, &st) != 0) return 0;
    
#ifdef _WIN32
    return (st.st_mode & _S_IFDIR) ? 1 : 0;
#else
    return S_ISDIR(st.st_mode) ? 1 : 0;
#endif
}

char* vizero_path_get_directory(const char* path) {
    if (!path) return NULL;
    
    const char* last_slash = strrchr(path, '/');
    const char* last_backslash = strrchr(path, '\\');
    
    /* Find the last path separator */
    const char* last_sep = last_slash;
    if (last_backslash && (!last_slash || last_backslash > last_slash)) {
        last_sep = last_backslash;
    }
    
    if (!last_sep) {
        /* No directory separator found */
        char* current_dir = (char*)malloc(2);
        if (current_dir) {
            strcpy(current_dir, ".");
        }
        return current_dir;
    }
    
    /* Calculate directory length */
    size_t dir_len = last_sep - path;
    if (dir_len == 0) {
        /* Root directory */
        char* root = (char*)malloc(2);
        if (root) {
#ifdef _WIN32
            strcpy(root, "\\");
#else
            strcpy(root, "/");
#endif
        }
        return root;
    }
    
    char* directory = (char*)malloc(dir_len + 1);
    if (!directory) return NULL;
    
    strncpy(directory, path, dir_len);
    directory[dir_len] = '\0';
    
    return directory;
}

char* vizero_path_get_filename(const char* path) {
    if (!path) return NULL;
    
    const char* last_slash = strrchr(path, '/');
    const char* last_backslash = strrchr(path, '\\');
    
    /* Find the last path separator */
    const char* last_sep = last_slash;
    if (last_backslash && (!last_slash || last_backslash > last_slash)) {
        last_sep = last_backslash;
    }
    
    const char* filename = last_sep ? (last_sep + 1) : path;
    
    /* Duplicate the filename */
    size_t len = strlen(filename);
    char* result = (char*)malloc(len + 1);
    if (result) {
        strcpy(result, filename);
    }
    
    return result;
}

char* vizero_path_get_extension(const char* path) {
    if (!path) return NULL;
    
    /* Get just the filename part */
    const char* filename = strrchr(path, '/');
    const char* filename2 = strrchr(path, '\\');
    
    if (filename2 && (!filename || filename2 > filename)) {
        filename = filename2;
    }
    
    if (filename) {
        filename++; /* Skip the separator */
    } else {
        filename = path;
    }
    
    /* Find the last dot in the filename */
    const char* last_dot = strrchr(filename, '.');
    if (!last_dot || last_dot == filename) {
        /* No extension or filename starts with dot */
        return NULL;
    }
    
    /* Duplicate the extension (including the dot) */
    size_t len = strlen(last_dot);
    char* extension = (char*)malloc(len + 1);
    if (extension) {
        strcpy(extension, last_dot);
    }
    
    return extension;
}

char* vizero_path_join(const char* dir, const char* filename) {
    if (!dir || !filename) return NULL;
    
    size_t dir_len = strlen(dir);
    size_t filename_len = strlen(filename);
    
    /* Check if we need a separator */
    int need_separator = 1;
    if (dir_len > 0) {
        char last_char = dir[dir_len - 1];
        if (last_char == '/' || last_char == '\\') {
            need_separator = 0;
        }
    }
    
    /* Calculate total length */
    size_t total_len = dir_len + filename_len + (need_separator ? 1 : 0);
    
    char* result = (char*)malloc(total_len + 1);
    if (!result) return NULL;
    
    strcpy(result, dir);
    if (need_separator) {
#ifdef _WIN32
        strcat(result, "\\");
#else
        strcat(result, "/");
#endif
    }
    strcat(result, filename);
    
    return result;
}

int vizero_directory_create(const char* path) {
    if (!path) return -1;
    
#ifdef _WIN32
    return (_mkdir(path) == 0) ? 0 : -1;
#else
    return (mkdir(path, 0755) == 0) ? 0 : -1;
#endif
}

int vizero_directory_exists(const char* path) {
    if (!path) return 0;
    
    return vizero_file_is_directory(path);
}

char* vizero_get_executable_path(void) {
#ifdef _WIN32
    char buffer[MAX_PATH];
    DWORD length = GetModuleFileNameA(NULL, buffer, MAX_PATH);
    if (length == 0 || length == MAX_PATH) {
        return NULL;
    }
    
    char* result = (char*)malloc(length + 1);
    if (result) {
        strcpy(result, buffer);
    }
    return result;
#else
    char buffer[4096];
    ssize_t length = readlink("/proc/self/exe", buffer, sizeof(buffer) - 1);
    if (length == -1) {
        return NULL;
    }
    
    buffer[length] = '\0';
    char* result = (char*)malloc(length + 1);
    if (result) {
        strcpy(result, buffer);
    }
    return result;
#endif
}

char* vizero_get_executable_directory(void) {
    char* exe_path = vizero_get_executable_path();
    if (!exe_path) {
        return NULL;
    }
    
    char* exe_dir = vizero_path_get_directory(exe_path);
    free(exe_path);
    return exe_dir;
}

char* vizero_get_resource_path(const char* relative_path) {
    if (!relative_path) return NULL;
    
    char* exe_dir = vizero_get_executable_directory();
    if (!exe_dir) return NULL;
    
    char* resource_path = vizero_path_join(exe_dir, relative_path);
    free(exe_dir);
    return resource_path;
}
