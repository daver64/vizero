/* Project management implementation */
#include "vizero/project.h"
#include "vizero/buffer.h"
#include <stdlib.h>
#include <string.h>
#include <stdio.h>

#define MAX_PROJECT_BUFFERS 512

struct vizero_project_t {
    char* root_directory;
    char* name;
    vizero_buffer_t* buffers[MAX_PROJECT_BUFFERS];
    size_t buffer_count;
};

vizero_project_t* vizero_project_create(const char* root_directory) {
    if (!root_directory) return NULL;
    
    vizero_project_t* project = (vizero_project_t*)calloc(1, sizeof(vizero_project_t));
    if (!project) return NULL;
    
    project->root_directory = _strdup(root_directory);
    if (!project->root_directory) {
        free(project);
        return NULL;
    }
    
    /* Extract project name from directory path */
    const char* last_slash = strrchr(root_directory, '/');
    const char* last_backslash = strrchr(root_directory, '\\');
    const char* name_start = root_directory;
    
    if (last_slash && (!last_backslash || last_slash > last_backslash)) {
        name_start = last_slash + 1;
    } else if (last_backslash) {
        name_start = last_backslash + 1;
    }
    
    project->name = _strdup(name_start);
    project->buffer_count = 0;
    
    return project;
}

vizero_project_t* vizero_project_create_empty(void) {
    vizero_project_t* project = (vizero_project_t*)calloc(1, sizeof(vizero_project_t));
    if (project) {
        project->name = _strdup("Untitled Project");
        project->buffer_count = 0;
    }
    return project;
}

void vizero_project_destroy(vizero_project_t* project) {
    if (!project) return;
    
    /* Close all buffers */
    for (size_t i = 0; i < project->buffer_count; i++) {
        if (project->buffers[i]) {
            vizero_buffer_destroy(project->buffers[i]);
        }
    }
    
    if (project->root_directory) free(project->root_directory);
    if (project->name) free(project->name);
    free(project);
}

const char* vizero_project_get_root_directory(vizero_project_t* project) {
    return project ? project->root_directory : NULL;
}

int vizero_project_set_root_directory(vizero_project_t* project, const char* directory) {
    if (!project || !directory) return -1;
    
    char* new_dir = _strdup(directory);
    if (!new_dir) return -1;
    
    if (project->root_directory) free(project->root_directory);
    project->root_directory = new_dir;
    return 0;
}

const char* vizero_project_get_name(vizero_project_t* project) {
    return project ? project->name : NULL;
}

int vizero_project_set_name(vizero_project_t* project, const char* name) {
    if (!project || !name) return -1;
    
    char* new_name = _strdup(name);
    if (!new_name) return -1;
    
    if (project->name) free(project->name);
    project->name = new_name;
    return 0;
}

size_t vizero_project_get_buffer_count(vizero_project_t* project) {
    return project ? project->buffer_count : 0;
}

vizero_buffer_t* vizero_project_get_buffer(vizero_project_t* project, size_t index) {
    if (!project || index >= project->buffer_count) return NULL;
    return project->buffers[index];
}

vizero_buffer_t* vizero_project_find_buffer_by_filename(vizero_project_t* project, const char* filename) {
    if (!project || !filename) return NULL;
    
    for (size_t i = 0; i < project->buffer_count; i++) {
        const char* buffer_filename = vizero_buffer_get_filename(project->buffers[i]);
        if (buffer_filename && strcmp(buffer_filename, filename) == 0) {
            return project->buffers[i];
        }
    }
    
    return NULL;
}

int vizero_project_add_buffer(vizero_project_t* project, vizero_buffer_t* buffer) {
    if (!project || !buffer || project->buffer_count >= MAX_PROJECT_BUFFERS) return -1;
    
    project->buffers[project->buffer_count] = buffer;
    project->buffer_count++;
    return 0;
}

int vizero_project_remove_buffer(vizero_project_t* project, vizero_buffer_t* buffer) {
    if (!project || !buffer) return -1;
    
    /* Find buffer index */
    size_t buffer_index = SIZE_MAX;
    for (size_t i = 0; i < project->buffer_count; i++) {
        if (project->buffers[i] == buffer) {
            buffer_index = i;
            break;
        }
    }
    
    if (buffer_index == SIZE_MAX) return -1;
    
    /* Shift buffers down */
    for (size_t i = buffer_index; i < project->buffer_count - 1; i++) {
        project->buffers[i] = project->buffers[i + 1];
    }
    
    project->buffer_count--;
    return 0;
}

int vizero_project_close_buffer(vizero_project_t* project, size_t buffer_index) {
    if (!project || buffer_index >= project->buffer_count) return -1;
    
    vizero_buffer_t* buffer = project->buffers[buffer_index];
    vizero_buffer_destroy(buffer);
    
    /* Shift buffers down */
    for (size_t i = buffer_index; i < project->buffer_count - 1; i++) {
        project->buffers[i] = project->buffers[i + 1];
    }
    
    project->buffer_count--;
    return 0;
}

vizero_buffer_t* vizero_project_open_file(vizero_project_t* project, const char* filename) {
    if (!project || !filename) return NULL;
    
    /* Check if already open */
    vizero_buffer_t* existing = vizero_project_find_buffer_by_filename(project, filename);
    if (existing) return existing;
    
    /* Create new buffer */
    vizero_buffer_t* buffer = vizero_buffer_create_from_file(filename);
    if (!buffer) return NULL;
    
    if (vizero_project_add_buffer(project, buffer) != 0) {
        vizero_buffer_destroy(buffer);
        return NULL;
    }
    
    return buffer;
}

int vizero_project_create_new_buffer(vizero_project_t* project, const char* name) {
    if (!project || project->buffer_count >= MAX_PROJECT_BUFFERS) return -1;
    
    vizero_buffer_t* buffer = vizero_buffer_create();
    if (!buffer) return -1;
    
    if (name) {
        vizero_buffer_set_filename(buffer, name);
    }
    
    if (vizero_project_add_buffer(project, buffer) != 0) {
        vizero_buffer_destroy(buffer);
        return -1;
    }
    
    return 0;
}

int vizero_project_save_all_buffers(vizero_project_t* project) {
    if (!project) return -1;
    
    int errors = 0;
    for (size_t i = 0; i < project->buffer_count; i++) {
        if (vizero_buffer_is_modified(project->buffers[i])) {
            if (vizero_buffer_save(project->buffers[i]) != 0) {
                errors++;
            }
        }
    }
    
    return errors == 0 ? 0 : -1;
}

int vizero_project_has_unsaved_changes(vizero_project_t* project) {
    if (!project) return 0;
    
    for (size_t i = 0; i < project->buffer_count; i++) {
        if (vizero_buffer_is_modified(project->buffers[i])) {
            return 1;
        }
    }
    
    return 0;
}

/* File list operations */
void vizero_file_list_init(vizero_file_list_t* list) {
    if (list) {
        list->files = NULL;
        list->count = 0;
        list->capacity = 0;
    }
}

void vizero_file_list_destroy(vizero_file_list_t* list) {
    if (!list) return;
    
    for (size_t i = 0; i < list->count; i++) {
        if (list->files[i]) free(list->files[i]);
    }
    
    if (list->files) free(list->files);
    list->files = NULL;
    list->count = 0;
    list->capacity = 0;
}

int vizero_project_scan_files(vizero_project_t* project, vizero_file_list_t* files, 
                             const char* pattern, int recursive) {
    /* TODO: Implement file scanning */
    (void)project; (void)files; (void)pattern; (void)recursive;
    return 0;
}

int vizero_project_save_workspace(vizero_project_t* project, const char* filename) {
    /* TODO: Implement workspace saving */
    (void)project; (void)filename;
    return 0;
}

int vizero_project_load_workspace(vizero_project_t* project, const char* filename) {
    /* TODO: Implement workspace loading */
    (void)project; (void)filename;
    return 0;
}