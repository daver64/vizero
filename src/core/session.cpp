#include "vizero/session.h"
#include "vizero/editor_state.h"
#include "vizero/settings.h"
#include <stdlib.h>
#include <string.h>
#include <stdio.h>
#include <sys/stat.h>
#include <time.h>

#ifdef _WIN32
#include <direct.h>
#include <io.h>
#define mkdir(path, mode) _mkdir(path)
#define access(path, mode) _access(path, mode)
#define F_OK 0
#else
#include <unistd.h>
#endif

/* Session manager lifecycle */
vizero_session_manager_t* vizero_session_manager_create(void) {
    vizero_session_manager_t* manager = (vizero_session_manager_t*)malloc(sizeof(vizero_session_manager_t));
    if (!manager) return NULL;
    
    memset(manager, 0, sizeof(vizero_session_manager_t));
    
    /* Set up sessions directory */
    const char* sessions_dir = vizero_session_get_sessions_directory();
    strncpy(manager->sessions_directory, sessions_dir, sizeof(manager->sessions_directory) - 1);
    manager->sessions_directory[sizeof(manager->sessions_directory) - 1] = '\0';
    
    /* Ensure sessions directory exists */
    vizero_session_ensure_sessions_directory();
    
    return manager;
}

void vizero_session_manager_destroy(vizero_session_manager_t* manager) {
    if (!manager) return;
    
    if (manager->current_session) {
        vizero_session_destroy(manager->current_session);
    }
    
    free(manager);
}

/* Session operations */
vizero_session_t* vizero_session_create(const char* name, const char* project_path) {
    if (!name || !project_path) return NULL;
    
    vizero_session_t* session = (vizero_session_t*)malloc(sizeof(vizero_session_t));
    if (!session) return NULL;
    
    memset(session, 0, sizeof(vizero_session_t));
    
    /* Set basic info */
    strncpy(session->name, name, sizeof(session->name) - 1);
    session->name[sizeof(session->name) - 1] = '\0';
    
    strncpy(session->project_path, project_path, sizeof(session->project_path) - 1);
    session->project_path[sizeof(session->project_path) - 1] = '\0';
    
    /* Generate session file path */
    snprintf(session->session_file, sizeof(session->session_file), 
             "%s%c.vizero-session", project_path, 
#ifdef _WIN32
             '\\'
#else
             '/'
#endif
    );
    
    /* Set timestamps */
    session->created = time(NULL);
    session->last_modified = session->created;
    
    /* Set default project settings */
    strncpy(session->build_command, "make", sizeof(session->build_command) - 1);
    strncpy(session->run_command, "./a.out", sizeof(session->run_command) - 1);
    
    /* Detect project type */
    vizero_session_detect_project_type(project_path, session->project_type, sizeof(session->project_type));
    
    return session;
}

void vizero_session_destroy(vizero_session_t* session) {
    if (!session) return;
    free(session);
}

/* Utility functions */
const char* vizero_session_get_sessions_directory(void) {
    static char sessions_dir[512];
    static bool initialized = false;
    
    if (!initialized) {
#ifdef _WIN32
        const char* appdata = getenv("APPDATA");
        if (appdata) {
            snprintf(sessions_dir, sizeof(sessions_dir), "%s\\Vizero\\sessions", appdata);
        } else {
            strncpy(sessions_dir, ".\\sessions", sizeof(sessions_dir) - 1);
        }
#else
        const char* home = getenv("HOME");
        if (home) {
            snprintf(sessions_dir, sizeof(sessions_dir), "%s/.vizero/sessions", home);
        } else {
            strncpy(sessions_dir, "./sessions", sizeof(sessions_dir) - 1);
        }
#endif
        sessions_dir[sizeof(sessions_dir) - 1] = '\0';
        initialized = true;
    }
    
    return sessions_dir;
}

int vizero_session_ensure_sessions_directory(void) {
    const char* sessions_dir = vizero_session_get_sessions_directory();
    
    /* Check if directory exists */
    if (access(sessions_dir, F_OK) == 0) {
        return 0; /* Already exists */
    }
    
    /* Create parent directory first if needed */
#ifdef _WIN32
    char parent_dir[512];
    strncpy(parent_dir, sessions_dir, sizeof(parent_dir) - 1);
    parent_dir[sizeof(parent_dir) - 1] = '\0';
    
    char* last_slash = strrchr(parent_dir, '\\');
    if (last_slash) {
        *last_slash = '\0';
        if (access(parent_dir, F_OK) != 0) {
            if (mkdir(parent_dir, 0755) != 0) {
                return -1;
            }
        }
    }
#else
    char parent_dir[512];
    strncpy(parent_dir, sessions_dir, sizeof(parent_dir) - 1);
    parent_dir[sizeof(parent_dir) - 1] = '\0';
    
    char* last_slash = strrchr(parent_dir, '/');
    if (last_slash) {
        *last_slash = '\0';
        if (access(parent_dir, F_OK) != 0) {
            if (mkdir(parent_dir, 0755) != 0) {
                return -1;
            }
        }
    }
#endif
    
    /* Create sessions directory */
    if (mkdir(sessions_dir, 0755) != 0) {
        return -1;
    }
    
    return 0;
}

/* Project detection */
int vizero_session_detect_project_type(const char* project_path, char* project_type, size_t type_size) {
    if (!project_path || !project_type || type_size == 0) return -1;
    
    char filepath[1024];
    
    /* Check for common project files */
    
    /* C/C++ projects */
    snprintf(filepath, sizeof(filepath), "%s%cMakefile", project_path, 
#ifdef _WIN32
             '\\'
#else
             '/'
#endif
    );
    if (access(filepath, F_OK) == 0) {
        strncpy(project_type, "cpp", type_size - 1);
        project_type[type_size - 1] = '\0';
        return 0;
    }
    
    snprintf(filepath, sizeof(filepath), "%s%cCMakeLists.txt", project_path, 
#ifdef _WIN32
             '\\'
#else
             '/'
#endif
    );
    if (access(filepath, F_OK) == 0) {
        strncpy(project_type, "cpp", type_size - 1);
        project_type[type_size - 1] = '\0';
        return 0;
    }
    
    /* Python projects */
    snprintf(filepath, sizeof(filepath), "%s%crequirements.txt", project_path, 
#ifdef _WIN32
             '\\'
#else
             '/'
#endif
    );
    if (access(filepath, F_OK) == 0) {
        strncpy(project_type, "python", type_size - 1);
        project_type[type_size - 1] = '\0';
        return 0;
    }
    
    snprintf(filepath, sizeof(filepath), "%s%csetup.py", project_path, 
#ifdef _WIN32
             '\\'
#else
             '/'
#endif
    );
    if (access(filepath, F_OK) == 0) {
        strncpy(project_type, "python", type_size - 1);
        project_type[type_size - 1] = '\0';
        return 0;
    }
    
    /* JavaScript/Node.js projects */
    snprintf(filepath, sizeof(filepath), "%s%cpackage.json", project_path, 
#ifdef _WIN32
             '\\'
#else
             '/'
#endif
    );
    if (access(filepath, F_OK) == 0) {
        strncpy(project_type, "javascript", type_size - 1);
        project_type[type_size - 1] = '\0';
        return 0;
    }
    
    /* Default to generic */
    strncpy(project_type, "generic", type_size - 1);
    project_type[type_size - 1] = '\0';
    return 0;
}

/* Session persistence - Basic INI-style format */
int vizero_session_save_to_file(const vizero_session_t* session, const char* filepath) {
    if (!session || !filepath) return -1;
    
    FILE* file = fopen(filepath, "w");
    if (!file) return -1;
    
    /* Write session header */
    fprintf(file, "[session]\n");
    fprintf(file, "name=%s\n", session->name);
    fprintf(file, "project_path=%s\n", session->project_path);
    fprintf(file, "created=%ld\n", (long)session->created);
    fprintf(file, "last_modified=%ld\n", (long)session->last_modified);
    fprintf(file, "\n");
    
    /* Write project settings */
    fprintf(file, "[project]\n");
    fprintf(file, "type=%s\n", session->project_type);
    fprintf(file, "build_command=%s\n", session->build_command);
    fprintf(file, "run_command=%s\n", session->run_command);
    fprintf(file, "\n");
    
    /* Write buffers */
    fprintf(file, "[buffers]\n");
    fprintf(file, "count=%zu\n", session->buffer_count);
    for (size_t i = 0; i < session->buffer_count; i++) {
        if (session->buffers[i].is_active) {
            fprintf(file, "buffer%zu_file=%s\n", i, session->buffers[i].filepath);
            fprintf(file, "buffer%zu_cursor_line=%zu\n", i, session->buffers[i].cursor_line);
            fprintf(file, "buffer%zu_cursor_column=%zu\n", i, session->buffers[i].cursor_column);
            fprintf(file, "buffer%zu_scroll_line=%zu\n", i, session->buffers[i].scroll_line);
            fprintf(file, "buffer%zu_modified=%s\n", i, session->buffers[i].is_modified ? "true" : "false");
        }
    }
    fprintf(file, "\n");
    
    /* Write windows */
    fprintf(file, "[windows]\n");
    fprintf(file, "count=%zu\n", session->window_count);
    for (size_t i = 0; i < session->window_count; i++) {
        if (session->windows[i].is_active) {
            fprintf(file, "window%zu_buffer=%zu\n", i, session->windows[i].buffer_index);
            fprintf(file, "window%zu_focused=%s\n", i, session->windows[i].is_focused ? "true" : "false");
        }
    }
    fprintf(file, "\n");
    
    /* Write MRU order */
    fprintf(file, "[mru]\n");
    fprintf(file, "count=%zu\n", session->mru_count);
    if (session->mru_count > 0) {
        fprintf(file, "order=");
        for (size_t i = 0; i < session->mru_count; i++) {
            fprintf(file, "%zu", session->mru_buffer_indices[i]);
            if (i < session->mru_count - 1) {
                fprintf(file, ",");
            }
        }
        fprintf(file, "\n");
    }
    
    fclose(file);
    return 0;
}

/* Simple INI parser for session loading */
static char* trim_whitespace(char* str) {
    if (!str) return NULL;
    
    /* Trim leading whitespace */
    while (*str == ' ' || *str == '\t') str++;
    
    /* Trim trailing whitespace */
    char* end = str + strlen(str) - 1;
    while (end > str && (*end == ' ' || *end == '\t' || *end == '\n' || *end == '\r')) {
        *end = '\0';
        end--;
    }
    
    return str;
}

vizero_session_t* vizero_session_load_from_file(const char* filepath) {
    if (!filepath) return NULL;
    
    FILE* file = fopen(filepath, "r");
    if (!file) return NULL;
    
    vizero_session_t* session = (vizero_session_t*)malloc(sizeof(vizero_session_t));
    if (!session) {
        fclose(file);
        return NULL;
    }
    
    memset(session, 0, sizeof(vizero_session_t));
    strncpy(session->session_file, filepath, sizeof(session->session_file) - 1);
    
    char line[1024];
    char current_section[64] = "";
    
    while (fgets(line, sizeof(line), file)) {
        char* trimmed = trim_whitespace(line);
        if (!trimmed || *trimmed == '\0' || *trimmed == '#') continue;
        
        /* Check for section headers */
        if (*trimmed == '[') {
            char* end = strchr(trimmed, ']');
            if (end) {
                *end = '\0';
                strncpy(current_section, trimmed + 1, sizeof(current_section) - 1);
                current_section[sizeof(current_section) - 1] = '\0';
            }
            continue;
        }
        
        /* Parse key=value pairs */
        char* equals = strchr(trimmed, '=');
        if (!equals) continue;
        
        *equals = '\0';
        char* key = trim_whitespace(trimmed);
        char* value = trim_whitespace(equals + 1);
        
        if (!key || !value) continue;
        
        /* Handle different sections */
        if (strcmp(current_section, "session") == 0) {
            if (strcmp(key, "name") == 0) {
                strncpy(session->name, value, sizeof(session->name) - 1);
            } else if (strcmp(key, "project_path") == 0) {
                strncpy(session->project_path, value, sizeof(session->project_path) - 1);
            } else if (strcmp(key, "created") == 0) {
                session->created = (time_t)atol(value);
            } else if (strcmp(key, "last_modified") == 0) {
                session->last_modified = (time_t)atol(value);
            }
        } else if (strcmp(current_section, "project") == 0) {
            if (strcmp(key, "type") == 0) {
                strncpy(session->project_type, value, sizeof(session->project_type) - 1);
            } else if (strcmp(key, "build_command") == 0) {
                strncpy(session->build_command, value, sizeof(session->build_command) - 1);
            } else if (strcmp(key, "run_command") == 0) {
                strncpy(session->run_command, value, sizeof(session->run_command) - 1);
            }
        } else if (strcmp(current_section, "buffers") == 0) {
            if (strcmp(key, "count") == 0) {
                session->buffer_count = (size_t)atol(value);
            } else if (strncmp(key, "buffer", 6) == 0) {
                /* Parse buffer index and property */
                char* underscore = strchr(key + 6, '_');
                if (underscore) {
                    size_t buffer_idx = (size_t)atol(key + 6);
                    if (buffer_idx < MAX_SESSION_BUFFERS) {
                        session->buffers[buffer_idx].is_active = true;
                        char* property = underscore + 1;
                        
                        if (strcmp(property, "file") == 0) {
                            strncpy(session->buffers[buffer_idx].filepath, value, 
                                   sizeof(session->buffers[buffer_idx].filepath) - 1);
                        } else if (strcmp(property, "cursor_line") == 0) {
                            session->buffers[buffer_idx].cursor_line = (size_t)atol(value);
                        } else if (strcmp(property, "cursor_column") == 0) {
                            session->buffers[buffer_idx].cursor_column = (size_t)atol(value);
                        } else if (strcmp(property, "scroll_line") == 0) {
                            session->buffers[buffer_idx].scroll_line = (size_t)atol(value);
                        } else if (strcmp(property, "modified") == 0) {
                            session->buffers[buffer_idx].is_modified = (strcmp(value, "true") == 0);
                        }
                    }
                }
            }
        }
    }
    
    fclose(file);
    return session;
}