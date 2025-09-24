#ifndef VIZERO_SESSION_H
#define VIZERO_SESSION_H

#ifdef __cplusplus
extern "C" {
#endif

#include <stddef.h>
#include <stdint.h>
#include <stdbool.h>
#include <time.h>

#define MAX_SESSION_BUFFERS 32
#define MAX_SESSION_WINDOWS 16
#define MAX_RECENT_SESSIONS 16
#define MAX_SESSION_NAME 256
#define MAX_SESSION_PATH 512

/* Forward declarations */
typedef struct vizero_editor_state_t vizero_editor_state_t;
typedef struct vizero_window_manager_t vizero_window_manager_t;

/* Session buffer info */
typedef struct vizero_session_buffer_t {
    char filepath[MAX_SESSION_PATH];    /* Relative to project_path */
    size_t cursor_line;
    size_t cursor_column;
    size_t scroll_line;
    bool is_modified;
    bool is_active;                     /* Whether this buffer slot is used */
} vizero_session_buffer_t;

/* Session window info */
typedef struct vizero_session_window_t {
    uint32_t window_id;
    size_t buffer_index;
    int x, y, width, height;
    bool is_focused;
    int split_type;                     /* 0=none, 1=horizontal, 2=vertical */
    bool is_active;                     /* Whether this window slot is used */
} vizero_session_window_t;

/* Main session structure */
typedef struct vizero_session_t {
    char name[MAX_SESSION_NAME];
    char project_path[MAX_SESSION_PATH];
    char session_file[MAX_SESSION_PATH];
    
    /* Buffer state */
    size_t buffer_count;
    vizero_session_buffer_t buffers[MAX_SESSION_BUFFERS];
    
    /* Window layout */
    size_t window_count;
    vizero_session_window_t windows[MAX_SESSION_WINDOWS];
    
    /* Project settings */
    char build_command[256];
    char run_command[256];
    char project_type[64];
    
    /* MRU buffer order */
    size_t mru_buffer_indices[MAX_SESSION_BUFFERS];
    size_t mru_count;
    
    /* Timestamps */
    time_t created;
    time_t last_modified;
} vizero_session_t;

/* Recent session entry */
typedef struct vizero_recent_session_t {
    char name[MAX_SESSION_NAME];
    char path[MAX_SESSION_PATH];
    time_t last_accessed;
} vizero_recent_session_t;

/* Session manager */
typedef struct vizero_session_manager_t {
    vizero_session_t* current_session;
    char sessions_directory[MAX_SESSION_PATH];
    
    /* Recent sessions list */
    vizero_recent_session_t recent_sessions[MAX_RECENT_SESSIONS];
    size_t recent_count;
} vizero_session_manager_t;

/* Session manager lifecycle */
vizero_session_manager_t* vizero_session_manager_create(void);
void vizero_session_manager_destroy(vizero_session_manager_t* manager);

/* Session operations */
vizero_session_t* vizero_session_create(const char* name, const char* project_path);
void vizero_session_destroy(vizero_session_t* session);

/* Session persistence */
int vizero_session_save_to_file(const vizero_session_t* session, const char* filepath);
vizero_session_t* vizero_session_load_from_file(const char* filepath);
int vizero_session_save_current(vizero_session_manager_t* manager);

/* Session capture/restore */
int vizero_session_capture_state(vizero_session_t* session, vizero_editor_state_t* editor_state);
int vizero_session_restore_state(const vizero_session_t* session, vizero_editor_state_t* editor_state);

/* Session management */
int vizero_session_manager_create_session(vizero_session_manager_t* manager, const char* name, const char* project_path);
int vizero_session_manager_load_session(vizero_session_manager_t* manager, const char* name, vizero_editor_state_t* editor_state);
int vizero_session_manager_delete_session(vizero_session_manager_t* manager, const char* name);

/* Session listing and search */
char** vizero_session_manager_list_sessions(vizero_session_manager_t* manager, size_t* count);
void vizero_session_manager_free_session_list(char** sessions, size_t count);

/* Recent sessions */
int vizero_session_manager_add_recent(vizero_session_manager_t* manager, const char* name, const char* path);
const vizero_recent_session_t* vizero_session_manager_get_recent(vizero_session_manager_t* manager, size_t index);

/* Project detection */
int vizero_session_detect_project_type(const char* project_path, char* project_type, size_t type_size);
int vizero_session_find_or_create_for_project(vizero_session_manager_t* manager, const char* project_path, vizero_editor_state_t* editor_state);

/* Utility functions */
const char* vizero_session_get_sessions_directory(void);
int vizero_session_ensure_sessions_directory(void);

#ifdef __cplusplus
}
#endif

#endif /* VIZERO_SESSION_H */