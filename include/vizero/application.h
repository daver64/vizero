#ifndef VIZERO_APPLICATION_H
#define VIZERO_APPLICATION_H

#ifdef __cplusplus
extern "C" {
#endif

#include <stdint.h>

/* Forward declarations */
typedef struct vizero_application_t vizero_application_t;
typedef struct vizero_window_t vizero_window_t;
typedef struct vizero_editor_state_t vizero_editor_state_t;
typedef struct vizero_plugin_manager_t vizero_plugin_manager_t;

/* Application configuration */
typedef struct {
    const char* title;
    int width;
    int height;
    int fullscreen;
    const char* config_dir;
    const char* plugin_dir;
} vizero_app_config_t;

/* Application lifecycle */
vizero_application_t* vizero_application_create(const vizero_app_config_t* config);
void vizero_application_destroy(vizero_application_t* app);

int vizero_application_initialize(vizero_application_t* app);
void vizero_application_shutdown(vizero_application_t* app);

int vizero_application_run(vizero_application_t* app);
void vizero_application_quit(vizero_application_t* app);

/* Application queries */
vizero_window_t* vizero_application_get_window(vizero_application_t* app);
vizero_editor_state_t* vizero_application_get_editor(vizero_application_t* app);
vizero_plugin_manager_t* vizero_application_get_plugin_manager(vizero_application_t* app);

/* Application events */
void vizero_application_on_window_resize(vizero_application_t* app, int width, int height);
void vizero_application_on_file_drop(vizero_application_t* app, const char* filename);
void vizero_application_quit(vizero_application_t* app);

#ifdef __cplusplus
}
#endif

#endif /* VIZERO_APPLICATION_H */