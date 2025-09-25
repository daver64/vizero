#include "vizero/vizero.h"
#include "vizero/file_utils.h"
#include <stdio.h>
#include <stdlib.h>
int main(int argc, char* argv[]) {
    printf("Vizero - Vi Clone v1.0.0\n");
    printf("Built with SDL2 and cross-platform plugin support\n\n");
    
    /* Application configuration */
    vizero_app_config_t config;
    config.title = "Vizero";
    config.width = 1200;
    config.height = 800;
    config.fullscreen = 0;
    config.config_dir = NULL;  /* Will use default */
    config.plugin_dir = NULL;  /* Will be set below */
    
    /* Set plugin directory relative to executable */
    char* plugin_dir = vizero_get_resource_path("plugins");
    config.plugin_dir = plugin_dir;
    
    /* Create application */
    vizero_application_t* app = vizero_application_create(&config);
    if (!app) {
        fprintf(stderr, "Failed to create application\n");
        return 1;
 
    // Global variable for startup filename
   
    }
    
    /* Initialize application */

    if (vizero_application_initialize(app) != 0) {
        fprintf(stderr, "Failed to initialize application\n");
        vizero_application_destroy(app);
        return 1;
    }
    
    /* Load files from command line */
    for (int i = 1; i < argc; i++) {
        printf("Loading file: %s\n", argv[i]);
        
        /* Get the editor state and open the file */
        vizero_editor_state_t* editor = vizero_application_get_editor(app);
        if (editor) {
            if (vizero_editor_open_buffer(editor, argv[i]) == 0) {
                printf("Successfully opened: %s\n", argv[i]);
                /* Hide welcome screen when files are loaded */
                vizero_application_on_user_input(app);
            } else {
                printf("Warning: Could not open file: %s\n", argv[i]);
            }
        } else {
            printf("Error: Could not access editor\n");
        }
    }
    
    /* Run main loop */
    int result = vizero_application_run(app);
    
    /* Cleanup */
    vizero_application_shutdown(app);
    vizero_application_destroy(app);
    
    /* Free plugin directory path */
    if (plugin_dir) {
        free(plugin_dir);
    }
    
    return result;
}
