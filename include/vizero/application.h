/**
 * @file application.h
 * @brief Vizero Application Core API
 * 
 * This header provides the main application management API for Vizero editor.
 * The application serves as the central controller that manages the SDL2 window,
 * OpenGL rendering context, editor state, plugin system, and main event loop.
 * 
 * The application follows a typical lifecycle: create → initialize → run → shutdown → destroy
 * 
 * @author Vizero Team
 * @version 1.0.0
 * @date 2025
 * @copyright Licensed under the same terms as Vizero
 */

#ifndef VIZERO_APPLICATION_H
#define VIZERO_APPLICATION_H

#ifdef __cplusplus
extern "C" {
#endif

#include <stdint.h>

/**
 * @defgroup application_api Application Management API
 * @brief Core application lifecycle and management functions
 * @{
 */

/**
 * @defgroup application_types Application Data Types
 * @brief Data structures for application configuration and management
 * @{
 */

/** @brief Opaque application instance structure */
typedef struct vizero_application_t vizero_application_t;

/** @brief Opaque window management structure */
typedef struct vizero_window_t vizero_window_t;

/** @brief Opaque editor state structure */
typedef struct vizero_editor_state_t vizero_editor_state_t;

/** @brief Opaque plugin manager structure */
typedef struct vizero_plugin_manager_t vizero_plugin_manager_t;

/**
 * @brief Application configuration structure
 * 
 * Contains all the configuration parameters needed to initialize
 * the Vizero application including window settings, directories,
 * and initial display options.
 * 
 * @since 1.0.0
 */
typedef struct {
    /** @brief Window title string (must not be NULL) */
    const char* title;
    
    /** @brief Initial window width in pixels */
    int width;
    
    /** @brief Initial window height in pixels */
    int height;
    
    /** @brief 1 for fullscreen mode, 0 for windowed mode */
    int fullscreen;
    
    /** @brief Configuration directory path (may be NULL for default) */
    const char* config_dir;
    
    /** @brief Plugin directory path (may be NULL for default) */
    const char* plugin_dir;
} vizero_app_config_t;

/** @} */ // end of application_types group

/**
 * @defgroup application_lifecycle Application Lifecycle Management
 * @brief Functions for creating, initializing, running, and destroying the application
 * @{
 */

/**
 * @brief Create a new application instance
 * 
 * Allocates and initializes a new Vizero application with the specified
 * configuration. This does not initialize SDL2 or create windows - that
 * happens in vizero_application_initialize().
 * 
 * @param config Configuration parameters (must not be NULL)
 * @return Pointer to new application instance, or NULL on failure
 * @retval NULL if config is NULL or memory allocation fails
 * 
 * @pre config must not be NULL
 * @pre config->title must not be NULL
 * @post Application is in created but uninitialized state
 * 
 * @see vizero_application_initialize() to complete initialization
 * @see vizero_application_destroy() for cleanup
 * 
 * @since 1.0.0
 * @thread_safety This function is not thread-safe
 * 
 * @example
 * @code
 * vizero_app_config_t config = {
 *     .title = "My Editor",
 *     .width = 1200,
 *     .height = 800,
 *     .fullscreen = 0,
 *     .config_dir = NULL,  // Use default
 *     .plugin_dir = NULL   // Use default
 * };
 * 
 * vizero_application_t* app = vizero_application_create(&config);
 * if (!app) {
 *     fprintf(stderr, "Failed to create application\n");
 *     return -1;
 * }
 * @endcode
 */
vizero_application_t* vizero_application_create(const vizero_app_config_t* config);

/**
 * @brief Destroy an application instance
 * 
 * Destroys the application instance and frees all associated memory.
 * The application should be shut down before calling this function.
 * 
 * @param app Application to destroy (may be NULL)
 * 
 * @note It is safe to pass NULL to this function
 * @note Application should be shut down before destruction
 * @warning Calling this on a running application may cause crashes
 * 
 * @see vizero_application_shutdown() to properly shutdown first
 * 
 * @since 1.0.0
 * @thread_safety This function is not thread-safe
 */
void vizero_application_destroy(vizero_application_t* app);

/**
 * @brief Initialize the application
 * 
 * Performs full application initialization including SDL2 setup,
 * window creation, OpenGL context initialization, plugin loading,
 * and editor state setup.
 * 
 * @param app Application to initialize (must not be NULL)
 * @return 0 on success, negative error code on failure
 * @retval 0 Success
 * @retval -1 Invalid application pointer
 * @retval -2 SDL2 initialization failed
 * @retval -3 Window creation failed
 * @retval -4 OpenGL context creation failed
 * @retval -5 Plugin system initialization failed
 * @retval -6 Editor initialization failed
 * 
 * @pre app must not be NULL
 * @pre app must be in created but uninitialized state
 * @post On success, application is ready to run main loop
 * @post On failure, application remains uninitialized
 * 
 * @since 1.0.0
 * @thread_safety This function is not thread-safe
 */
int vizero_application_initialize(vizero_application_t* app);

/**
 * @brief Shutdown the application
 * 
 * Performs orderly shutdown of all application subsystems including
 * plugin unloading, editor cleanup, OpenGL context destruction,
 * and SDL2 cleanup. The application can be destroyed after shutdown.
 * 
 * @param app Application to shutdown (must not be NULL)
 * 
 * @pre app must not be NULL
 * @pre app must be in initialized state
 * @post Application is in shutdown state, ready for destruction
 * 
 * @since 1.0.0
 * @thread_safety This function is not thread-safe
 */
void vizero_application_shutdown(vizero_application_t* app);

/**
 * @brief Run the main application loop
 * 
 * Starts the main event loop that handles SDL2 events, updates the editor
 * state, renders frames, and manages the overall application lifecycle.
 * This function blocks until the application receives a quit signal.
 * 
 * @param app Application to run (must not be NULL)
 * @return 0 on normal exit, negative error code on failure
 * @retval 0 Normal exit
 * @retval -1 Invalid application pointer
 * @retval -2 Application not initialized
 * @retval -3 Main loop error
 * 
 * @pre app must not be NULL
 * @pre app must be in initialized state
 * @post Application may be in quit state
 * 
 * @note This function blocks until application quit
 * @see vizero_application_quit() to request exit from main loop
 * 
 * @since 1.0.0
 * @thread_safety This function is not thread-safe
 */
int vizero_application_run(vizero_application_t* app);

/**
 * @brief Request application to quit
 * 
 * Signals the main event loop to exit gracefully. This does not
 * immediately terminate the application - it sets a flag that
 * causes the main loop to exit after the current frame.
 * 
 * @param app Application to quit (must not be NULL)
 * 
 * @pre app must not be NULL
 * @note This function is safe to call from event handlers
 * @note The application will exit after the current frame completes
 * 
 * @since 1.0.0
 * @thread_safety This function is thread-safe
 */
void vizero_application_quit(vizero_application_t* app);

/** @} */ // end of application_lifecycle group

/**
 * @defgroup application_queries Application Component Access
 * @brief Functions for accessing application subsystems
 * @{
 */

/**
 * @brief Get the application's window manager
 * 
 * Returns a pointer to the window management subsystem which handles
 * SDL2 window operations, OpenGL context management, and display settings.
 * 
 * @param app Application instance (must not be NULL)
 * @return Pointer to window manager, or NULL if app is invalid
 * 
 * @pre app must not be NULL
 * @pre app must be initialized
 * @note The returned pointer is valid until application shutdown
 * 
 * @since 1.0.0
 * @thread_safety This function is not thread-safe
 */
vizero_window_t* vizero_application_get_window(vizero_application_t* app);

/**
 * @brief Get the application's editor state manager
 * 
 * Returns a pointer to the editor state subsystem which manages
 * vi modes, command parsing, buffers, cursors, and editor behavior.
 * 
 * @param app Application instance (must not be NULL)
 * @return Pointer to editor state manager, or NULL if app is invalid
 * 
 * @pre app must not be NULL
 * @pre app must be initialized
 * @note The returned pointer is valid until application shutdown
 * 
 * @since 1.0.0
 * @thread_safety This function is not thread-safe
 */
vizero_editor_state_t* vizero_application_get_editor(vizero_application_t* app);

/**
 * @brief Get the application's plugin manager
 * 
 * Returns a pointer to the plugin management subsystem which handles
 * plugin loading, unloading, registration, and callback dispatching.
 * 
 * @param app Application instance (must not be NULL)
 * @return Pointer to plugin manager, or NULL if app is invalid
 * 
 * @pre app must not be NULL
 * @pre app must be initialized
 * @note The returned pointer is valid until application shutdown
 * 
 * @since 1.0.0
 * @thread_safety This function is not thread-safe
 */
vizero_plugin_manager_t* vizero_application_get_plugin_manager(vizero_application_t* app);

/** @} */ // end of application_queries group

/**
 * @defgroup application_events Application Event Handling
 * @brief Functions for handling application-level events
 * @{
 */

/**
 * @brief Handle window resize events
 * 
 * Called when the application window is resized by the user or system.
 * Updates internal state and triggers necessary re-rendering operations.
 * 
 * @param app Application instance (must not be NULL)
 * @param width New window width in pixels
 * @param height New window height in pixels
 * 
 * @pre app must not be NULL
 * @pre app must be initialized
 * @pre width and height must be positive
 * @post Internal viewport and rendering state updated
 * 
 * @note This is typically called from the main event loop
 * 
 * @since 1.0.0
 * @thread_safety This function is not thread-safe
 */
void vizero_application_on_window_resize(vizero_application_t* app, int width, int height);

/**
 * @brief Handle file drag-and-drop events
 * 
 * Called when files are dropped onto the application window.
 * Attempts to open the dropped file in the editor.
 * 
 * @param app Application instance (must not be NULL)
 * @param filename Path to the dropped file (must not be NULL)
 * 
 * @pre app must not be NULL
 * @pre app must be initialized
 * @pre filename must not be NULL
 * @post File may be opened in editor if valid
 * 
 * @note This is typically called from the main event loop
 * @note Invalid files are ignored with error messages
 * 
 * @since 1.0.0
 * @thread_safety This function is not thread-safe
 */
void vizero_application_on_file_drop(vizero_application_t* app, const char* filename);

/**
 * @brief Handle user input events
 * 
 * Processes user input events including keyboard input, mouse events,
 * and other user interactions. Dispatches events to appropriate
 * subsystems like the editor or plugin system.
 * 
 * @param app Application instance (must not be NULL)
 * 
 * @pre app must not be NULL
 * @pre app must be initialized
 * @post Input events processed and dispatched
 * 
 * @note This is typically called from the main event loop
 * @note Input is processed according to current editor mode
 * 
 * @since 1.0.0
 * @thread_safety This function is not thread-safe
 */
void vizero_application_on_user_input(vizero_application_t* app);

/** @} */ // end of application_events group

/** @} */ // end of application_api group

#ifdef __cplusplus
}
#endif

#endif /* VIZERO_APPLICATION_H */