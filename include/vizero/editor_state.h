/**
 * @file editor_state.h
 * @brief Vizero Editor State Management API
 * 
 * This header provides comprehensive editor state management for Vizero,
 * including vi mode handling, buffer management, undo/redo system, 
 * text selection, clipboard operations, search functionality, and
 * LSP completion UI. The editor state serves as the central controller
 * for all text editing operations and vi-compatible behavior.
 * 
 * @author Vizero Team
 * @version 1.0.0
 * @date 2025
 * @copyright Licensed under the same terms as Vizero
 */

#ifndef VIZERO_EDITOR_STATE_H
#define VIZERO_EDITOR_STATE_H

#ifdef __cplusplus
extern "C" {
#endif

#include <stddef.h>
#include <SDL_keycode.h>
#include "plugin_interface.h"  /* For vizero_position_t */
#include "settings.h"  /* For vizero_settings_t */

/**
 * @defgroup editor_state_api Editor State Management API
 * @brief Core editor state management and vi-mode functionality
 * @{
 */

/**
 * @defgroup editor_forward_declarations Forward Declarations
 * @brief Opaque structure forward declarations
 * @{
 */

/** @brief Opaque editor state structure */
typedef struct vizero_editor_state_t vizero_editor_state_t;

/** @brief Opaque text buffer structure */
typedef struct vizero_buffer_t vizero_buffer_t;

/** @brief Opaque cursor structure */
typedef struct vizero_cursor_t vizero_cursor_t;

/** @brief Opaque plugin manager structure */
typedef struct vizero_plugin_manager_t vizero_plugin_manager_t;

/** @brief Opaque window manager structure */
typedef struct vizero_window_manager_t vizero_window_manager_t;

/** @brief Opaque editor window structure */
typedef struct vizero_editor_window_t vizero_editor_window_t;

/** @brief Opaque project structure */
typedef struct vizero_project_t vizero_project_t;

/** @} */ // end of editor_forward_declarations group

/**
 * @defgroup editor_modes Vi Editor Modes
 * @brief Vi-compatible editor mode definitions and management
 * @{
 */

/**
 * @brief Vi editor modes enumeration
 * 
 * Defines the different editing modes supported by Vizero, following
 * vi/vim conventions. Each mode has different behavior for key handling,
 * text input, and command processing.
 * 
 * @since 1.0.0
 */
typedef enum {
    /** 
     * @brief Normal mode - default vi mode for navigation and commands
     * 
     * In normal mode, keys are interpreted as movement commands,
     * text manipulation commands, or mode switching commands.
     * This is the default mode when the editor starts.
     */
    VIZERO_MODE_NORMAL,
    
    /** 
     * @brief Insert mode - text input mode
     * 
     * In insert mode, most keys insert literal text at the cursor
     * position. Special keys like Escape return to normal mode.
     */
    VIZERO_MODE_INSERT,
    
    /** 
     * @brief Visual mode - character-wise text selection
     * 
     * In visual mode, cursor movement extends the text selection.
     * Commands operate on the selected text.
     */
    VIZERO_MODE_VISUAL,
    
    /** 
     * @brief Visual line mode - line-wise text selection
     * 
     * Similar to visual mode but selects entire lines. Cursor
     * movement extends the selection line by line.
     */
    VIZERO_MODE_VISUAL_LINE,
    
    /** 
     * @brief Visual block mode - block/column text selection
     * 
     * Allows rectangular selection of text blocks across multiple
     * lines. Useful for column-oriented editing.
     */
    VIZERO_MODE_VISUAL_BLOCK,
    
    /** 
     * @brief Command mode - command line input
     * 
     * In command mode, the user types ex-style commands that are
     * executed when Enter is pressed. Commands start with ':'.
     */
    VIZERO_MODE_COMMAND
} vizero_editor_mode_t;

/** @} */ // end of editor_modes group

/**
 * @defgroup undo_system Undo/Redo System
 * @brief Multi-level undo/redo system for text operations
 * @{
 */

/**
 * @brief Types of operations that can be undone
 * 
 * Categorizes different text modification operations for the undo system.
 * Each type requires different handling during undo/redo operations.
 * 
 * @since 1.0.0
 */
typedef enum {
    /** @brief Single character insertion */
    VIZERO_UNDO_INSERT_CHAR,
    
    /** @brief Single character deletion */
    VIZERO_UNDO_DELETE_CHAR,
    
    /** @brief Entire line insertion */
    VIZERO_UNDO_INSERT_LINE,
    
    /** @brief Entire line deletion */
    VIZERO_UNDO_DELETE_LINE,
    
    /** @brief Multi-character text insertion */
    VIZERO_UNDO_INSERT_TEXT,
    
    /** @brief Text range deletion */
    VIZERO_UNDO_DELETE_RANGE
} vizero_undo_type_t;

/**
 * @brief Single undo operation record
 * 
 * Stores all information needed to undo or redo a specific text
 * modification operation including the operation type, position,
 * and affected text content.
 * 
 * @since 1.0.0
 */
typedef struct {
    /** @brief Type of operation performed */
    vizero_undo_type_t type;
    
    /** @brief Starting position of the operation */
    vizero_position_t position;
    
    /** @brief Ending position (for range operations) */
    vizero_position_t end_position;
    
    /** @brief Text content affected by the operation */
    char* text;
    
    /** @brief Length of the text content */
    size_t text_length;
} vizero_undo_operation_t;

/** @brief Maximum number of undo operations to keep in history */
#define MAX_UNDO_OPERATIONS 1000

/**
 * @brief Undo operation stack
 * 
 * Manages a stack of undo operations with support for both undo and redo.
 * Operations are stored in a circular buffer with a current index pointer.
 * 
 * @since 1.0.0
 */
typedef struct {
    /** @brief Array of undo operations */
    vizero_undo_operation_t* operations;
    
    /** @brief Current number of operations in the stack */
    size_t count;
    
    /** @brief Maximum capacity of the operations array */
    size_t capacity;
    
    /** @brief Current position in the undo stack */
    size_t current_index;
} vizero_undo_stack_t;

/** @} */ // end of undo_system group

/**
 * @defgroup editor_lifecycle Editor State Lifecycle
 * @brief Functions for creating and destroying editor state
 * @{
 */

/**
 * @brief Create a new editor state with default settings
 * 
 * Allocates and initializes a new editor state instance with default
 * configuration. The editor starts in normal mode with an empty buffer.
 * 
 * @return Pointer to new editor state, or NULL on failure
 * @retval NULL if memory allocation fails
 * 
 * @see vizero_editor_state_create_with_settings() for custom settings
 * @see vizero_editor_state_destroy() for cleanup
 * 
 * @since 1.0.0
 * @thread_safety This function is not thread-safe
 */
vizero_editor_state_t* vizero_editor_state_create(void);

/**
 * @brief Create a new editor state with custom settings
 * 
 * Allocates and initializes a new editor state instance using the
 * provided settings configuration.
 * 
 * @param settings Settings configuration to use (may be NULL for defaults)
 * @return Pointer to new editor state, or NULL on failure
 * @retval NULL if memory allocation fails
 * 
 * @note If settings is NULL, default settings are used
 * @see vizero_editor_state_create() for default settings
 * @see vizero_editor_state_destroy() for cleanup
 * 
 * @since 1.0.0
 * @thread_safety This function is not thread-safe
 */
vizero_editor_state_t* vizero_editor_state_create_with_settings(vizero_settings_t* settings);

/**
 * @brief Destroy an editor state instance
 * 
 * Destroys the editor state and frees all associated memory including
 * buffers, undo history, and internal data structures.
 * 
 * @param state Editor state to destroy (may be NULL)
 * 
 * @note It is safe to pass NULL to this function
 * @note All buffers and cursors become invalid after this call
 * 
 * @since 1.0.0
 * @thread_safety This function is not thread-safe
 */
void vizero_editor_state_destroy(vizero_editor_state_t* state);

/** @} */ // end of editor_lifecycle group

/**
 * @defgroup mode_management Vi Mode Management
 * @brief Functions for managing vi editor modes
 * @{
 */

/**
 * @brief Get the current editor mode
 * 
 * Returns the current vi mode that determines how user input is interpreted.
 * 
 * @param state Editor state to query (must not be NULL)
 * @return Current editor mode
 * 
 * @pre state must not be NULL
 * 
 * @since 1.0.0
 * @thread_safety This function is not thread-safe
 */
vizero_editor_mode_t vizero_editor_get_mode(vizero_editor_state_t* state);

/**
 * @brief Set the current editor mode
 * 
 * Changes the editor mode, which affects how subsequent user input
 * is processed. Mode changes may trigger additional state updates
 * such as clearing selections or updating the status bar.
 * 
 * @param state Editor state to modify (must not be NULL)
 * @param mode New editor mode to set
 * 
 * @pre state must not be NULL
 * @post Editor mode is changed to the specified mode
 * @post Mode-specific state may be updated (e.g., selections cleared)
 * 
 * @since 1.0.0
 * @thread_safety This function is not thread-safe
 */
void vizero_editor_set_mode(vizero_editor_state_t* state, vizero_editor_mode_t mode);

/** @} */ // end of mode_management group

/**
 * @defgroup buffer_management Buffer Management
 * @brief Functions for managing multiple buffers in the editor
 * @{
 */

/**
 * @brief Get the current active buffer
 * 
 * Returns the buffer that is currently being edited. All text operations
 * are performed on this buffer.
 * 
 * @param state Editor state to query (must not be NULL)
 * @return Pointer to current buffer, or NULL if no buffers are open
 * 
 * @pre state must not be NULL
 * 
 * @see vizero_editor_switch_buffer() for changing the active buffer
 * @see vizero_buffer.h for buffer operations
 * 
 * @since 1.0.0
 * @thread_safety This function is not thread-safe
 */
vizero_buffer_t* vizero_editor_get_current_buffer(vizero_editor_state_t* state);

/**
 * @brief Get the cursor for the current buffer
 * 
 * Returns the cursor object that tracks the current editing position
 * within the active buffer.
 * 
 * @param state Editor state to query (must not be NULL)
 * @return Pointer to current cursor, or NULL if no buffers are open
 * 
 * @pre state must not be NULL
 * 
 * @see vizero_cursor.h for cursor operations
 * 
 * @since 1.0.0
 * @thread_safety This function is not thread-safe
 */
vizero_cursor_t* vizero_editor_get_current_cursor(vizero_editor_state_t* state);

/**
 * @brief Get the index of the current buffer
 * 
 * Returns the zero-based index of the currently active buffer in the
 * buffer list.
 * 
 * @param state Editor state to query (must not be NULL)
 * @return Index of current buffer, or 0 if no buffers are open
 * 
 * @pre state must not be NULL
 * 
 * @since 1.0.0
 * @thread_safety This function is not thread-safe
 */
size_t vizero_editor_get_current_buffer_index(vizero_editor_state_t* state);

/**
 * @brief Get the total number of open buffers
 * 
 * Returns the count of buffers currently managed by the editor state.
 * 
 * @param state Editor state to query (must not be NULL)
 * @return Number of open buffers (0 or greater)
 * 
 * @pre state must not be NULL
 * 
 * @since 1.0.0
 * @thread_safety This function is not thread-safe
 */
size_t vizero_editor_get_buffer_count(vizero_editor_state_t* state);

/**
 * @brief Get a buffer by index
 * 
 * Returns the buffer at the specified index in the buffer list.
 * 
 * @param state Editor state to query (must not be NULL)
 * @param index Zero-based buffer index
 * @return Pointer to buffer at index, or NULL if index is invalid
 * 
 * @pre state must not be NULL
 * @pre index must be less than the buffer count
 * 
 * @since 1.0.0
 * @thread_safety This function is not thread-safe
 */
vizero_buffer_t* vizero_editor_get_buffer(vizero_editor_state_t* state, size_t index);

/**
 * @brief Get a cursor by buffer index
 * 
 * Returns the cursor associated with the buffer at the specified index.
 * Each buffer maintains its own cursor position.
 * 
 * @param state Editor state to query (must not be NULL)
 * @param index Zero-based buffer index
 * @return Pointer to cursor for buffer at index, or NULL if index is invalid
 * 
 * @pre state must not be NULL
 * @pre index must be less than the buffer count
 * 
 * @since 1.0.0
 * @thread_safety This function is not thread-safe
 */
vizero_cursor_t* vizero_editor_get_cursor(vizero_editor_state_t* state, size_t index);

/** @} */ // end of buffer_management group

/**
 * @defgroup buffer_operations Buffer Operations
 * @brief Functions for creating, opening, closing, and switching buffers
 * @{
 */

/**
 * @brief Open a file into a new buffer
 * 
 * Creates a new buffer and loads the specified file into it. The new
 * buffer becomes the active buffer if successful.
 * 
 * @param state Editor state to modify (must not be NULL)
 * @param filename Path to file to open (must not be NULL)
 * @return 0 on success, -1 on failure
 * @retval 0 File opened successfully, new buffer is active
 * @retval -1 Failed to open file or create buffer
 * 
 * @pre state must not be NULL
 * @pre filename must not be NULL
 * @post New buffer is added to buffer list if successful
 * @post New buffer becomes active if successful
 * 
 * @since 1.0.0
 * @thread_safety This function is not thread-safe
 */
int vizero_editor_open_buffer(vizero_editor_state_t* state, const char* filename);

/**
 * @brief Close a buffer
 * 
 * Removes the specified buffer from the editor and frees its resources.
 * If the buffer being closed is the active buffer, another buffer becomes
 * active automatically.
 * 
 * @param state Editor state to modify (must not be NULL)
 * @param buffer Buffer to close (must not be NULL)
 * @return 0 on success, -1 on failure
 * @retval 0 Buffer closed successfully
 * @retval -1 Buffer not found or other error
 * 
 * @pre state must not be NULL
 * @pre buffer must not be NULL
 * @post Buffer is removed from buffer list if successful
 * @post Another buffer becomes active if the closed buffer was active
 * 
 * @since 1.0.0
 * @thread_safety This function is not thread-safe
 */
int vizero_editor_close_buffer(vizero_editor_state_t* state, vizero_buffer_t* buffer);

/**
 * @brief Switch to a buffer by index
 * 
 * Makes the buffer at the specified index the active buffer.
 * 
 * @param state Editor state to modify (must not be NULL)
 * @param buffer_index Zero-based index of buffer to switch to
 * @return 0 on success, -1 on failure
 * @retval 0 Successfully switched to buffer
 * @retval -1 Invalid buffer index
 * 
 * @pre state must not be NULL
 * @pre buffer_index must be less than buffer count
 * @post Buffer at index becomes active if successful
 * 
 * @since 1.0.0
 * @thread_safety This function is not thread-safe
 */
int vizero_editor_switch_buffer(vizero_editor_state_t* state, size_t buffer_index);

/**
 * @brief Switch to the next buffer
 * 
 * Makes the next buffer in the buffer list the active buffer.
 * Wraps to the first buffer if currently on the last buffer.
 * 
 * @param state Editor state to modify (must not be NULL)
 * @return 0 on success, -1 on failure
 * @retval 0 Successfully switched to next buffer
 * @retval -1 No buffers are open
 * 
 * @pre state must not be NULL
 * @post Next buffer becomes active if successful
 * 
 * @since 1.0.0
 * @thread_safety This function is not thread-safe
 */
int vizero_editor_next_buffer(vizero_editor_state_t* state);

/**
 * @brief Switch to the previous buffer
 * 
 * Makes the previous buffer in the buffer list the active buffer.
 * Wraps to the last buffer if currently on the first buffer.
 * 
 * @param state Editor state to modify (must not be NULL)
 * @return 0 on success, -1 on failure
 * @retval 0 Successfully switched to previous buffer
 * @retval -1 No buffers are open
 * 
 * @pre state must not be NULL
 * @post Previous buffer becomes active if successful
 * 
 * @since 1.0.0
 * @thread_safety This function is not thread-safe
 */
int vizero_editor_previous_buffer(vizero_editor_state_t* state);

/**
 * @brief Create a new empty buffer
 * 
 * Creates a new buffer with the specified name. The buffer starts empty
 * and becomes the active buffer if successful.
 * 
 * @param state Editor state to modify (must not be NULL)
 * @param name Name for the new buffer (may be NULL for default name)
 * @return 0 on success, -1 on failure
 * @retval 0 Buffer created successfully, new buffer is active
 * @retval -1 Failed to create buffer
 * 
 * @pre state must not be NULL
 * @post New empty buffer is added to buffer list if successful
 * @post New buffer becomes active if successful
 * 
 * @note If name is NULL, a default name is generated
 * 
 * @since 1.0.0
 * @thread_safety This function is not thread-safe
 */
int vizero_editor_create_new_buffer(vizero_editor_state_t* state, const char* name);

/** @} */ // end of buffer_operations group
int vizero_editor_close_current_buffer(vizero_editor_state_t* state);
int vizero_editor_close_buffer_by_index(vizero_editor_state_t* state, size_t buffer_index);

/* Project management */
vizero_project_t* vizero_editor_get_current_project(vizero_editor_state_t* state);
int vizero_editor_open_project(vizero_editor_state_t* state, const char* root_directory);
int vizero_editor_close_project(vizero_editor_state_t* state);
int vizero_editor_save_project_workspace(vizero_editor_state_t* state, const char* filename);

/**
 * @defgroup command_execution Command Execution
 * @brief Functions for executing vi commands and managing status messages
 * @{
 */

/**
 * @brief Execute a vi command
 * 
 * Parses and executes a vi-style command string. Commands can be movement
 * commands, editing commands, or ex-style commands starting with ':'.
 * 
 * @param state Editor state to modify (must not be NULL)
 * @param command Command string to execute (must not be NULL)
 * @return 0 on success, -1 on failure
 * @retval 0 Command executed successfully
 * @retval -1 Invalid command or execution error
 * 
 * @pre state must not be NULL
 * @pre command must not be NULL
 * @post Editor state may be modified based on command
 * @post Status message may be updated with command result
 * 
 * Example commands:
 * - "w" - save current buffer
 * - "q" - quit editor
 * - "10G" - go to line 10
 * - "/pattern" - search for pattern
 * 
 * @since 1.0.0
 * @thread_safety This function is not thread-safe
 */
int vizero_editor_execute_command(vizero_editor_state_t* state, const char* command);

/**
 * @brief Set a status message
 * 
 * Sets a message to be displayed in the editor's status area. The message
 * persists until replaced or the editor is closed.
 * 
 * @param state Editor state to modify (must not be NULL)
 * @param message Status message to display (may be NULL to clear)
 * 
 * @pre state must not be NULL
 * @post Status message is updated
 * 
 * @note Passing NULL clears the status message
 * @see vizero_editor_set_status_message_with_timeout() for timed messages
 * 
 * @since 1.0.0
 * @thread_safety This function is not thread-safe
 */
void vizero_editor_set_status_message(vizero_editor_state_t* state, const char* message);

/**
 * @brief Set a status message with timeout
 * 
 * Sets a message to be displayed in the editor's status area for a
 * specified duration. The message automatically clears after the timeout.
 * 
 * @param state Editor state to modify (must not be NULL)
 * @param message Status message to display (must not be NULL)
 * @param timeout_ms Duration to display message in milliseconds
 * 
 * @pre state must not be NULL
 * @pre message must not be NULL
 * @post Status message is updated with timeout
 * 
 * @since 1.0.0
 * @thread_safety This function is not thread-safe
 */
void vizero_editor_set_status_message_with_timeout(vizero_editor_state_t* state, const char* message, unsigned int timeout_ms);

/**
 * @brief Get the current status message
 * 
 * Returns the currently displayed status message, if any.
 * 
 * @param state Editor state to query (must not be NULL)
 * @return Current status message, or NULL if none set
 * 
 * @pre state must not be NULL
 * 
 * @note The returned string should not be modified or freed
 * 
 * @since 1.0.0
 * @thread_safety This function is not thread-safe
 */
const char* vizero_editor_get_status_message(vizero_editor_state_t* state);

/** @} */ // end of command_execution group

/**
 * @defgroup command_mode Command Mode Input
 * @brief Functions for managing command line input in command mode
 * @{
 */

/**
 * @brief Get the current command buffer contents
 * 
 * Returns the text currently being entered in command mode (after pressing ':').
 * 
 * @param state Editor state to query (must not be NULL)
 * @return Current command buffer contents, or empty string if none
 * 
 * @pre state must not be NULL
 * 
 * @note The returned string should not be modified or freed
 * @see vizero_editor_append_to_command() for adding characters
 * 
 * @since 1.0.0
 * @thread_safety This function is not thread-safe
 */
const char* vizero_editor_get_command_buffer(vizero_editor_state_t* state);

/**
 * @brief Clear the command buffer
 * 
 * Empties the command buffer, typically when exiting command mode
 * without executing a command.
 * 
 * @param state Editor state to modify (must not be NULL)
 * 
 * @pre state must not be NULL
 * @post Command buffer is empty
 * 
 * @since 1.0.0
 * @thread_safety This function is not thread-safe
 */
void vizero_editor_clear_command_buffer(vizero_editor_state_t* state);

/**
 * @brief Append a character to the command buffer
 * 
 * Adds a character to the end of the command being typed in command mode.
 * 
 * @param state Editor state to modify (must not be NULL)
 * @param c Character to append to command buffer
 * @return 0 on success, -1 on failure
 * @retval 0 Character appended successfully
 * @retval -1 Command buffer is full or other error
 * 
 * @pre state must not be NULL
 * @post Character is added to command buffer if successful
 * 
 * @since 1.0.0
 * @thread_safety This function is not thread-safe
 */
int vizero_editor_append_to_command(vizero_editor_state_t* state, char c);

/**
 * @brief Remove the last character from command buffer
 * 
 * Implements backspace functionality in command mode by removing the
 * last character from the command buffer.
 * 
 * @param state Editor state to modify (must not be NULL)
 * @return 0 on success, -1 on failure
 * @retval 0 Character removed successfully
 * @retval -1 Command buffer is empty or other error
 * 
 * @pre state must not be NULL
 * @post Last character is removed from command buffer if successful
 * 
 * @since 1.0.0
 * @thread_safety This function is not thread-safe
 */
int vizero_editor_backspace_command(vizero_editor_state_t* state);

/**
 * @brief Execute the current command buffer contents
 * 
 * Executes the command currently typed in the command buffer and clears
 * the buffer. Typically called when Enter is pressed in command mode.
 * 
 * @param state Editor state to modify (must not be NULL)
 * @return 0 on success, -1 on failure
 * @retval 0 Command executed successfully
 * @retval -1 Invalid command or execution error
 * 
 * @pre state must not be NULL
 * @post Command buffer is cleared
 * @post Editor state may be modified based on command
 * @post Editor typically returns to normal mode
 * 
 * @since 1.0.0
 * @thread_safety This function is not thread-safe
 */
int vizero_editor_execute_current_command(vizero_editor_state_t* state);

/** @} */ // end of command_mode group

/* Plugin manager integration */
void vizero_editor_set_plugin_manager(vizero_editor_state_t* state, vizero_plugin_manager_t* manager);
vizero_plugin_manager_t* vizero_editor_get_plugin_manager(vizero_editor_state_t* state);

/* Theme manager integration - functions defined in editor_state.cpp */
void vizero_editor_set_theme_manager(vizero_editor_state_t* state, void* manager);
void* vizero_editor_get_theme_manager(vizero_editor_state_t* state);

/* Session manager integration */
void vizero_editor_set_session_manager(vizero_editor_state_t* state, void* manager);
void* vizero_editor_get_session_manager(vizero_editor_state_t* state);

/* Mode Manager */
typedef struct vizero_mode_manager_t vizero_mode_manager_t;
vizero_mode_manager_t* vizero_editor_get_mode_manager(vizero_editor_state_t* state);

/* Settings */
typedef struct vizero_settings_t vizero_settings_t;
vizero_settings_t* vizero_editor_get_settings(vizero_editor_state_t* state);

/* Text selection */
int vizero_editor_has_selection(vizero_editor_state_t* state);
void vizero_editor_start_selection(vizero_editor_state_t* state);
void vizero_editor_start_selection_at(vizero_editor_state_t* state, size_t line, size_t column);
void vizero_editor_update_selection(vizero_editor_state_t* state);
void vizero_editor_clear_selection(vizero_editor_state_t* state);
void vizero_editor_get_selection_range(vizero_editor_state_t* state, vizero_position_t* start, vizero_position_t* end);
char* vizero_editor_get_selected_text(vizero_editor_state_t* state);

/* Clipboard operations */
int vizero_editor_copy_selection(vizero_editor_state_t* state);
int vizero_editor_copy_current_line(vizero_editor_state_t* state);
int vizero_editor_cut_selection(vizero_editor_state_t* state);
int vizero_editor_cut_current_line(vizero_editor_state_t* state);
int vizero_editor_paste_at_cursor(vizero_editor_state_t* state);
const char* vizero_editor_get_clipboard_content(vizero_editor_state_t* state);

/* Navigation operations */
int vizero_editor_go_to_line(vizero_editor_state_t* state, size_t line_number);
int vizero_editor_go_to_end(vizero_editor_state_t* state);

/* Line insertion operations */
int vizero_editor_open_line_below(vizero_editor_state_t* state);
int vizero_editor_open_line_above(vizero_editor_state_t* state);

/* Undo/Redo operations */
int vizero_editor_undo(vizero_editor_state_t* state);
int vizero_editor_redo(vizero_editor_state_t* state);
void vizero_editor_push_undo_operation(vizero_editor_state_t* state, vizero_undo_type_t type, 
                                       vizero_position_t position, vizero_position_t end_position, 
                                       const char* text);

/* Popup system */
void vizero_editor_show_popup(vizero_editor_state_t* state, const char* content, uint32_t duration_ms);
void vizero_editor_hide_popup(vizero_editor_state_t* state);
int vizero_editor_is_popup_visible(vizero_editor_state_t* state);
const char* vizero_editor_get_popup_content(vizero_editor_state_t* state);
uint32_t vizero_editor_get_popup_duration(vizero_editor_state_t* state);
void vizero_editor_scroll_popup(vizero_editor_state_t* state, int lines);
int vizero_editor_get_popup_scroll_offset(vizero_editor_state_t* state);

/* Interactive buffer selector */
void vizero_editor_show_buffer_selector(vizero_editor_state_t* state);
void vizero_editor_update_buffer_selector_content(vizero_editor_state_t* state);
int vizero_editor_handle_buffer_selector_key(vizero_editor_state_t* state, int key);

/* Help system */
int vizero_editor_enter_help_mode(vizero_editor_state_t* state);
int vizero_editor_exit_help_mode(vizero_editor_state_t* state);
int vizero_editor_is_help_mode_active(vizero_editor_state_t* state);

/* Application control */
int vizero_editor_should_quit(vizero_editor_state_t* state);
void vizero_editor_set_quit_flag(vizero_editor_state_t* state);

/**
 * @defgroup search_replace Search and Replace
 * @brief Functions and types for text search and replacement
 * @{
 */

/**
 * @brief Search direction enumeration
 * 
 * Specifies the direction to search for text patterns.
 */
typedef enum {
    VIZERO_SEARCH_FORWARD = 0,    /**< Search forward from cursor position */
    VIZERO_SEARCH_BACKWARD = 1    /**< Search backward from cursor position */
} vizero_search_direction_t;

/**
 * @brief Search match information
 * 
 * Contains position and length information for a search match.
 */
typedef struct {
    int line;      /**< Line number of the match (0-based) */
    int column;    /**< Column offset of the match (0-based) */
    int length;    /**< Length of the matched text */
} vizero_search_match_t;

/**
 * @brief Search for a pattern in the buffer
 * 
 * Searches for the specified pattern in the current buffer starting from
 * the cursor position in the given direction.
 * 
 * @param state Editor state to search in (must not be NULL)
 * @param pattern Search pattern (must not be NULL)
 * @param direction Direction to search (forward or backward)
 * @return 0 if matches found, -1 if no matches or error
 * @retval 0 Pattern found, cursor moved to first match
 * @retval -1 Pattern not found or search error
 * 
 * @pre state must not be NULL
 * @pre pattern must not be NULL
 * @post Cursor is moved to first match if found
 * @post Search state is updated with matches
 * 
 * @since 1.0.0
 * @thread_safety This function is not thread-safe
 */
int vizero_editor_search(vizero_editor_state_t* state, const char* pattern, vizero_search_direction_t direction);

/**
 * @brief Move to the next search match
 * 
 * Moves the cursor to the next occurrence of the current search pattern.
 * 
 * @param state Editor state to modify (must not be NULL)
 * @return 0 if next match found, -1 if no more matches
 * @retval 0 Moved to next match successfully
 * @retval -1 No more matches or no active search
 * 
 * @pre state must not be NULL
 * @pre A search must be active (pattern previously searched)
 * @post Cursor is moved to next match if found
 * 
 * @since 1.0.0
 * @thread_safety This function is not thread-safe
 */
int vizero_editor_search_next(vizero_editor_state_t* state);

/**
 * @brief Move to the previous search match
 * 
 * Moves the cursor to the previous occurrence of the current search pattern.
 * 
 * @param state Editor state to modify (must not be NULL)
 * @return 0 if previous match found, -1 if no more matches
 * @retval 0 Moved to previous match successfully
 * @retval -1 No more matches or no active search
 * 
 * @pre state must not be NULL
 * @pre A search must be active (pattern previously searched)
 * @post Cursor is moved to previous match if found
 * 
 * @since 1.0.0
 * @thread_safety This function is not thread-safe
 */
int vizero_editor_search_previous(vizero_editor_state_t* state);

/**
 * @brief Substitute text with replacement
 * 
 * Replaces occurrences of a pattern with replacement text within the
 * specified line range. Supports global replacement within the range.
 * 
 * @param state Editor state to modify (must not be NULL)
 * @param pattern Pattern to search for (must not be NULL)
 * @param replacement Replacement text (must not be NULL)
 * @param line_start Starting line for substitution (0-based)
 * @param line_end Ending line for substitution (0-based, -1 for end of buffer)
 * @param global If non-zero, replace all occurrences per line; if zero, replace first occurrence only
 * @return Number of substitutions made, or -1 on error
 * @retval >=0 Number of successful substitutions
 * @retval -1 Error in pattern or substitution
 * 
 * @pre state must not be NULL
 * @pre pattern must not be NULL
 * @pre replacement must not be NULL
 * @post Buffer content is modified with replacements
 * @post Undo operation is recorded for changes
 * 
 * @since 1.0.0
 * @thread_safety This function is not thread-safe
 */
int vizero_editor_substitute(vizero_editor_state_t* state, const char* pattern, const char* replacement, 
                           int line_start, int line_end, int global);

/**
 * @brief Clear the current search state
 * 
 * Clears any active search, removing search highlights and resetting
 * search-related state.
 * 
 * @param state Editor state to modify (must not be NULL)
 * 
 * @pre state must not be NULL
 * @post Search state is cleared
 * @post Search highlights are removed
 * 
 * @since 1.0.0
 * @thread_safety This function is not thread-safe
 */
void vizero_editor_clear_search(vizero_editor_state_t* state);

/**
 * @brief Check if there are active search results
 * 
 * Returns whether there is an active search with results that can be
 * navigated with search_next/search_previous.
 * 
 * @param state Editor state to query (must not be NULL)
 * @return Non-zero if search results exist, 0 otherwise
 * 
 * @pre state must not be NULL
 * 
 * @since 1.0.0
 * @thread_safety This function is not thread-safe
 */
int vizero_editor_has_search_results(vizero_editor_state_t* state);

/**
 * @brief Get the current search pattern
 * 
 * Returns the pattern string for the currently active search.
 * 
 * @param state Editor state to query (must not be NULL)
 * @return Current search pattern, or NULL if no active search
 * 
 * @pre state must not be NULL
 * 
 * @note The returned string should not be modified or freed
 * 
 * @since 1.0.0
 * @thread_safety This function is not thread-safe
 */
const char* vizero_editor_get_search_pattern(vizero_editor_state_t* state);

/**
 * @brief Get the total number of search matches
 * 
 * Returns the count of all matches found for the current search pattern.
 * 
 * @param state Editor state to query (must not be NULL)
 * @return Number of matches found, or 0 if no active search
 * 
 * @pre state must not be NULL
 * 
 * @since 1.0.0
 * @thread_safety This function is not thread-safe
 */
int vizero_editor_get_search_match_count(vizero_editor_state_t* state);

/**
 * @brief Get the index of the current search match
 * 
 * Returns the 0-based index of the currently selected search match
 * within the list of all matches.
 * 
 * @param state Editor state to query (must not be NULL)
 * @return Index of current match (0-based), or -1 if no active search
 * 
 * @pre state must not be NULL
 * 
 * @since 1.0.0
 * @thread_safety This function is not thread-safe
 */
int vizero_editor_get_current_match_index(vizero_editor_state_t* state);

/** @} */ // end of search_replace group

/**
 * @defgroup window_management Window Management
 * @brief Functions for managing editor windows and splits
 * @{
 */

/**
 * @brief Get the window manager instance
 * 
 * Returns the window manager that handles multiple windows and splits
 * within the editor.
 * 
 * @param state Editor state to query (must not be NULL)
 * @return Pointer to window manager, or NULL if not initialized
 * 
 * @pre state must not be NULL
 * 
 * @since 1.0.0
 * @thread_safety This function is not thread-safe
 */
vizero_window_manager_t* vizero_editor_get_window_manager(vizero_editor_state_t* state);

/**
 * @brief Get the currently active window
 * 
 * Returns the window that currently has focus and receives input.
 * 
 * @param state Editor state to query (must not be NULL)
 * @return Pointer to active window, or NULL if no windows exist
 * 
 * @pre state must not be NULL
 * 
 * @since 1.0.0
 * @thread_safety This function is not thread-safe
 */
vizero_editor_window_t* vizero_editor_get_active_window(vizero_editor_state_t* state);

/**
 * @brief Create a new window for a buffer
 * 
 * Creates a new editor window that displays the specified buffer with
 * the given dimensions.
 * 
 * @param state Editor state to modify (must not be NULL)
 * @param buffer Buffer to display in the new window (must not be NULL)
 * @param window_width Width of the new window in characters
 * @param window_height Height of the new window in characters
 * @return 0 on success, -1 on failure
 * @retval 0 Window created successfully
 * @retval -1 Failed to create window
 * 
 * @pre state must not be NULL
 * @pre buffer must not be NULL
 * @pre window_width must be greater than 0
 * @pre window_height must be greater than 0
 * @post New window is added to window manager if successful
 * 
 * @since 1.0.0
 * @thread_safety This function is not thread-safe
 */
int vizero_editor_create_window_for_buffer(vizero_editor_state_t* state, vizero_buffer_t* buffer, 
                                         int window_width, int window_height);

/** @} */ // end of window_management group

/**
 * @defgroup lsp_completion LSP Completion UI
 * @brief Functions for managing LSP code completion interface
 * @{
 */

/**
 * @brief Show completion popup with LSP items
 * 
 * Displays a completion popup containing the provided completion items
 * at the specified trigger position. The popup allows users to select
 * and insert completion items.
 * 
 * @param state Editor state to modify (must not be NULL)
 * @param items Array of completion items to display (must not be NULL)
 * @param count Number of items in the array (must be > 0)
 * @param trigger_position Position where completion was triggered
 * 
 * @pre state must not be NULL
 * @pre items must not be NULL
 * @pre count must be greater than 0
 * @post Completion popup is displayed at trigger position
 * @post First completion item is selected by default
 * 
 * @see vizero_completion_item_t for item structure
 * @see vizero_editor_hide_completion() to dismiss popup
 * 
 * @since 1.0.0
 * @thread_safety This function is not thread-safe
 */
void vizero_editor_show_completion(vizero_editor_state_t* state, 
                                   vizero_completion_item_t* items, 
                                   size_t count, 
                                   vizero_position_t trigger_position);

/**
 * @brief Hide the completion popup
 * 
 * Dismisses any currently visible completion popup and returns the
 * editor to normal editing mode.
 * 
 * @param state Editor state to modify (must not be NULL)
 * 
 * @pre state must not be NULL
 * @post Completion popup is hidden if it was visible
 * @post Editor focus returns to normal editing
 * 
 * @since 1.0.0
 * @thread_safety This function is not thread-safe
 */
void vizero_editor_hide_completion(vizero_editor_state_t* state);

/**
 * @brief Check if completion popup is visible
 * 
 * Returns whether the LSP completion popup is currently displayed.
 * 
 * @param state Editor state to query (must not be NULL)
 * @return Non-zero if completion popup is visible, 0 otherwise
 * 
 * @pre state must not be NULL
 * 
 * @since 1.0.0
 * @thread_safety This function is not thread-safe
 */
int vizero_editor_is_completion_visible(vizero_editor_state_t* state);

/**
 * @brief Handle keyboard input for completion popup
 * 
 * Processes keyboard input when the completion popup is visible, handling
 * navigation, selection, and dismissal of the completion interface.
 * 
 * @param state Editor state to modify (must not be NULL)
 * @param key SDL keycode for the pressed key
 * @return 0 if key was handled, -1 if key should be processed normally
 * @retval 0 Key was handled by completion UI
 * @retval -1 Key not handled, should be processed normally
 * 
 * @pre state must not be NULL
 * @pre Completion popup should be visible
 * @post Completion selection may be changed
 * @post Completion may be accepted or dismissed
 * 
 * @since 1.0.0
 * @thread_safety This function is not thread-safe
 */
int vizero_editor_handle_completion_key(vizero_editor_state_t* state, SDL_Keycode key);

/**
 * @brief Accept the currently selected completion item
 * 
 * Inserts the currently selected completion item into the buffer at the
 * trigger position and dismisses the completion popup.
 * 
 * @param state Editor state to modify (must not be NULL)
 * 
 * @pre state must not be NULL
 * @pre Completion popup should be visible
 * @pre A completion item should be selected
 * @post Selected completion text is inserted into buffer
 * @post Completion popup is dismissed
 * @post Cursor is positioned after inserted text
 * 
 * @since 1.0.0
 * @thread_safety This function is not thread-safe
 */
void vizero_editor_accept_completion(vizero_editor_state_t* state);

/**
 * @brief Get the completion items array
 * 
 * Returns the array of completion items currently displayed in the popup.
 * Used for rendering the completion interface.
 * 
 * @param state Editor state to query (must not be NULL)
 * @return Pointer to completion items array, or NULL if no completion visible
 * 
 * @pre state must not be NULL
 * 
 * @note The returned array should not be modified or freed
 * @see vizero_editor_get_completion_count() for array size
 * 
 * @since 1.0.0
 * @thread_safety This function is not thread-safe
 */
vizero_completion_item_t* vizero_editor_get_completion_items(vizero_editor_state_t* state);

/**
 * @brief Get the number of completion items
 * 
 * Returns the count of completion items in the currently visible popup.
 * 
 * @param state Editor state to query (must not be NULL)
 * @return Number of completion items, or 0 if no completion visible
 * 
 * @pre state must not be NULL
 * 
 * @since 1.0.0
 * @thread_safety This function is not thread-safe
 */
size_t vizero_editor_get_completion_count(vizero_editor_state_t* state);

/**
 * @brief Get the index of the selected completion item
 * 
 * Returns the 0-based index of the currently selected completion item
 * within the completion popup.
 * 
 * @param state Editor state to query (must not be NULL)
 * @return Index of selected item (0-based), or 0 if no completion visible
 * 
 * @pre state must not be NULL
 * 
 * @since 1.0.0
 * @thread_safety This function is not thread-safe
 */
size_t vizero_editor_get_completion_selected_index(vizero_editor_state_t* state);

/**
 * @brief Get the completion trigger position
 * 
 * Returns the position in the buffer where completion was triggered.
 * This is used for positioning the completion popup correctly.
 * 
 * @param state Editor state to query (must not be NULL)
 * @return Position where completion was triggered
 * 
 * @pre state must not be NULL
 * 
 * @since 1.0.0
 * @thread_safety This function is not thread-safe
 */
vizero_position_t vizero_editor_get_completion_trigger_position(vizero_editor_state_t* state);

/** @} */ // end of lsp_completion group

/**
 * @defgroup lsp_diagnostics LSP Diagnostics and Hover
 * @brief Functions for managing LSP diagnostic messages and hover information
 * @{
 */

/**
 * @brief Update diagnostics for a buffer
 * 
 * Updates the diagnostic information displayed for the specified buffer,
 * typically called when LSP diagnostics are received from a language server.
 * 
 * @param state Editor state to modify (must not be NULL)
 * @param buffer Buffer to update diagnostics for (must not be NULL)
 * 
 * @pre state must not be NULL
 * @pre buffer must not be NULL
 * @post Diagnostic underlines are updated for the buffer
 * 
 * @since 1.0.0
 * @thread_safety This function is not thread-safe
 */
void vizero_editor_update_diagnostics(vizero_editor_state_t* state, vizero_buffer_t* buffer);

/**
 * @brief Get diagnostics for a buffer
 * 
 * Returns the array of diagnostic messages for the specified buffer.
 * 
 * @param state Editor state to query (must not be NULL)
 * @param buffer Buffer to get diagnostics for (must not be NULL)
 * @param count Pointer to store the number of diagnostics (must not be NULL)
 * @return Array of diagnostics, or NULL if none exist
 * 
 * @pre state must not be NULL
 * @pre buffer must not be NULL
 * @pre count must not be NULL
 * @post count is set to the number of diagnostics returned
 * 
 * @since 1.0.0
 * @thread_safety This function is not thread-safe
 */
vizero_diagnostic_t* vizero_editor_get_diagnostics(vizero_editor_state_t* state, 
                                                  vizero_buffer_t* buffer, 
                                                  size_t* count);

/**
 * @brief Show hover information
 * 
 * Displays hover information popup with the provided text at the specified
 * position. The popup appears near the cursor with contextual information.
 * 
 * @param state Editor state to modify (must not be NULL)
 * @param text Hover text to display (must not be NULL)
 * @param position Buffer position for the hover (line/column)
 * @param screen_x Screen X coordinate for popup positioning
 * @param screen_y Screen Y coordinate for popup positioning
 * 
 * @pre state must not be NULL
 * @pre text must not be NULL
 * @post Hover popup is displayed with the provided text
 * 
 * @since 1.0.0
 * @thread_safety This function is not thread-safe
 */
void vizero_editor_show_hover(vizero_editor_state_t* state, const char* text, 
                             vizero_position_t position, int screen_x, int screen_y);

/**
 * @brief Hide hover information
 * 
 * Dismisses any currently visible hover popup.
 * 
 * @param state Editor state to modify (must not be NULL)
 * 
 * @pre state must not be NULL
 * @post Hover popup is hidden if it was visible
 * 
 * @since 1.0.0
 * @thread_safety This function is not thread-safe
 */
void vizero_editor_hide_hover(vizero_editor_state_t* state);

/**
 * @brief Check if hover popup is visible
 * 
 * Returns whether a hover information popup is currently displayed.
 * 
 * @param state Editor state to query (must not be NULL)
 * @return Non-zero if hover popup is visible, 0 otherwise
 * 
 * @pre state must not be NULL
 * 
 * @since 1.0.0
 * @thread_safety This function is not thread-safe
 */
int vizero_editor_is_hover_visible(vizero_editor_state_t* state);

/**
 * @brief Get hover text content
 * 
 * Returns the text content of the currently visible hover popup.
 * 
 * @param state Editor state to query (must not be NULL)
 * @return Hover text content, or NULL if no hover is visible
 * 
 * @pre state must not be NULL
 * 
 * @note The returned string should not be modified or freed
 * 
 * @since 1.0.0
 * @thread_safety This function is not thread-safe
 */
const char* vizero_editor_get_hover_text(vizero_editor_state_t* state);

/**
 * @brief Get hover buffer position
 * 
 * Returns the buffer position (line/column) associated with the current hover.
 * 
 * @param state Editor state to query (must not be NULL)
 * @return Buffer position for the hover
 * 
 * @pre state must not be NULL
 * 
 * @since 1.0.0
 * @thread_safety This function is not thread-safe
 */
vizero_position_t vizero_editor_get_hover_position(vizero_editor_state_t* state);

/**
 * @brief Get hover screen position
 * 
 * Returns the screen coordinates where the hover popup should be positioned.
 * 
 * @param state Editor state to query (must not be NULL)
 * @param x Pointer to store screen X coordinate (must not be NULL)
 * @param y Pointer to store screen Y coordinate (must not be NULL)
 * 
 * @pre state must not be NULL
 * @pre x must not be NULL
 * @pre y must not be NULL
 * @post x and y are set to the hover screen coordinates
 * 
 * @since 1.0.0
 * @thread_safety This function is not thread-safe
 */
void vizero_editor_get_hover_screen_position(vizero_editor_state_t* state, int* x, int* y);

/** @} */ // end of lsp_diagnostics group

#ifdef __cplusplus
}
#endif

#endif /* VIZERO_EDITOR_STATE_H */