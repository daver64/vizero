/**
 * @file plugin_interface.h
 * @brief Vizero Plugin System API Interface
 * 
 * This header defines the complete plugin API for Vizero editor, providing
 * interfaces for syntax highlighting, LSP integration, command extensions,
 * and other plugin types. All plugins must implement the functions defined
 * here to integrate with the Vizero plugin system.
 * 
 * @author Vizero Team
 * @version 1.0.0
 * @date 2025
 * @copyright Licensed under the same terms as Vizero
 * 
 * @see plugins/example/example_plugin.c for plugin development examples
 * @see plugins/example/example_plugin.c for a complete example
 */

#ifndef VIZERO_PLUGIN_INTERFACE_H
#define VIZERO_PLUGIN_INTERFACE_H

#ifdef __cplusplus
extern "C" {
#endif

#include <stdint.h>
#include <stddef.h>
#include <stdbool.h>
#include <SDL.h>

/**
 * @defgroup plugin_api Plugin API
 * @brief Core plugin system interfaces and structures
 * @{
 */

/**
 * @defgroup plugin_version Version Information
 * @brief Plugin API version constants
 * @{
 */

/** @brief Major version of the plugin API */
#define VIZERO_PLUGIN_API_VERSION_MAJOR 1

/** @brief Minor version of the plugin API */
#define VIZERO_PLUGIN_API_VERSION_MINOR 0

/** @brief Patch version of the plugin API */
#define VIZERO_PLUGIN_API_VERSION_PATCH 0

/** @} */ // end of plugin_version group

/**
 * @defgroup plugin_macros Export Macros
 * @brief Platform-specific plugin export macros
 * @{
 */

/**
 * @brief Plugin API export macro for cross-platform compatibility
 * 
 * This macro handles the platform-specific details of exporting functions
 * from dynamic libraries. On Windows, it uses __declspec(dllexport/dllimport),
 * while on Unix-like systems it uses __attribute__((visibility("default"))).
 * 
 * @note All plugin functions must be marked with VIZERO_PLUGIN_API
 */
#ifdef _WIN32
    #ifdef VIZERO_PLUGIN_EXPORT
        #define VIZERO_PLUGIN_API __declspec(dllexport)
    #else
        #define VIZERO_PLUGIN_API __declspec(dllimport)
    #endif
#else
    #define VIZERO_PLUGIN_API __attribute__((visibility("default")))
#endif

/** @} */ // end of plugin_macros group

/**
 * @defgroup forward_declarations Forward Declarations
 * @brief Opaque structure forward declarations
 * @{
 */

/** @brief Opaque editor state structure */
typedef struct vizero_editor_t vizero_editor_t;

/** @brief Opaque text buffer structure */
typedef struct vizero_buffer_t vizero_buffer_t;

/** @brief Opaque cursor position structure */
typedef struct vizero_cursor_t vizero_cursor_t;

/** @brief Opaque plugin instance structure */
typedef struct vizero_plugin_t vizero_plugin_t;

/** @brief Opaque renderer context structure */
typedef struct vizero_renderer_t vizero_renderer_t;

/** @} */ // end of forward_declarations group

/**
 * @defgroup plugin_types Plugin Types
 * @brief Plugin classification and type system
 * @{
 */

/**
 * @brief Plugin type classification for the plugin system
 * 
 * Defines the different categories of plugins supported by Vizero.
 * Each type has specific callback requirements and loading behavior.
 * The plugin type determines which callbacks are called and when.
 * 
 * @since 1.0.0
 */
typedef enum {
    /** 
     * @brief Syntax highlighting plugins for language support
     * 
     * These plugins provide syntax highlighting for specific file types.
     * They must implement the highlight_syntax callback and are loaded
     * on-demand when files with matching extensions are opened.
     */
    VIZERO_PLUGIN_TYPE_SYNTAX_HIGHLIGHTER,
    
    /** 
     * @brief Command extensions that add new vi commands
     * 
     * These plugins extend the vi command set with new functionality.
     * They register new commands that can be invoked from command mode.
     */
    VIZERO_PLUGIN_TYPE_COMMAND_EXTENSION,
    
    /** 
     * @brief Text filtering and processing plugins
     * 
     * These plugins provide text transformation and filtering capabilities.
     * They can process selected text or entire buffers.
     */
    VIZERO_PLUGIN_TYPE_FILTER,
    
    /** 
     * @brief UI extensions that modify the interface
     * 
     * These plugins add new UI elements or modify existing interface
     * components like status bars, menus, or editor decorations.
     */
    VIZERO_PLUGIN_TYPE_UI_EXTENSION,
    
    /** 
     * @brief File type handlers for special file formats
     * 
     * These plugins provide custom handling for specific file types,
     * such as binary files, archives, or specialized formats.
     */
    VIZERO_PLUGIN_TYPE_FILE_TYPE_HANDLER,
    
    /** 
     * @brief Language Server Protocol integration plugins
     * 
     * These plugins provide LSP client functionality for specific
     * programming languages, enabling features like code completion,
     * diagnostics, go-to-definition, and hover information.
     */
    VIZERO_PLUGIN_TYPE_LANGUAGE_SERVER,
    
    /** 
     * @brief Generic plugins that don't fit other categories
     * 
     * Catch-all category for plugins that provide functionality
     * not covered by the other specific types.
     */
    VIZERO_PLUGIN_TYPE_GENERIC
} vizero_plugin_type_t;

/** @} */ // end of plugin_types group

/**
 * @defgroup plugin_info Plugin Information
 * @brief Plugin metadata and identification structures
 * @{
 */

/**
 * @brief Plugin information and metadata structure
 * 
 * Contains all the metadata about a plugin including its name, version,
 * author, and API compatibility information. This structure is returned
 * by the vizero_plugin_get_info() function that every plugin must implement.
 * 
 * @note All string pointers must remain valid for the lifetime of the plugin
 * @since 1.0.0
 */
typedef struct {
    /** @brief Human-readable plugin name (e.g., "PHP Syntax Highlighter") */
    const char* name;
    
    /** @brief Plugin version string (e.g., "1.0.0") */
    const char* version;
    
    /** @brief Plugin author or organization name */
    const char* author;
    
    /** @brief Brief description of plugin functionality */
    const char* description;
    
    /** @brief Plugin type classification */
    vizero_plugin_type_t type;
    
    /** @brief Major version of plugin API this plugin was built against */
    uint32_t api_version_major;
    
    /** @brief Minor version of plugin API this plugin was built against */
    uint32_t api_version_minor;
    
    /** @brief Patch version of plugin API this plugin was built against */
    uint32_t api_version_patch;
} vizero_plugin_info_t;

/** @} */ // end of plugin_info group

/**
 * @defgroup text_structures Text Position and Range Structures
 * @brief Structures for representing text positions and ranges
 * @{
 */

/**
 * @brief Text position structure for line/column coordinates
 * 
 * Represents a specific position in a text buffer using zero-based
 * line and column numbers. Used throughout the API for cursor positions,
 * selection boundaries, and syntax highlighting ranges.
 * 
 * @note Both line and column are zero-based indices
 * @since 1.0.0
 */
typedef struct {
    /** @brief Zero-based line number */
    size_t line;
    
    /** @brief Zero-based column number (character offset within line) */
    size_t column;
} vizero_position_t;

/**
 * @brief Text range structure for start/end positions
 * 
 * Represents a range of text from a start position to an end position.
 * Used for selections, syntax highlighting spans, and text operations.
 * The range includes the start position but excludes the end position.
 * 
 * @note Range is half-open: [start, end)
 * @since 1.0.0
 */
typedef struct {
    /** @brief Starting position of the range (inclusive) */
    vizero_position_t start;
    
    /** @brief Ending position of the range (exclusive) */
    vizero_position_t end;
} vizero_range_t;

/** @} */ // end of text_structures group

/**
 * @defgroup syntax_highlighting Syntax Highlighting
 * @brief Structures and definitions for syntax highlighting plugins
 * @{
 */

/**
 * @brief RGBA color structure for syntax highlighting
 * 
 * Represents a color with red, green, blue, and alpha (transparency)
 * components. Used for syntax highlighting to specify text colors.
 * All components are in the range 0-255.
 * 
 * @since 1.0.0
 */
typedef struct {
    /** @brief Red component (0-255) */
    uint8_t r;
    
    /** @brief Green component (0-255) */
    uint8_t g;
    
    /** @brief Blue component (0-255) */
    uint8_t b;
    
    /** @brief Alpha/transparency component (0-255, 255=opaque) */
    uint8_t a;
} vizero_plugin_colour_t;

/**
 * @brief Text formatting flags for syntax highlighting
 * 
 * Bitfield flags that can be combined to specify text formatting
 * options like bold, italic, underline, etc.
 */
typedef enum {
    /** @brief No special formatting */
    VIZERO_TEXT_NORMAL     = 0x00,
    
    /** @brief Bold text weight */
    VIZERO_TEXT_BOLD       = 0x01,
    
    /** @brief Italic text style */
    VIZERO_TEXT_ITALIC     = 0x02,
    
    /** @brief Underlined text */
    VIZERO_TEXT_UNDERLINE  = 0x04,
    
    /** @brief Strikethrough text */
    VIZERO_TEXT_STRIKETHROUGH = 0x08
} vizero_text_flags_t;

/**
 * @brief Syntax highlighting token structure
 * 
 * Represents a single syntax highlighting token with its text range,
 * color, and formatting flags. Syntax highlighting plugins return
 * arrays of these tokens to specify how text should be highlighted.
 * 
 * @since 1.0.0
 */
typedef struct {
    /** @brief Text range covered by this token */
    vizero_range_t range;
    
    /** @brief Color for rendering this token */
    vizero_plugin_colour_t colour;
    
    /** @brief Formatting flags (bold, italic, etc.) */
    uint32_t flags;
} vizero_syntax_token_t;

/** @} */ // end of syntax_highlighting group

/**
 * @defgroup lsp_integration Language Server Protocol Integration
 * @brief Structures and definitions for LSP plugin support
 * @{
 */

/**
 * @brief LSP completion item kinds
 * 
 * Enumeration of completion item types as defined by the Language Server
 * Protocol specification. These values correspond directly to the LSP
 * CompletionItemKind enumeration and are used to categorize completion
 * suggestions from language servers.
 * 
 * @see https://microsoft.github.io/language-server-protocol/specifications/lsp/3.17/specification/#completionItemKind
 * @since 1.0.0
 */
typedef enum {
    /** @brief Plain text completion */
    VIZERO_COMPLETION_TEXT = 1,
    
    /** @brief Method completion */
    VIZERO_COMPLETION_METHOD = 2,
    
    /** @brief Function completion */
    VIZERO_COMPLETION_FUNCTION = 3,
    
    /** @brief Constructor completion */
    VIZERO_COMPLETION_CONSTRUCTOR = 4,
    
    /** @brief Field/member variable completion */
    VIZERO_COMPLETION_FIELD = 5,
    
    /** @brief Variable completion */
    VIZERO_COMPLETION_VARIABLE = 6,
    
    /** @brief Class completion */
    VIZERO_COMPLETION_CLASS = 7,
    
    /** @brief Interface completion */
    VIZERO_COMPLETION_INTERFACE = 8,
    
    /** @brief Module completion */
    VIZERO_COMPLETION_MODULE = 9,
    
    /** @brief Property completion */
    VIZERO_COMPLETION_PROPERTY = 10,
    
    /** @brief Unit/measurement completion */
    VIZERO_COMPLETION_UNIT = 11,
    
    /** @brief Value completion */
    VIZERO_COMPLETION_VALUE = 12,
    
    /** @brief Enum completion */
    VIZERO_COMPLETION_ENUM = 13,
    
    /** @brief Keyword completion */
    VIZERO_COMPLETION_KEYWORD = 14,
    
    /** @brief Code snippet completion */
    VIZERO_COMPLETION_SNIPPET = 15,
    
    /** @brief Color value completion */
    VIZERO_COMPLETION_COLOR = 16,
    
    /** @brief File completion */
    VIZERO_COMPLETION_FILE = 17,
    
    /** @brief Reference completion */
    VIZERO_COMPLETION_REFERENCE = 18
} vizero_completion_kind_t;

/**
 * @brief LSP completion item structure
 * 
 * Represents a single completion suggestion from a language server.
 * Contains all the information needed to display and insert the completion.
 * Memory management for string fields is handled by the plugin.
 * 
 * @since 1.0.0
 */
typedef struct {
    /** @brief Display label for the completion item (e.g., "push_back") */
    char* label;
    
    /** @brief Detailed signature or type info (e.g., "void push_back(const T&)") */
    char* detail;
    
    /** @brief Documentation string for this completion */
    char* documentation;
    
    /** @brief Text to insert when this completion is selected */
    char* insert_text;
    
    /** @brief Text used for filtering completions (defaults to label if NULL) */
    char* filter_text;
    
    /** @brief Text used for sorting completions (defaults to label if NULL) */
    char* sort_text;
    
    /** @brief Kind/category of this completion item */
    vizero_completion_kind_t kind;
    
    /** @brief Whether this completion item is deprecated */
    bool deprecated;
} vizero_completion_item_t;

/**
 * @brief LSP completion list structure
 * 
 * Contains a list of completion items returned by a language server,
 * along with metadata about whether the list is complete or truncated.
 * 
 * @since 1.0.0
 */
typedef struct {
    /** @brief Array of completion items */
    vizero_completion_item_t* items;
    
    /** @brief Number of items in the array */
    size_t item_count;
    
    /** @brief True if there are more completions available */
    bool is_incomplete;
} vizero_completion_list_t;

/**
 * @brief LSP location structure for navigation
 * 
 * Represents a location in a source file, used for go-to-definition,
 * find-references, and other navigation features.
 * 
 * @since 1.0.0
 */
typedef struct {
    /** @brief Absolute file path */
    char* file_path;
    
    /** @brief Position within the file */
    vizero_position_t position;
} vizero_location_t;

/**
 * @brief LSP diagnostic severity levels
 * 
 * Severity levels for diagnostics (errors, warnings, etc.) as defined
 * by the Language Server Protocol specification.
 * 
 * @since 1.0.0
 */
typedef enum {
    /** @brief Error-level diagnostic */
    VIZERO_DIAGNOSTIC_ERROR = 1,
    
    /** @brief Warning-level diagnostic */
    VIZERO_DIAGNOSTIC_WARNING = 2,
    
    /** @brief Information-level diagnostic */
    VIZERO_DIAGNOSTIC_INFORMATION = 3,
    
    /** @brief Hint-level diagnostic */
    VIZERO_DIAGNOSTIC_HINT = 4
} vizero_diagnostic_severity_t;

/**
 * @brief LSP diagnostic structure
 * 
 * Represents a diagnostic message (error, warning, etc.) from a language
 * server. Used to display compiler errors, warnings, and other code issues
 * in the editor.
 * 
 * @since 1.0.0
 */
typedef struct {
    /** @brief Text range where the diagnostic applies */
    vizero_range_t range;
    
    /** @brief Severity level of the diagnostic */
    vizero_diagnostic_severity_t severity;
    
    /** @brief Human-readable diagnostic message */
    char* message;
    
    /** @brief Source of the diagnostic (e.g., "clangd", "gcc") */
    char* source;
    
    /** @brief Optional numeric error code */
    int code;
} vizero_diagnostic_t;

/** @} */ // end of lsp_integration group

/**
 * @defgroup editor_api Editor API
 * @brief Function pointers provided by Vizero to plugins
 * @{
 */

/**
 * @brief Editor API function table provided to plugins
 * 
 * This structure contains function pointers that plugins can use to
 * interact with the editor. The editor passes this structure to plugins
 * during initialization, allowing plugins to query and modify editor state.
 * 
 * @note All function pointers are guaranteed to be non-NULL
 * @note Plugins should not store these function pointers beyond the
 *       scope of the callback that received them
 * @since 1.0.0
 */
typedef struct {
    /**
     * @defgroup buffer_operations Buffer Operations
     * @brief Functions for querying and modifying text buffers
     * @{
     */
    
    /** @brief Get the entire text content of a buffer as a string */
    const char* (*get_buffer_text)(vizero_buffer_t* buffer);
    
    /** @brief Get the filename associated with a buffer */
    const char* (*get_buffer_filename)(vizero_buffer_t* buffer);
    
    /** @brief Get the number of lines in a buffer */
    size_t (*get_buffer_line_count)(vizero_buffer_t* buffer);
    
    /** @brief Get the text content of a specific line */
    const char* (*get_buffer_line)(vizero_buffer_t* buffer, size_t line_num);
    
    /** @brief Get the length of a specific line */
    size_t (*get_buffer_line_length)(vizero_buffer_t* buffer, size_t line_num);
    
    /** @brief Insert text at a specific position */
    int (*insert_text)(vizero_buffer_t* buffer, vizero_position_t pos, const char* text);
    
    /** @brief Insert text with proper newline handling (splits lines on \n) */
    int (*insert_text_multiline)(vizero_buffer_t* buffer, vizero_position_t pos, const char* text);
    
    /** @brief Delete text within a specific range */
    int (*delete_text)(vizero_buffer_t* buffer, vizero_range_t range);
    
    /** @brief Check if a buffer is read-only */
    int (*is_buffer_readonly)(vizero_buffer_t* buffer);
    
    /** @brief Set the read-only status of a buffer */
    void (*set_buffer_readonly)(vizero_buffer_t* buffer, int readonly);
    
    /** @} */ // end of buffer_operations group
    
    /**
     * @defgroup cursor_api Cursor Operations
     * @brief Functions for querying and modifying cursor position
     * @{
     */
    
    /** @brief Get the current cursor position */
    vizero_position_t (*get_cursor_position)(vizero_cursor_t* cursor);
    
    /** @brief Set the cursor position */
    int (*set_cursor_position)(vizero_cursor_t* cursor, vizero_position_t pos);
    
    /** @} */ // end of cursor_api group
    
    /**
     * @defgroup editor_operations Editor Operations
     * @brief High-level editor operations and queries
     * @{
     */
    
    /** @brief Get the currently active buffer */
    vizero_buffer_t* (*get_current_buffer)(vizero_editor_t* editor);
    
    /** @brief Get the current cursor instance */
    vizero_cursor_t* (*get_current_cursor)(vizero_editor_t* editor);
    
    /** @brief Execute a vi command programmatically */
    int (*execute_command)(vizero_editor_t* editor, const char* command);
    
    /** @brief Set a temporary status bar message */
    int (*set_status_message)(vizero_editor_t* editor, const char* message);
    
    /** @} */ // end of editor_operations group
    
    /**
     * @defgroup file_operations File Operations
     * @brief Functions for file I/O operations
     * @{
     */
    
    /** @brief Open a file in the editor */
    int (*open_file)(vizero_editor_t* editor, const char* filename);
    
    /** @brief Save the current buffer to a file */
    int (*save_file)(vizero_editor_t* editor, const char* filename);
    
    /** @} */ // end of file_operations group
    
    /**
     * @defgroup rendering_api Rendering Operations
     * @brief Functions for controlling text rendering and highlighting
     * @{
     */
    
    /** @brief Add syntax highlighting tokens to the current buffer */
    int (*add_syntax_tokens)(vizero_editor_t* editor, vizero_syntax_token_t* tokens, size_t count);
    
    /** @brief Clear all syntax highlighting tokens */
    int (*clear_syntax_tokens)(vizero_editor_t* editor);
    
    /** @} */ // end of rendering_api group
} vizero_editor_api_t;

/** @} */ // end of editor_api group

/**
 * @defgroup plugin_commands Plugin Commands
 * @brief Structures for registering plugin commands
 * @{
 */

/**
 * @brief Plugin command registration structure
 * 
 * Used to register new vi commands that can be invoked from command mode.
 * Commands are registered during plugin initialization and remain available
 * until the plugin is unloaded.
 * 
 * @since 1.0.0
 */
typedef struct {
    /** @brief Command name (e.g., "connect", "irc") - max 63 chars */
    char command[64];
    
    /** @brief Help text describing the command - max 255 chars */
    char description[256];
    
    /** @brief Function to call when command is invoked */
    int (*handler)(vizero_editor_t* editor, const char* args);
    
    /** @brief Plugin-specific user data passed to handler */
    void* user_data;
} vizero_plugin_command_t;

/** @} */ // end of plugin_commands group

/**
 * @defgroup plugin_callbacks Plugin Callback Functions
 * @brief Callback function structure for plugin implementations
 * @{
 */

/**
 * @brief Plugin callback function table
 * 
 * This structure defines all the callback functions that a plugin can
 * implement. Only init() and cleanup() are required; all other callbacks
 * are optional and can be set to NULL if not needed.
 * 
 * The callbacks are invoked by Vizero at appropriate times during editor
 * operation to allow plugins to respond to events and provide functionality.
 * 
 * @since 1.0.0
 */
typedef struct {
    /**
     * @brief Plugin initialization callback (REQUIRED)
     * 
     * Called when the plugin is loaded. Must perform all necessary
     * initialization and return 0 on success. If this function returns
     * a non-zero value, the plugin will be unloaded.
     * 
     * @param editor The editor instance
     * @param api Function table for interacting with the editor
     * @return 0 on success, negative error code on failure
     * 
     * @note This callback must be implemented by all plugins
     * @thread_safety Called from main thread only
     */
    int (*init)(vizero_editor_t* editor, const vizero_editor_api_t* api);
    
    /**
     * @brief Plugin cleanup callback (REQUIRED)
     * 
     * Called when the plugin is being unloaded. Must clean up all
     * resources allocated by the plugin, including memory, file handles,
     * and any background threads.
     * 
     * @note This callback must be implemented by all plugins
     * @note Should not call any editor API functions
     * @thread_safety Called from main thread only
     */
    void (*cleanup)(void);
    
    /**
     * @brief Buffer opened callback (OPTIONAL)
     * 
     * Called when a new buffer is opened in the editor. This allows
     * plugins to perform per-buffer initialization or setup.
     * 
     * @param buffer The newly opened buffer
     * @param filename The filename of the opened buffer (may be NULL)
     * @return 0 on success, negative error code on failure
     * 
     * @thread_safety Called from main thread only
     */
    int (*on_buffer_open)(vizero_buffer_t* buffer, const char* filename);
    
    /**
     * @brief Buffer closed callback (OPTIONAL)
     * 
     * Called when a buffer is closed. Plugins should clean up any
     * per-buffer resources or state associated with this buffer.
     * 
     * @param buffer The buffer being closed
     * 
     * @note The buffer pointer becomes invalid after this callback returns
     * @thread_safety Called from main thread only
     */
    void (*on_buffer_close)(vizero_buffer_t* buffer);
    
    /**
     * @brief Text changed callback (OPTIONAL)
     * 
     * Called when text in a buffer is modified. This allows plugins
     * to track changes for features like syntax highlighting updates,
     * auto-completion, or real-time linting.
     * 
     * @param buffer The buffer that was modified
     * @param range The range of text that was affected
     * @param new_text The new text that was inserted (NULL for deletions)
     * @return 0 on success, negative error code on failure
     * 
     * @thread_safety Called from main thread only
     */
    int (*on_text_changed)(vizero_buffer_t* buffer, vizero_range_t range, const char* new_text);
    
    /**
     * @brief Cursor moved callback (OPTIONAL)
     * 
     * Called when the cursor position changes. Useful for plugins that
     * need to update UI elements or provide context-sensitive information
     * based on cursor location.
     * 
     * @param cursor The cursor that moved
     * @param old_pos Previous cursor position
     * @param new_pos New cursor position
     * 
     * @thread_safety Called from main thread only
     */
    void (*on_cursor_moved)(vizero_cursor_t* cursor, vizero_position_t old_pos, vizero_position_t new_pos);
    
    /* Optional: Called for command processing */
    int (*on_command)(vizero_editor_t* editor, const char* command, const char* args);
    
    /* Optional: Called for syntax highlighting (see full signature below) */
    /* int (*highlight_syntax) - defined in syntax_highlighting_callbacks section */
    
    /* Optional: Called for key input processing */
    int (*on_key_input)(vizero_editor_t* editor, uint32_t key, uint32_t modifiers);
    
    /**
     * @defgroup syntax_highlighting_callbacks Syntax Highlighting Callbacks
     * @brief Callbacks for syntax highlighting plugins
     * @{
     */
    
    /**
     * @brief Syntax highlighting callback (REQUIRED for syntax highlighter plugins)
     * 
     * Called to highlight a specific range of text in a buffer. The plugin
     * should analyze the text and fill the provided token array with
     * highlighting information.
     * 
     * @param buffer The buffer to highlight
     * @param start_line Starting line (inclusive)
     * @param end_line Ending line (exclusive)
     * @param tokens Array to fill with highlighting tokens
     * @param max_tokens Maximum number of tokens that can be stored
     * @param token_count Returns the actual number of tokens generated
     * @return 0 on success, negative error code on failure
     * 
     * @note Required for VIZERO_PLUGIN_TYPE_SYNTAX_HIGHLIGHTER plugins
     * @thread_safety Called from main thread only
     */
    int (*highlight_syntax)(vizero_buffer_t* buffer, size_t start_line, size_t end_line,
                           vizero_syntax_token_t* tokens, size_t max_tokens, size_t* token_count);
    
    /** @} */ // end of syntax_highlighting_callbacks group
    
    /**
     * @defgroup lsp_callbacks LSP Integration Callbacks
     * @brief Callbacks for Language Server Protocol plugins
     * @{
     */
    
    /** @brief Initialize LSP connection for a project */
    int (*lsp_initialize)(const char* project_root, const char* session_config);
    
    /** @brief Request code completion at a specific position */
    int (*lsp_completion)(vizero_buffer_t* buffer, vizero_position_t position, vizero_completion_list_t** result);
    
    /** @brief Request hover information at a specific position */
    int (*lsp_hover)(vizero_buffer_t* buffer, vizero_position_t position, char** hover_text);
    
    /** @brief Request go-to-definition locations */
    int (*lsp_goto_definition)(vizero_buffer_t* buffer, vizero_position_t position, vizero_location_t** locations, size_t* location_count);
    
    /** @brief Get current diagnostics for a buffer */
    int (*lsp_get_diagnostics)(vizero_buffer_t* buffer, vizero_diagnostic_t** diagnostics, size_t* diagnostic_count);
    
    /** @brief Shutdown LSP connection */
    void (*lsp_shutdown)(void);
    
    /** @} */ // end of lsp_callbacks group
    
    /**
     * @defgroup command_system Command System Integration
     * @brief Command registration and handling
     * @{
     */
    
    /** @brief Array of commands this plugin provides (can be NULL) */
    vizero_plugin_command_t* commands;
    
    /** @brief Number of commands in the commands array */
    size_t command_count;
    
    /** @} */ // end of command_system group
    
    /**
     * @defgroup ui_callbacks UI Extension Callbacks
     * @brief Callbacks for plugins that provide custom UI
     * @{
     */
    
    /** @brief Custom full-window rendering callback */
    int (*render_full_window)(vizero_editor_t* editor, vizero_renderer_t* renderer, int width, int height);
    
    /** @brief Check if plugin wants to take over the full window */
    int (*wants_full_window)(vizero_editor_t* editor);
    
    /** @} */ // end of ui_callbacks group
} vizero_plugin_callbacks_t;

/** @} */ // end of plugin_callbacks group

/**
 * @defgroup plugin_structure Main Plugin Structure
 * @brief Core plugin data structure
 * @{
 */

/**
 * @brief Main plugin structure
 * 
 * This structure encapsulates all plugin data including metadata,
 * callback functions, and runtime state. Managed internally by
 * the plugin system.
 * 
 * @note This structure is primarily for internal use by the plugin system
 * @since 1.0.0
 */
struct vizero_plugin_t {
    /** @brief Plugin metadata and version information */
    vizero_plugin_info_t info;
    
    /** @brief Plugin callback function table */
    vizero_plugin_callbacks_t callbacks;
    
    /** @brief Plugin-specific user data pointer */
    void* user_data;
    
    /** @brief Internal: DLL/shared library handle */
    void* dll_handle;
};

/** @} */ // end of plugin_structure group

/**
 * @defgroup plugin_exports Required Plugin Exports
 * @brief Functions that every plugin must export
 * @{
 */

/**
 * @brief Get plugin information (REQUIRED EXPORT)
 * 
 * Every plugin must export this function to provide metadata about
 * the plugin including name, version, author, and API compatibility.
 * This function is called during plugin discovery and loading.
 * 
 * @return Pointer to static plugin information structure
 * 
 * @note The returned pointer must remain valid for the plugin's lifetime
 * @note This function must be exported with VIZERO_PLUGIN_API linkage
 * @thread_safety Must be thread-safe
 * 
 * @example
 * @code
 * VIZERO_PLUGIN_API const vizero_plugin_info_t* vizero_plugin_get_info(void) {
 *     static const vizero_plugin_info_t info = {
 *         .name = "My Plugin",
 *         .version = "1.0.0",
 *         .author = "Plugin Author",
 *         .description = "Plugin description",
 *         .type = VIZERO_PLUGIN_TYPE_SYNTAX_HIGHLIGHTER,
 *         .api_version_major = VIZERO_PLUGIN_API_VERSION_MAJOR,
 *         .api_version_minor = VIZERO_PLUGIN_API_VERSION_MINOR,
 *         .api_version_patch = VIZERO_PLUGIN_API_VERSION_PATCH
 *     };
 *     return &info;
 * }
 * @endcode
 */
VIZERO_PLUGIN_API const vizero_plugin_info_t* vizero_plugin_get_info(void);

/**
 * @brief Initialize plugin (REQUIRED EXPORT)
 * 
 * Every plugin must export this function to perform initialization.
 * Called after the plugin is loaded and before any other plugin
 * functions are invoked. The plugin should set up its callback
 * functions and perform any necessary initialization.
 * 
 * @param plugin Plugin structure to initialize
 * @param editor Editor instance
 * @param api Function table for interacting with the editor
 * @return 0 on success, negative error code on failure
 * 
 * @note If this function returns non-zero, the plugin will be unloaded
 * @note This function must be exported with VIZERO_PLUGIN_API linkage
 * @thread_safety Called from main thread only
 * 
 * @example
 * @code
 * VIZERO_PLUGIN_API int vizero_plugin_init(vizero_plugin_t* plugin, 
 *                                          vizero_editor_t* editor, 
 *                                          const vizero_editor_api_t* api) {
 *     // Set up callbacks
 *     plugin->callbacks.init = my_init_callback;
 *     plugin->callbacks.cleanup = my_cleanup_callback;
 *     plugin->callbacks.highlight_syntax = my_highlight_callback;
 *     
 *     // Perform initialization
 *     return my_plugin_initialize(editor, api);
 * }
 * @endcode
 */
VIZERO_PLUGIN_API int vizero_plugin_init(vizero_plugin_t* plugin, vizero_editor_t* editor, const vizero_editor_api_t* api);

/**
 * @brief Cleanup plugin (REQUIRED EXPORT)
 * 
 * Every plugin must export this function to perform cleanup when
 * the plugin is being unloaded. Should clean up all resources
 * allocated by the plugin, including memory, file handles, and
 * background threads.
 * 
 * @param plugin Plugin structure to clean up
 * 
 * @note This function must be exported with VIZERO_PLUGIN_API linkage
 * @note Should not call any editor API functions
 * @thread_safety Called from main thread only
 * 
 * @example
 * @code
 * VIZERO_PLUGIN_API void vizero_plugin_cleanup(vizero_plugin_t* plugin) {
 *     // Clean up plugin resources
 *     my_plugin_shutdown();
 *     
 *     // Free any allocated memory
 *     if (plugin->user_data) {
 *         free(plugin->user_data);
 *         plugin->user_data = NULL;
 *     }
 * }
 * @endcode
 */
VIZERO_PLUGIN_API void vizero_plugin_cleanup(vizero_plugin_t* plugin);

/** @} */ // end of plugin_exports group

/**
 * @defgroup utility_macros Utility Macros
 * @brief Helper macros for plugin development
 * @{
 */

/**
 * @brief Convenience macro for defining plugin information
 * 
 * This macro simplifies the creation of the plugin information structure
 * and the required vizero_plugin_get_info() export function. It automatically
 * sets the current API version numbers.
 * 
 * @param name Plugin name string
 * @param version Plugin version string  
 * @param author Plugin author string
 * @param desc Plugin description string
 * @param type Plugin type from vizero_plugin_type_t
 * 
 * @example
 * @code
 * VIZERO_PLUGIN_DEFINE_INFO(
 *     "My Syntax Highlighter",
 *     "1.0.0", 
 *     "John Doe",
 *     "Syntax highlighting for my language",
 *     VIZERO_PLUGIN_TYPE_SYNTAX_HIGHLIGHTER
 * );
 * @endcode
 */
#define VIZERO_PLUGIN_DEFINE_INFO(name, version, author, desc, type) \
    static const vizero_plugin_info_t plugin_info = { \
        name, \
        version, \
        author, \
        desc, \
        type, \
        VIZERO_PLUGIN_API_VERSION_MAJOR, \
        VIZERO_PLUGIN_API_VERSION_MINOR, \
        VIZERO_PLUGIN_API_VERSION_PATCH \
    }; \
    VIZERO_PLUGIN_API const vizero_plugin_info_t* vizero_plugin_get_info(void) { \
        return &plugin_info; \
    }

/** @} */ // end of utility_macros group

/**
 * @defgroup internal_functions Internal Functions
 * @brief Functions for internal plugin system use
 * @{
 */

/**
 * @brief Initialize a plugin API structure with function pointers
 * 
 * This function is used internally by the plugin manager to set up
 * the editor API function table that is passed to plugins during
 * initialization.
 * 
 * @param api API structure to initialize
 * @param editor Editor instance to bind API functions to
 * 
 * @note This function is for internal use by the plugin system
 */
void vizero_plugin_interface_init_api(vizero_editor_api_t* api, vizero_editor_t* editor);

/**
 * @brief Check if a plugin's API version is compatible
 * 
 * Verifies that a plugin was built against a compatible version of
 * the plugin API. Used during plugin loading to prevent version
 * mismatches.
 * 
 * @param info Plugin information structure to check
 * @return 1 if compatible, 0 if incompatible
 * 
 * @note This function is for internal use by the plugin system
 */
int vizero_plugin_interface_check_version(const vizero_plugin_info_t* info);

/** @} */ // end of internal_functions group

/** @} */ // end of plugin_api group

#ifdef __cplusplus
}
#endif

#endif /* VIZERO_PLUGIN_INTERFACE_H */