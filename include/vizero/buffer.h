
/**
 * @file buffer.h
 * @brief Text Buffer Management API
 * 
 * This header provides the complete API for managing text buffers in Vizero.
 * Text buffers are the core data structure for storing and manipulating
 * document content, providing efficient operations for editing, searching,
 * and undo/redo functionality.
 * 
 * @author Vizero Team
 * @version 1.0.0
 * @date 2025
 * @copyright Licensed under the same terms as Vizero
 */

#ifndef VIZERO_BUFFER_H
#define VIZERO_BUFFER_H

#include <stddef.h>
#include <stdint.h>

#ifdef __cplusplus
extern "C" {
#endif

/**
 * @defgroup buffer_api Text Buffer API
 * @brief Core text buffer management and operations
 * @{
 */

/**
 * @defgroup buffer_types Buffer Data Types
 * @brief Data structures for buffer operations
 * @{
 */

/** @brief Opaque text buffer structure */
typedef struct vizero_buffer_t vizero_buffer_t;

/** @brief Opaque text line structure */
typedef struct vizero_line_t vizero_line_t;

/**
 * @brief Search result structure
 * 
 * Represents a single search match within a text buffer, including
 * the position and length of the matched text.
 * 
 * @since 1.0.0
 */
typedef struct {
    /** @brief Zero-based line number where match was found */
    size_t line;
    
    /** @brief Zero-based column where match starts */
    size_t column;
    
    /** @brief Length of the matched text in characters */
    size_t length;
} vizero_search_result_t;

/**
 * @brief Buffer statistics structure
 * 
 * Contains various statistics about a text buffer including
 * line count, character count, word count, and byte size.
 * Used for status bar display and document analysis.
 * 
 * @since 1.0.0
 */
typedef struct {
    /** @brief Total number of lines in the buffer */
    size_t line_count;
    
    /** @brief Total number of characters (excluding line endings) */
    size_t character_count;
    
    /** @brief Total number of words (whitespace-separated) */
    size_t word_count;
    
    /** @brief Total size in bytes (including line endings) */
    size_t byte_count;
} vizero_buffer_stats_t;

/** @} */ // end of buffer_types group

/**
 * @defgroup buffer_lifecycle Buffer Creation and Destruction
 * @brief Functions for creating and destroying text buffers
 * @{
 */

/**
 * @brief Create a new empty text buffer
 * 
 * Allocates and initializes a new empty text buffer. The buffer starts
 * with no associated filename and is marked as unmodified. Memory must
 * be freed using vizero_buffer_destroy().
 * 
 * @return Pointer to newly created buffer on success, NULL on failure
 * @retval NULL if memory allocation fails
 * 
 * @see vizero_buffer_create_from_file() for file-backed buffers
 * @see vizero_buffer_destroy() for cleanup
 * 
 * @since 1.0.0
 * @thread_safety This function is not thread-safe
 */
vizero_buffer_t* vizero_buffer_create(void);

/**
 * @brief Create a text buffer from a file
 * 
 * Creates a new text buffer and loads its content from the specified
 * file. If the file cannot be read, an empty buffer is created but
 * the filename is still associated with it.
 * 
 * @param filename Path to the file to load (must not be NULL)
 * @return Pointer to newly created buffer on success, NULL on failure
 * @retval NULL if memory allocation fails or filename is NULL
 * 
 * @note If file loading fails, an empty buffer with the filename is created
 * @see vizero_buffer_create() for empty buffers
 * @see vizero_buffer_load_from_file() to load into existing buffer
 * 
 * @since 1.0.0
 * @thread_safety This function is not thread-safe
 */
vizero_buffer_t* vizero_buffer_create_from_file(const char* filename);

/**
 * @brief Destroy a text buffer and free its memory
 * 
 * Frees all memory associated with a text buffer including its content,
 * undo history, and metadata. The buffer pointer becomes invalid after
 * this call.
 * 
 * @param buffer Buffer to destroy (may be NULL)
 * 
 * @note It is safe to pass NULL to this function
 * @note All references to this buffer become invalid after this call
 * 
 * @since 1.0.0
 * @thread_safety This function is not thread-safe
 */
void vizero_buffer_destroy(vizero_buffer_t* buffer);

/** @} */ // end of buffer_lifecycle group

/**
 * @defgroup buffer_properties Buffer Properties and Metadata
 * @brief Functions for querying and setting buffer properties
 * @{
 */

/**
 * @brief Get the filename associated with a buffer
 * 
 * Returns the filename that was set when the buffer was created or
 * loaded from a file. May be NULL if the buffer has no associated filename.
 * 
 * @param buffer Buffer to query (must not be NULL)
 * @return Pointer to filename string, or NULL if no filename is set
 * 
 * @note The returned pointer is valid until the buffer is destroyed
 *       or the filename is changed
 * @pre buffer must not be NULL
 * 
 * @since 1.0.0
 * @thread_safety This function is not thread-safe
 */
const char* vizero_buffer_get_filename(vizero_buffer_t* buffer);

/**
 * @brief Set the filename associated with a buffer
 * 
 * Associates a filename with the buffer. This does not perform any
 * file operations - it only updates the buffer's metadata.
 * 
 * @param buffer Buffer to modify (must not be NULL)
 * @param filename New filename to associate (may be NULL to clear)
 * @return 0 on success, negative error code on failure
 * @retval 0 Success
 * @retval -1 Invalid buffer pointer
 * @retval -2 Memory allocation failure
 * 
 * @pre buffer must not be NULL
 * @note Pass NULL for filename to clear the associated filename
 * 
 * @since 1.0.0
 * @thread_safety This function is not thread-safe
 */
int vizero_buffer_set_filename(vizero_buffer_t* buffer, const char* filename);

/**
 * @brief Check if a buffer has been modified
 * 
 * Returns whether the buffer has been modified since it was last
 * saved or loaded from a file.
 * 
 * @param buffer Buffer to query (must not be NULL)
 * @return 1 if buffer is modified, 0 if not modified
 * 
 * @pre buffer must not be NULL
 * 
 * @since 1.0.0
 * @thread_safety This function is not thread-safe
 */
int vizero_buffer_is_modified(vizero_buffer_t* buffer);

/**
 * @brief Check if a buffer is read-only
 * 
 * Returns whether the buffer is currently marked as read-only.
 * Read-only buffers cannot be modified through normal editing operations.
 * 
 * @param buffer Buffer to query (must not be NULL)
 * @return 1 if buffer is read-only, 0 if writable
 * 
 * @pre buffer must not be NULL
 * 
 * @since 1.0.0
 * @thread_safety This function is not thread-safe
 */
int vizero_buffer_is_readonly(vizero_buffer_t* buffer);

/**
 * @brief Set the read-only status of a buffer
 * 
 * Marks a buffer as read-only or writable. Read-only buffers will
 * reject modification attempts.
 * 
 * @param buffer Buffer to modify (must not be NULL)
 * @param readonly 1 to make read-only, 0 to make writable
 * 
 * @pre buffer must not be NULL
 * 
 * @since 1.0.0
 * @thread_safety This function is not thread-safe
 */
void vizero_buffer_set_readonly(vizero_buffer_t* buffer, int readonly);

/**
 * @brief Check if a buffer is marked as scratch/transient
 * 
 * Returns whether the buffer is currently marked as scratch. Scratch
 * buffers are temporary buffers that don't trigger unsaved change warnings
 * when quitting the editor.
 * 
 * @param buffer Buffer to query (must not be NULL)
 * @return 1 if buffer is scratch, 0 if not
 * 
 * @pre buffer must not be NULL
 * 
 * @since 1.0.0
 * @thread_safety This function is not thread-safe
 */
int vizero_buffer_is_scratch(vizero_buffer_t* buffer);

/**
 * @brief Set the scratch/transient status of a buffer
 * 
 * Marks a buffer as scratch or regular. Scratch buffers are temporary
 * and don't trigger unsaved change warnings when quitting the editor.
 * This is useful for IRC channels, REPL buffers, and other transient content.
 * 
 * @param buffer Buffer to modify (must not be NULL)
 * @param scratch 1 to mark as scratch, 0 to mark as regular
 * 
 * @pre buffer must not be NULL
 * 
 * @since 1.0.0
 * @thread_safety This function is not thread-safe
 */
void vizero_buffer_set_scratch(vizero_buffer_t* buffer, int scratch);

/** @} */ // end of buffer_properties group

/**
 * @defgroup buffer_content Buffer Content Access
 * @brief Functions for accessing buffer content
 * @{
 */

/**
 * @brief Get the number of lines in a buffer
 * 
 * Returns the total number of lines in the buffer. An empty buffer
 * has 1 line (the empty line).
 * 
 * @param buffer Buffer to query (must not be NULL)
 * @return Number of lines in the buffer (always >= 1)
 * 
 * @pre buffer must not be NULL
 * 
 * @since 1.0.0
 * @thread_safety This function is not thread-safe
 */
size_t vizero_buffer_get_line_count(vizero_buffer_t* buffer);

/**
 * @brief Get a line structure by line number
 * 
 * Returns a pointer to the internal line structure. This is primarily
 * for internal use - plugins should use vizero_buffer_get_line_text().
 * 
 * @param buffer Buffer to query (must not be NULL)
 * @param line_num Zero-based line number
 * @return Pointer to line structure, or NULL if line number is invalid
 * 
 * @pre buffer must not be NULL
 * @note The returned pointer may become invalid after buffer modifications
 * @warning This function returns internal structures - use with caution
 * 
 * @since 1.0.0
 * @thread_safety This function is not thread-safe
 */
vizero_line_t* vizero_buffer_get_line(vizero_buffer_t* buffer, size_t line_num);

/**
 * @brief Get text content of a specific line
 * 
 * Returns the text content of the specified line as a null-terminated
 * string. The line ending character is not included.
 * 
 * @param buffer Buffer to query (must not be NULL)
 * @param line_num Zero-based line number
 * @return Pointer to line text, or NULL if line number is invalid
 * 
 * @pre buffer must not be NULL
 * @note The returned pointer may become invalid after buffer modifications
 * @note The returned string does not include the line ending
 * 
 * @since 1.0.0
 * @thread_safety This function is not thread-safe
 */
const char* vizero_buffer_get_line_text(vizero_buffer_t* buffer, size_t line_num);

/**
 * @brief Get the length of a specific line
 * 
 * Returns the length of the specified line in characters, not including
 * the line ending character.
 * 
 * @param buffer Buffer to query (must not be NULL)
 * @param line_num Zero-based line number
 * @return Length of the line in characters, or 0 if line number is invalid
 * 
 * @pre buffer must not be NULL
 * 
 * @since 1.0.0
 * @thread_safety This function is not thread-safe
 */
size_t vizero_buffer_get_line_length(vizero_buffer_t* buffer, size_t line_num);

/**
 * @brief Get entire buffer content as a string
 * 
 * Returns the complete buffer content as a single null-terminated string
 * with appropriate line endings. The returned string is dynamically
 * allocated and must be freed by the caller.
 * 
 * @param buffer Buffer to query (must not be NULL)
 * @return Dynamically allocated string containing buffer content, or NULL on failure
 * @retval NULL if buffer is NULL or memory allocation fails
 * 
 * @pre buffer must not be NULL
 * @note The caller must free() the returned string
 * @warning This can be expensive for large buffers
 * 
 * @since 1.0.0
 * @thread_safety This function is not thread-safe
 */
const char* vizero_buffer_get_text(vizero_buffer_t* buffer);

/** @} */ // end of buffer_content group

/**
 * @defgroup text_modification Text Modification Operations
 * @brief Functions for modifying buffer content
 * @{
 */

/**
 * @brief Insert a single character at a specific position
 * 
 * Inserts a single character at the specified line and column position.
 * The operation is recorded in the undo history.
 * 
 * @param buffer Target buffer (must not be NULL)
 * @param line Zero-based line number
 * @param col Zero-based column number
 * @param c Character to insert
 * @return 0 on success, negative error code on failure
 * @retval 0 Success
 * @retval -1 Invalid buffer pointer
 * @retval -2 Buffer is read-only
 * @retval -3 Invalid position
 * @retval -4 Memory allocation failure
 * 
 * @pre buffer must not be NULL
 * @post Buffer modification flag is set
 * @post Undo history contains new entry
 * 
 * @since 1.0.0
 * @thread_safety This function is not thread-safe
 */
int vizero_buffer_insert_char(vizero_buffer_t* buffer, size_t line, size_t col, char c);

/**
 * @brief Insert text at a specific position
 * 
 * Inserts the given text string at the specified position. Multi-line
 * text will create new lines as needed. The operation is recorded in
 * the undo history.
 * 
 * @param buffer Target buffer (must not be NULL)
 * @param line Zero-based line number
 * @param col Zero-based column number
 * @param text Text to insert (must not be NULL)
 * @return 0 on success, negative error code on failure
 * @retval 0 Success
 * @retval -1 Invalid buffer pointer
 * @retval -2 Invalid text pointer
 * @retval -3 Buffer is read-only
 * @retval -4 Invalid position
 * @retval -5 Memory allocation failure
 * 
 * @pre buffer must not be NULL
 * @pre text must not be NULL
 * @post Buffer modification flag is set
 * @post Undo history contains new entry
 * 
 * @since 1.0.0
 * @thread_safety This function is not thread-safe
 */
int vizero_buffer_insert_text(vizero_buffer_t* buffer, size_t line, size_t col, const char* text);

/**
 * @brief Delete a single character at a specific position
 * 
 * Deletes the character at the specified position. The operation is
 * recorded in the undo history.
 * 
 * @param buffer Target buffer (must not be NULL)
 * @param line Zero-based line number
 * @param col Zero-based column number
 * @return 0 on success, negative error code on failure
 * @retval 0 Success
 * @retval -1 Invalid buffer pointer
 * @retval -2 Buffer is read-only
 * @retval -3 Invalid position
 * @retval -4 No character to delete
 * 
 * @pre buffer must not be NULL
 * @post Buffer modification flag is set if character was deleted
 * @post Undo history contains new entry if character was deleted
 * 
 * @since 1.0.0
 * @thread_safety This function is not thread-safe
 */
int vizero_buffer_delete_char(vizero_buffer_t* buffer, size_t line, size_t col);

/**
 * @brief Delete a range of text
 * 
 * Deletes all text within the specified range. The range is specified
 * by start and end positions. The operation is recorded in the undo history.
 * 
 * @param buffer Target buffer (must not be NULL)
 * @param start_line Zero-based starting line number
 * @param start_col Zero-based starting column number
 * @param end_line Zero-based ending line number
 * @param end_col Zero-based ending column number
 * @return 0 on success, negative error code on failure
 * @retval 0 Success
 * @retval -1 Invalid buffer pointer
 * @retval -2 Buffer is read-only
 * @retval -3 Invalid range
 * @retval -4 Memory allocation failure
 * 
 * @pre buffer must not be NULL
 * @pre Range must be valid (start <= end)
 * @post Buffer modification flag is set if text was deleted
 * @post Undo history contains new entry if text was deleted
 * 
 * @since 1.0.0
 * @thread_safety This function is not thread-safe
 */
int vizero_buffer_delete_range(vizero_buffer_t* buffer, size_t start_line, size_t start_col, 
                              size_t end_line, size_t end_col);

/** @} */ // end of text_modification group

/**
 * @defgroup line_operations Line-Level Operations
 * @brief Functions for manipulating entire lines
 * @{
 */

/**
 * @brief Insert a new line with specified content
 * 
 * Inserts a new line at the specified position with the given text content.
 * Existing lines at and after the specified position are shifted down.
 * 
 * @param buffer Target buffer (must not be NULL)
 * @param line_num Zero-based line number where to insert
 * @param text Text content for the new line (may be NULL for empty line)
 * @return 0 on success, negative error code on failure
 * 
 * @pre buffer must not be NULL
 * @since 1.0.0
 * @thread_safety This function is not thread-safe
 */
int vizero_buffer_insert_line(vizero_buffer_t* buffer, size_t line_num, const char* text);

/**
 * @brief Delete an entire line
 * 
 * Removes the specified line from the buffer. Lines after the deleted
 * line are shifted up to fill the gap.
 * 
 * @param buffer Target buffer (must not be NULL)
 * @param line_num Zero-based line number to delete
 * @return 0 on success, negative error code on failure
 * 
 * @pre buffer must not be NULL
 * @since 1.0.0
 * @thread_safety This function is not thread-safe
 */
int vizero_buffer_delete_line(vizero_buffer_t* buffer, size_t line_num);

/**
 * @brief Split a line at a specific column
 * 
 * Splits the specified line at the given column position, creating
 * two lines. The text after the split point becomes a new line.
 * 
 * @param buffer Target buffer (must not be NULL)
 * @param line_num Zero-based line number to split
 * @param col Zero-based column position where to split
 * @return 0 on success, negative error code on failure
 * 
 * @pre buffer must not be NULL
 * @since 1.0.0
 * @thread_safety This function is not thread-safe
 */
int vizero_buffer_split_line(vizero_buffer_t* buffer, size_t line_num, size_t col);

/**
 * @brief Join two consecutive lines
 * 
 * Joins the specified line with the next line, removing the line break
 * between them. The next line's content is appended to the current line.
 * 
 * @param buffer Target buffer (must not be NULL)
 * @param line_num Zero-based line number to join with the next line
 * @return 0 on success, negative error code on failure
 * 
 * @pre buffer must not be NULL
 * @since 1.0.0
 * @thread_safety This function is not thread-safe
 */
int vizero_buffer_join_lines(vizero_buffer_t* buffer, size_t line_num);

/** @} */ // end of line_operations group

/**
 * @defgroup file_io File I/O Operations
 * @brief Functions for loading and saving buffer content
 * @{
 */

/**
 * @brief Load buffer content from a file
 * 
 * Replaces the current buffer content with the content of the specified
 * file. The buffer's filename is updated and the modification flag is cleared.
 * 
 * @param buffer Target buffer (must not be NULL)
 * @param filename Path to file to load (must not be NULL)
 * @return 0 on success, negative error code on failure
 * 
 * @pre buffer must not be NULL
 * @pre filename must not be NULL
 * @since 1.0.0
 * @thread_safety This function is not thread-safe
 */
int vizero_buffer_load_from_file(vizero_buffer_t* buffer, const char* filename);

/**
 * @brief Save buffer content to a specific file
 * 
 * Saves the buffer content to the specified file. Updates the buffer's
 * filename and clears the modification flag on success.
 * 
 * @param buffer Buffer to save (must not be NULL)
 * @param filename Path to file to save (must not be NULL)
 * @return 0 on success, negative error code on failure
 * 
 * @pre buffer must not be NULL
 * @pre filename must not be NULL
 * @since 1.0.0
 * @thread_safety This function is not thread-safe
 */
int vizero_buffer_save_to_file(vizero_buffer_t* buffer, const char* filename);

/**
 * @brief Save buffer to its associated file
 * 
 * Saves the buffer content to its currently associated filename.
 * Clears the modification flag on success.
 * 
 * @param buffer Buffer to save (must not be NULL)
 * @return 0 on success, negative error code on failure
 * @retval -1 Invalid buffer pointer
 * @retval -2 No filename associated with buffer
 * @retval -3 File write error
 * 
 * @pre buffer must not be NULL
 * @pre buffer must have an associated filename
 * @since 1.0.0
 * @thread_safety This function is not thread-safe
 */
int vizero_buffer_save(vizero_buffer_t* buffer);

/** @} */ // end of file_io group

/**
 * @defgroup search_operations Search Operations
 * @brief Functions for searching within buffer content
 * @{
 */

/**
 * @brief Search for text within the buffer
 * 
 * Searches the buffer for occurrences of the specified pattern using
 * either literal string matching or regular expressions.
 * 
 * @param buffer Buffer to search (must not be NULL)
 * @param pattern Search pattern (must not be NULL)
 * @param use_regex 1 to use regex matching, 0 for literal string matching
 * @param results Array to store search results (may be NULL)
 * @param max_results Maximum number of results to store
 * @param result_count Returns actual number of results found (may be NULL)
 * @return 0 on success, negative error code on failure
 * 
 * @pre buffer must not be NULL
 * @pre pattern must not be NULL
 * @note If results is NULL, only result_count is updated
 * @since 1.0.0
 * @thread_safety This function is not thread-safe
 */
int vizero_buffer_search(vizero_buffer_t* buffer, const char* pattern, int use_regex,
                        vizero_search_result_t* results, size_t max_results, size_t* result_count);

/** @} */ // end of search_operations group

/**
 * @defgroup file_monitoring File Monitoring Support
 * @brief Functions for tracking file modification times
 * @{
 */

/**
 * @brief Get the last known disk modification time
 * 
 * Returns the modification time of the file when it was last loaded
 * or saved. Used for detecting external file changes.
 * 
 * @param buffer Buffer to query (must not be NULL)
 * @return Last known modification time as Unix timestamp
 * 
 * @pre buffer must not be NULL
 * @since 1.0.0
 * @thread_safety This function is not thread-safe
 */
uint64_t vizero_buffer_get_last_disk_mtime(vizero_buffer_t* buffer);

/**
 * @brief Set the last known disk modification time
 * 
 * Updates the stored modification time for the buffer's associated file.
 * Used internally when loading or saving files.
 * 
 * @param buffer Buffer to update (must not be NULL)
 * @param mtime New modification time as Unix timestamp
 * 
 * @pre buffer must not be NULL
 * @since 1.0.0
 * @thread_safety This function is not thread-safe
 */
void vizero_buffer_set_last_disk_mtime(vizero_buffer_t* buffer, uint64_t mtime);

/** @} */ // end of file_monitoring group

/**
 * @defgroup undo_redo Undo/Redo Operations
 * @brief Functions for undo and redo functionality
 * @{
 */

/**
 * @brief Undo the last operation
 * 
 * Undoes the most recent modification operation if possible.
 * Updates the buffer content and cursor position.
 * 
 * @param buffer Target buffer (must not be NULL)
 * @return 0 on success, negative error code on failure
 * @retval -1 Nothing to undo
 * 
 * @pre buffer must not be NULL
 * @since 1.0.0
 * @thread_safety This function is not thread-safe
 */
int vizero_buffer_undo(vizero_buffer_t* buffer);

/**
 * @brief Redo the last undone operation
 * 
 * Redoes the most recently undone operation if possible.
 * Updates the buffer content and cursor position.
 * 
 * @param buffer Target buffer (must not be NULL)
 * @return 0 on success, negative error code on failure
 * @retval -1 Nothing to redo
 * 
 * @pre buffer must not be NULL
 * @since 1.0.0
 * @thread_safety This function is not thread-safe
 */
int vizero_buffer_redo(vizero_buffer_t* buffer);

/**
 * @brief Check if undo is possible
 * 
 * Returns whether there are operations that can be undone.
 * 
 * @param buffer Buffer to query (must not be NULL)
 * @return 1 if undo is possible, 0 if not
 * 
 * @pre buffer must not be NULL
 * @since 1.0.0
 * @thread_safety This function is not thread-safe
 */
int vizero_buffer_can_undo(vizero_buffer_t* buffer);

/**
 * @brief Check if redo is possible
 * 
 * Returns whether there are operations that can be redone.
 * 
 * @param buffer Buffer to query (must not be NULL)
 * @return 1 if redo is possible, 0 if not
 * 
 * @pre buffer must not be NULL
 * @since 1.0.0
 * @thread_safety This function is not thread-safe
 */
int vizero_buffer_can_redo(vizero_buffer_t* buffer);

/** @} */ // end of undo_redo group

/**
 * @defgroup buffer_statistics Buffer Statistics
 * @brief Functions for buffer analysis and statistics
 * @{
 */

/**
 * @brief Get comprehensive buffer statistics
 * 
 * Calculates and returns various statistics about the buffer including
 * line count, character count, word count, and byte size.
 * 
 * @param buffer Buffer to analyze (must not be NULL)
 * @param stats Pointer to structure to fill with statistics (must not be NULL)
 * 
 * @pre buffer must not be NULL
 * @pre stats must not be NULL
 * @note This operation may be expensive for large buffers
 * @since 1.0.0
 * @thread_safety This function is not thread-safe
 */
void vizero_buffer_get_stats(vizero_buffer_t* buffer, vizero_buffer_stats_t* stats);

/** @} */ // end of buffer_statistics group

/** @} */ // end of buffer_api group

#ifdef __cplusplus
}
#endif

#endif /* VIZERO_BUFFER_H */