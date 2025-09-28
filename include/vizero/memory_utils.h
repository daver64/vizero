#ifndef VIZERO_MEMORY_UTILS_H
#define VIZERO_MEMORY_UTILS_H

#ifdef __cplusplus
extern "C" {
#endif

#include <stddef.h>
#include "vizero/error.h"
#include "vizero/plugin_interface.h"

/**
 * @brief Safe string duplication with error handling
 * 
 * @param str String to duplicate (can be NULL)
 * @return Duplicated string or NULL if input was NULL or allocation failed
 * @note Caller must free() the returned string
 */
char* vizero_safe_strdup(const char* str);

/**
 * @brief Safe string free (handles NULL pointers)
 * 
 * @param str Pointer to string to free (can be NULL)
 */
void vizero_safe_free(void* ptr);

/**
 * @brief Safe realloc with null check and error handling
 * 
 * @param ptr Existing pointer (can be NULL)
 * @param new_size New size to allocate
 * @return New pointer on success, NULL on failure
 * @note On failure, original pointer is NOT freed (caller must handle)
 */
void* vizero_safe_realloc(void* ptr, size_t new_size);

/**
 * @brief Safely free a completion item and all its allocated strings
 * 
 * @param item Completion item to free (can be NULL)
 */
void vizero_completion_item_free(vizero_completion_item_t* item);

/**
 * @brief Safely free an array of completion items
 * 
 * @param items Array of completion items (can be NULL)
 * @param count Number of items in the array
 */
void vizero_completion_items_free(vizero_completion_item_t* items, size_t count);

/**
 * @brief Safely free a completion list and all its contents
 * 
 * @param list Completion list to free (can be NULL)
 */
void vizero_completion_list_free(vizero_completion_list_t* list);

/**
 * @brief Safe string array free (handles NULL entries)
 * 
 * @param array Array of strings (can be NULL)
 * @param count Number of strings in array
 */
void vizero_string_array_safe_free(char** array, size_t count);

#ifdef __cplusplus
}
#endif

#endif /* VIZERO_MEMORY_UTILS_H */