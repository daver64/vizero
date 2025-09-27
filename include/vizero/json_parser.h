#ifndef VIZERO_JSON_PARSER_H
#define VIZERO_JSON_PARSER_H

#include <stddef.h>

#ifdef __cplusplus
extern "C" {
#endif

/* Opaque handle for JSON document */
typedef struct vizero_json_t vizero_json_t;

/* JSON parser functions */

/**
 * Parse JSON string into a JSON object handle
 * @param json_string The JSON string to parse
 * @param length Length of the JSON string 
 * @return JSON handle on success, NULL on failure
 */
vizero_json_t* vizero_json_parse(const char* json_string, size_t length);

/**
 * Free a JSON object handle
 * @param json The JSON handle to free
 */
void vizero_json_free(vizero_json_t* json);

/**
 * Extract a string value from a JSON object
 * @param json The JSON handle
 * @param field_name The field name to extract
 * @return Allocated string (caller must free) or NULL if not found/error
 */
char* vizero_json_get_string(vizero_json_t* json, const char* field_name);

/**
 * Extract an integer value from a JSON object
 * @param json The JSON handle
 * @param field_name The field name to extract
 * @param default_value Value to return if field not found or invalid
 * @return Integer value or default_value if not found/error
 */
int vizero_json_get_int(vizero_json_t* json, const char* field_name, int default_value);

/**
 * Extract a boolean value from a JSON object
 * @param json The JSON handle
 * @param field_name The field name to extract
 * @param default_value Value to return if field not found or invalid
 * @return Boolean value (0/1) or default_value if not found/error
 */
int vizero_json_get_bool(vizero_json_t* json, const char* field_name, int default_value);

/**
 * Check if a field exists in the JSON object
 * @param json The JSON handle
 * @param field_name The field name to check
 * @return 1 if exists, 0 if not
 */
int vizero_json_has_field(vizero_json_t* json, const char* field_name);

/**
 * Get a nested object from the JSON
 * @param json The JSON handle
 * @param field_name The field name containing the object
 * @return JSON handle for the nested object, or NULL if not found/error
 */
vizero_json_t* vizero_json_get_object(vizero_json_t* json, const char* field_name);

/**
 * Get array size
 * @param json The JSON handle (must be an array)
 * @return Array size, or -1 if not an array or error
 */
int vizero_json_array_size(vizero_json_t* json);

/**
 * Get array element by index
 * @param json The JSON handle (must be an array)
 * @param index The array index
 * @return JSON handle for the array element, or NULL if invalid index/error
 */
vizero_json_t* vizero_json_array_get(vizero_json_t* json, int index);

#ifdef __cplusplus
}
#endif

#endif /* VIZERO_JSON_PARSER_H */