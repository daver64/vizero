#ifndef VIZERO_ERROR_H
#define VIZERO_ERROR_H

#ifdef __cplusplus
extern "C" {
#endif

/**
 * @brief Standard Vizero result codes
 * 
 * All Vizero functions should return one of these standardized error codes
 * for consistent error handling throughout the codebase.
 */
typedef enum {
    /** @brief Operation completed successfully */
    VIZERO_SUCCESS = 0,
    
    /** @brief One or more required parameters were NULL */
    VIZERO_ERROR_NULL_PARAM = -1,
    
    /** @brief Memory allocation failed */
    VIZERO_ERROR_MEMORY = -2,
    
    /** @brief File I/O operation failed */
    VIZERO_ERROR_IO = -3,
    
    /** @brief Invalid argument or parameter value */
    VIZERO_ERROR_INVALID_ARG = -4,
    
    /** @brief Resource not found */
    VIZERO_ERROR_NOT_FOUND = -5,
    
    /** @brief Operation not supported */
    VIZERO_ERROR_NOT_SUPPORTED = -6,
    
    /** @brief Buffer too small for operation */
    VIZERO_ERROR_BUFFER_TOO_SMALL = -7,
    
    /** @brief Operation would cause overflow */
    VIZERO_ERROR_OVERFLOW = -8,
    
    /** @brief Generic internal error */
    VIZERO_ERROR_INTERNAL = -100
} vizero_result_t;

/**
 * @brief Convert error code to human-readable string
 * 
 * @param result The error code to convert
 * @return String description of the error (never NULL)
 */
const char* vizero_error_string(vizero_result_t result);

/**
 * @brief Check if result indicates success
 * 
 * @param result The result code to check
 * @return 1 if successful, 0 if error
 */
#define VIZERO_SUCCESS_P(result) ((result) == VIZERO_SUCCESS)

/**
 * @brief Check if result indicates failure
 * 
 * @param result The result code to check  
 * @return 1 if error, 0 if successful
 */
#define VIZERO_ERROR_P(result) ((result) != VIZERO_SUCCESS)

#ifdef __cplusplus
}
#endif

#endif /* VIZERO_ERROR_H */