#include "vizero/error.h"

const char* vizero_error_string(vizero_result_t result) {
    switch (result) {
        case VIZERO_SUCCESS:
            return "Success";
        case VIZERO_ERROR_NULL_PARAM:
            return "Null parameter";
        case VIZERO_ERROR_MEMORY:
            return "Memory allocation failed";
        case VIZERO_ERROR_IO:
            return "I/O operation failed";
        case VIZERO_ERROR_INVALID_ARG:
            return "Invalid argument";
        case VIZERO_ERROR_NOT_FOUND:
            return "Resource not found";
        case VIZERO_ERROR_NOT_SUPPORTED:
            return "Operation not supported";
        case VIZERO_ERROR_BUFFER_TOO_SMALL:
            return "Buffer too small";
        case VIZERO_ERROR_OVERFLOW:
            return "Overflow error";
        case VIZERO_ERROR_INTERNAL:
            return "Internal error";
        default:
            return "Unknown error";
    }
}