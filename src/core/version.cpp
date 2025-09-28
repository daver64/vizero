#include "vizero/version.h"
#include <stdio.h>

const char* vizero_get_version_string(void) {
    return VIZERO_VERSION_STRING;
}

int vizero_get_version_major(void) {
    return VIZERO_VERSION_MAJOR;
}

int vizero_get_version_minor(void) {
    return VIZERO_VERSION_MINOR;
}

int vizero_get_version_patch(void) {
    return VIZERO_VERSION_PATCH;
}

const char* vizero_get_build_info(void) {
    static char build_info[256];
    snprintf(build_info, sizeof(build_info), 
             "Vizero %s (built %s %s)", 
             VIZERO_VERSION_STRING, 
             VIZERO_BUILD_DATE, 
             VIZERO_BUILD_TIME);
    return build_info;
}