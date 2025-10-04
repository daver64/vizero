#ifndef VIZERO_VERSION_H
#define VIZERO_VERSION_H

#ifdef __cplusplus
extern "C" {
#endif

/* Vizero Core Version - Canonical Source */
#define VIZERO_VERSION_MAJOR 0
#define VIZERO_VERSION_MINOR 0
#define VIZERO_VERSION_PATCH 6

#define VIZERO_VERSION_STRING "0.0.6"
#define VIZERO_VERSION_NUMERIC ((VIZERO_VERSION_MAJOR << 16) | (VIZERO_VERSION_MINOR << 8) | VIZERO_VERSION_PATCH)

/* Build information */
#define VIZERO_BUILD_DATE __DATE__
#define VIZERO_BUILD_TIME __TIME__

/* Version comparison macros */
#define VIZERO_VERSION_AT_LEAST(major, minor, patch) \
    (VIZERO_VERSION_NUMERIC >= (((major) << 16) | ((minor) << 8) | (patch)))

/* Plugin API version - increment when plugin interface changes */
#define VIZERO_PLUGIN_API_VERSION 1
#define VIZERO_PLUGIN_API_STRING "1.0"

/* Plugin version macro - all plugins should use this */
#define VIZERO_PLUGIN_VERSION VIZERO_VERSION_STRING

/* Version information functions */
const char* vizero_get_version_string(void);
int vizero_get_version_major(void);
int vizero_get_version_minor(void);
int vizero_get_version_patch(void);
const char* vizero_get_build_info(void);

#ifdef __cplusplus
}
#endif

#endif /* VIZERO_VERSION_H */