#include "vizero/settings.h"
#include <stdlib.h>
#include <string.h>
#include <stdio.h>
#include <errno.h>

#ifdef _WIN32
#include <windows.h>
#include <shlobj.h>
#include <direct.h>
#else
#include <sys/stat.h>
#include <unistd.h>
#include <limits.h>
#define MAX_PATH PATH_MAX
#endif

#define MAX_SETTINGS 32
#define MAX_KEY_LENGTH 64
#define MAX_STRING_VALUE_LENGTH 256

typedef enum {
    SETTING_TYPE_BOOL,
    SETTING_TYPE_INT,
    SETTING_TYPE_STRING
} setting_type_t;

typedef struct {
    char key[MAX_KEY_LENGTH];
    setting_type_t type;
    union {
        int bool_value;
        int int_value;
        char string_value[MAX_STRING_VALUE_LENGTH];
    } value;
} setting_entry_t;

struct vizero_settings_t {
    setting_entry_t entries[MAX_SETTINGS];
    size_t count;
};

vizero_settings_t* vizero_settings_create(void) {
    vizero_settings_t* settings = (vizero_settings_t*)calloc(1, sizeof(vizero_settings_t));
    if (settings) {
        /* Set default values */
        vizero_settings_set_bool(settings, VIZERO_SETTING_LINE_NUMBERS, 0); /* Off by default */
        vizero_settings_set_int(settings, VIZERO_SETTING_TAB_SIZE, 4);
        vizero_settings_set_bool(settings, VIZERO_SETTING_WORD_WRAP, 0);
        vizero_settings_set_bool(settings, VIZERO_SETTING_SYNTAX_HIGHLIGHTING, 1);
        
        /* Set default window settings (will be overridden when window position is saved) */
        vizero_settings_set_int(settings, VIZERO_SETTING_WINDOW_X, -1); /* -1 means use centered */
        vizero_settings_set_int(settings, VIZERO_SETTING_WINDOW_Y, -1); /* -1 means use centered */
        vizero_settings_set_int(settings, VIZERO_SETTING_WINDOW_WIDTH, 1200);
        vizero_settings_set_int(settings, VIZERO_SETTING_WINDOW_HEIGHT, 800);
        vizero_settings_set_bool(settings, VIZERO_SETTING_WINDOW_MAXIMIZED, 0);
        
        /* Set default compiler settings */
#ifdef _WIN32
        vizero_settings_set_string(settings, VIZERO_SETTING_C_COMPILER, "gcc");
        vizero_settings_set_string(settings, VIZERO_SETTING_CPP_COMPILER, "g++");
        vizero_settings_set_string(settings, VIZERO_SETTING_ASSEMBLER, "nasm");
#else
        vizero_settings_set_string(settings, VIZERO_SETTING_C_COMPILER, "gcc");
        vizero_settings_set_string(settings, VIZERO_SETTING_CPP_COMPILER, "g++");
        vizero_settings_set_string(settings, VIZERO_SETTING_ASSEMBLER, "nasm");
#endif
    }
    return settings;
}

void vizero_settings_destroy(vizero_settings_t* settings) {
    free(settings);
}

static setting_entry_t* find_setting(vizero_settings_t* settings, const char* key) {
    if (!settings || !key) return NULL;
    
    for (size_t i = 0; i < settings->count; i++) {
        if (strcmp(settings->entries[i].key, key) == 0) {
            return &settings->entries[i];
        }
    }
    return NULL;
}

static setting_entry_t* create_setting(vizero_settings_t* settings, const char* key, setting_type_t type) {
    if (!settings || !key || settings->count >= MAX_SETTINGS) return NULL;
    
    setting_entry_t* entry = &settings->entries[settings->count];
    strncpy(entry->key, key, MAX_KEY_LENGTH - 1);
    entry->key[MAX_KEY_LENGTH - 1] = '\0';
    entry->type = type;
    settings->count++;
    return entry;
}

int vizero_settings_get_bool(vizero_settings_t* settings, const char* key) {
    setting_entry_t* entry = find_setting(settings, key);
    if (entry && entry->type == SETTING_TYPE_BOOL) {
        return entry->value.bool_value;
    }
    return 0; /* Default false */
}

void vizero_settings_set_bool(vizero_settings_t* settings, const char* key, int value) {
    setting_entry_t* entry = find_setting(settings, key);
    if (!entry) {
        entry = create_setting(settings, key, SETTING_TYPE_BOOL);
    }
    if (entry && entry->type == SETTING_TYPE_BOOL) {
        entry->value.bool_value = value ? 1 : 0;
    }
}

int vizero_settings_get_int(vizero_settings_t* settings, const char* key) {
    setting_entry_t* entry = find_setting(settings, key);
    if (entry && entry->type == SETTING_TYPE_INT) {
        return entry->value.int_value;
    }
    return 0; /* Default zero */
}

void vizero_settings_set_int(vizero_settings_t* settings, const char* key, int value) {
    setting_entry_t* entry = find_setting(settings, key);
    if (!entry) {
        entry = create_setting(settings, key, SETTING_TYPE_INT);
    }
    if (entry && entry->type == SETTING_TYPE_INT) {
        entry->value.int_value = value;
    }
}

const char* vizero_settings_get_string(vizero_settings_t* settings, const char* key) {
    setting_entry_t* entry = find_setting(settings, key);
    if (entry && entry->type == SETTING_TYPE_STRING) {
        return entry->value.string_value;
    }
    return ""; /* Default empty string */
}

void vizero_settings_set_string(vizero_settings_t* settings, const char* key, const char* value) {
    setting_entry_t* entry = find_setting(settings, key);
    if (!entry) {
        entry = create_setting(settings, key, SETTING_TYPE_STRING);
    }
    if (entry && entry->type == SETTING_TYPE_STRING && value) {
        strncpy(entry->value.string_value, value, MAX_STRING_VALUE_LENGTH - 1);
        entry->value.string_value[MAX_STRING_VALUE_LENGTH - 1] = '\0';
    }
}

/* Settings persistence implementation */

const char* vizero_settings_get_config_directory(void) {
    static char config_dir[MAX_PATH] = {0};
    
    if (config_dir[0] == '\0') {
#ifdef _WIN32
        /* Get AppData folder */
        char appdata[MAX_PATH];
        if (SHGetFolderPathA(NULL, CSIDL_APPDATA, NULL, 0, appdata) == S_OK) {
            snprintf(config_dir, sizeof(config_dir), "%s\\Vizero", appdata);
        } else {
            /* Fallback to current directory */
            strcpy(config_dir, ".");
        }
#else
        /* Linux/BSD - XDG config directory */
        const char* xdg_config = getenv("XDG_CONFIG_HOME");
        if (xdg_config) {
            snprintf(config_dir, sizeof(config_dir), "%s/vizero", xdg_config);
        } else {
            const char* home = getenv("HOME");
            if (home) {
                snprintf(config_dir, sizeof(config_dir), "%s/.config/vizero", home);
            } else {
                strcpy(config_dir, ".");
            }
        }
#endif
    }
    
    return config_dir;
}

const char* vizero_settings_get_config_file_path(void) {
    static char config_file[MAX_PATH] = {0};
    
    if (config_file[0] == '\0') {
        const char* config_dir = vizero_settings_get_config_directory();
#ifdef _WIN32
        snprintf(config_file, sizeof(config_file), "%s\\settings.ini", config_dir);
#else
        snprintf(config_file, sizeof(config_file), "%s/settings.ini", config_dir);
#endif
    }
    
    return config_file;
}

static int create_config_directory(void) {
    const char* config_dir = vizero_settings_get_config_directory();
    
#ifdef _WIN32
    int result = _mkdir(config_dir);
    if (result == 0) {
        /* Directory created successfully */
        return 1;
    } else {
        /* Check if directory already exists */
        DWORD attrs = GetFileAttributesA(config_dir);
        if (attrs != INVALID_FILE_ATTRIBUTES && (attrs & FILE_ATTRIBUTE_DIRECTORY)) {
            /* Directory already exists */
            return 1;
        }
        /* Failed to create directory */
        return 0;
    }
#else
    return mkdir(config_dir, 0755) == 0 || errno == EEXIST;
#endif
}

int vizero_settings_save_to_file(vizero_settings_t* settings) {
    if (!settings) return -1;
    
    /* Only save if there are settings to save */
    if (settings->count == 0) {
        return 0; /* Nothing to save, not an error */
    }
    
    /* Ensure config directory exists */
    if (!create_config_directory()) {
        fprintf(stderr, "Warning: Could not create config directory for settings\n");
        return -1;
    }
    
    const char* config_file = vizero_settings_get_config_file_path();
    FILE* file = fopen(config_file, "w");
    if (!file) {
        fprintf(stderr, "Warning: Could not save settings to %s\n", config_file);
        return -1;
    }
    
    fprintf(file, "[vizero]\n");
    
    /* Write all settings to INI format */
    for (size_t i = 0; i < settings->count; i++) {
        setting_entry_t* entry = &settings->entries[i];
        switch (entry->type) {
            case SETTING_TYPE_BOOL:
                fprintf(file, "%s=%s\n", entry->key, entry->value.bool_value ? "true" : "false");
                break;
            case SETTING_TYPE_INT:
                fprintf(file, "%s=%d\n", entry->key, entry->value.int_value);
                break;
            case SETTING_TYPE_STRING:
                fprintf(file, "%s=%s\n", entry->key, entry->value.string_value);
                break;
        }
    }
    
    fclose(file);
    return 0;
}

int vizero_settings_load_from_file(vizero_settings_t* settings) {
    if (!settings) return -1;
    
    const char* config_file = vizero_settings_get_config_file_path();
    FILE* file = fopen(config_file, "r");
    if (!file) {
        /* File doesn't exist, not an error - use defaults */
        return 0;
    }
    
    char line[512];
    while (fgets(line, sizeof(line), file)) {
        /* Skip comments and section headers */
        if (line[0] == '#' || line[0] == ';' || line[0] == '[') continue;
        
        /* Find key=value separator */
        char* equals = strchr(line, '=');
        if (!equals) continue;
        
        *equals = '\0';
        char* key = line;
        char* value = equals + 1;
        
        /* Trim whitespace */
        while (*key == ' ' || *key == '\t') key++;
        while (*value == ' ' || *value == '\t') value++;
        
        /* Remove trailing newline/whitespace from value */
        char* end = value + strlen(value) - 1;
        while (end > value && (*end == '\n' || *end == '\r' || *end == ' ' || *end == '\t')) {
            *end = '\0';
            end--;
        }
        
        /* Parse value based on content */
        if (strcmp(value, "true") == 0 || strcmp(value, "false") == 0) {
            /* Boolean value */
            vizero_settings_set_bool(settings, key, strcmp(value, "true") == 0);
        } else {
            /* Check if it's a number */
            char* endptr;
            long int_val = strtol(value, &endptr, 10);
            if (*endptr == '\0') {
                /* Integer value */
                vizero_settings_set_int(settings, key, (int)int_val);
            } else {
                /* String value */
                vizero_settings_set_string(settings, key, value);
            }
        }
    }
    
    fclose(file);
    return 0;
}

char* vizero_settings_get_all_as_string(vizero_settings_t* settings) {
    if (!settings) {
        return NULL;
    }
    
    /* Calculate required buffer size */
    size_t buffer_size = 1024; /* Start with 1KB */
    char* result = (char*)malloc(buffer_size);
    if (!result) {
        return NULL;
    }
    
    result[0] = '\0';
    size_t pos = 0;
    
    /* Add header */
    const char* header = "Current Settings:\n\n";
    size_t header_len = strlen(header);
    if (pos + header_len >= buffer_size) {
        buffer_size *= 2;
        result = (char*)realloc(result, buffer_size);
        if (!result) return NULL;
    }
    strcpy(result + pos, header);
    pos += header_len;
    
    /* Iterate through all settings */
    for (size_t i = 0; i < settings->count; i++) {
        const setting_entry_t* entry = &settings->entries[i];
        char line[512];
        
        switch (entry->type) {
            case SETTING_TYPE_BOOL:
                snprintf(line, sizeof(line), "%s = %s\n", 
                        entry->key, entry->value.bool_value ? "true" : "false");
                break;
            case SETTING_TYPE_INT:
                snprintf(line, sizeof(line), "%s = %d\n", 
                        entry->key, entry->value.int_value);
                break;
            case SETTING_TYPE_STRING:
                snprintf(line, sizeof(line), "%s = %s\n", 
                        entry->key, entry->value.string_value);
                break;
            default:
                continue;
        }
        
        size_t line_len = strlen(line);
        
        /* Ensure buffer is large enough */
        while (pos + line_len >= buffer_size) {
            buffer_size *= 2;
            char* new_result = (char*)realloc(result, buffer_size);
            if (!new_result) {
                free(result);
                return NULL;
            }
            result = new_result;
        }
        
        strcpy(result + pos, line);
        pos += line_len;
    }
    
    /* Add summary */
    char summary[128];
    snprintf(summary, sizeof(summary), "\nTotal: %zu setting%s", 
            settings->count, settings->count == 1 ? "" : "s");
    size_t summary_len = strlen(summary);
    
    while (pos + summary_len >= buffer_size) {
        buffer_size *= 2;
        char* new_result = (char*)realloc(result, buffer_size);
        if (!new_result) {
            free(result);
            return NULL;
        }
        result = new_result;
    }
    
    strcpy(result + pos, summary);
    
    return result;
}

/* Window state management functions */
void vizero_settings_save_window_state(vizero_settings_t* settings, int x, int y, int width, int height, int maximized) {
    if (!settings) return;
    
    vizero_settings_set_int(settings, VIZERO_SETTING_WINDOW_X, x);
    vizero_settings_set_int(settings, VIZERO_SETTING_WINDOW_Y, y);
    vizero_settings_set_int(settings, VIZERO_SETTING_WINDOW_WIDTH, width);
    vizero_settings_set_int(settings, VIZERO_SETTING_WINDOW_HEIGHT, height);
    vizero_settings_set_bool(settings, VIZERO_SETTING_WINDOW_MAXIMIZED, maximized);
}

void vizero_settings_load_window_state(vizero_settings_t* settings, int* x, int* y, int* width, int* height, int* maximized) {
    if (!settings) {
        /* Set safe defaults if settings is null */
        if (x) *x = -1;
        if (y) *y = -1;
        if (width) *width = 1200;
        if (height) *height = 800;
        if (maximized) *maximized = 0;
        return;
    }
    
    if (x) *x = vizero_settings_get_int(settings, VIZERO_SETTING_WINDOW_X);
    if (y) *y = vizero_settings_get_int(settings, VIZERO_SETTING_WINDOW_Y);
    if (width) *width = vizero_settings_get_int(settings, VIZERO_SETTING_WINDOW_WIDTH);
    if (height) *height = vizero_settings_get_int(settings, VIZERO_SETTING_WINDOW_HEIGHT);
    if (maximized) *maximized = vizero_settings_get_bool(settings, VIZERO_SETTING_WINDOW_MAXIMIZED);
}