#ifndef VIZERO_PLUGIN_REGISTRY_H
#define VIZERO_PLUGIN_REGISTRY_H

#ifdef __cplusplus
extern "C" {
#endif

#include "plugin_interface.h"
#include <stddef.h>
#include <stdbool.h>

#define MAX_FILE_EXTENSIONS 32
#define MAX_REPL_PATTERNS 16
#define MAX_PLUGIN_PATH 512
#define MAX_PLUGIN_NAME 128
#define MAX_PLUGIN_DESCRIPTION 256

/* Plugin registry entry */
typedef struct {
    char name[MAX_PLUGIN_NAME];
    char dll_path[MAX_PLUGIN_PATH];
    char description[MAX_PLUGIN_DESCRIPTION];
    vizero_plugin_type_t type;
    bool load_on_demand;
    bool always_load;
    int priority;
    
    /* File type associations */
    char file_extensions[MAX_FILE_EXTENSIONS][16];
    size_t extension_count;
    
    /* REPL buffer patterns */
    char repl_patterns[MAX_REPL_PATTERNS][64];
    size_t repl_pattern_count;
    
    /* Loading state */
    bool is_loaded;
    vizero_plugin_t* plugin_instance;
} vizero_plugin_registry_entry_t;

/* Plugin registry */
typedef struct {
    vizero_plugin_registry_entry_t entries[128];
    size_t entry_count;
    
    /* Quick lookup maps */
    struct {
        char extension[16];
        size_t plugin_indices[8]; /* Multiple plugins can claim same extension */
        size_t plugin_count;
    } extension_map[256]; /* Hash table for fast extension lookup */
    size_t extension_map_count;
} vizero_plugin_registry_t;

/* Registry management */
vizero_plugin_registry_t* vizero_plugin_registry_create(void);
void vizero_plugin_registry_destroy(vizero_plugin_registry_t* registry);

/* Manifest loading */
int vizero_plugin_registry_load_manifest(vizero_plugin_registry_t* registry, const char* manifest_path);

/* Plugin queries */
const vizero_plugin_registry_entry_t* vizero_plugin_registry_find_by_name(
    const vizero_plugin_registry_t* registry, const char* name);

size_t vizero_plugin_registry_find_by_extension(
    const vizero_plugin_registry_t* registry, const char* extension, 
    vizero_plugin_registry_entry_t** entries, size_t max_entries);

size_t vizero_plugin_registry_find_by_filename(
    const vizero_plugin_registry_t* registry, const char* filename,
    vizero_plugin_registry_entry_t** entries, size_t max_entries);

/* Utility functions */
const char* vizero_plugin_registry_get_file_extension(const char* filename);
bool vizero_plugin_registry_matches_repl_pattern(const char* filename, const char* pattern);

#ifdef __cplusplus
}
#endif

#endif /* VIZERO_PLUGIN_REGISTRY_H */