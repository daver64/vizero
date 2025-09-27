#include "vizero/plugin_registry.h"
#include "vizero/json_parser.h"
#include <stdio.h>
#include <stdlib.h>
#include <string.h>

/* Helper function to safely copy string from JSON parser */
static void safe_copy_string(char* dest, const char* src, size_t dest_size) {
    if (src) {
        strncpy(dest, src, dest_size - 1);
        dest[dest_size - 1] = '\0';
    } else {
        dest[0] = '\0';
    }
}

static vizero_plugin_type_t parse_plugin_type(const char* type_str) {
    if (strcmp(type_str, "syntax_highlighter") == 0) {
        return VIZERO_PLUGIN_TYPE_SYNTAX_HIGHLIGHTER;
    } else if (strcmp(type_str, "file_browser") == 0) {
        return VIZERO_PLUGIN_TYPE_FILE_TYPE_HANDLER;
    } else if (strcmp(type_str, "generic") == 0) {
        return VIZERO_PLUGIN_TYPE_GENERIC;
    }
    return VIZERO_PLUGIN_TYPE_GENERIC;
}

/* Registry management */
vizero_plugin_registry_t* vizero_plugin_registry_create(void) {
    vizero_plugin_registry_t* registry = (vizero_plugin_registry_t*)calloc(1, sizeof(vizero_plugin_registry_t));
    return registry;
}

void vizero_plugin_registry_destroy(vizero_plugin_registry_t* registry) {
    if (registry) {
        free(registry);
    }
}

int vizero_plugin_registry_load_manifest(vizero_plugin_registry_t* registry, const char* manifest_path) {
    if (!registry || !manifest_path) return -1;
    
    FILE* file = fopen(manifest_path, "r");
    if (!file) {
        printf("[REGISTRY] Warning: Could not open manifest file: %s\n", manifest_path);
        return -1;
    }
    
    /* Read entire file */
    fseek(file, 0, SEEK_END);
    long file_size = ftell(file);
    fseek(file, 0, SEEK_SET);
    
    char* json_content = (char*)malloc(file_size + 1);
    if (!json_content) {
        fclose(file);
        return -1;
    }
    
    fread(json_content, 1, file_size, file);
    json_content[file_size] = '\0';
    fclose(file);
    
    /* Parse JSON using proper parser */
    vizero_json_t* json = vizero_json_parse(json_content, file_size);
    free(json_content);
    
    if (!json) {
        printf("[REGISTRY] Error: Invalid JSON in manifest file\n");
        return -1;
    }
    
    /* Get plugins object */
    vizero_json_t* plugins_obj = vizero_json_get_object(json, "plugins");
    if (!plugins_obj) {
        printf("[REGISTRY] Error: Invalid manifest format - no plugins object\n");
        vizero_json_free(json);
        return -1;
    }
    
    /* We need to iterate through plugin entries - for now we'll use a workaround */
    /* since our JSON parser doesn't have object iteration yet */
    registry->entry_count = 0;
    
    /* Try to parse known plugin names from the manifest */
    /* This is a temporary solution until we add object iteration to json_parser */
    const char* known_plugins[] = {
        "file_browser", "syntax_c", "syntax_csharp", "syntax_markdown", 
        "syntax_xml", "syntax_python", "syntax_lisp", "syntax_php", 
        "syntax_javascript", "irc", "lisp_repl", "clangd", "example_plugin"
    };
    
    for (size_t i = 0; i < sizeof(known_plugins) / sizeof(known_plugins[0]) && registry->entry_count < 128; i++) {
        vizero_json_t* plugin_obj = vizero_json_get_object(plugins_obj, known_plugins[i]);
        if (!plugin_obj) continue;
        
        vizero_plugin_registry_entry_t* entry = &registry->entries[registry->entry_count];
        
        /* Set plugin name */
        safe_copy_string(entry->name, known_plugins[i], sizeof(entry->name));
        
        /* Parse type */
        char* type_str = vizero_json_get_string(plugin_obj, "type");
        if (type_str) {
            entry->type = parse_plugin_type(type_str);
            free(type_str);
        } else {
            entry->type = VIZERO_PLUGIN_TYPE_GENERIC;
        }
        
        /* Parse dll_path */
        char* dll_path = vizero_json_get_string(plugin_obj, "dll_path");
        safe_copy_string(entry->dll_path, dll_path, sizeof(entry->dll_path));
        if (dll_path) free(dll_path);
        
        /* Parse description */
        char* description = vizero_json_get_string(plugin_obj, "description");
        safe_copy_string(entry->description, description, sizeof(entry->description));
        if (description) free(description);
        
        /* Parse boolean flags */
        entry->load_on_demand = vizero_json_get_bool(plugin_obj, "load_on_demand", 0);
        entry->always_load = vizero_json_get_bool(plugin_obj, "always_load", 0);
        
        /* Parse priority */
        entry->priority = vizero_json_get_int(plugin_obj, "priority", 0);
        
        /* Initialize arrays - we'll need to enhance json_parser to handle arrays properly */
        entry->extension_count = 0;
        entry->repl_pattern_count = 0;
        
        /* For now, hardcode some known extensions based on plugin type */
        if (strcmp(entry->name, "syntax_c") == 0) {
            entry->extension_count = 3;
            strcpy(entry->file_extensions[0], ".c");
            strcpy(entry->file_extensions[1], ".h");
            strcpy(entry->file_extensions[2], ".cpp");
        } else if (strcmp(entry->name, "syntax_python") == 0) {
            entry->extension_count = 1;
            strcpy(entry->file_extensions[0], ".py");
        } else if (strcmp(entry->name, "syntax_javascript") == 0) {
            entry->extension_count = 1;
            strcpy(entry->file_extensions[0], ".js");
        } else if (strcmp(entry->name, "clangd") == 0) {
            entry->extension_count = 3;
            strcpy(entry->file_extensions[0], ".c");
            strcpy(entry->file_extensions[1], ".cpp");
            strcpy(entry->file_extensions[2], ".h");
        }
        
        entry->is_loaded = false;
        entry->plugin_instance = NULL;
        
        vizero_json_free(plugin_obj);
        registry->entry_count++;
    }
    
    vizero_json_free(plugins_obj);
    
    /* Build extension lookup map */
    registry->extension_map_count = 0;
    for (size_t i = 0; i < registry->entry_count; i++) {
        const vizero_plugin_registry_entry_t* entry = &registry->entries[i];
        
        for (size_t j = 0; j < entry->extension_count; j++) {
            const char* ext = entry->file_extensions[j];
            
            /* Find or create extension map entry */
            size_t map_idx = registry->extension_map_count;
            for (size_t k = 0; k < registry->extension_map_count; k++) {
                if (strcmp(registry->extension_map[k].extension, ext) == 0) {
                    map_idx = k;
                    break;
                }
            }
            
            if (map_idx == registry->extension_map_count) {
                /* Create new map entry */
                strncpy(registry->extension_map[map_idx].extension, ext, 15);
                registry->extension_map[map_idx].extension[15] = '\0';
                registry->extension_map[map_idx].plugin_count = 0;
                registry->extension_map_count++;
            }
            
            /* Add plugin to map entry */
            if (registry->extension_map[map_idx].plugin_count < 8) {
                registry->extension_map[map_idx].plugin_indices[registry->extension_map[map_idx].plugin_count] = i;
                registry->extension_map[map_idx].plugin_count++;
            }
        }
    }
    
    vizero_json_free(json);
    printf("[REGISTRY] Loaded %zu plugins from manifest\n", registry->entry_count);
    return 0;
}

/* Plugin queries */
const vizero_plugin_registry_entry_t* vizero_plugin_registry_find_by_name(
    const vizero_plugin_registry_t* registry, const char* name) {
    if (!registry || !name) return NULL;
    
    for (size_t i = 0; i < registry->entry_count; i++) {
        if (strcmp(registry->entries[i].name, name) == 0) {
            return &registry->entries[i];
        }
    }
    return NULL;
}

size_t vizero_plugin_registry_find_by_extension(
    const vizero_plugin_registry_t* registry, const char* extension,
    vizero_plugin_registry_entry_t** entries, size_t max_entries) {
    if (!registry || !extension || !entries) return 0;
    
    /* Find in extension map */
    for (size_t i = 0; i < registry->extension_map_count; i++) {
        if (strcmp(registry->extension_map[i].extension, extension) == 0) {
            size_t count = 0;
            for (size_t j = 0; j < registry->extension_map[i].plugin_count && count < max_entries; j++) {
                size_t plugin_idx = registry->extension_map[i].plugin_indices[j];
                entries[count] = (vizero_plugin_registry_entry_t*)&registry->entries[plugin_idx];
                count++;
            }
            return count;
        }
    }
    
    return 0;
}

size_t vizero_plugin_registry_find_by_filename(
    const vizero_plugin_registry_t* registry, const char* filename,
    vizero_plugin_registry_entry_t** entries, size_t max_entries) {
    if (!registry || !filename || !entries) return 0;
    
    size_t total_found = 0;
    
    /* Check file extension */
    const char* ext = vizero_plugin_registry_get_file_extension(filename);
    if (ext) {
        total_found = vizero_plugin_registry_find_by_extension(registry, ext, entries, max_entries);
    }
    
    /* Check REPL patterns */
    for (size_t i = 0; i < registry->entry_count && total_found < max_entries; i++) {
        const vizero_plugin_registry_entry_t* entry = &registry->entries[i];
        
        for (size_t j = 0; j < entry->repl_pattern_count; j++) {
            if (vizero_plugin_registry_matches_repl_pattern(filename, entry->repl_patterns[j])) {
                /* Check if already added */
                bool already_added = false;
                for (size_t k = 0; k < total_found; k++) {
                    if (entries[k] == entry) {
                        already_added = true;
                        break;
                    }
                }
                
                if (!already_added) {
                    entries[total_found] = (vizero_plugin_registry_entry_t*)entry;
                    total_found++;
                    break;
                }
            }
        }
    }
    
    return total_found;
}

/* Utility functions */
const char* vizero_plugin_registry_get_file_extension(const char* filename) {
    if (!filename) return NULL;
    
    const char* ext = strrchr(filename, '.');
    if (!ext || ext == filename) return NULL;
    
    return ext;
}

bool vizero_plugin_registry_matches_repl_pattern(const char* filename, const char* pattern) {
    if (!filename || !pattern) return false;
    
    /* Simple pattern matching - supports * as wildcard */
    const char* star = strchr(pattern, '*');
    if (!star) {
        /* No wildcard, exact match */
        return (strstr(filename, pattern) != NULL);
    }
    
    /* Pattern has wildcard */
    size_t prefix_len = star - pattern;
    if (prefix_len > 0) {
        if (strncmp(filename, pattern, prefix_len) != 0) {
            return false;
        }
    }
    
    /* Check suffix if any */
    const char* suffix = star + 1;
    if (*suffix) {
        size_t suffix_len = strlen(suffix);
        size_t filename_len = strlen(filename);
        if (filename_len >= suffix_len) {
            return (strcmp(filename + filename_len - suffix_len, suffix) == 0);
        }
    }
    
    return true;
}