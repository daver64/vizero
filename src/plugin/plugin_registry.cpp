#include "vizero/plugin_registry.h"
#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <ctype.h>

/* Simple JSON parser for manifest - just what we need */
static char* skip_whitespace(char* json) {
    while (*json && isspace(*json)) json++;
    return json;
}

static char* parse_string(char* json, char* output, size_t max_len) {
    json = skip_whitespace(json);
    if (*json != '"') return NULL;
    json++; /* Skip opening quote */
    
    size_t i = 0;
    while (*json && *json != '"' && i < max_len - 1) {
        if (*json == '\\' && *(json + 1)) {
            json++; /* Skip escape char */
            switch (*json) {
                case 'n': output[i++] = '\n'; break;
                case 't': output[i++] = '\t'; break;
                case 'r': output[i++] = '\r'; break;
                case '\\': output[i++] = '\\'; break;
                case '"': output[i++] = '"'; break;
                default: output[i++] = *json; break;
            }
        } else {
            output[i++] = *json;
        }
        json++;
    }
    output[i] = '\0';
    
    if (*json == '"') json++; /* Skip closing quote */
    return json;
}

static char* parse_array(char* json, char items[][64], size_t* count, size_t max_items) {
    json = skip_whitespace(json);
    if (*json != '[') return NULL;
    json++; /* Skip opening bracket */
    
    *count = 0;
    json = skip_whitespace(json);
    
    while (*json && *json != ']' && *count < max_items) {
        if (*count > 0) {
            json = skip_whitespace(json);
            if (*json == ',') json++;
            json = skip_whitespace(json);
        }
        
        json = parse_string(json, items[*count], 64);
        if (!json) break;
        (*count)++;
        
        json = skip_whitespace(json);
    }
    
    if (*json == ']') json++; /* Skip closing bracket */
    return json;
}

static char* find_object_value(char* json, const char* key) {
    char* pos = strstr(json, key);
    if (!pos) return NULL;
    
    pos += strlen(key);
    pos = skip_whitespace(pos);
    if (*pos != ':') return NULL;
    pos++;
    
    return skip_whitespace(pos);
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
    
    char* json = (char*)malloc(file_size + 1);
    if (!json) {
        fclose(file);
        return -1;
    }
    
    fread(json, 1, file_size, file);
    json[file_size] = '\0';
    fclose(file);
    
    /* Find plugins object */
    char* plugins_start = find_object_value(json, "\"plugins\"");
    if (!plugins_start || *plugins_start != '{') {
        printf("[REGISTRY] Error: Invalid manifest format - no plugins object\n");
        free(json);
        return -1;
    }
    
    plugins_start++; /* Skip opening brace */
    char* pos = plugins_start;
    
    /* Parse each plugin entry */
    registry->entry_count = 0;
    while (*pos && *pos != '}' && registry->entry_count < 128) {
        pos = skip_whitespace(pos);
        if (*pos == ',') pos++;
        pos = skip_whitespace(pos);
        if (*pos == '}') break;
        
        /* Parse plugin name */
        char plugin_name[MAX_PLUGIN_NAME];
        pos = parse_string(pos, plugin_name, sizeof(plugin_name));
        if (!pos) break;
        
        pos = skip_whitespace(pos);
        if (*pos != ':') break;
        pos++;
        
        pos = skip_whitespace(pos);
        if (*pos != '{') break;
        
        /* Find end of this plugin object */
        char* plugin_start = pos;
        int brace_count = 1;
        pos++;
        while (*pos && brace_count > 0) {
            if (*pos == '{') brace_count++;
            else if (*pos == '}') brace_count--;
            pos++;
        }
        
        /* Parse plugin properties */
        vizero_plugin_registry_entry_t* entry = &registry->entries[registry->entry_count];
        strncpy(entry->name, plugin_name, sizeof(entry->name) - 1);
        entry->name[sizeof(entry->name) - 1] = '\0';
        
        /* Parse type */
        char* type_pos = find_object_value(plugin_start, "\"type\"");
        if (type_pos) {
            char type_str[64];
            parse_string(type_pos, type_str, sizeof(type_str));
            entry->type = parse_plugin_type(type_str);
        }
        
        /* Parse dll_path */
        char* dll_pos = find_object_value(plugin_start, "\"dll_path\"");
        if (dll_pos) {
            parse_string(dll_pos, entry->dll_path, sizeof(entry->dll_path));
        }
        
        /* Parse description */
        char* desc_pos = find_object_value(plugin_start, "\"description\"");
        if (desc_pos) {
            parse_string(desc_pos, entry->description, sizeof(entry->description));
        }
        
        /* Parse load_on_demand */
        char* demand_pos = find_object_value(plugin_start, "\"load_on_demand\"");
        if (demand_pos) {
            entry->load_on_demand = (strncmp(demand_pos, "true", 4) == 0);
        }
        
        /* Parse always_load */
        char* always_pos = find_object_value(plugin_start, "\"always_load\"");
        if (always_pos) {
            entry->always_load = (strncmp(always_pos, "true", 4) == 0);
        }
        
        /* Parse priority */
        char* priority_pos = find_object_value(plugin_start, "\"priority\"");
        if (priority_pos) {
            entry->priority = atoi(priority_pos);
        } else {
            entry->priority = 0;
        }
        
        /* Parse file_extensions array */
        char* ext_pos = find_object_value(plugin_start, "\"file_extensions\"");
        if (ext_pos) {
            char extensions[MAX_FILE_EXTENSIONS][64];
            size_t ext_count;
            parse_array(ext_pos, extensions, &ext_count, MAX_FILE_EXTENSIONS);
            
            entry->extension_count = ext_count;
            for (size_t i = 0; i < ext_count; i++) {
                strncpy(entry->file_extensions[i], extensions[i], 15);
                entry->file_extensions[i][15] = '\0';
            }
        }
        
        /* Parse repl_patterns array */
        char* repl_pos = find_object_value(plugin_start, "\"repl_patterns\"");
        if (repl_pos) {
            char patterns[MAX_REPL_PATTERNS][64];
            size_t pattern_count;
            parse_array(repl_pos, patterns, &pattern_count, MAX_REPL_PATTERNS);
            
            entry->repl_pattern_count = pattern_count;
            for (size_t i = 0; i < pattern_count; i++) {
                strncpy(entry->repl_patterns[i], patterns[i], 63);
                entry->repl_patterns[i][63] = '\0';
            }
        }
        
        entry->is_loaded = false;
        entry->plugin_instance = NULL;
        
        registry->entry_count++;
    }
    
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
    
    free(json);
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