#include "vizero/plugin_manager.h"
#include "vizero/plugin_interface.h"
#include "vizero/plugin_registry.h"
#include "vizero/buffer.h"
#include "vizero/window.h"
#include <stdlib.h>
#include <string.h>
#include <stdio.h>
#include <stdbool.h>
#include <SDL.h>

#ifdef _WIN32
    #include <windows.h>
    #define PLUGIN_LOAD(path) LoadLibraryA(path)
    #define PLUGIN_GET_PROC(handle, name) GetProcAddress((HMODULE)handle, name)
    #define PLUGIN_UNLOAD(handle) FreeLibrary((HMODULE)handle)
    #define PLUGIN_ERROR() GetLastError()
#else
    #include <dlfcn.h>
    #include <dirent.h>
    #include <sys/stat.h>
    #define PLUGIN_LOAD(path) dlopen(path, RTLD_LAZY)
    #define PLUGIN_GET_PROC(handle, name) dlsym(handle, name)
    #define PLUGIN_UNLOAD(handle) dlclose(handle)
    #define PLUGIN_ERROR() dlerror()
#endif

#define MAX_PLUGINS 128

struct vizero_plugin_manager_t {
    vizero_editor_t* editor;
    vizero_editor_api_t api;
    vizero_plugin_t* plugins[MAX_PLUGINS];
    size_t plugin_count;
    
    /* Plugin registry for on-demand loading */
    vizero_plugin_registry_t* registry;
    char plugin_directory[512];
    
    /* SDL rendering support for plugins */
    SDL_Renderer* sdl_renderer;
    SDL_Texture* plugin_texture;
    int texture_width;
    int texture_height;
};

/* Function type definitions for plugin entry points */
typedef const vizero_plugin_info_t* (*plugin_get_info_func_t)(void);
typedef int (*plugin_init_func_t)(vizero_plugin_t*, vizero_editor_t*, const vizero_editor_api_t*);
typedef void (*plugin_cleanup_func_t)(vizero_plugin_t*);

/* Initialize the editor API structure */
static void init_editor_api(vizero_editor_api_t* api, vizero_editor_t* editor);

vizero_plugin_manager_t* vizero_plugin_manager_create(vizero_editor_t* editor) {
    vizero_plugin_manager_t* manager = (vizero_plugin_manager_t*)calloc(1, sizeof(vizero_plugin_manager_t));
    if (!manager) {
        return NULL;
    }
    
    manager->editor = editor;
    manager->plugin_count = 0;
    manager->registry = vizero_plugin_registry_create();
    
    /* Initialize the editor API */
    vizero_plugin_interface_init_api(&manager->api, editor);
    
    return manager;
}

void vizero_plugin_manager_destroy(vizero_plugin_manager_t* manager) {
    if (!manager) {
        return;
    }
    
    /* Plugin rendering resources no longer need cleanup */
    
    /* Unload all plugins */
    for (size_t i = 0; i < manager->plugin_count; i++) {
        if (manager->plugins[i]) {
            if (manager->plugins[i]->callbacks.cleanup) {
                manager->plugins[i]->callbacks.cleanup();
            }
            
            if (manager->plugins[i]->dll_handle) {
                PLUGIN_UNLOAD(manager->plugins[i]->dll_handle);
            }
            
            free(manager->plugins[i]);
        }
    }
    
    /* Destroy registry */
    if (manager->registry) {
        vizero_plugin_registry_destroy(manager->registry);
    }
    
    free(manager);
}

int vizero_plugin_manager_load_plugin(vizero_plugin_manager_t* manager, const char* plugin_path) {
    if (!manager || !plugin_path || manager->plugin_count >= MAX_PLUGINS) {
        return -1;
    }
    
    /* Load the dynamic library */
    void* handle = PLUGIN_LOAD(plugin_path);
    if (!handle) {
#ifdef _WIN32
        DWORD error = GetLastError();
        fprintf(stderr, "Failed to load plugin %s: Error code %lu\n", plugin_path, error);
#else
        fprintf(stderr, "Failed to load plugin %s: %s\n", plugin_path, PLUGIN_ERROR());
#endif
        return -1;
    }
    
    /* Get plugin entry points */
    plugin_get_info_func_t get_info = (plugin_get_info_func_t)PLUGIN_GET_PROC(handle, "vizero_plugin_get_info");
    plugin_init_func_t init_func = (plugin_init_func_t)PLUGIN_GET_PROC(handle, "vizero_plugin_init");
    plugin_cleanup_func_t cleanup_func = (plugin_cleanup_func_t)PLUGIN_GET_PROC(handle, "vizero_plugin_cleanup");
    
    if (!get_info || !init_func || !cleanup_func) {
        fprintf(stderr, "Plugin %s is missing required entry points\n", plugin_path);
        PLUGIN_UNLOAD(handle);
        return -1;
    }
    
    /* Get plugin info */
    const vizero_plugin_info_t* info = get_info();
    if (!info) {
        fprintf(stderr, "Plugin %s failed to provide info\n", plugin_path);
        PLUGIN_UNLOAD(handle);
        return -1;
    }
    
    /* Check API compatibility */
    if (info->api_version_major != VIZERO_PLUGIN_API_VERSION_MAJOR) {
        fprintf(stderr, "Plugin %s has incompatible API version %d.%d.%d (expected %d.x.x)\n",
                plugin_path, info->api_version_major, info->api_version_minor, info->api_version_patch,
                VIZERO_PLUGIN_API_VERSION_MAJOR);
        PLUGIN_UNLOAD(handle);
        return -1;
    }
    
    /* Create plugin instance */
    vizero_plugin_t* plugin = (vizero_plugin_t*)calloc(1, sizeof(vizero_plugin_t));
    if (!plugin) {
        PLUGIN_UNLOAD(handle);
        return -1;
    }
    
    /* Copy plugin info */
    plugin->info = *info;
    plugin->dll_handle = handle;
    
    /* Initialize plugin */
    if (init_func(plugin, manager->editor, &manager->api) != 0) {
        fprintf(stderr, "Plugin %s failed to initialize\n", plugin_path);
        free(plugin);
        PLUGIN_UNLOAD(handle);
        return -1;
    }
    
    /* Add to plugin list */
    manager->plugins[manager->plugin_count] = plugin;
    manager->plugin_count++;
    
    printf("Loaded plugin: %s v%s by %s\n", info->name, info->version, info->author);
    return 0;
}

int vizero_plugin_manager_unload_plugin(vizero_plugin_manager_t* manager, const char* plugin_name) {
    if (!manager || !plugin_name) {
        return -1;
    }
    
    for (size_t i = 0; i < manager->plugin_count; i++) {
        vizero_plugin_t* plugin = manager->plugins[i];
        if (plugin && strcmp(plugin->info.name, plugin_name) == 0) {
            /* Cleanup plugin */
            if (plugin->callbacks.cleanup) {
                plugin->callbacks.cleanup();
            }
            
            /* Unload DLL */
            if (plugin->dll_handle) {
                PLUGIN_UNLOAD(plugin->dll_handle);
            }
            
            free(plugin);
            
            /* Remove from array */
            for (size_t j = i; j < manager->plugin_count - 1; j++) {
                manager->plugins[j] = manager->plugins[j + 1];
            }
            manager->plugin_count--;
            
            return 0;
        }
    }
    
    return -1; /* Plugin not found */
}

int vizero_plugin_manager_scan_directory(vizero_plugin_manager_t* manager, const char* directory) {
    if (!manager || !directory) {
        return -1;
    }
    
    int loaded_count = 0;
    
#ifdef _WIN32
    /* Windows directory scanning */
    char search_path[1024];
    snprintf(search_path, sizeof(search_path), "%s\\*.dll", directory);
    
    WIN32_FIND_DATAA find_data;
    HANDLE hFind = FindFirstFileA(search_path, &find_data);
    
    if (hFind != INVALID_HANDLE_VALUE) {
        do {
            if (!(find_data.dwFileAttributes & FILE_ATTRIBUTE_DIRECTORY)) {
                /* Build full path */
                char plugin_path[1024];
                snprintf(plugin_path, sizeof(plugin_path), "%s\\%s", directory, find_data.cFileName);
                
                /* Try to load the plugin */
                if (vizero_plugin_manager_load_plugin(manager, plugin_path) == 0) {
                    loaded_count++;
                }
            }
        } while (FindNextFileA(hFind, &find_data) != 0);
        
        FindClose(hFind);
    }
#else
    /* Unix directory scanning */
    DIR* dir = opendir(directory);
    if (!dir) {
        return -1;
    }
    
    struct dirent* entry;
    
    while ((entry = readdir(dir)) != NULL) {
        /* Check for plugin extension */
        const char* ext = strrchr(entry->d_name, '.');
        if (!ext || strcmp(ext, ".so") != 0) continue;
        
        /* Build full path */
        char plugin_path[1024];
        snprintf(plugin_path, sizeof(plugin_path), "%s/%s", directory, entry->d_name);
        
        /* Try to load the plugin */
        if (vizero_plugin_manager_load_plugin(manager, plugin_path) == 0) {
            loaded_count++;
        }
    }
    
    closedir(dir);
#endif
    
    return loaded_count;
}

/* Registry management */
int vizero_plugin_manager_load_manifest(vizero_plugin_manager_t* manager, const char* manifest_path) {
    if (!manager || !manager->registry || !manifest_path) {
        return -1;
    }
    
    /* Extract plugin directory from manifest path */
    const char* last_slash = strrchr(manifest_path, '/');
    const char* last_backslash = strrchr(manifest_path, '\\');
    const char* separator = (last_slash > last_backslash) ? last_slash : last_backslash;
    
    if (separator) {
        size_t dir_len = separator - manifest_path;
        if (dir_len < sizeof(manager->plugin_directory) - 1) {
            strncpy(manager->plugin_directory, manifest_path, dir_len);
            manager->plugin_directory[dir_len] = '\0';
        }
    } else {
        strcpy(manager->plugin_directory, ".");
    }
    
    return vizero_plugin_registry_load_manifest(manager->registry, manifest_path);
}

/* On-demand loading */
int vizero_plugin_manager_load_plugins_for_file(vizero_plugin_manager_t* manager, const char* filename) {
    if (!manager || !manager->registry || !filename) {
        return -1;
    }
    
    /* Find plugins that should handle this file */
    vizero_plugin_registry_entry_t* entries[8];
    size_t entry_count = vizero_plugin_registry_find_by_filename(manager->registry, filename, entries, 8);
    
    int loaded_count = 0;
    for (size_t i = 0; i < entry_count; i++) {
        vizero_plugin_registry_entry_t* entry = entries[i];
        
        /* Skip if already loaded */
        if (entry->is_loaded) {
            continue;
        }
        
        /* Load the plugin */
        char plugin_path[1024];
        char plugin_filename[512];
        strncpy(plugin_filename, entry->dll_path, sizeof(plugin_filename) - 1);
        plugin_filename[sizeof(plugin_filename) - 1] = '\0';
        
#ifdef _WIN32
        /* Convert .so extension to .dll on Windows */
        char* ext = strrchr(plugin_filename, '.');
        if (ext && strcmp(ext, ".so") == 0) {
            strcpy(ext, ".dll");
        }
        
        snprintf(plugin_path, sizeof(plugin_path), "%s\\%s", manager->plugin_directory, plugin_filename);
#else
        /* Convert .dll extension to .so on Unix platforms */
        char* ext = strrchr(plugin_filename, '.');
        if (ext && strcmp(ext, ".dll") == 0) {
            strcpy(ext, ".so");
        }
        
        snprintf(plugin_path, sizeof(plugin_path), "%s/%s", manager->plugin_directory, plugin_filename);
#endif
        
        if (vizero_plugin_manager_load_plugin(manager, plugin_path) == 0) {
            entry->is_loaded = true;
            entry->plugin_instance = manager->plugins[manager->plugin_count - 1];
            loaded_count++;
            printf("[PLUGIN] Loaded on-demand: %s for file %s\n", entry->name, filename);
        }
    }
    
    return loaded_count;
}

int vizero_plugin_manager_ensure_always_loaded(vizero_plugin_manager_t* manager) {
    if (!manager || !manager->registry) {
        return -1;
    }
    
    int loaded_count = 0;
    for (size_t i = 0; i < manager->registry->entry_count; i++) {
        vizero_plugin_registry_entry_t* entry = &manager->registry->entries[i];
        
        if (entry->always_load && !entry->is_loaded) {
            /* Load the plugin */
            char plugin_path[1024];
            char plugin_filename[512];
            strncpy(plugin_filename, entry->dll_path, sizeof(plugin_filename) - 1);
            plugin_filename[sizeof(plugin_filename) - 1] = '\0';
            
#ifdef _WIN32
            /* Convert .so extension to .dll on Windows */
            char* ext = strrchr(plugin_filename, '.');
            if (ext && strcmp(ext, ".so") == 0) {
                strcpy(ext, ".dll");
            }
            
            snprintf(plugin_path, sizeof(plugin_path), "%s\\%s", manager->plugin_directory, plugin_filename);
#else
            /* Convert .dll extension to .so on Unix platforms */
            char* ext = strrchr(plugin_filename, '.');
            if (ext && strcmp(ext, ".dll") == 0) {
                strcpy(ext, ".so");
            }
            
            snprintf(plugin_path, sizeof(plugin_path), "%s/%s", manager->plugin_directory, plugin_filename);
#endif
            
            if (vizero_plugin_manager_load_plugin(manager, plugin_path) == 0) {
                entry->is_loaded = true;
                entry->plugin_instance = manager->plugins[manager->plugin_count - 1];
                loaded_count++;
                printf("[PLUGIN] Loaded always-load: %s\n", entry->name);
            }
        }
    }
    
    return loaded_count;
}

/* Plugin queries */
bool vizero_plugin_manager_is_plugin_loaded(vizero_plugin_manager_t* manager, const char* plugin_name) {
    if (!manager || !plugin_name) {
        return false;
    }
    
    for (size_t i = 0; i < manager->plugin_count; i++) {
        if (manager->plugins[i] && strcmp(manager->plugins[i]->info.name, plugin_name) == 0) {
            return true;
        }
    }
    
    return false;
}

size_t vizero_plugin_manager_get_plugin_count(vizero_plugin_manager_t* manager) {
    return manager ? manager->plugin_count : 0;
}

vizero_plugin_t* vizero_plugin_manager_get_plugin(vizero_plugin_manager_t* manager, size_t index) {
    if (!manager || index >= manager->plugin_count) {
        return NULL;
    }
    return manager->plugins[index];
}

vizero_plugin_t* vizero_plugin_manager_find_plugin(vizero_plugin_manager_t* manager, const char* name) {
    if (!manager || !name) {
        return NULL;
    }
    
    for (size_t i = 0; i < manager->plugin_count; i++) {
        if (manager->plugins[i] && strcmp(manager->plugins[i]->info.name, name) == 0) {
            return manager->plugins[i];
        }
    }
    
    return NULL;
}

/* Event dispatching functions */
void vizero_plugin_manager_on_buffer_open(vizero_plugin_manager_t* manager, vizero_buffer_t* buffer, const char* filename) {
    if (!manager) return;
    
    /* Load plugins on-demand for this file type */
    if (filename && manager->registry) {
        vizero_plugin_manager_load_plugins_for_file(manager, filename);
    }
    
    /* Notify all loaded plugins */
    for (size_t i = 0; i < manager->plugin_count; i++) {
        vizero_plugin_t* plugin = manager->plugins[i];
        if (plugin && plugin->callbacks.on_buffer_open) {
            plugin->callbacks.on_buffer_open(buffer, filename);
        }
    }
}

void vizero_plugin_manager_on_buffer_close(vizero_plugin_manager_t* manager, vizero_buffer_t* buffer) {
    if (!manager) return;
    
    for (size_t i = 0; i < manager->plugin_count; i++) {
        vizero_plugin_t* plugin = manager->plugins[i];
        if (plugin && plugin->callbacks.on_buffer_close) {
            plugin->callbacks.on_buffer_close(buffer);
        }
    }
}

void vizero_plugin_manager_on_text_changed(vizero_plugin_manager_t* manager, vizero_buffer_t* buffer, 
                                          vizero_range_t range, const char* new_text) {
    if (!manager) return;
    
    for (size_t i = 0; i < manager->plugin_count; i++) {
        vizero_plugin_t* plugin = manager->plugins[i];
        if (plugin && plugin->callbacks.on_text_changed) {
            plugin->callbacks.on_text_changed(buffer, range, new_text);
        }
    }
}

void vizero_plugin_manager_on_cursor_moved(vizero_plugin_manager_t* manager, vizero_cursor_t* cursor, 
                                          vizero_position_t old_pos, vizero_position_t new_pos) {
    if (!manager) return;
    
    for (size_t i = 0; i < manager->plugin_count; i++) {
        vizero_plugin_t* plugin = manager->plugins[i];
        if (plugin && plugin->callbacks.on_cursor_moved) {
            plugin->callbacks.on_cursor_moved(cursor, old_pos, new_pos);
        }
    }
}

int vizero_plugin_manager_on_command(vizero_plugin_manager_t* manager, vizero_editor_t* editor, 
                                     const char* command, const char* args) {
    if (!manager) return 0;
    
    for (size_t i = 0; i < manager->plugin_count; i++) {
        vizero_plugin_t* plugin = manager->plugins[i];
        if (plugin && plugin->callbacks.on_command) {
            if (plugin->callbacks.on_command(editor, command, args) != 0) {
                return 1; /* Command handled */
            }
        }
    }
    
    return 0; /* Command not handled */
}

int vizero_plugin_manager_on_key_input(vizero_plugin_manager_t* manager, vizero_editor_t* editor, 
                                       uint32_t key, uint32_t modifiers) {
    if (!manager) return 0;
    
    for (size_t i = 0; i < manager->plugin_count; i++) {
        vizero_plugin_t* plugin = manager->plugins[i];
        if (plugin && plugin->callbacks.on_key_input) {
            if (plugin->callbacks.on_key_input(editor, key, modifiers) != 0) {
                return 1; /* Key handled */
            }
        }
    }
    
    return 0; /* Key not handled */
}

// --- PATCH: Use caller-allocated tokens buffer for plugin syntax highlighting ---
// Helper for max tokens per line
#define VIZERO_SYNTAX_MAX_TOKENS_PER_LINE 32

// Updated API: caller allocates tokens buffer, plugin fills it
int vizero_plugin_manager_highlight_syntax(
    vizero_plugin_manager_t* manager,
    vizero_buffer_t* buffer,
    size_t start_line,
    size_t end_line,
    vizero_syntax_token_t* tokens,
    size_t max_tokens,
    size_t* token_count)
{
    if (!manager || !tokens || !token_count) return 0;
    *token_count = 0;
    
    /* First-come-first-served approach with conflict logging */
    for (size_t i = 0; i < manager->plugin_count; i++) {
        vizero_plugin_t* plugin = manager->plugins[i];
        if (plugin && plugin->info.type == VIZERO_PLUGIN_TYPE_SYNTAX_HIGHLIGHTER && plugin->callbacks.highlight_syntax) {
            int n = plugin->callbacks.highlight_syntax(buffer, start_line, end_line, tokens, max_tokens, token_count);
            if (n > 0) {
                *token_count = (size_t)n;
                
                /* Check if any other plugins also claim this buffer (for debugging) */
                #ifdef DEBUG_PLUGIN_CONFLICTS
                for (size_t j = i + 1; j < manager->plugin_count; j++) {
                    vizero_plugin_t* other = manager->plugins[j];
                    if (other && other->info.type == VIZERO_PLUGIN_TYPE_SYNTAX_HIGHLIGHTER && other->callbacks.highlight_syntax) {
                        vizero_syntax_token_t test_tokens[1];
                        int other_n = other->callbacks.highlight_syntax(buffer, start_line, start_line, test_tokens, 1);
                        if (other_n > 0) {
                            const char* filename = manager->api.get_buffer_filename(buffer);
                            printf("[PLUGIN] Conflict: Both %s and %s claim file %s (using %s)\n",
                                   plugin->info.name, other->info.name,
                                   filename ? filename : "<unnamed>", plugin->info.name);
                        }
                    }
                }
                #endif
                
                return 1; /* First plugin wins */
            }
        }
    }
    
    return 0; /* No plugin handled the buffer */
}

/* API function implementations */
static const char* api_get_buffer_text(vizero_buffer_t* buffer) {
    return vizero_buffer_get_text(buffer);
}

static const char* api_get_buffer_filename(vizero_buffer_t* buffer) {
    return vizero_buffer_get_filename(buffer);
}

static size_t api_get_buffer_line_count(vizero_buffer_t* buffer) {
    if (!buffer) return 0;
    return vizero_buffer_get_line_count(buffer);
}

static const char* api_get_buffer_line(vizero_buffer_t* buffer, size_t line_num) {
    if (!buffer) return NULL;
    
    size_t line_count = vizero_buffer_get_line_count(buffer);
    if (line_num >= line_count) return NULL;
    
    return vizero_buffer_get_line_text(buffer, line_num);
}

static size_t api_get_buffer_line_length(vizero_buffer_t* buffer, size_t line_num) {
    return vizero_buffer_get_line_length(buffer, line_num);
}

static int api_is_buffer_readonly(vizero_buffer_t* buffer) {
    return vizero_buffer_is_readonly(buffer);
}

static void api_set_buffer_readonly(vizero_buffer_t* buffer, int readonly) {
    vizero_buffer_set_readonly(buffer, readonly);
}

/* Initialize the editor API structure - this would be implemented in the core editor */
static void init_editor_api(vizero_editor_api_t* api, vizero_editor_t* editor) {
    /* Buffer operations */
    api->get_buffer_text = api_get_buffer_text;
    api->get_buffer_filename = api_get_buffer_filename;
    api->get_buffer_line_count = api_get_buffer_line_count;
    api->get_buffer_line = api_get_buffer_line;
    api->get_buffer_line_length = api_get_buffer_line_length;
    api->is_buffer_readonly = api_is_buffer_readonly;
    api->set_buffer_readonly = api_set_buffer_readonly;
    
    /* These would be implemented by the actual editor core */
    api->insert_text = NULL;               /* vizero_buffer_insert_text_impl */
    api->delete_text = NULL;               /* vizero_buffer_delete_text_impl */
    api->get_cursor_position = NULL;       /* vizero_cursor_get_position_impl */
    api->set_cursor_position = NULL;       /* vizero_cursor_set_position_impl */
    api->get_current_buffer = NULL;        /* vizero_editor_get_current_buffer_impl */
    api->get_current_cursor = NULL;        /* vizero_editor_get_current_cursor_impl */
    api->execute_command = NULL;           /* vizero_editor_execute_command_impl */
    api->set_status_message = NULL;        /* vizero_editor_set_status_message_impl */
    api->open_file = NULL;                 /* vizero_editor_open_file_impl */
    api->save_file = NULL;                 /* vizero_editor_save_file_impl */
    api->add_syntax_tokens = NULL;         /* vizero_editor_add_syntax_tokens_impl */
    api->clear_syntax_tokens = NULL;       /* vizero_editor_clear_syntax_tokens_impl */
    
    /* Store reference to editor for implementations */
    (void)editor;
}

/* LSP completion function */
int vizero_plugin_manager_lsp_completion(
    vizero_plugin_manager_t* manager,
    vizero_buffer_t* buffer,
    vizero_position_t position,
    vizero_completion_list_t** result) 
{
    if (!manager || !buffer || !result) {
        return -1;
    }
    
    *result = NULL;
    
    printf("[DEBUG] Searching %zu plugins for LSP completion support\n", manager->plugin_count);
    
    /* Find plugins that support LSP completion */
    for (size_t i = 0; i < manager->plugin_count; i++) {
        vizero_plugin_t* plugin = manager->plugins[i];
        printf("[DEBUG] Plugin %zu: name=%s, lsp_completion=%p\n", 
               i, plugin ? (plugin->info.name ? plugin->info.name : "unknown") : "null",
               plugin ? (void*)plugin->callbacks.lsp_completion : NULL);
        
        if (plugin && plugin->callbacks.lsp_completion) {
            printf("[DEBUG] Trying LSP completion with plugin: %s\n", 
                   plugin->info.name ? plugin->info.name : "unknown");
            
            int ret = plugin->callbacks.lsp_completion(buffer, position, result);
            if (ret == 0 && *result) {
                printf("[DEBUG] LSP completion successful, got %zu items\n", (*result)->item_count);
                return 0;
            } else {
                printf("[DEBUG] LSP completion failed: ret=%d, result=%p\n", ret, (void*)*result);
            }
        }
    }
    
    printf("[DEBUG] No LSP completion plugin found or no results\n");
    return -1;
}

/* LSP hover function */
int vizero_plugin_manager_lsp_hover(
    vizero_plugin_manager_t* manager,
    vizero_buffer_t* buffer,
    vizero_position_t position,
    char** hover_text) 
{
    if (!manager || !buffer || !hover_text) {
        return -1;
    }
    
    *hover_text = NULL;
    
    /* Find plugins that support LSP hover */
    for (size_t i = 0; i < manager->plugin_count; i++) {
        vizero_plugin_t* plugin = manager->plugins[i];
        if (plugin && plugin->callbacks.lsp_hover) {
            int ret = plugin->callbacks.lsp_hover(buffer, position, hover_text);
            if (ret == 0 && *hover_text) {
                return 0;
            }
        }
    }
    
    return -1;
}

/* LSP goto definition function */
int vizero_plugin_manager_lsp_goto_definition(
    vizero_plugin_manager_t* manager,
    vizero_buffer_t* buffer,
    vizero_position_t position,
    vizero_location_t** locations,
    size_t* location_count) 
{
    if (!manager || !buffer || !locations || !location_count) {
        return -1;
    }
    
    *locations = NULL;
    *location_count = 0;
    
    /* Find plugins that support LSP goto definition */
    for (size_t i = 0; i < manager->plugin_count; i++) {
        vizero_plugin_t* plugin = manager->plugins[i];
        if (plugin && plugin->callbacks.lsp_goto_definition) {
            int ret = plugin->callbacks.lsp_goto_definition(buffer, position, locations, location_count);
            if (ret == 0 && *locations && *location_count > 0) {
                return 0;
            }
        }
    }
    
    return -1;
}

/* Process LSP messages in background (non-blocking) */
void vizero_plugin_manager_process_lsp_messages(vizero_plugin_manager_t* manager) {
    if (!manager) {
        return;
    }
    
    /* Process LSP messages for all plugins that have LSP clients */
    for (size_t i = 0; i < manager->plugin_count; i++) {
        vizero_plugin_t* plugin = manager->plugins[i];
        if (plugin && plugin->callbacks.lsp_completion) {
            /* This is an LSP plugin - call its message processing function */
            /* For clangd plugin, we'll call the exported function directly */
            if (plugin->dll_handle) {
                typedef int (*process_lsp_messages_func_t)(void);
                process_lsp_messages_func_t process_func = 
                    (process_lsp_messages_func_t)PLUGIN_GET_PROC(plugin->dll_handle, "clangd_process_lsp_messages");
                if (process_func) {
                    process_func();
                }
            }
        }
    }
}

/* Check for pending LSP completion results */
int vizero_plugin_manager_check_completion_results(
    vizero_plugin_manager_t* manager,
    vizero_completion_list_t** result) 
{
    if (!manager || !result) {
        return -1;
    }
    
    *result = NULL;
    
    /* Check all LSP plugins for completion results */
    for (size_t i = 0; i < manager->plugin_count; i++) {
        vizero_plugin_t* plugin = manager->plugins[i];
        if (plugin && plugin->callbacks.lsp_completion) {
            /* Try to get completion results from this plugin */
            if (plugin->dll_handle) {
                typedef int (*get_completion_results_func_t)(vizero_completion_list_t**);
                get_completion_results_func_t get_results_func = 
                    (get_completion_results_func_t)PLUGIN_GET_PROC(plugin->dll_handle, "clangd_get_completion_results");
                if (get_results_func) {
                    int ret = get_results_func(result);
                    if (ret == 0 && *result) {
                        return 0; /* Found results */
                    }
                }
            }
        }
    }
    
    return -1; /* No results available */
}

/* Plugin command execution */
int vizero_plugin_manager_execute_command(
    vizero_plugin_manager_t* manager,
    vizero_editor_t* editor,
    const char* command,
    const char* args)
{
    if (!manager || !editor || !command) {
        return -1;
    }
    
    printf("[PLUGIN] Searching for command: %s\n", command);
    
    /* Search all loaded plugins for this command */
    for (size_t i = 0; i < manager->plugin_count; i++) {
        vizero_plugin_t* plugin = manager->plugins[i];
        if (!plugin || !plugin->callbacks.commands || plugin->callbacks.command_count == 0) {
            continue;
        }
        
        /* Check each command registered by this plugin */
        for (size_t j = 0; j < plugin->callbacks.command_count; j++) {
            vizero_plugin_command_t* cmd = &plugin->callbacks.commands[j];
            if (strcmp(cmd->command, command) == 0) {
                printf("[PLUGIN] Found command '%s' in plugin '%s'\n", command, plugin->info.name);
                
                /* Execute the command handler */
                if (cmd->handler) {
                    return cmd->handler(editor, args);
                } else {
                    printf("[PLUGIN] Warning: Command '%s' has no handler\n", command);
                    return -1;
                }
            }
        }
    }
    
    printf("[PLUGIN] Command '%s' not found in any loaded plugin\n", command);
    return -1; /* Command not found */
}

/* Get list of all registered commands for help/completion */
int vizero_plugin_manager_get_commands(
    vizero_plugin_manager_t* manager,
    vizero_plugin_command_t** commands,
    size_t* command_count)
{
    if (!manager || !commands || !command_count) {
        return -1;
    }
    
    /* Count total commands */
    size_t total_commands = 0;
    for (size_t i = 0; i < manager->plugin_count; i++) {
        vizero_plugin_t* plugin = manager->plugins[i];
        if (plugin && plugin->callbacks.commands) {
            total_commands += plugin->callbacks.command_count;
        }
    }
    
    if (total_commands == 0) {
        *commands = NULL;
        *command_count = 0;
        return 0;
    }
    
    /* Allocate array for all commands */
    vizero_plugin_command_t* cmd_array = (vizero_plugin_command_t*)malloc(total_commands * sizeof(vizero_plugin_command_t));
    if (!cmd_array) {
        return -1;
    }
    
    /* Copy all commands */
    size_t cmd_index = 0;
    for (size_t i = 0; i < manager->plugin_count; i++) {
        vizero_plugin_t* plugin = manager->plugins[i];
        if (plugin && plugin->callbacks.commands) {
            for (size_t j = 0; j < plugin->callbacks.command_count; j++) {
                cmd_array[cmd_index++] = plugin->callbacks.commands[j];
            }
        }
    }
    
    *commands = cmd_array;
    *command_count = total_commands;
    return 0;
}

/* Initialize SDL renderer for plugin rendering */
int vizero_plugin_manager_init_renderer(vizero_plugin_manager_t* manager, SDL_Window* window) {
    if (!manager || !window) {
        return -1;
    }
    
    /* Create SDL renderer from the window */
    manager->sdl_renderer = SDL_CreateRenderer(window, -1, SDL_RENDERER_ACCELERATED | SDL_RENDERER_TARGETTEXTURE);
    if (!manager->sdl_renderer) {
        printf("[PLUGIN] Failed to create SDL renderer: %s\n", SDL_GetError());
        return -1;
    }
    
    /* Initialize texture dimensions */
    manager->texture_width = 0;
    manager->texture_height = 0;
    manager->plugin_texture = NULL;
    
    printf("[PLUGIN] SDL renderer initialized for plugin rendering\n");
    return 0;
}

/* Check if any plugin wants full window control */
int vizero_plugin_manager_wants_full_window(vizero_plugin_manager_t* manager) {
    if (!manager) {
        return 0;
    }
    
    for (size_t i = 0; i < manager->plugin_count; i++) {
        vizero_plugin_t* plugin = manager->plugins[i];
        if (plugin && plugin->callbacks.wants_full_window) {
            if (plugin->callbacks.wants_full_window(manager->editor)) {
                return 1;
            }
        }
    }
    
    return 0;
}

/* Render plugin UI to texture and return it */
SDL_Texture* vizero_plugin_manager_render_plugin_ui(vizero_plugin_manager_t* manager, int width, int height) {
    if (!manager || !manager->sdl_renderer) {
        return NULL;
    }
    
    /* Create or recreate texture if size changed */
    if (!manager->plugin_texture || manager->texture_width != width || manager->texture_height != height) {
        if (manager->plugin_texture) {
            SDL_DestroyTexture(manager->plugin_texture);
        }
        
        manager->plugin_texture = SDL_CreateTexture(manager->sdl_renderer, 
                                                   SDL_PIXELFORMAT_RGBA8888,
                                                   SDL_TEXTUREACCESS_TARGET,
                                                   width, height);
        if (!manager->plugin_texture) {
            printf("[PLUGIN] Failed to create plugin texture: %s\n", SDL_GetError());
            return NULL;
        }
        
        manager->texture_width = width;
        manager->texture_height = height;
    }
    
    /* Find plugin that wants to render */
    for (size_t i = 0; i < manager->plugin_count; i++) {
        vizero_plugin_t* plugin = manager->plugins[i];
        /* OLD SDL RENDERING - DISABLED
        if (plugin && plugin->callbacks.render_to_texture && plugin->callbacks.wants_full_window) {
            if (plugin->callbacks.wants_full_window(manager->editor)) {
                // Let plugin render to the texture
                if (plugin->callbacks.render_to_texture(manager->editor, manager->sdl_renderer, 
                                                       manager->plugin_texture, width, height)) {
                    return manager->plugin_texture;
                }
            }
        }
        OLD SDL RENDERING - DISABLED */
    }
    
    return NULL;
}

/*  OLD SDL RENDERING FUNCTION - DISABLED
int vizero_plugin_manager_render_and_present(vizero_plugin_manager_t* manager, int width, int height) {
    if (!manager || !manager->sdl_renderer) {
        return 0;
    }
    
    static int debug_count = 0;
    if (debug_count < 3) {
        printf("[PLUGIN] render_and_present called: %dx%d\n", width, height);
        debug_count++;
    }
    
    for (size_t i = 0; i < manager->plugin_count; i++) {
        vizero_plugin_t* plugin = manager->plugins[i];
        if (plugin && plugin->callbacks.render_to_texture && plugin->callbacks.wants_full_window) {
            if (plugin->callbacks.wants_full_window(manager->editor)) {
                if (!manager->plugin_texture || manager->texture_width != width || manager->texture_height != height) {
                    if (manager->plugin_texture) {
                        SDL_DestroyTexture(manager->plugin_texture);
                    }
                    
                    manager->plugin_texture = SDL_CreateTexture(manager->sdl_renderer, SDL_PIXELFORMAT_RGBA8888,
                                                               SDL_TEXTUREACCESS_TARGET, width, height);
                    if (!manager->plugin_texture) {
                        return 0;
                    }
                    
                    manager->texture_width = width;
                    manager->texture_height = height;
                }
                
                if (plugin->callbacks.render_to_texture(manager->editor, manager->sdl_renderer, 
                                                       manager->plugin_texture, width, height)) {
                    SDL_SetRenderTarget(manager->sdl_renderer, NULL);
                    SDL_SetRenderDrawColor(manager->sdl_renderer, 0, 0, 0, 255);
                    SDL_RenderClear(manager->sdl_renderer);
                    
                    SDL_RenderCopy(manager->sdl_renderer, manager->plugin_texture, NULL, NULL);
                    
                    SDL_RenderPresent(manager->sdl_renderer);
                    
                    return 1;
                }
            }
        }
    }
    
    return 0;
}
OLD SDL RENDERING FUNCTION - DISABLED */

/* Render plugin UI using OpenGL renderer */
int vizero_plugin_manager_render_full_window(vizero_plugin_manager_t* manager, vizero_renderer_t* renderer, int width, int height) {
    if (!manager || !renderer) {
        return 0;
    }
    
    /* Find plugin that wants to render */
    for (size_t i = 0; i < manager->plugin_count; i++) {
        vizero_plugin_t* plugin = manager->plugins[i];
        if (plugin && plugin->callbacks.render_full_window && plugin->callbacks.wants_full_window) {
            if (plugin->callbacks.wants_full_window(manager->editor)) {
                /* Let plugin render using OpenGL */
                return plugin->callbacks.render_full_window(manager->editor, renderer, width, height);
            }
        }
    }
    
    return 0;
}

/* OLD SDL CLEANUP FUNCTION - DISABLED
void vizero_plugin_manager_cleanup_renderer(vizero_plugin_manager_t* manager) {
    if (!manager) {
        return;
    }
    
    if (manager->plugin_texture) {
        SDL_DestroyTexture(manager->plugin_texture);
        manager->plugin_texture = NULL;
    }
    
    if (manager->sdl_renderer) {
        SDL_DestroyRenderer(manager->sdl_renderer);
        manager->sdl_renderer = NULL;
    }
    
    manager->texture_width = 0;
    manager->texture_height = 0;
}
OLD SDL CLEANUP FUNCTION - DISABLED */


