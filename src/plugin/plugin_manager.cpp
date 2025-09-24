#include "vizero/plugin_manager.h"
#include "vizero/plugin_interface.h"
#include "vizero/buffer.h"
#include <stdlib.h>
#include <string.h>
#include <stdio.h>

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
    
    /* Initialize the editor API */
    init_editor_api(&manager->api, editor);
    
    return manager;
}

void vizero_plugin_manager_destroy(vizero_plugin_manager_t* manager) {
    if (!manager) {
        return;
    }
    
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
    for (size_t i = 0; i < manager->plugin_count; i++) {
        vizero_plugin_t* plugin = manager->plugins[i];
        if (plugin && plugin->info.type == VIZERO_PLUGIN_TYPE_SYNTAX_HIGHLIGHTER && plugin->callbacks.highlight_syntax) {
            int n = plugin->callbacks.highlight_syntax(buffer, start_line, end_line, tokens, max_tokens);
            if (n > 0) {
                *token_count = (size_t)n;
                return 1;
            }
        }
    }
    return 0;
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

/* Initialize the editor API structure - this would be implemented in the core editor */
static void init_editor_api(vizero_editor_api_t* api, vizero_editor_t* editor) {
    /* Buffer operations */
    api->get_buffer_text = api_get_buffer_text;
    api->get_buffer_filename = api_get_buffer_filename;
    api->get_buffer_line_count = api_get_buffer_line_count;
    api->get_buffer_line = api_get_buffer_line;
    api->get_buffer_line_length = api_get_buffer_line_length;
    
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