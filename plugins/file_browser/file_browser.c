/* Placeholder */
#include "vizero/plugin_interface.h"

VIZERO_PLUGIN_DEFINE_INFO(
    "File Browser",
    "1.0.0",
    "Vizero Team",
    "File browser plugin",
    VIZERO_PLUGIN_TYPE_UI_EXTENSION
)

VIZERO_PLUGIN_API int vizero_plugin_init(vizero_plugin_t* plugin, vizero_editor_t* editor, const vizero_editor_api_t* api) {
    (void)plugin; (void)editor; (void)api;
    return 0;
}

VIZERO_PLUGIN_API void vizero_plugin_cleanup(vizero_plugin_t* plugin) {
    (void)plugin;
}
