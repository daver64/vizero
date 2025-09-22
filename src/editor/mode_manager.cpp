/* Stub implementations */
#include "vizero/mode_manager.h"
#include <stdlib.h>

struct vizero_mode_manager_t { 
    vizero_editor_state_t* state;
};

vizero_mode_manager_t* vizero_mode_manager_create(vizero_editor_state_t* state) {
    vizero_mode_manager_t* manager = (vizero_mode_manager_t*)calloc(1, sizeof(vizero_mode_manager_t));
    if (manager) manager->state = state;
    return manager;
}

void vizero_mode_manager_destroy(vizero_mode_manager_t* manager) {
    free(manager);
}

void vizero_mode_manager_enter_normal_mode(vizero_mode_manager_t* manager) {
    (void)manager;
}

void vizero_mode_manager_enter_insert_mode(vizero_mode_manager_t* manager) {
    (void)manager;
}

void vizero_mode_manager_enter_visual_mode(vizero_mode_manager_t* manager) {
    (void)manager;
}

void vizero_mode_manager_enter_command_mode(vizero_mode_manager_t* manager) {
    (void)manager;
}

int vizero_mode_manager_handle_key(vizero_mode_manager_t* manager, uint32_t key, uint32_t modifiers) {
    (void)manager; (void)key; (void)modifiers; return 0;
}