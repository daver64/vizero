#ifndef VIZERO_MODE_MANAGER_H
#define VIZERO_MODE_MANAGER_H

#ifdef __cplusplus
extern "C" {
#endif

#include <stdint.h>
#include "editor_state.h"

/* Forward declarations */
typedef struct vizero_mode_manager_t vizero_mode_manager_t;

/* Mode manager creation and destruction */
vizero_mode_manager_t* vizero_mode_manager_create(vizero_editor_state_t* state);
void vizero_mode_manager_destroy(vizero_mode_manager_t* manager);

/* Mode transitions */
void vizero_mode_manager_enter_normal_mode(vizero_mode_manager_t* manager);
void vizero_mode_manager_enter_insert_mode(vizero_mode_manager_t* manager);
void vizero_mode_manager_enter_visual_mode(vizero_mode_manager_t* manager);
void vizero_mode_manager_enter_command_mode(vizero_mode_manager_t* manager);

/* Key handling */
int vizero_mode_manager_handle_key(vizero_mode_manager_t* manager, uint32_t key, uint32_t modifiers);

#ifdef __cplusplus
}
#endif

#endif /* VIZERO_MODE_MANAGER_H */