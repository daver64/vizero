#ifndef VIZERO_INPUT_MANAGER_H
#define VIZERO_INPUT_MANAGER_H

#ifdef __cplusplus
extern "C" {
#endif

#include <stdint.h>

/* Forward declarations */
typedef struct vizero_input_manager_t vizero_input_manager_t;
typedef struct vizero_application_t vizero_application_t;

/* Key modifier flags */
#define VIZERO_MOD_SHIFT   0x01
#define VIZERO_MOD_CTRL    0x02
#define VIZERO_MOD_ALT     0x04
#define VIZERO_MOD_GUI     0x08

/* Input manager creation and destruction */
vizero_input_manager_t* vizero_input_manager_create(vizero_application_t* app);
void vizero_input_manager_destroy(vizero_input_manager_t* input);

/* Event processing */
void vizero_input_manager_process_events(vizero_input_manager_t* input);

/* Key state queries */
int vizero_input_manager_is_key_pressed(vizero_input_manager_t* input, uint32_t key);
int vizero_input_manager_was_key_pressed(vizero_input_manager_t* input, uint32_t key);
uint32_t vizero_input_manager_get_modifiers(vizero_input_manager_t* input);

/* Mouse state queries */
void vizero_input_manager_get_mouse_position(vizero_input_manager_t* input, int* x, int* y);
int vizero_input_manager_is_mouse_button_pressed(vizero_input_manager_t* input, int button);

#ifdef __cplusplus
}
#endif

#endif /* VIZERO_INPUT_MANAGER_H */