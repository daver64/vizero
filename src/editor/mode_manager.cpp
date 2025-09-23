/* Complete mode manager implementation */
#include "vizero/mode_manager.h"
#include "vizero/editor_state.h"
#include <stdlib.h>
#include <stdio.h>

/* Forward declarations for helper functions */
static int handle_normal_mode_key(vizero_mode_manager_t* manager, uint32_t key, uint32_t modifiers);
static int handle_insert_mode_key(vizero_mode_manager_t* manager, uint32_t key, uint32_t modifiers);
static int handle_visual_mode_key(vizero_mode_manager_t* manager, uint32_t key, uint32_t modifiers);
static int handle_command_mode_key(vizero_mode_manager_t* manager, uint32_t key, uint32_t modifiers);

struct vizero_mode_manager_t { 
    vizero_editor_state_t* state;
    vizero_editor_mode_t current_mode;
    char command_buffer[256];       /* For command mode input */
    size_t command_length;
};

vizero_mode_manager_t* vizero_mode_manager_create(vizero_editor_state_t* state) {
    vizero_mode_manager_t* manager = (vizero_mode_manager_t*)calloc(1, sizeof(vizero_mode_manager_t));
    if (manager) {
        manager->state = state;
        manager->current_mode = VIZERO_MODE_NORMAL;  /* Start in normal mode */
        manager->command_length = 0;
    }
    return manager;
}

void vizero_mode_manager_destroy(vizero_mode_manager_t* manager) {
    free(manager);
}

void vizero_mode_manager_enter_normal_mode(vizero_mode_manager_t* manager) {
    if (!manager) return;
    
    manager->current_mode = VIZERO_MODE_NORMAL;
    manager->command_length = 0;  /* Clear command buffer */
    
    /* Update editor state mode */
    vizero_editor_set_mode(manager->state, VIZERO_MODE_NORMAL);
    vizero_editor_set_status_message(manager->state, "-- NORMAL --");
}

void vizero_mode_manager_enter_insert_mode(vizero_mode_manager_t* manager) {
    if (!manager) return;
    
    manager->current_mode = VIZERO_MODE_INSERT;
    
    /* Update editor state mode */
    vizero_editor_set_mode(manager->state, VIZERO_MODE_INSERT);
    vizero_editor_set_status_message(manager->state, "-- INSERT --");
}

void vizero_mode_manager_enter_visual_mode(vizero_mode_manager_t* manager) {
    if (!manager) return;
    
    manager->current_mode = VIZERO_MODE_VISUAL;
    
    /* Update editor state mode */
    vizero_editor_set_mode(manager->state, VIZERO_MODE_VISUAL);
    vizero_editor_set_status_message(manager->state, "-- VISUAL --");
}

void vizero_mode_manager_enter_command_mode(vizero_mode_manager_t* manager) {
    if (!manager) return;
    
    manager->current_mode = VIZERO_MODE_COMMAND;
    manager->command_length = 0;
    manager->command_buffer[0] = '\0';
    
    /* Update editor state mode */
    vizero_editor_set_mode(manager->state, VIZERO_MODE_COMMAND);
    vizero_editor_set_status_message(manager->state, ":");
}

int vizero_mode_manager_handle_key(vizero_mode_manager_t* manager, uint32_t key, uint32_t modifiers) {
    if (!manager) return 0;
    
    /* Handle Escape key - return to normal mode from any mode */
    if (key == 27) { /* Escape */
        if (manager->current_mode != VIZERO_MODE_NORMAL) {
            vizero_mode_manager_enter_normal_mode(manager);
            return 1; /* Key handled */
        }
        return 0;
    }
    
    switch (manager->current_mode) {
        case VIZERO_MODE_NORMAL:
            return handle_normal_mode_key(manager, key, modifiers);
            
        case VIZERO_MODE_INSERT:
            return handle_insert_mode_key(manager, key, modifiers);
            
        case VIZERO_MODE_VISUAL:
            return handle_visual_mode_key(manager, key, modifiers);
            
        case VIZERO_MODE_COMMAND:
            return handle_command_mode_key(manager, key, modifiers);
            
        default:
            return 0;
    }
}

/* Helper functions for mode-specific key handling */
static int handle_normal_mode_key(vizero_mode_manager_t* manager, uint32_t key, uint32_t modifiers) {
    (void)modifiers; /* Most vi commands don't use modifiers */
    
    switch (key) {
        case 'i': /* Insert mode */
            vizero_mode_manager_enter_insert_mode(manager);
            return 1;
            
        case 'v': /* Visual mode */
            vizero_mode_manager_enter_visual_mode(manager);
            return 1;
            
        case ':': /* Command mode */
            vizero_mode_manager_enter_command_mode(manager);
            return 1;
            
        /* Basic movement keys */
        case 'h': /* Left */
        case 'j': /* Down */
        case 'k': /* Up */
        case 'l': /* Right */
            /* These would be handled by the cursor system */
            return 0; /* Let the main editor handle movement */
            
        /* Basic editing */
        case 'x': /* Delete character */
        case 'd': /* Delete (would need to handle dd, dw, etc.) */
        case 'y': /* Yank */
        case 'p': /* Paste */
            /* These would be handled by the main editor */
            return 0;
            
        default:
            return 0; /* Key not handled */
    }
}

static int handle_insert_mode_key(vizero_mode_manager_t* manager, uint32_t key, uint32_t modifiers) {
    (void)manager; (void)modifiers;
    
    /* In insert mode, most keys should be passed to the editor for text insertion */
    /* Only handle special keys here */
    
    switch (key) {
        case 13: /* Enter */
        case 8:  /* Backspace */
        case 127: /* Delete */
            return 0; /* Let editor handle these */
            
        default:
            /* All printable characters go to the editor */
            return 0;
    }
}

static int handle_visual_mode_key(vizero_mode_manager_t* manager, uint32_t key, uint32_t modifiers) {
    (void)modifiers;
    
    switch (key) {
        case 'd': /* Delete selection */
        case 'y': /* Yank selection */
        case 'c': /* Change selection */
            /* These would be handled by the main editor */
            vizero_mode_manager_enter_normal_mode(manager); /* Return to normal after operation */
            return 0; /* Let editor handle the operation */
            
        /* Movement keys extend selection */
        case 'h':
        case 'j':
        case 'k':
        case 'l':
            return 0; /* Let editor handle movement and selection extension */
            
        default:
            return 0;
    }
}

static int handle_command_mode_key(vizero_mode_manager_t* manager, uint32_t key, uint32_t modifiers) {
    (void)modifiers;
    
    switch (key) {
        case 13: /* Enter - execute command */
            if (manager->command_length > 0) {
                manager->command_buffer[manager->command_length] = '\0';
                /* Execute the command - this would be handled by the main editor */
                vizero_editor_execute_command(manager->state, manager->command_buffer);
            }
            vizero_mode_manager_enter_normal_mode(manager);
            return 1;
            
        case 8: /* Backspace */
            if (manager->command_length > 0) {
                manager->command_length--;
                manager->command_buffer[manager->command_length] = '\0';
                
                /* Update status message */
                char status[258];
                snprintf(status, sizeof(status), ":%s", manager->command_buffer);
                vizero_editor_set_status_message(manager->state, status);
            }
            return 1;
            
        default:
            /* Add printable characters to command buffer */
            if (key >= 32 && key < 127 && manager->command_length < sizeof(manager->command_buffer) - 1) {
                manager->command_buffer[manager->command_length] = (char)key;
                manager->command_length++;
                manager->command_buffer[manager->command_length] = '\0';
                
                /* Update status message */
                char status[258];
                snprintf(status, sizeof(status), ":%s", manager->command_buffer);
                vizero_editor_set_status_message(manager->state, status);
                return 1;
            }
            return 0;
    }
}