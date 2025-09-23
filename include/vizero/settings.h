#ifndef VIZERO_SETTINGS_H
#define VIZERO_SETTINGS_H

#ifdef __cplusplus
extern "C" {
#endif

#include <stddef.h>

/* Forward declarations */
typedef struct vizero_settings_t vizero_settings_t;

/* Settings creation and destruction */
vizero_settings_t* vizero_settings_create(void);
void vizero_settings_destroy(vizero_settings_t* settings);

/* Boolean settings */
int vizero_settings_get_bool(vizero_settings_t* settings, const char* key);
void vizero_settings_set_bool(vizero_settings_t* settings, const char* key, int value);

/* Integer settings */
int vizero_settings_get_int(vizero_settings_t* settings, const char* key);
void vizero_settings_set_int(vizero_settings_t* settings, const char* key, int value);

/* String settings */
const char* vizero_settings_get_string(vizero_settings_t* settings, const char* key);
void vizero_settings_set_string(vizero_settings_t* settings, const char* key, const char* value);

/* Settings persistence */
const char* vizero_settings_get_config_directory(void);
const char* vizero_settings_get_config_file_path(void);
int vizero_settings_load_from_file(vizero_settings_t* settings);
int vizero_settings_save_to_file(vizero_settings_t* settings);

/* Settings enumeration */
char* vizero_settings_get_all_as_string(vizero_settings_t* settings);

/* Predefined settings */
#define VIZERO_SETTING_LINE_NUMBERS "line_numbers"
#define VIZERO_SETTING_SHOW_LINE_NUMBERS "show_line_numbers"
#define VIZERO_SETTING_TAB_SIZE "tab_size"
#define VIZERO_SETTING_WORD_WRAP "word_wrap"
#define VIZERO_SETTING_SYNTAX_HIGHLIGHTING "syntax_highlighting"

/* Compiler settings */
#define VIZERO_SETTING_ASSEMBLER "assembler"          /* "fasm" or "nasm" */
#define VIZERO_SETTING_C_COMPILER "c_compiler"        /* "gcc" or "msvc" */
#define VIZERO_SETTING_CPP_COMPILER "cpp_compiler"    /* "g++" or "msvc" */
#define VIZERO_SETTING_ASSEMBLER_PATH "assembler_path"
#define VIZERO_SETTING_C_COMPILER_PATH "c_compiler_path"
#define VIZERO_SETTING_CPP_COMPILER_PATH "cpp_compiler_path"

#ifdef __cplusplus
}
#endif

#endif /* VIZERO_SETTINGS_H */