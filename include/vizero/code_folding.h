#ifndef VIZERO_CODE_FOLDING_H
#define VIZERO_CODE_FOLDING_H

#include <stddef.h>
#include <stdbool.h>

#ifdef __cplusplus
extern "C" {
#endif

/* Forward declarations */
typedef struct vizero_buffer_t vizero_buffer_t;
typedef struct vizero_editor_state_t vizero_editor_state_t;

/* Buffer change notification callbacks for code folding */
typedef struct vizero_code_folding_t vizero_code_folding_t;

typedef struct {
    void (*on_lines_inserted)(vizero_code_folding_t* folding, size_t line, size_t count);
    void (*on_lines_deleted)(vizero_code_folding_t* folding, size_t line, size_t count);
    void (*on_text_changed)(vizero_code_folding_t* folding, size_t line, size_t start_col, size_t end_col);
    void (*on_buffer_cleared)(vizero_code_folding_t* folding);
} vizero_folding_callbacks_t;

/* Fold types */
typedef enum {
    VIZERO_FOLD_MANUAL,         /* Manually created fold */
    VIZERO_FOLD_FUNCTION,       /* Function/method fold */
    VIZERO_FOLD_CLASS,          /* Class/struct fold */
    VIZERO_FOLD_BLOCK,          /* Code block fold (braces) */
    VIZERO_FOLD_COMMENT,        /* Comment block fold */
    VIZERO_FOLD_REGION,         /* Region fold (#region/#endregion) */
    VIZERO_FOLD_IMPORTS         /* Import/include statements */
} vizero_fold_type_t;

/* Code fold structure */
typedef struct vizero_code_fold_t {
    size_t start_line;          /* Starting line number */
    size_t end_line;            /* Ending line number */
    vizero_fold_type_t type;    /* Type of fold */
    bool is_folded;             /* Whether fold is collapsed */
    bool is_valid;              /* Whether fold is still valid */
    char label[128];            /* Display label for folded region */
    int level;                  /* Nesting level */
} vizero_code_fold_t;

/* Code folding manager */
typedef struct vizero_code_folding_t {
    vizero_code_fold_t* folds;  /* Array of folds */
    size_t fold_count;          /* Number of folds */
    size_t fold_capacity;       /* Allocated capacity */
    
    vizero_buffer_t* buffer;    /* Associated buffer */
    bool auto_fold_enabled;     /* Automatic folding enabled */
    bool show_fold_markers;     /* Show fold markers in gutter */
    
    /* Folding preferences */
    bool fold_functions;        /* Auto-fold functions */
    bool fold_classes;          /* Auto-fold classes */
    bool fold_comments;         /* Auto-fold large comment blocks */
    bool fold_imports;          /* Auto-fold import sections */
    size_t min_fold_lines;      /* Minimum lines required for folding */
} vizero_code_folding_t;

/* Code folding lifecycle */
vizero_code_folding_t* vizero_code_folding_create(vizero_buffer_t* buffer);
void vizero_code_folding_destroy(vizero_code_folding_t* folding);

/* Fold management */
int vizero_code_folding_add_fold(vizero_code_folding_t* folding, size_t start_line, 
                                size_t end_line, vizero_fold_type_t type, const char* label);
int vizero_code_folding_remove_fold(vizero_code_folding_t* folding, size_t fold_index);
void vizero_code_folding_clear_folds(vizero_code_folding_t* folding);

/* Fold operations */
int vizero_code_folding_fold_range(vizero_code_folding_t* folding, size_t start_line, size_t end_line);
int vizero_code_folding_unfold_range(vizero_code_folding_t* folding, size_t start_line, size_t end_line);
int vizero_code_folding_toggle_fold(vizero_code_folding_t* folding, size_t line);
int vizero_code_folding_fold_all(vizero_code_folding_t* folding);
int vizero_code_folding_unfold_all(vizero_code_folding_t* folding);

/* Automatic folding */
void vizero_code_folding_analyze_buffer(vizero_code_folding_t* folding);
void vizero_code_folding_update_after_edit(vizero_code_folding_t* folding, size_t line, int lines_added);

/* Fold queries */
const vizero_code_fold_t* vizero_code_folding_get_fold_at_line(const vizero_code_folding_t* folding, size_t line);
bool vizero_code_folding_is_line_folded(const vizero_code_folding_t* folding, size_t line);
bool vizero_code_folding_is_line_visible(const vizero_code_folding_t* folding, size_t line);
size_t vizero_code_folding_get_fold_count(const vizero_code_folding_t* folding);
const vizero_code_fold_t* vizero_code_folding_get_fold_at_index(const vizero_code_folding_t* folding, size_t index);

/* Line number mapping for folded content */
size_t vizero_code_folding_visual_to_logical_line(const vizero_code_folding_t* folding, size_t visual_line);
size_t vizero_code_folding_logical_to_visual_line(const vizero_code_folding_t* folding, size_t logical_line);

/* Configuration */
void vizero_code_folding_set_auto_fold(vizero_code_folding_t* folding, bool enabled);
void vizero_code_folding_set_show_markers(vizero_code_folding_t* folding, bool show);
void vizero_code_folding_set_min_fold_lines(vizero_code_folding_t* folding, size_t min_lines);

/* Language-specific folding */
void vizero_code_folding_set_fold_functions(vizero_code_folding_t* folding, bool enabled);
void vizero_code_folding_set_fold_classes(vizero_code_folding_t* folding, bool enabled);
void vizero_code_folding_set_fold_comments(vizero_code_folding_t* folding, bool enabled);
void vizero_code_folding_set_fold_imports(vizero_code_folding_t* folding, bool enabled);

/* Buffer change notification integration */
void vizero_code_folding_register_with_buffer(vizero_code_folding_t* folding, vizero_buffer_t* buffer);
void vizero_code_folding_unregister_from_buffer(vizero_code_folding_t* folding, vizero_buffer_t* buffer);

/* Buffer change notification handlers (called by buffer) */
void vizero_code_folding_on_lines_inserted(vizero_code_folding_t* folding, size_t line, size_t count);
void vizero_code_folding_on_lines_deleted(vizero_code_folding_t* folding, size_t line, size_t count);
void vizero_code_folding_on_text_changed(vizero_code_folding_t* folding, size_t line, size_t start_col, size_t end_col);
void vizero_code_folding_on_buffer_cleared(vizero_code_folding_t* folding);

#ifdef __cplusplus
}
#endif

#endif /* VIZERO_CODE_FOLDING_H */