/* Simple code folding implementation */
#include "vizero/code_folding.h"
#include "vizero/buffer.h"
#include "vizero/memory_utils.h"
#include <stdlib.h>
#include <string.h>
#include <stdio.h>
#include <stdbool.h>

vizero_code_folding_t* vizero_code_folding_create(vizero_buffer_t* buffer) {
    if (!buffer) return NULL;
    
    vizero_code_folding_t* folding = (vizero_code_folding_t*)vizero_safe_malloc(sizeof(vizero_code_folding_t));
    if (!folding) return NULL;
    
    memset(folding, 0, sizeof(vizero_code_folding_t));
    folding->buffer = buffer;
    folding->fold_capacity = 50;
    folding->folds = (vizero_code_fold_t*)vizero_safe_malloc(folding->fold_capacity * sizeof(vizero_code_fold_t));
    
    folding->auto_fold_enabled = false;
    folding->show_fold_markers = true;
    folding->min_fold_lines = 3;
    
    if (!folding->folds) {
        vizero_safe_free(folding);
        return NULL;
    }
    memset(folding->folds, 0, folding->fold_capacity * sizeof(vizero_code_fold_t));
    
    return folding;
}

void vizero_code_folding_destroy(vizero_code_folding_t* folding) {
    if (!folding) return;
    
    vizero_safe_free(folding->folds);
    vizero_safe_free(folding);
}

int vizero_code_folding_fold_range(vizero_code_folding_t* folding, size_t start_line, size_t end_line) {
    if (!folding || start_line >= end_line) return -1;
    
    /* Add to folds array if we have space */
    if (folding->fold_count < folding->fold_capacity) {
        vizero_code_fold_t* fold = &folding->folds[folding->fold_count];
        fold->start_line = start_line;
        fold->end_line = end_line;
        fold->type = VIZERO_FOLD_BLOCK;
        fold->is_folded = true;
        fold->is_valid = true;
        fold->level = 0;
        snprintf(fold->label, sizeof(fold->label), "{ ... } (%zu lines)", end_line - start_line + 1);
        folding->fold_count++;
        return 0;
    }
    
    return -1; /* No space for more folds */
}

int vizero_code_folding_unfold_range(vizero_code_folding_t* folding, size_t start_line, size_t end_line) {
    if (!folding || start_line >= end_line) return -1;
    
    /* Find and unfold the matching fold */
    for (size_t i = 0; i < folding->fold_count; i++) {
        vizero_code_fold_t* fold = &folding->folds[i];
        if (fold->start_line == start_line && fold->end_line == end_line) {
            fold->is_folded = false;
            return 0;
        }
    }
    
    return -1; /* Fold not found */
}

int vizero_code_folding_toggle_fold(vizero_code_folding_t* folding, size_t line) {
    if (!folding) return -1;
    
    /* Check if this line is part of an existing fold */
    for (size_t i = 0; i < folding->fold_count; i++) {
        vizero_code_fold_t* fold = &folding->folds[i];
        if (fold->is_valid && line >= fold->start_line && line <= fold->end_line) {
            if (fold->is_folded) {
                return vizero_code_folding_unfold_range(folding, fold->start_line, fold->end_line);
            } else {
                return vizero_code_folding_fold_range(folding, fold->start_line, fold->end_line);
            }
        }
    }
    
    /* No existing fold found - try to create a new one by finding matching braces */
    return -1; /* Will be handled by the calling function */
}

bool vizero_code_folding_is_line_folded(const vizero_code_folding_t* folding, size_t line) {
    if (!folding) return false;
    
    /* Check if this line is within any folded range (but not the first line) */
    for (size_t i = 0; i < folding->fold_count; i++) {
        const vizero_code_fold_t* fold = &folding->folds[i];
        if (fold->is_folded && fold->is_valid && 
            line > fold->start_line && line <= fold->end_line) {
            return true;
        }
    }
    return false;
}

bool vizero_code_folding_is_line_visible(const vizero_code_folding_t* folding, size_t line) {
    return !vizero_code_folding_is_line_folded(folding, line);
}

const vizero_code_fold_t* vizero_code_folding_get_fold_at_line(const vizero_code_folding_t* folding, size_t line) {
    if (!folding) return NULL;
    
    for (size_t i = 0; i < folding->fold_count; i++) {
        const vizero_code_fold_t* fold = &folding->folds[i];
        if (fold->is_valid && line >= fold->start_line && line <= fold->end_line) {
            return fold;
        }
    }
    return NULL;
}

int vizero_code_folding_fold_all(vizero_code_folding_t* folding) {
    /* Simple implementation - fold all existing folds */
    if (!folding) return -1;
    
    int folded_count = 0;
    for (size_t i = 0; i < folding->fold_count; i++) {
        vizero_code_fold_t* fold = &folding->folds[i];
        if (fold->is_valid && !fold->is_folded) {
            if (vizero_code_folding_fold_range(folding, fold->start_line, fold->end_line) == 0) {
                folded_count++;
            }
        }
    }
    
    return folded_count;
}

int vizero_code_folding_unfold_all(vizero_code_folding_t* folding) {
    /* Simple implementation - unfold all existing folds */
    if (!folding) return -1;
    
    int unfolded_count = 0;
    for (size_t i = 0; i < folding->fold_count; i++) {
        vizero_code_fold_t* fold = &folding->folds[i];
        if (fold->is_valid && fold->is_folded) {
            if (vizero_code_folding_unfold_range(folding, fold->start_line, fold->end_line) == 0) {
                unfolded_count++;
            }
        }
    }
    
    return unfolded_count;
}

/* Visual line mapping functions */
size_t vizero_code_folding_logical_to_visual_line(const vizero_code_folding_t* folding, size_t logical_line) {
    if (!folding) return logical_line;
    
    size_t visual_line = 0;
    for (size_t i = 0; i <= logical_line; i++) {
        if (!vizero_code_folding_is_line_folded(folding, i)) {
            if (i == logical_line) return visual_line;
            visual_line++;
        }
    }
    return visual_line;
}

size_t vizero_code_folding_visual_to_logical_line(const vizero_code_folding_t* folding, size_t visual_line) {
    if (!folding) return visual_line;
    
    size_t current_visual = 0;
    for (size_t i = 0; i < 10000; i++) { /* Reasonable upper bound */
        if (!vizero_code_folding_is_line_folded(folding, i)) {
            if (current_visual == visual_line) return i;
            current_visual++;
        }
    }
    return visual_line; /* Fallback */
}

/* Configuration functions */
void vizero_code_folding_set_show_markers(vizero_code_folding_t* folding, bool show) {
    if (folding) folding->show_fold_markers = show;
}

void vizero_code_folding_set_min_fold_lines(vizero_code_folding_t* folding, size_t min_lines) {
    if (folding) folding->min_fold_lines = min_lines;
}

/* Fold management functions */
int vizero_code_folding_add_fold(vizero_code_folding_t* folding, size_t start_line, size_t end_line, vizero_fold_type_t type, const char* label) {
    if (!folding || start_line >= end_line) return -1;
    
    /* Check if we need to expand the folds array */
    if (folding->fold_count >= folding->fold_capacity) {
        size_t new_capacity = folding->fold_capacity == 0 ? 16 : folding->fold_capacity * 2;
        vizero_code_fold_t* new_folds = (vizero_code_fold_t*)vizero_safe_realloc(folding->folds, new_capacity * sizeof(vizero_code_fold_t));
        if (!new_folds) return -1;
        
        folding->folds = new_folds;
        folding->fold_capacity = new_capacity;
        
        /* Initialize new slots */
        for (size_t i = folding->fold_count; i < new_capacity; i++) {
            folding->folds[i].is_valid = false;
            folding->folds[i].is_folded = false;
        }
    }
    
    /* Add the new fold */
    vizero_code_fold_t* fold = &folding->folds[folding->fold_count];
    fold->start_line = start_line;
    fold->end_line = end_line;
    fold->type = type;
    fold->is_folded = false;  /* Start unfolded */
    fold->is_valid = true;
    fold->level = 0;  /* Calculate level later if needed */
    
    /* Copy label */
    if (label) {
        strncpy(fold->label, label, sizeof(fold->label) - 1);
        fold->label[sizeof(fold->label) - 1] = '\0';
    } else {
        snprintf(fold->label, sizeof(fold->label), "[%zu lines]", end_line - start_line + 1);
    }
    
    folding->fold_count++;
    return 0;
}

int vizero_code_folding_remove_fold(vizero_code_folding_t* folding, size_t fold_index) {
    if (!folding || fold_index >= folding->fold_count) return -1;
    
    /* Shift all folds after this one down */
    for (size_t i = fold_index; i < folding->fold_count - 1; i++) {
        folding->folds[i] = folding->folds[i + 1];
    }
    
    /* Clear the last fold slot */
    folding->fold_count--;
    if (folding->fold_count < folding->fold_capacity) {
        folding->folds[folding->fold_count].is_valid = false;
        folding->folds[folding->fold_count].is_folded = false;
    }
    
    return 0;
}

void vizero_code_folding_clear_folds(vizero_code_folding_t* folding) {
    if (!folding) return;
    folding->fold_count = 0;
    /* Clear all folds */
    for (size_t i = 0; i < folding->fold_capacity; i++) {
        folding->folds[i].is_valid = false;
        folding->folds[i].is_folded = false;
    }
}

void vizero_code_folding_analyze_buffer(vizero_code_folding_t* folding) {
    if (!folding || !folding->buffer || !folding->auto_fold_enabled) return;
    
    /* Clear existing folds first */
    vizero_code_folding_clear_folds(folding);
    
    /* Only analyze if buffer has enough lines */
    size_t line_count = vizero_buffer_get_line_count(folding->buffer);
    if (line_count < folding->min_fold_lines) return;
    
    /* Look for brace-based blocks if enabled */
    if (folding->fold_functions || folding->fold_classes) {
        for (size_t i = 0; i < line_count - 1; i++) {
            const char* line = vizero_buffer_get_line_text(folding->buffer, i);
            if (!line) continue;
            
            /* Look for opening braces at end of line */
            size_t len = strlen(line);
            if (len > 0 && line[len - 1] == '{') {
                /* Find matching closing brace */
                int brace_count = 1;
                size_t end_line = i;
                
                for (size_t j = i + 1; j < line_count && brace_count > 0; j++) {
                    const char* next_line = vizero_buffer_get_line_text(folding->buffer, j);
                    if (!next_line) continue;
                    
                    /* Count braces in this line */
                    for (const char* c = next_line; *c; c++) {
                        if (*c == '{') brace_count++;
                        else if (*c == '}') brace_count--;
                    }
                    
                    if (brace_count == 0) {
                        end_line = j;
                        break;
                    }
                }
                
                /* Create fold if it's large enough */
                if (end_line > i && (end_line - i + 1) >= folding->min_fold_lines) {
                    char label[128];
                    snprintf(label, sizeof(label), "{...} [%zu lines]", end_line - i + 1);
                    vizero_code_folding_add_fold(folding, i, end_line, VIZERO_FOLD_BLOCK, label);
                }
            }
        }
    }
}

void vizero_code_folding_update_after_edit(vizero_code_folding_t* folding, size_t line, int lines_added) {
    if (!folding) return;
    
    /* Update line numbers for all folds after the edit point */
    for (size_t i = 0; i < folding->fold_count; i++) {
        vizero_code_fold_t* fold = &folding->folds[i];
        
        if (fold->start_line > line) {
            /* Fold starts after edit - adjust both start and end */
            if (lines_added < 0 && fold->start_line < line + (size_t)(-lines_added)) {
                /* Fold was deleted */
                fold->is_valid = false;
            } else {
                fold->start_line = (lines_added < 0) ? 
                    fold->start_line - (size_t)(-lines_added) : 
                    fold->start_line + (size_t)lines_added;
                fold->end_line = (lines_added < 0) ? 
                    fold->end_line - (size_t)(-lines_added) : 
                    fold->end_line + (size_t)lines_added;
            }
        } else if (fold->end_line >= line) {
            /* Edit is within fold - adjust end line only */
            if (lines_added < 0 && fold->end_line < line + (size_t)(-lines_added)) {
                /* Fold end was deleted - truncate or invalidate */
                if (fold->start_line >= line) {
                    fold->is_valid = false;
                } else {
                    fold->end_line = line - 1;
                    if (fold->end_line <= fold->start_line) {
                        fold->is_valid = false;
                    }
                }
            } else {
                fold->end_line = (lines_added < 0) ? 
                    fold->end_line - (size_t)(-lines_added) : 
                    fold->end_line + (size_t)lines_added;
            }
        }
    }
    
    /* Remove invalid folds */
    for (size_t i = 0; i < folding->fold_count; ) {
        if (!folding->folds[i].is_valid) {
            vizero_code_folding_remove_fold(folding, i);
            /* Don't increment i since we removed an element */
        } else {
            i++;
        }
    }
}

size_t vizero_code_folding_get_fold_count(const vizero_code_folding_t* folding) {
    return folding ? folding->fold_count : 0;
}

const vizero_code_fold_t* vizero_code_folding_get_fold_at_index(const vizero_code_folding_t* folding, size_t index) {
    if (!folding || index >= folding->fold_count) return NULL;
    return &folding->folds[index];
}

void vizero_code_folding_set_auto_fold(vizero_code_folding_t* folding, bool enabled) {
    if (folding) folding->auto_fold_enabled = enabled;
}

void vizero_code_folding_set_fold_functions(vizero_code_folding_t* folding, bool enabled) {
    if (folding) {
        folding->fold_functions = enabled;
        if (enabled && folding->auto_fold_enabled) {
            /* Re-analyze buffer with new settings */
            vizero_code_folding_analyze_buffer(folding);
        }
    }
}

void vizero_code_folding_set_fold_classes(vizero_code_folding_t* folding, bool enabled) {
    if (folding) {
        folding->fold_classes = enabled;
        if (enabled && folding->auto_fold_enabled) {
            /* Re-analyze buffer with new settings */
            vizero_code_folding_analyze_buffer(folding);
        }
    }
}

void vizero_code_folding_set_fold_comments(vizero_code_folding_t* folding, bool enabled) {
    if (folding) {
        folding->fold_comments = enabled;
        if (enabled && folding->auto_fold_enabled) {
            /* Re-analyze buffer with new settings */
            vizero_code_folding_analyze_buffer(folding);
        }
    }
}

void vizero_code_folding_set_fold_imports(vizero_code_folding_t* folding, bool enabled) {
    if (folding) {
        folding->fold_imports = enabled;
        if (enabled && folding->auto_fold_enabled) {
            /* Re-analyze buffer with new settings */
            vizero_code_folding_analyze_buffer(folding);
        }
    }
}