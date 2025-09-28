#ifndef VIZERO_SEARCH_H
#define VIZERO_SEARCH_H

#ifdef __cplusplus
extern "C" {
#endif

#include "editor_state.h"

/* Search functions - these are implemented in C++ */
int vizero_search_forward(vizero_editor_state_t* state, const char* pattern);
int vizero_search_backward(vizero_editor_state_t* state, const char* pattern);
int vizero_search_next(vizero_editor_state_t* state);
int vizero_search_previous(vizero_editor_state_t* state);
/* Navigate to next search result (forward=1) or previous (forward=0) */
int vizero_search_next_direction(vizero_editor_state_t* state, int forward);
int vizero_substitute_line(vizero_editor_state_t* state, const char* pattern, const char* replacement, 
                          int line_number, int global);
int vizero_substitute_range(vizero_editor_state_t* state, const char* pattern, const char* replacement, 
                           int line_start, int line_end, int global);
int vizero_substitute_all(vizero_editor_state_t* state, const char* pattern, const char* replacement, int global);

/* Search state management */
void vizero_search_clear(vizero_editor_state_t* state);
int vizero_search_has_results(vizero_editor_state_t* state);
const char* vizero_search_get_pattern(vizero_editor_state_t* state);
int vizero_search_get_match_count(vizero_editor_state_t* state);
int vizero_search_get_current_match_index(vizero_editor_state_t* state);
/* Get all search matches for highlighting - returns pointer to internal array, do not free */
const vizero_search_match_t* vizero_search_get_all_matches(vizero_editor_state_t* state);
/* Check if search results are for the specified buffer */
int vizero_search_results_for_buffer(vizero_editor_state_t* state, vizero_buffer_t* buffer);

/* Find all matches in a specific buffer without affecting global search state
 * Returns number of matches found, fills matches array up to max_matches
 * Caller must provide the matches array */
int vizero_search_find_all_in_buffer(vizero_buffer_t* buffer, const char* pattern, 
                                     vizero_search_match_t* matches, int max_matches);

/* Performance optimization: clear search caches to force recomputation
 * Call this when buffer content changes significantly */
void vizero_search_clear_caches(void);

/* Search history management */
const char** vizero_search_get_history(vizero_editor_state_t* state, size_t* count);
void vizero_search_clear_history(vizero_editor_state_t* state);

/* Incremental search for real-time feedback */
int vizero_search_incremental_begin(vizero_editor_state_t* state);
int vizero_search_incremental_update(vizero_editor_state_t* state, const char* pattern);
void vizero_search_incremental_end(vizero_editor_state_t* state);

/* Cleanup function - call when editor state is destroyed */
void vizero_search_cleanup_editor_state(vizero_editor_state_t* state);

/* Advanced regex search and replace with capture groups */
int vizero_search_regex(vizero_editor_state_t* state, const char* regex_pattern);
int vizero_substitute_regex(vizero_editor_state_t* state, const char* regex_pattern, 
                            const char* replacement_template, int line_start, int line_end, int global);
int vizero_substitute_regex_all(vizero_editor_state_t* state, const char* regex_pattern, 
                                const char* replacement_template, int global);

/* Interactive replacement with confirmation */
typedef enum {
    VIZERO_REPLACE_YES,
    VIZERO_REPLACE_NO,
    VIZERO_REPLACE_ALL,
    VIZERO_REPLACE_QUIT
} vizero_replace_action_t;

typedef vizero_replace_action_t (*vizero_replace_callback_t)(const char* match, 
                                                             const char* replacement, 
                                                             size_t line, size_t column, 
                                                             void* user_data);

int vizero_substitute_interactive(vizero_editor_state_t* state, const char* pattern, 
                                  const char* replacement, vizero_replace_callback_t callback, 
                                  void* user_data);

/* Case-sensitive/insensitive control */
void vizero_search_set_case_sensitive(vizero_editor_state_t* state, int case_sensitive);
int vizero_search_is_case_sensitive(vizero_editor_state_t* state);

/* Whole word matching */
void vizero_search_set_whole_word(vizero_editor_state_t* state, int whole_word);
int vizero_search_is_whole_word(vizero_editor_state_t* state);

#ifdef __cplusplus
}
#endif

#endif /* VIZERO_SEARCH_H */