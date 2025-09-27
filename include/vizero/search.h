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

/* Find all matches in a specific buffer without affecting global search state
 * Returns number of matches found, fills matches array up to max_matches
 * Caller must provide the matches array */
int vizero_search_find_all_in_buffer(vizero_buffer_t* buffer, const char* pattern, 
                                     vizero_search_match_t* matches, int max_matches);

#ifdef __cplusplus
}
#endif

#endif /* VIZERO_SEARCH_H */