#include "vizero/search.h"
#include "vizero/editor_state.h"
#include "vizero/buffer.h"
#include "vizero/cursor.h"
#include <regex>
#include <string>
#include <vector>
#include <memory>
#include <unordered_map>
#include <algorithm>
#include <mutex>

/* Search state structure - internal C++ implementation */
struct SearchState {
    std::regex compiled_pattern;
    std::string pattern_string;
    std::vector<vizero_search_match_t> matches;
    int current_match_index;
    bool has_pattern;
    vizero_search_direction_t last_direction;
    
    /* Performance optimization: cached compiled patterns */
    std::string last_compiled_pattern;
    bool pattern_cache_valid;
    
    /* Performance optimization: match result cache */
    vizero_buffer_t* cached_buffer;
    uint64_t cached_buffer_version;
    bool matches_cache_valid;
    
    /* Feature enhancement: search history */
    std::vector<std::string> search_history;
    static const size_t MAX_HISTORY_SIZE = 50;
    
    /* Incremental search optimization */
    std::string incremental_pattern;
    std::vector<vizero_search_match_t> incremental_matches;
    bool incremental_mode;
    size_t last_incremental_length;
    
    SearchState() : current_match_index(-1), has_pattern(false), last_direction(VIZERO_SEARCH_FORWARD),
                   pattern_cache_valid(false), cached_buffer(nullptr), cached_buffer_version(0), 
                   matches_cache_valid(false), incremental_mode(false), last_incremental_length(0) {}
    
    void add_to_history(const std::string& pattern) {
        /* Avoid duplicates - remove existing entry if present */
        auto it = std::find(search_history.begin(), search_history.end(), pattern);
        if (it != search_history.end()) {
            search_history.erase(it);
        }
        
        /* Add to front */
        search_history.insert(search_history.begin(), pattern);
        
        /* Limit history size */
        if (search_history.size() > MAX_HISTORY_SIZE) {
            search_history.resize(MAX_HISTORY_SIZE);
        }
    }
};

/* Per-editor search states - fully implemented for proper multi-buffer support */
static std::unordered_map<vizero_editor_state_t*, std::unique_ptr<SearchState>> g_editor_search_states;
static std::mutex g_search_states_mutex; /* Thread safety for multi-editor scenarios */

/* Helper function to get or create search state for editor */
static SearchState* get_search_state(vizero_editor_state_t* state) {
    if (!state) return nullptr;
    
    std::lock_guard<std::mutex> lock(g_search_states_mutex);
    
    auto it = g_editor_search_states.find(state);
    if (it == g_editor_search_states.end()) {
        /* Create new search state for this editor */
        auto search_state = std::make_unique<SearchState>();
        SearchState* ptr = search_state.get();
        g_editor_search_states[state] = std::move(search_state);
        return ptr;
    }
    
    return it->second.get();
}

/* Helper function to find all matches in buffer with caching */
static void find_all_matches(vizero_editor_state_t* editor_state, const std::regex& pattern) {
    vizero_buffer_t* buffer = vizero_editor_get_current_buffer(editor_state);
    if (!buffer) return;
    
    SearchState* search_state = get_search_state(editor_state);
    if (!search_state) return;
    
    /* Check if we can use cached results - compare buffer and pattern */
    uint64_t current_buffer_version = vizero_buffer_get_modification_time(buffer);
    if (search_state->matches_cache_valid && 
        search_state->cached_buffer == buffer &&
        search_state->cached_buffer_version == current_buffer_version) {
        /* Cache is valid, no need to recompute */
        return;
    }
    
    /* Cache is invalid, recompute matches */
    search_state->matches.clear();
    
    size_t line_count = vizero_buffer_get_line_count(buffer);
    
    /* Performance optimization: reserve space for matches to reduce reallocations */
    search_state->matches.reserve(line_count / 10); /* Estimate ~10% of lines might match */
    
    for (size_t line = 0; line < line_count; line++) {
        const char* line_text = vizero_buffer_get_line_text(buffer, line);
        if (!line_text) continue;
        
        std::string text(line_text);
        std::sregex_iterator iter(text.begin(), text.end(), pattern);
        std::sregex_iterator end;
        
        for (; iter != end; ++iter) {
            const std::smatch& match = *iter;
            vizero_search_match_t result;
            result.line = (int)line;
            result.column = (int)match.position();
            result.length = (int)match.length();
            search_state->matches.push_back(result);
        }
    }
    
    /* Update cache metadata */
    search_state->cached_buffer = buffer;
    search_state->cached_buffer_version = current_buffer_version;
    search_state->matches_cache_valid = true;
}

/* Helper function to find closest match to cursor */
static int find_closest_match(vizero_editor_state_t* editor_state, vizero_search_direction_t direction) {
    SearchState* search_state = get_search_state(editor_state);
    if (!search_state || search_state->matches.empty()) return -1;
    
    vizero_cursor_t* cursor = vizero_editor_get_current_cursor(editor_state);
    if (!cursor) return -1;
    
    size_t cursor_line = vizero_cursor_get_line(cursor);
    size_t cursor_col = vizero_cursor_get_column(cursor);
    
    for (size_t i = 0; i < search_state->matches.size(); i++) {
        const auto& match = search_state->matches[i];
        
        if (direction == VIZERO_SEARCH_FORWARD) {
            if (match.line > (int)cursor_line || 
                (match.line == (int)cursor_line && match.column > (int)cursor_col)) {
                return (int)i;
            }
        } else {
            if (match.line < (int)cursor_line || 
                (match.line == (int)cursor_line && match.column < (int)cursor_col)) {
                return (int)i;
            }
        }
    }
    
    /* Wrap around */
    if (direction == VIZERO_SEARCH_FORWARD && !search_state->matches.empty()) {
        return 0;
    } else if (direction == VIZERO_SEARCH_BACKWARD && !search_state->matches.empty()) {
        return (int)(search_state->matches.size() - 1);
    }
    
    return -1;
}

/* Helper function to move cursor to match */
static void move_to_match(vizero_editor_state_t* editor_state, int match_index) {
    SearchState* search_state = get_search_state(editor_state);
    if (!search_state || match_index < 0 || match_index >= (int)search_state->matches.size()) return;
    
    const auto& match = search_state->matches[match_index];
    vizero_cursor_t* cursor = vizero_editor_get_current_cursor(editor_state);
    if (cursor) {
        vizero_cursor_set_position(cursor, match.line, match.column);
        search_state->current_match_index = match_index;
        
        /* Update status message */
        char status[256];
        snprintf(status, sizeof(status), "Match %d of %d: %s", 
                match_index + 1, (int)search_state->matches.size(), 
                search_state->pattern_string.c_str());
        vizero_editor_set_status_message(editor_state, status);
    }
}

/* Public API implementations */
extern "C" {

int vizero_search_forward(vizero_editor_state_t* state, const char* pattern) {
    if (!state || !pattern) return -1;
    
    SearchState* search_state = get_search_state(state);
    if (!search_state) return -1;
    
    try {
        /* Performance optimization: check if we can reuse compiled regex */
        bool need_recompile = !search_state->pattern_cache_valid || 
                             search_state->last_compiled_pattern != pattern;
        
        if (need_recompile) {
            search_state->compiled_pattern = std::regex(pattern, std::regex_constants::ECMAScript);
            search_state->last_compiled_pattern = pattern;
            search_state->pattern_cache_valid = true;
            /* Invalidate match cache when pattern changes */
            search_state->matches_cache_valid = false;
        }
        
        search_state->pattern_string = pattern;
        search_state->has_pattern = true;
        search_state->last_direction = VIZERO_SEARCH_FORWARD;
        
        /* Add to search history */
        search_state->add_to_history(pattern);
        
        find_all_matches(state, search_state->compiled_pattern);
        
        if (search_state->matches.empty()) {
            vizero_editor_set_status_message(state, "Pattern not found");
            return 0;
        }
        
        int match_index = find_closest_match(state, VIZERO_SEARCH_FORWARD);
        if (match_index >= 0) {
            move_to_match(state, match_index);
            return 1;
        }
        
        return 0;
    } catch (const std::regex_error& e) {
        (void)e; /* Suppress unused variable warning */
        vizero_editor_set_status_message(state, "Invalid regex pattern");
        return -1;
    }
}

int vizero_search_backward(vizero_editor_state_t* state, const char* pattern) {
    if (!state || !pattern) return -1;
    
    SearchState* search_state = get_search_state(state);
    if (!search_state) return -1;
    
    try {
        /* Performance optimization: check if we can reuse compiled regex */
        bool need_recompile = !search_state->pattern_cache_valid || 
                             search_state->last_compiled_pattern != pattern;
        
        if (need_recompile) {
            search_state->compiled_pattern = std::regex(pattern, std::regex_constants::ECMAScript);
            search_state->last_compiled_pattern = pattern;
            search_state->pattern_cache_valid = true;
            /* Invalidate match cache when pattern changes */
            search_state->matches_cache_valid = false;
        }
        
        search_state->pattern_string = pattern;
        search_state->has_pattern = true;
        search_state->last_direction = VIZERO_SEARCH_BACKWARD;
        
        /* Add to search history */
        search_state->add_to_history(pattern);
        
        find_all_matches(state, search_state->compiled_pattern);
        
        if (search_state->matches.empty()) {
            vizero_editor_set_status_message(state, "Pattern not found");
            return 0;
        }
        
        int match_index = find_closest_match(state, VIZERO_SEARCH_BACKWARD);
        if (match_index >= 0) {
            move_to_match(state, match_index);
            return 1;
        }
        
        return 0;
    } catch (const std::regex_error& e) {
        (void)e; /* Suppress unused variable warning */
        vizero_editor_set_status_message(state, "Invalid regex pattern");
        return -1;
    }
}

int vizero_search_next(vizero_editor_state_t* state) {
    SearchState* search_state = get_search_state(state);
    if (!state || !search_state || !search_state->has_pattern) return -1;
    
    if (search_state->matches.empty()) return 0;
    
    int next_index = search_state->current_match_index + 1;
    if (next_index >= (int)search_state->matches.size()) {
        next_index = 0; /* Wrap around */
    }
    
    move_to_match(state, next_index);
    return 1;
}

int vizero_search_previous(vizero_editor_state_t* state) {
    SearchState* search_state = get_search_state(state);
    if (!state || !search_state || !search_state->has_pattern) return -1;
    
    if (search_state->matches.empty()) return 0;
    
    int prev_index = search_state->current_match_index - 1;
    if (prev_index < 0) {
        prev_index = (int)search_state->matches.size() - 1; /* Wrap around */
    }
    
    move_to_match(state, prev_index);
    return 1;
}

int vizero_substitute_line(vizero_editor_state_t* state, const char* pattern, const char* replacement, 
                          int line_number, int global) {
    if (!state || !pattern || !replacement) return -1;
    
    try {
        std::regex regex_pattern(pattern, std::regex_constants::ECMAScript);
        
        vizero_buffer_t* buffer = vizero_editor_get_current_buffer(state);
        if (!buffer) return -1;
        
        const char* line_text = vizero_buffer_get_line_text(buffer, line_number);
        if (!line_text) return -1;
        
        std::string text(line_text);
        std::string result;
        
        if (global) {
            result = std::regex_replace(text, regex_pattern, replacement);
        } else {
            result = std::regex_replace(text, regex_pattern, replacement, 
                                      std::regex_constants::format_first_only);
        }
        
        if (result != text) {
            /* Replace the line in the buffer */
            /* First delete the entire line content */
            if (vizero_buffer_delete_range(buffer, line_number, 0, line_number, text.length()) == 0) {
                /* Then insert the new line content */
                vizero_buffer_insert_text(buffer, line_number, 0, result.c_str());
            }
            return 1;
        }
        
        return 0;
    } catch (const std::regex_error&) {
        vizero_editor_set_status_message(state, "Invalid regex pattern");
        return -1;
    }
}

int vizero_substitute_range(vizero_editor_state_t* state, const char* pattern, const char* replacement, 
                           int line_start, int line_end, int global) {
    if (!state || !pattern || !replacement) return -1;
    
    int substitutions = 0;
    for (int line = line_start; line <= line_end; line++) {
        int result = vizero_substitute_line(state, pattern, replacement, line, global);
        if (result > 0) substitutions += result;
    }
    
    if (substitutions > 0) {
        char status[256];
        snprintf(status, sizeof(status), "%d substitution%s made", 
                substitutions, substitutions == 1 ? "" : "s");
        vizero_editor_set_status_message(state, status);
    } else {
        vizero_editor_set_status_message(state, "No substitutions made");
    }
    
    return substitutions;
}

int vizero_substitute_all(vizero_editor_state_t* state, const char* pattern, const char* replacement, int global) {
    if (!state) return -1;
    
    vizero_buffer_t* buffer = vizero_editor_get_current_buffer(state);
    if (!buffer) return -1;
    
    size_t line_count = vizero_buffer_get_line_count(buffer);
    return vizero_substitute_range(state, pattern, replacement, 0, (int)line_count - 1, global);
}

void vizero_search_clear(vizero_editor_state_t* state) {
    SearchState* search_state = get_search_state(state);
    if (!search_state) return;
    
    search_state->matches.clear();
    search_state->current_match_index = -1;
    search_state->has_pattern = false;
    search_state->pattern_string.clear();
}

int vizero_search_has_results(vizero_editor_state_t* state) {
    SearchState* search_state = get_search_state(state);
    if (!search_state) return 0;
    return search_state->has_pattern && !search_state->matches.empty();
}

const char* vizero_search_get_pattern(vizero_editor_state_t* state) {
    SearchState* search_state = get_search_state(state);
    if (!search_state) return nullptr;
    return search_state->has_pattern ? search_state->pattern_string.c_str() : nullptr;
}

int vizero_search_get_match_count(vizero_editor_state_t* state) {
    SearchState* search_state = get_search_state(state);
    if (!search_state) return 0;
    return (int)search_state->matches.size();
}

int vizero_search_get_current_match_index(vizero_editor_state_t* state) {
    SearchState* search_state = get_search_state(state);
    if (!search_state) return -1;
    return search_state->current_match_index;
}

int vizero_search_next_direction(vizero_editor_state_t* state, int forward) {
    if (forward) {
        return vizero_search_next(state);
    } else {
        return vizero_search_previous(state);
    }
}

const vizero_search_match_t* vizero_search_get_all_matches(vizero_editor_state_t* state) {
    SearchState* search_state = get_search_state(state);
    if (!search_state) return nullptr;
    return search_state->matches.empty() ? nullptr : search_state->matches.data();
}

int vizero_search_find_all_in_buffer(vizero_buffer_t* buffer, const char* pattern, 
                                     vizero_search_match_t* matches, int max_matches) {
    if (!buffer || !pattern || !matches || max_matches <= 0) return 0;
    
    try {
        std::regex compiled_pattern(pattern, std::regex_constants::ECMAScript);
        int match_count = 0;
        
        size_t line_count = vizero_buffer_get_line_count(buffer);
        
        for (size_t line = 0; line < line_count && match_count < max_matches; line++) {
            const char* line_text = vizero_buffer_get_line_text(buffer, line);
            if (!line_text) continue;
            
            std::string text(line_text);
            std::sregex_iterator iter(text.begin(), text.end(), compiled_pattern);
            std::sregex_iterator end;
            
            for (; iter != end && match_count < max_matches; ++iter) {
                const std::smatch& match = *iter;
                matches[match_count].line = (int)line;
                matches[match_count].column = (int)match.position();
                matches[match_count].length = (int)match.length();
                match_count++;
            }
        }
        
        return match_count;
    } catch (const std::regex_error& e) {
        (void)e; /* Suppress unused variable warning */
        return 0; /* Invalid regex */
    }
}

void vizero_search_clear_caches(void) {
    /* Clear all caches for all editor instances */
    for (auto& pair : g_editor_search_states) {
        SearchState* search_state = pair.second.get();
        if (search_state) {
            search_state->pattern_cache_valid = false;
            search_state->matches_cache_valid = false;
            search_state->cached_buffer = nullptr;
            search_state->cached_buffer_version = 0;
            search_state->last_compiled_pattern.clear();
            search_state->matches.clear();
        }
    }
}

const char** vizero_search_get_history(vizero_editor_state_t* state, size_t* count) {
    if (!state || !count) {
        if (count) *count = 0;
        return nullptr;
    }
    
    SearchState* search_state = get_search_state(state);
    if (!search_state) {
        *count = 0;
        return nullptr;
    }
    
    *count = search_state->search_history.size();
    if (search_state->search_history.empty()) {
        return nullptr;
    }
    
    /* Note: This returns pointers to internal strings - valid until next search operation */
    static thread_local std::vector<const char*> history_ptrs;
    history_ptrs.clear();
    history_ptrs.reserve(search_state->search_history.size());
    
    for (const auto& pattern : search_state->search_history) {
        history_ptrs.push_back(pattern.c_str());
    }
    
    return history_ptrs.data();
}

void vizero_search_clear_history(vizero_editor_state_t* state) {
    SearchState* search_state = get_search_state(state);
    if (!search_state) return;
    
    search_state->search_history.clear();
}

/* Incremental search functions for performance */
int vizero_search_incremental_begin(vizero_editor_state_t* state) {
    SearchState* search_state = get_search_state(state);
    if (!search_state) return -1;
    
    search_state->incremental_mode = true;
    search_state->incremental_pattern.clear();
    search_state->incremental_matches.clear();
    search_state->last_incremental_length = 0;
    
    return 0;
}

int vizero_search_incremental_update(vizero_editor_state_t* state, const char* pattern) {
    if (!state || !pattern) return -1;
    
    SearchState* search_state = get_search_state(state);
    if (!search_state || !search_state->incremental_mode) return -1;
    
    std::string new_pattern(pattern);
    size_t new_length = new_pattern.length();
    
    /* Check if we can optimize by filtering existing results */
    if (new_length > search_state->last_incremental_length && 
        new_pattern.substr(0, search_state->last_incremental_length) == search_state->incremental_pattern) {
        
        /* Pattern extended - filter existing matches */
        try {
            std::regex extended_pattern(new_pattern, std::regex_constants::ECMAScript);
            std::vector<vizero_search_match_t> filtered_matches;
            
            vizero_buffer_t* buffer = vizero_editor_get_current_buffer(state);
            if (buffer) {
                for (const auto& match : search_state->incremental_matches) {
                    const char* line_text = vizero_buffer_get_line_text(buffer, match.line);
                    if (line_text) {
                        std::string text(line_text);
                        if (std::regex_search(text.begin() + match.column, text.end(), extended_pattern)) {
                            filtered_matches.push_back(match);
                        }
                    }
                }
            }
            
            search_state->incremental_matches = filtered_matches;
        } catch (const std::regex_error&) {
            return -1;
        }
    } else {
        /* Pattern changed significantly - full search */
        try {
            std::regex compiled_pattern(new_pattern, std::regex_constants::ECMAScript);
            search_state->incremental_matches.clear();
            
            vizero_buffer_t* buffer = vizero_editor_get_current_buffer(state);
            if (buffer) {
                size_t line_count = vizero_buffer_get_line_count(buffer);
                search_state->incremental_matches.reserve(line_count / 20);
                
                for (size_t line = 0; line < line_count; line++) {
                    const char* line_text = vizero_buffer_get_line_text(buffer, line);
                    if (!line_text) continue;
                    
                    std::string text(line_text);
                    std::sregex_iterator iter(text.begin(), text.end(), compiled_pattern);
                    std::sregex_iterator end;
                    
                    for (; iter != end; ++iter) {
                        const std::smatch& match = *iter;
                        vizero_search_match_t result;
                        result.line = (int)line;
                        result.column = (int)match.position();
                        result.length = (int)match.length();
                        search_state->incremental_matches.push_back(result);
                    }
                }
            }
        } catch (const std::regex_error&) {
            return -1;
        }
    }
    
    search_state->incremental_pattern = new_pattern;
    search_state->last_incremental_length = new_length;
    
    /* Update status */
    char status[256];
    snprintf(status, sizeof(status), "Incremental: %d match%s for '%s'", 
            (int)search_state->incremental_matches.size(),
            search_state->incremental_matches.size() == 1 ? "" : "es",
            pattern);
    vizero_editor_set_status_message(state, status);
    
    return (int)search_state->incremental_matches.size();
}

void vizero_search_incremental_end(vizero_editor_state_t* state) {
    SearchState* search_state = get_search_state(state);
    if (!search_state) return;
    
    if (search_state->incremental_mode) {
        /* Copy incremental results to main search results */
        search_state->matches = search_state->incremental_matches;
        search_state->pattern_string = search_state->incremental_pattern;
        search_state->has_pattern = !search_state->incremental_pattern.empty();
        search_state->current_match_index = search_state->matches.empty() ? -1 : 0;
        
        /* Clear incremental state */
        search_state->incremental_mode = false;
        search_state->incremental_pattern.clear();
        search_state->incremental_matches.clear();
        search_state->last_incremental_length = 0;
    }
}

void vizero_search_cleanup_editor_state(vizero_editor_state_t* state) {
    if (!state) return;
    
    auto it = g_editor_search_states.find(state);
    if (it != g_editor_search_states.end()) {
        g_editor_search_states.erase(it);
    }
}

int vizero_search_results_for_buffer(vizero_editor_state_t* state, vizero_buffer_t* buffer) {
    SearchState* search_state = get_search_state(state);
    if (!search_state || !buffer) return 0;
    
    /* Check if search results exist and are for the specified buffer */
    return search_state->has_pattern && 
           search_state->matches_cache_valid && 
           search_state->cached_buffer == buffer;
}

} /* extern "C" */
