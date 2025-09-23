#include "vizero/search.h"
#include "vizero/editor_state.h"
#include "vizero/buffer.h"
#include "vizero/cursor.h"
#include <regex>
#include <string>
#include <vector>
#include <memory>

/* Search state structure - internal C++ implementation */
struct SearchState {
    std::regex compiled_pattern;
    std::string pattern_string;
    std::vector<vizero_search_match_t> matches;
    int current_match_index;
    bool has_pattern;
    vizero_search_direction_t last_direction;
    
    SearchState() : current_match_index(-1), has_pattern(false), last_direction(VIZERO_SEARCH_FORWARD) {}
};

/* Global search state - in a real implementation this would be per-editor */
static std::unique_ptr<SearchState> g_search_state = std::make_unique<SearchState>();

/* Helper function to find all matches in buffer */
static void find_all_matches(vizero_editor_state_t* state, const std::regex& pattern) {
    g_search_state->matches.clear();
    
    vizero_buffer_t* buffer = vizero_editor_get_current_buffer(state);
    if (!buffer) return;
    
    size_t line_count = vizero_buffer_get_line_count(buffer);
    
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
            g_search_state->matches.push_back(result);
        }
    }
}

/* Helper function to find closest match to cursor */
static int find_closest_match(vizero_editor_state_t* state, vizero_search_direction_t direction) {
    if (g_search_state->matches.empty()) return -1;
    
    vizero_cursor_t* cursor = vizero_editor_get_current_cursor(state);
    if (!cursor) return -1;
    
    size_t cursor_line = vizero_cursor_get_line(cursor);
    size_t cursor_col = vizero_cursor_get_column(cursor);
    
    for (size_t i = 0; i < g_search_state->matches.size(); i++) {
        const auto& match = g_search_state->matches[i];
        
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
    if (direction == VIZERO_SEARCH_FORWARD && !g_search_state->matches.empty()) {
        return 0;
    } else if (direction == VIZERO_SEARCH_BACKWARD && !g_search_state->matches.empty()) {
        return (int)g_search_state->matches.size() - 1;
    }
    
    return -1;
}

/* Helper function to move cursor to match */
static void move_to_match(vizero_editor_state_t* state, int match_index) {
    if (match_index < 0 || match_index >= (int)g_search_state->matches.size()) return;
    
    const auto& match = g_search_state->matches[match_index];
    vizero_cursor_t* cursor = vizero_editor_get_current_cursor(state);
    if (cursor) {
        vizero_cursor_set_position(cursor, match.line, match.column);
        g_search_state->current_match_index = match_index;
        
        /* Update status message */
        char status[256];
        snprintf(status, sizeof(status), "Match %d of %d: %s", 
                match_index + 1, (int)g_search_state->matches.size(), 
                g_search_state->pattern_string.c_str());
        vizero_editor_set_status_message(state, status);
    }
}

/* Public API implementations */
extern "C" {

int vizero_search_forward(vizero_editor_state_t* state, const char* pattern) {
    if (!state || !pattern) return -1;
    
    try {
        g_search_state->pattern_string = pattern;
        g_search_state->compiled_pattern = std::regex(pattern, std::regex_constants::ECMAScript);
        g_search_state->has_pattern = true;
        g_search_state->last_direction = VIZERO_SEARCH_FORWARD;
        
        find_all_matches(state, g_search_state->compiled_pattern);
        
        if (g_search_state->matches.empty()) {
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
    
    try {
        g_search_state->pattern_string = pattern;
        g_search_state->compiled_pattern = std::regex(pattern, std::regex_constants::ECMAScript);
        g_search_state->has_pattern = true;
        g_search_state->last_direction = VIZERO_SEARCH_BACKWARD;
        
        find_all_matches(state, g_search_state->compiled_pattern);
        
        if (g_search_state->matches.empty()) {
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
    if (!state || !g_search_state->has_pattern) return -1;
    
    if (g_search_state->matches.empty()) return 0;
    
    int next_index = g_search_state->current_match_index + 1;
    if (next_index >= (int)g_search_state->matches.size()) {
        next_index = 0; /* Wrap around */
    }
    
    move_to_match(state, next_index);
    return 1;
}

int vizero_search_previous(vizero_editor_state_t* state) {
    if (!state || !g_search_state->has_pattern) return -1;
    
    if (g_search_state->matches.empty()) return 0;
    
    int prev_index = g_search_state->current_match_index - 1;
    if (prev_index < 0) {
        prev_index = (int)g_search_state->matches.size() - 1; /* Wrap around */
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
    (void)state; /* Unused parameter */
    g_search_state->matches.clear();
    g_search_state->current_match_index = -1;
    g_search_state->has_pattern = false;
    g_search_state->pattern_string.clear();
}

int vizero_search_has_results(vizero_editor_state_t* state) {
    (void)state; /* Unused parameter */
    return g_search_state->has_pattern && !g_search_state->matches.empty();
}

const char* vizero_search_get_pattern(vizero_editor_state_t* state) {
    (void)state; /* Unused parameter */
    return g_search_state->has_pattern ? g_search_state->pattern_string.c_str() : nullptr;
}

int vizero_search_get_match_count(vizero_editor_state_t* state) {
    (void)state; /* Unused parameter */
    return (int)g_search_state->matches.size();
}

int vizero_search_get_current_match_index(vizero_editor_state_t* state) {
    (void)state; /* Unused parameter */
    return g_search_state->current_match_index;
}

int vizero_search_next_direction(vizero_editor_state_t* state, int forward) {
    if (forward) {
        return vizero_search_next(state);
    } else {
        return vizero_search_previous(state);
    }
}

const vizero_search_match_t* vizero_search_get_all_matches(vizero_editor_state_t* state) {
    (void)state; /* Unused parameter */
    return g_search_state->matches.empty() ? nullptr : g_search_state->matches.data();
}

} /* extern "C" */