// Minimal, clean implementation for moving the cursor up/down by visual row (word wrap aware)
#include "vizero/editor_window.h"
#include "vizero/buffer.h"
#include "vizero/cursor.h"
#include <string.h>
#include <stdlib.h>

typedef struct {
    int line;
    int start_col;
    int visual_row;
    int visual_col_start;
    int visual_col_end;
} visual_map_entry_t;

int vizero_editor_window_move_visual_row(struct vizero_editor_window_t* window, int direction) {
    if (!window || !window->buffer || !window->cursor) return 0;
    vizero_buffer_t* buffer = window->buffer;
    int max_cols = window->width / 8;
    if (max_cols < 1) max_cols = 1;
    int word_wrap = 1;

    // Build visual map for the buffer (match renderer logic)
    size_t line_count = vizero_buffer_get_line_count(buffer);
    visual_map_entry_t visual_map[2048];
    int visual_map_count = 0;
    int row = 0;
    for (size_t i = 0; i < line_count; ++i) {
        const char* line = vizero_buffer_get_line_text(buffer, i);
        size_t len = line ? strlen(line) : 0;
        size_t start = 0;
        int indent_len = 0;
        if (line) {
            while (line[indent_len] == ' ' || line[indent_len] == '\t') indent_len++;
        }
        while (start < len) {
            size_t actual_chunk = 0;
            int wrap_at_space = 0;
            if (word_wrap) {
                size_t remaining = len - start;
                if (remaining > (size_t)max_cols) {
                    size_t try_chunk = (size_t)max_cols;
                    size_t last_space = 0;
                    for (size_t j = 0; j < try_chunk; ++j) {
                        if (line[start + j] == ' ') last_space = j;
                    }
                    if (last_space > 0) {
                        actual_chunk = last_space;
                        wrap_at_space = 1;
                    } else {
                        actual_chunk = try_chunk;
                    }
                } else {
                    actual_chunk = remaining;
                }
            } else {
                actual_chunk = len - start;
            }
            if (actual_chunk == 0 && start < len) actual_chunk = 1;
            if (visual_map_count < (int)(sizeof(visual_map)/sizeof(visual_map[0]))) {
                visual_map[visual_map_count].line = (int)i;
                visual_map[visual_map_count].start_col = (int)start;
                visual_map[visual_map_count].visual_row = row;
                visual_map[visual_map_count].visual_col_start = 0;
                visual_map[visual_map_count].visual_col_end = (int)actual_chunk;
                visual_map_count++;
            }
            row++;
            if (wrap_at_space) {
                start += actual_chunk + 1;
            } else {
                start += actual_chunk;
            }
            if (!word_wrap) break;
        }
        if (len == 0) {
            if (visual_map_count < (int)(sizeof(visual_map)/sizeof(visual_map[0]))) {
                visual_map[visual_map_count].line = (int)i;
                visual_map[visual_map_count].start_col = 0;
                visual_map[visual_map_count].visual_row = row;
                visual_map[visual_map_count].visual_col_start = 0;
                visual_map[visual_map_count].visual_col_end = 0;
                visual_map_count++;
            }
            row++;
        }
    }

    // Find current visual row and offset within row
    size_t cur_line = vizero_cursor_get_line(window->cursor);
    size_t cur_col = vizero_cursor_get_column(window->cursor);
    int cur_visual_row = -1;
    int offset_within_row = 0;
    int best_v = -1;
    int best_dist = 1000000;
    for (int v = 0; v < visual_map_count; ++v) {
        int chunk_len = visual_map[v].visual_col_end - visual_map[v].visual_col_start;
        int chunk_start = visual_map[v].start_col;
        int chunk_end = chunk_start + chunk_len;
        if (visual_map[v].line == (int)cur_line) {
            if (cur_col >= (size_t)chunk_start && cur_col < (size_t)chunk_end) {
                cur_visual_row = visual_map[v].visual_row;
                offset_within_row = (int)(cur_col - chunk_start);
                break;
            }
            // Track closest chunk if not found
            int dist = (int)abs((int)cur_col - chunk_start);
            if (dist < best_dist) {
                best_dist = dist;
                best_v = v;
            }
        }
    }
    if (cur_visual_row == -1 && best_v != -1) {
        cur_visual_row = visual_map[best_v].visual_row;
        offset_within_row = (int)(cur_col - visual_map[best_v].start_col);
    }
    if (cur_visual_row == -1) return 0;
    int target_visual_row = cur_visual_row + direction;
    for (int v = 0; v < visual_map_count; ++v) {
        int chunk_len = visual_map[v].visual_col_end - visual_map[v].visual_col_start;
        int chunk_start = visual_map[v].start_col;
        int chunk_end = chunk_start + chunk_len;
        if (visual_map[v].visual_row == target_visual_row) {
            int rel_col = offset_within_row;
            if (chunk_len <= 0) rel_col = 0;
            else if (rel_col < 0) rel_col = 0;
            else if (rel_col > chunk_len) rel_col = chunk_len;
            int target_col = chunk_start + rel_col;
            // Clamp to line length
            const char* line = vizero_buffer_get_line_text(buffer, visual_map[v].line);
            int line_len = line ? (int)strlen(line) : 0;
            if (target_col > line_len) target_col = line_len;
            if (target_col < chunk_start) target_col = chunk_start;
            if (target_col > chunk_end) target_col = chunk_end;
            vizero_cursor_set_position(window->cursor, visual_map[v].line, target_col);
            return 1;
        }
    }
    return 0;
}
