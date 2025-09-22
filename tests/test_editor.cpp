#include "test_framework.h"
#include "../include/vizero/editor_state.h"
#include "../include/vizero/buffer.h"
#include "../include/vizero/cursor.h"

/* Test editor state creation and destruction */
int test_editor_state_create_destroy(void) {
    vizero_editor_state_t* state = vizero_editor_state_create();
    TEST_ASSERT_NOT_NULL(state, "Editor state should be created");
    
    /* Should start in normal mode */
    TEST_ASSERT_EQUAL(VIZERO_MODE_NORMAL, vizero_editor_get_mode(state), "Should start in normal mode");
    
    /* Should have one buffer */
    TEST_ASSERT_EQUAL(1, vizero_editor_get_buffer_count(state), "Should start with one buffer");
    
    /* Should have a current buffer */
    vizero_buffer_t* buffer = vizero_editor_get_current_buffer(state);
    TEST_ASSERT_NOT_NULL(buffer, "Should have a current buffer");
    
    /* Should have a current cursor */
    vizero_cursor_t* cursor = vizero_editor_get_current_cursor(state);
    TEST_ASSERT_NOT_NULL(cursor, "Should have a current cursor");
    
    vizero_editor_state_destroy(state);
    return 1;
}

/* Test mode switching */
int test_editor_mode_switching(void) {
    vizero_editor_state_t* state = vizero_editor_state_create();
    TEST_ASSERT_NOT_NULL(state, "Editor state should be created");
    
    /* Test switching to insert mode */
    vizero_editor_set_mode(state, VIZERO_MODE_INSERT);
    TEST_ASSERT_EQUAL(VIZERO_MODE_INSERT, vizero_editor_get_mode(state), "Should be in insert mode");
    
    /* Test switching to command mode */
    vizero_editor_set_mode(state, VIZERO_MODE_COMMAND);
    TEST_ASSERT_EQUAL(VIZERO_MODE_COMMAND, vizero_editor_get_mode(state), "Should be in command mode");
    
    /* Test switching back to normal mode */
    vizero_editor_set_mode(state, VIZERO_MODE_NORMAL);
    TEST_ASSERT_EQUAL(VIZERO_MODE_NORMAL, vizero_editor_get_mode(state), "Should be back in normal mode");
    
    vizero_editor_state_destroy(state);
    return 1;
}

/* Test command buffer operations */
int test_command_buffer(void) {
    vizero_editor_state_t* state = vizero_editor_state_create();
    TEST_ASSERT_NOT_NULL(state, "Editor state should be created");
    
    /* Command buffer should start empty */
    const char* cmd = vizero_editor_get_command_buffer(state);
    TEST_ASSERT_NOT_NULL(cmd, "Command buffer should not be NULL");
    TEST_ASSERT_STR_EQUAL("", cmd, "Command buffer should start empty");
    
    /* Test appending to command */
    TEST_ASSERT_EQUAL(0, vizero_editor_append_to_command(state, 'w'), "Should append 'w'");
    cmd = vizero_editor_get_command_buffer(state);
    TEST_ASSERT_STR_EQUAL("w", cmd, "Command buffer should contain 'w'");
    
    /* Test appending more characters */
    TEST_ASSERT_EQUAL(0, vizero_editor_append_to_command(state, 'q'), "Should append 'q'");
    cmd = vizero_editor_get_command_buffer(state);
    TEST_ASSERT_STR_EQUAL("wq", cmd, "Command buffer should contain 'wq'");
    
    /* Test backspace */
    TEST_ASSERT_EQUAL(0, vizero_editor_backspace_command(state), "Should backspace successfully");
    cmd = vizero_editor_get_command_buffer(state);
    TEST_ASSERT_STR_EQUAL("w", cmd, "Command buffer should contain 'w' after backspace");
    
    /* Test clearing buffer */
    vizero_editor_clear_command_buffer(state);
    cmd = vizero_editor_get_command_buffer(state);
    TEST_ASSERT_STR_EQUAL("", cmd, "Command buffer should be empty after clear");
    
    vizero_editor_state_destroy(state);
    return 1;
}

/* Test multi-buffer operations */
int test_multi_buffer(void) {
    vizero_editor_state_t* state = vizero_editor_state_create();
    TEST_ASSERT_NOT_NULL(state, "Editor state should be created");
    
    /* Should start with one buffer */
    TEST_ASSERT_EQUAL(1, vizero_editor_get_buffer_count(state), "Should start with one buffer");
    TEST_ASSERT_EQUAL(0, vizero_editor_get_current_buffer_index(state), "Should start at buffer 0");
    
    /* Create a new buffer */
    TEST_ASSERT_EQUAL(0, vizero_editor_create_new_buffer(state, "test.txt"), "Should create new buffer");
    TEST_ASSERT_EQUAL(2, vizero_editor_get_buffer_count(state), "Should have two buffers");
    TEST_ASSERT_EQUAL(1, vizero_editor_get_current_buffer_index(state), "Should be at buffer 1");
    
    /* Test buffer switching */
    TEST_ASSERT_EQUAL(0, vizero_editor_switch_buffer(state, 0), "Should switch to buffer 0");
    TEST_ASSERT_EQUAL(0, vizero_editor_get_current_buffer_index(state), "Should be at buffer 0");
    
    /* Test next/previous buffer */
    TEST_ASSERT_EQUAL(0, vizero_editor_next_buffer(state), "Should go to next buffer");
    TEST_ASSERT_EQUAL(1, vizero_editor_get_current_buffer_index(state), "Should be at buffer 1");
    
    TEST_ASSERT_EQUAL(0, vizero_editor_previous_buffer(state), "Should go to previous buffer");
    TEST_ASSERT_EQUAL(0, vizero_editor_get_current_buffer_index(state), "Should be at buffer 0");
    
    vizero_editor_state_destroy(state);
    return 1;
}

/* Test cursor operations */
int test_cursor_operations(void) {
    vizero_editor_state_t* state = vizero_editor_state_create();
    TEST_ASSERT_NOT_NULL(state, "Editor state should be created");
    
    vizero_cursor_t* cursor = vizero_editor_get_current_cursor(state);
    TEST_ASSERT_NOT_NULL(cursor, "Should have a cursor");
    
    /* Should start at position 0,0 */
    TEST_ASSERT_EQUAL(0, vizero_cursor_get_line(cursor), "Should start at line 0");
    TEST_ASSERT_EQUAL(0, vizero_cursor_get_column(cursor), "Should start at column 0");
    
    /* Test cursor movement */
    vizero_cursor_move_right(cursor);
    TEST_ASSERT_EQUAL(1, vizero_cursor_get_column(cursor), "Should move to column 1");
    
    vizero_cursor_move_down(cursor);
    TEST_ASSERT_EQUAL(1, vizero_cursor_get_line(cursor), "Should move to line 1");
    
    vizero_cursor_move_left(cursor);
    TEST_ASSERT_EQUAL(0, vizero_cursor_get_column(cursor), "Should move to column 0");
    
    vizero_cursor_move_up(cursor);
    TEST_ASSERT_EQUAL(0, vizero_cursor_get_line(cursor), "Should move to line 0");
    
    /* Test position setting */
    vizero_cursor_set_position(cursor, 5, 10);
    TEST_ASSERT_EQUAL(5, vizero_cursor_get_line(cursor), "Should be at line 5");
    TEST_ASSERT_EQUAL(10, vizero_cursor_get_column(cursor), "Should be at column 10");
    
    vizero_editor_state_destroy(state);
    return 1;
}

/* Main test runner */
int main(void) {
    test_case_t tests[] = {
        {"editor_state_create_destroy", test_editor_state_create_destroy},
        {"editor_mode_switching", test_editor_mode_switching},
        {"command_buffer", test_command_buffer},
        {"multi_buffer", test_multi_buffer},
        {"cursor_operations", test_cursor_operations}
    };
    
    size_t test_count = sizeof(tests) / sizeof(tests[0]);
    return run_test_suite("Editor State Tests", tests, test_count);
}