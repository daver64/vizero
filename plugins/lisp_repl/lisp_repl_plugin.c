/**
 * @file lisp_repl_plugin.c
 * @brief Interactive LISP REPL Plugin for Vizero Editor
 * 
 * This plugin provides seamless integration between Vizero and SBCL (Steel Bank Common Lisp),
 * enabling interactive Lisp development with direct buffer typing, automatic expression 
 * evaluation, and comprehensive vi-style integration.
 * 
 * Current Features (Phase 2):
 * - Interactive buffer typing with real-time expression evaluation
 * - Automatic SBCL detection across Windows and Unix platforms
 * - Robust process management with proper I/O redirection
 * - Vi-style command integration with Escape+colon sequences
 * - Buffer switching support with automatic state restoration
 * - Graceful error handling when SBCL is unavailable
 * - Cross-platform compatibility (Windows, Linux, macOS)
 * 
 * Planned Features (Phase 3):
 * - SLIME protocol integration via :lisp-slime-connect command
 * - Interactive debugger with stack traces and condition handling
 * - Advanced code completion through SLIME backend
 * - Object inspection and modification capabilities
 * - Remote REPL connections for distributed development
 * 
 * @version 2.0.0
 * @date September 2025
 * @author Vizero Development Team
 * 
 * @todo Phase 3: Implement SLIME protocol support with separate connection pathway
 * @todo Add :lisp-slime-connect command handler for SLIME-based connections
 * @todo Integrate Swank server communication for advanced debugging features
 */

#include "vizero/plugin_interface.h"
#include "vizero/renderer.h"
#include "vizero/colour_theme.h"
#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <time.h>
#include <ctype.h>
#include <math.h>

#ifdef _WIN32
    #include <windows.h>
    #include <process.h>
#else
    #include <unistd.h>
    #include <sys/wait.h>
    #include <sys/select.h>
    #include <fcntl.h>
    #include <errno.h>
#endif

#ifdef _WIN32
    #include <windows.h>
    #include <io.h>
    #include <fcntl.h>
    #include <process.h>
    typedef HANDLE process_t;
    typedef HANDLE pipe_t;
    #define INVALID_PROCESS_VALUE NULL
    #define INVALID_PIPE_VALUE INVALID_HANDLE_VALUE
#else
    #include <unistd.h>
    #include <sys/wait.h>
    #include <sys/select.h>
    #include <fcntl.h>
    #include <signal.h>
    #include <errno.h>
    typedef pid_t process_t;
    typedef int pipe_t;
    #define INVALID_PROCESS_VALUE -1
    #define INVALID_PIPE_VALUE -1
#endif

/* Forward declarations */
static void lisp_log_message(const char* message);
static void lisp_set_active_buffer(const char* buffer_name);
static bool read_from_sbcl(char* buffer, size_t buffer_size, int timeout_ms);
static bool send_to_sbcl(const char* command);
static bool send_to_sbcl_silent(const char* command);
static bool send_to_sbcl_silent(const char* command);
static int insert_text_with_newlines(vizero_buffer_t* buffer, size_t line, size_t col, const char* text);
static int lisp_evaluate_interactive(vizero_editor_t* editor, const char* input);
static int lisp_handle_enter_key(vizero_editor_t* editor);
static int lisp_evaluate_current_input(vizero_editor_t* editor);
static int lisp_extract_current_input(char* buffer, size_t buffer_size);



/* Enhanced Lisp REPL Message structure for Phase 2 */
typedef struct {
    char timestamp[16];
    char content[2048];        /* Increased size for complex expressions */
    enum {
        LISP_MSG_INPUT,        /* User input */
        LISP_MSG_OUTPUT,       /* SBCL output */
        LISP_MSG_ERROR,        /* Error messages */
        LISP_MSG_RESULT,       /* Evaluation results */
        LISP_MSG_INFO,         /* System info */
        LISP_MSG_DEBUG,        /* Debug information */
        LISP_MSG_COMPLETION,   /* Code completion */
        LISP_MSG_INSPECTION,   /* Object inspection */
        LISP_MSG_TRACE,        /* Function tracing */
        LISP_MSG_DEBUGGER,     /* Debugger output */
        LISP_MSG_COMPILE       /* Compilation messages */
    } type;
    vizero_colour_t colour;
    int severity;              /* Error/warning severity (0-3) */
    bool is_multiline;         /* Multi-line message flag */
    char source_file[256];     /* Source file for compilation messages */
    int line_number;           /* Line number for errors/warnings */
} lisp_message_t;

/* SLIME Protocol Message Structure */
typedef struct {
    int id;                    /* Message ID for tracking */
    char command[64];          /* SLIME command name */
    char args[1024];           /* Command arguments */
    char package[64];          /* Target package */
    bool async;                /* Asynchronous execution flag */
    time_t timestamp;          /* Creation timestamp */
} slime_message_t;

/* Code Completion Structure */
typedef struct {
    char text[128];            /* Completion text */
    char description[256];     /* Documentation string */
    char type[32];             /* Symbol type (function, variable, etc.) */
    int score;                 /* Relevance score */
} lisp_completion_t;

/* Lisp Symbol Inspector */
typedef struct {
    char symbol[128];          /* Symbol name */
    char type[64];             /* Symbol type */
    char value[512];           /* Current value */
    char documentation[1024];  /* Documentation string */
    char package[64];          /* Symbol package */
    bool is_bound;             /* Symbol bound status */
    bool is_function;          /* Function symbol flag */
} lisp_symbol_info_t;

/* Enhanced SBCL Process Management with SLIME Support */
typedef struct {
    process_t process;
    pipe_t stdin_pipe;
    pipe_t stdout_pipe;
    pipe_t stderr_pipe;
    bool running;
    char sbcl_path[512];
    char working_directory[512];
    
    /* SLIME/Swank integration */
    bool swank_server_running;
    int swank_port;
    char swank_secret[128];
    bool slime_connected;
    
    /* Enhanced communication */
    char read_buffer[8192];    /* Larger buffer for complex responses */
    size_t buffer_pos;
    bool in_multiline_read;
    int paren_depth;
    
    /* Process options */
    int dynamic_space_size;    /* MB of dynamic space */
    bool enable_debugger;      /* Enable Lisp debugger */
    bool load_quicklisp;       /* Auto-load Quicklisp */
    char init_file[512];       /* Custom init file */
} sbcl_process_t;

/* Enhanced Lisp REPL Buffer for Phase 2 */
typedef struct {
    char name[64];
    char display_name[64];
    vizero_buffer_t* text_buffer;
    lisp_message_t messages[5000];    /* Increased message history */
    size_t message_count;
    size_t scroll_offset;
    
    /* Enhanced input system */
    char input_text[2048];            /* Larger input buffer */
    size_t input_cursor;
    char input_history[50][512];      /* Command history */
    size_t history_count;
    int history_index;
    
    /* Advanced REPL state */
    char current_package[64];
    int eval_counter;
    bool multiline_input;
    int paren_depth;
    bool in_string;
    bool in_comment;
    
    /* Code completion */
    lisp_completion_t completions[100];
    size_t completion_count;
    int selected_completion;
    bool showing_completions;
    
    /* Symbol inspection */
    lisp_symbol_info_t current_symbol;
    bool showing_inspection;
    
    /* UI enhancements */
    int window_width;
    int window_height;
    int line_height;
    int char_width;
    bool syntax_highlighting;
    bool show_line_numbers;
    
    /* Debugger state */
    bool in_debugger;
    char debugger_prompt[128];
    int stack_depth;
    char current_restart[256];
} lisp_buffer_t;

/* Enhanced Main REPL State for Phase 2 */
typedef struct {
    /* Core state */
    vizero_editor_t* editor;
    const vizero_editor_api_t* api;
    
    /* Enhanced SBCL process */
    sbcl_process_t sbcl;
    
    /* Vizero buffer integration */
    vizero_buffer_t* repl_buffer;     /* Reference to the actual Vizero REPL buffer */
    
    /* Multiple buffers support */
    lisp_buffer_t* buffers[32];       /* More buffers for different contexts */
    int buffer_count;
    char current_buffer[64];
    
    /* Advanced input handling */
    char input_buffer[2048];          /* Larger input buffer */
    bool in_vi_command;
    bool wants_full_window;
    bool custom_rendering;            /* Enable custom UI rendering */
    
    /* SLIME protocol state */
    slime_message_t pending_messages[10];
    int pending_count;
    int next_message_id;
    
    /* UI state */
    int ui_mode;                      /* 0=normal, 1=completion, 2=inspection, 3=debugger */
    bool show_welcome;
    bool animate_cursor;
    unsigned int last_blink_time;
    
    /* Theme and rendering */
    vizero_colour_t prompt_colour;
    vizero_colour_t input_colour;
    vizero_colour_t output_colour;
    vizero_colour_t error_colour;
    vizero_colour_t completion_colour;
    
    /* Feature flags */
    bool enable_completion;
    bool enable_inspection;
    bool enable_debugger;
    bool enable_tracing;
    bool auto_indent;
    
    /* Performance tracking */
    int total_evaluations;
    double total_eval_time;
    
    /* Original editor state */
    vizero_buffer_t* original_buffer;
    
    /* Interactive REPL state */
    bool interactive_mode;            /* True when in interactive REPL mode */
    bool user_exited_interactive;     /* True when user explicitly exited with Escape */
    bool command_mode_active;         /* True when processing : commands, prevent auto-re-enable */
    bool inserting_sbcl_output;       /* True when inserting SBCL responses (don't count parens) */
    vizero_position_t prompt_start;   /* Position where current prompt starts */
    int paren_balance;                /* Track parentheses balance for auto-evaluation */
    char pending_input[2048];         /* Buffer for accumulating multiline input */
    
} lisp_repl_state_t;

/* Global plugin state */
static lisp_repl_state_t* g_lisp_state = NULL;

/* Helper function to properly insert text with newline handling using line-by-line insertion */
static int insert_text_with_newlines(vizero_buffer_t* buffer, size_t start_line, size_t start_col, const char* text) {
    if (!buffer || !text || !g_lisp_state || !g_lisp_state->api || !g_lisp_state->api->insert_text) return -1;
    
    /* Split text by newlines and insert each line separately */
    char* text_copy = strdup(text);
    if (!text_copy) return -1;
    
    size_t current_line = start_line;
    size_t current_col = start_col;
    
    char* line_start = text_copy;
    char* newline_pos;
    
    while ((newline_pos = strchr(line_start, '\n')) != NULL) {
        /* Terminate current line at newline */
        *newline_pos = '\0';
        
        /* Insert this line segment */
        if (*line_start) {  /* Only insert if not empty */
            vizero_position_t pos = {current_line, current_col};
            if (g_lisp_state->api->insert_text(buffer, pos, line_start) != 0) {
                free(text_copy);
                return -1;
            }
        }
        
        /* Move to next line - simulate Enter key */
        /* For now, just position at next available line */
        size_t line_count = g_lisp_state->api->get_buffer_line_count(buffer);
        current_line = line_count;  /* Use next available line */
        current_col = 0;
        
        line_start = newline_pos + 1;  /* Move past the newline */
    }
    
    /* Insert remaining text (after last newline, if any) */
    if (*line_start) {
        vizero_position_t pos = {current_line, current_col};
        if (g_lisp_state->api->insert_text(buffer, pos, line_start) != 0) {
            free(text_copy);
            return -1;
        }
    }
    
    free(text_copy);
    return 0;
}

/* Enhanced Utility Functions for Phase 2 */

/* Get detailed timestamp with milliseconds */
static void get_timestamp(char* buffer, size_t size) {
    time_t now = time(NULL);
    struct tm* tm_info = localtime(&now);
    strftime(buffer, size, "%H:%M:%S", tm_info);
}

/* Parse Lisp expression for balanced parentheses */
static int count_parens(const char* expr) {
    int depth = 0;
    bool in_string = false;
    bool in_comment = false;
    
    for (const char* p = expr; *p; p++) {
        if (in_comment) {
            if (*p == '\n') in_comment = false;
            continue;
        }
        
        if (in_string) {
            if (*p == '"' && (p == expr || *(p-1) != '\\')) {
                in_string = false;
            }
            continue;
        }
        
        switch (*p) {
            case '"': in_string = true; break;
            case ';': in_comment = true; break;
            case '(': depth++; break;
            case ')': depth--; break;
        }
    }
    
    return depth;
}

/* Extract symbol at cursor position for completion */
static bool extract_symbol_at_cursor(const char* text, size_t cursor_pos, char* symbol, size_t symbol_size) {
    if (!text || cursor_pos >= strlen(text)) return false;
    
    /* Find start of symbol */
    size_t start = cursor_pos;
    while (start > 0 && (isalnum(text[start-1]) || text[start-1] == '-' || text[start-1] == '*' || text[start-1] == '+')) {
        start--;
    }
    
    /* Find end of symbol */
    size_t end = cursor_pos;
    size_t len = strlen(text);
    while (end < len && (isalnum(text[end]) || text[end] == '-' || text[end] == '*' || text[end] == '+')) {
        end++;
    }
    
    /* Extract symbol */
    size_t symbol_len = end - start;
    if (symbol_len == 0 || symbol_len >= symbol_size) return false;
    
    strncpy(symbol, text + start, symbol_len);
    symbol[symbol_len] = '\0';
    return true;
}

/* Format Lisp code with proper indentation */
static void format_lisp_code(const char* input, char* output, size_t output_size) {
    if (!input || !output || output_size == 0) return;
    
    size_t input_len = strlen(input);
    size_t out_pos = 0;
    int indent_level = 0;
    bool at_line_start = true;
    
    for (size_t i = 0; i < input_len && out_pos < output_size - 1; i++) {
        char c = input[i];
        
        if (c == '(') {
            if (at_line_start) {
                /* Add indentation */
                for (int j = 0; j < indent_level * 2 && out_pos < output_size - 1; j++) {
                    output[out_pos++] = ' ';
                }
                at_line_start = false;
            }
            output[out_pos++] = c;
            indent_level++;
        } else if (c == ')') {
            indent_level--;
            output[out_pos++] = c;
        } else if (c == '\n') {
            output[out_pos++] = c;
            at_line_start = true;
        } else if (c == ' ' && at_line_start) {
            /* Skip leading spaces, we'll add our own indentation */
            continue;
        } else {
            if (at_line_start) {
                /* Add indentation */
                for (int j = 0; j < indent_level * 2 && out_pos < output_size - 1; j++) {
                    output[out_pos++] = ' ';
                }
                at_line_start = false;
            }
            output[out_pos++] = c;
        }
    }
    
    output[out_pos] = '\0';
}

/* Generate SLIME protocol message */
static int generate_slime_message(slime_message_t* msg, const char* command, const char* args, const char* package) {
    if (!msg || !command) return -1;
    
    msg->id = g_lisp_state ? g_lisp_state->next_message_id++ : 1;
    strncpy(msg->command, command, sizeof(msg->command) - 1);
    msg->command[sizeof(msg->command) - 1] = '\0';
    
    if (args) {
        strncpy(msg->args, args, sizeof(msg->args) - 1);
        msg->args[sizeof(msg->args) - 1] = '\0';
    } else {
        msg->args[0] = '\0';
    }
    
    if (package) {
        strncpy(msg->package, package, sizeof(msg->package) - 1);
        msg->package[sizeof(msg->package) - 1] = '\0';
    } else {
        strcpy(msg->package, "CL-USER");
    }
    
    msg->async = false;
    msg->timestamp = time(NULL);
    
    return msg->id;
}

/* Utility: Log message to main REPL buffer */
static void lisp_log_message(const char* message) {
    /* Always log to console for debugging */
    printf("[LISP] %s\n", message);
    
    /* If we have a REPL buffer, also write to it */
    if (g_lisp_state && g_lisp_state->repl_buffer && g_lisp_state->api && g_lisp_state->api->insert_text) {
        /* Get current buffer position (end of buffer) */
        size_t line_count = 0;
        if (g_lisp_state->api->get_buffer_line_count) {
            line_count = g_lisp_state->api->get_buffer_line_count(g_lisp_state->repl_buffer);
        }
        
        /* Format message with newline */
        char formatted_msg[2048];
        snprintf(formatted_msg, sizeof(formatted_msg), "%s\n", message);
        
        /* Insert at end of buffer */
        vizero_position_t pos = {line_count, 0};
        g_lisp_state->api->insert_text(g_lisp_state->repl_buffer, pos, formatted_msg);
    }
    
    /* Also maintain internal message history for backwards compatibility */
    if (g_lisp_state && g_lisp_state->buffer_count > 0) {
        lisp_buffer_t* main_buffer = g_lisp_state->buffers[0];
        if (main_buffer->message_count >= 2000) {
            /* Shift messages to make room */
            memmove(main_buffer->messages, main_buffer->messages + 100, 
                    sizeof(lisp_message_t) * 1900);
            main_buffer->message_count = 1900;
        }
        
        lisp_message_t* msg = &main_buffer->messages[main_buffer->message_count++];
        get_timestamp(msg->timestamp, sizeof(msg->timestamp));
        strncpy(msg->content, message, sizeof(msg->content) - 1);
        msg->content[sizeof(msg->content) - 1] = '\0';
        msg->type = LISP_MSG_INFO;
        
        /* Set default colour */
        msg->colour = (vizero_colour_t){128, 128, 128, 255};
    }
}

/**
 * @brief Detects SBCL installation on the system
 * 
 * Searches for SBCL executable in common installation locations across Windows and Unix platforms.
 * Checks local Vizero directory first, then system PATH, then standard installation directories.
 * 
 * @param sbcl_path Buffer to store the detected SBCL executable path
 * @param path_size Size of the sbcl_path buffer
 * @return true if SBCL installation found, false otherwise
 */
static bool detect_sbcl_installation(char* sbcl_path, size_t path_size) {
    const char* possible_paths[] = {
        "sbcl\\sbcl.exe",                         /* Local Vizero directory */
        ".\\sbcl\\sbcl.exe",                      /* Relative to current dir */
        "sbcl",                                    /* In PATH */
        "sbcl.exe",                               /* Windows in PATH */
        "C:\\Program Files\\Steel Bank Common Lisp\\sbcl.exe",    /* Windows default */
        "C:\\Program Files (x86)\\Steel Bank Common Lisp\\sbcl.exe",
        "/usr/local/bin/sbcl",                    /* Unix/Linux common */
        "/usr/bin/sbcl",                          /* System install */
        "/opt/sbcl/bin/sbcl",                     /* Manual install */
        "/home/user/.local/bin/sbcl",             /* User install */
        NULL
    };
    
    for (int i = 0; possible_paths[i] != NULL; i++) {
        /* Try to execute with --version flag */
#ifdef _WIN32
        char test_cmd[1024];
        snprintf(test_cmd, sizeof(test_cmd), "\"%s\" --version >nul 2>&1", possible_paths[i]);
        if (system(test_cmd) == 0) {
            strncpy(sbcl_path, possible_paths[i], path_size - 1);
            sbcl_path[path_size - 1] = '\0';
            return true;
        }
#else
        char test_cmd[1024];
        snprintf(test_cmd, sizeof(test_cmd), "%s --version >/dev/null 2>&1", possible_paths[i]);
        if (system(test_cmd) == 0) {
            strncpy(sbcl_path, possible_paths[i], path_size - 1);
            sbcl_path[path_size - 1] = '\0';
            return true;
        }
#endif
    }
    
    return false;
}

/**
 * @brief Starts SBCL process with enhanced configuration
 * 
 * Initializes and starts an SBCL subprocess with proper I/O redirection, optimized settings,
 * and cross-platform process management. Sets up pipes for communication and configures
 * SBCL with no debugger, custom dynamic space size, and clean initialization.
 * 
 * @param proc Pointer to sbcl_process_t structure to initialize
 * @return true if process started successfully, false on failure
 */
static bool start_sbcl_process(sbcl_process_t* proc) {
    if (!proc->sbcl_path[0]) {
        lisp_log_message("ERROR: No SBCL path configured");
        return false;
    }
    
    lisp_log_message("Starting enhanced SBCL process with Swank support...");
    
    /* Initialize enhanced process state */
    proc->swank_server_running = false;
    proc->swank_port = 4005;  /* Default SLIME port */
    proc->slime_connected = false;
    proc->buffer_pos = 0;
    proc->in_multiline_read = false;
    proc->paren_depth = 0;
    proc->dynamic_space_size = proc->dynamic_space_size > 0 ? proc->dynamic_space_size : 512;
    
#ifdef _WIN32
    SECURITY_ATTRIBUTES sa;
    sa.nLength = sizeof(SECURITY_ATTRIBUTES);
    sa.bInheritHandle = TRUE;
    sa.lpSecurityDescriptor = NULL;
    
    HANDLE stdin_read, stdin_write;
    HANDLE stdout_read, stdout_write;
    HANDLE stderr_read, stderr_write;
    
    /* Create pipes */
    if (!CreatePipe(&stdin_read, &stdin_write, &sa, 0) ||
        !CreatePipe(&stdout_read, &stdout_write, &sa, 0) ||
        !CreatePipe(&stderr_read, &stderr_write, &sa, 0)) {
        lisp_log_message("ERROR: Failed to create pipes");
        return false;
    }
    
    /* Make sure write handles are not inherited */
    SetHandleInformation(stdin_write, HANDLE_FLAG_INHERIT, 0);
    SetHandleInformation(stdout_read, HANDLE_FLAG_INHERIT, 0);
    SetHandleInformation(stderr_read, HANDLE_FLAG_INHERIT, 0);
    
    /* Setup process startup info */
    STARTUPINFO si;
    PROCESS_INFORMATION pi;
    ZeroMemory(&si, sizeof(si));
    si.cb = sizeof(si);
    si.hStdError = stderr_write;
    si.hStdOutput = stdout_write;
    si.hStdInput = stdin_read;
    si.dwFlags |= STARTF_USESTDHANDLES;
    
    /* Create enhanced SBCL command line with Swank support */
    char cmdline[1024];
    snprintf(cmdline, sizeof(cmdline), 
             "\"%s\" --dynamic-space-size %d --disable-debugger --no-sysinit --no-userinit", 
             proc->sbcl_path, proc->dynamic_space_size);
    
    /* Start the process */
    if (!CreateProcess(NULL, cmdline, NULL, NULL, TRUE, 0, NULL, 
                      proc->working_directory[0] ? proc->working_directory : NULL, &si, &pi)) {
        lisp_log_message("ERROR: Failed to start SBCL process");
        CloseHandle(stdin_read);
        CloseHandle(stdin_write);
        CloseHandle(stdout_read);
        CloseHandle(stdout_write);
        CloseHandle(stderr_read);
        CloseHandle(stderr_write);
        return false;
    }
    
    /* Store handles */
    proc->process = pi.hProcess;
    proc->stdin_pipe = stdin_write;
    proc->stdout_pipe = stdout_read;
    proc->stderr_pipe = stderr_read;
    
    /* Close unused handles */
    CloseHandle(pi.hThread);
    CloseHandle(stdin_read);
    CloseHandle(stdout_write);
    CloseHandle(stderr_write);
    
#else
    int stdin_pipe[2], stdout_pipe[2], stderr_pipe[2];
    
    /* Create pipes */
    if (pipe(stdin_pipe) == -1 || pipe(stdout_pipe) == -1 || pipe(stderr_pipe) == -1) {
        lisp_log_message("ERROR: Failed to create pipes");
        return false;
    }
    
    /* Fork process */
    pid_t pid = fork();
    if (pid == -1) {
        lisp_log_message("ERROR: Failed to fork process");
        return false;
    }
    
    if (pid == 0) {
        /* Child process */
        close(stdin_pipe[1]);   /* Close write end of stdin */
        close(stdout_pipe[0]);  /* Close read end of stdout */
        close(stderr_pipe[0]);  /* Close read end of stderr */
        
        /* Redirect standard streams */
        dup2(stdin_pipe[0], STDIN_FILENO);
        dup2(stdout_pipe[1], STDOUT_FILENO);
        dup2(stderr_pipe[1], STDERR_FILENO);
        
        /* Close remaining pipe ends */
        close(stdin_pipe[0]);
        close(stdout_pipe[1]);
        close(stderr_pipe[1]);
        
        /* Change directory if specified */
        if (proc->working_directory[0]) {
            chdir(proc->working_directory);
        }
        
        /* Execute enhanced SBCL */
        char dynamic_space[16];
        snprintf(dynamic_space, sizeof(dynamic_space), "%d", proc->dynamic_space_size);
        execl(proc->sbcl_path, "sbcl", 
              "--dynamic-space-size", dynamic_space,
              "--disable-debugger", 
              "--no-sysinit", 
              "--no-userinit", 
              NULL);
        
        /* If we get here, exec failed */
        _exit(1);
    } else {
        /* Parent process */
        close(stdin_pipe[0]);   /* Close read end of stdin */
        close(stdout_pipe[1]);  /* Close write end of stdout */
        close(stderr_pipe[1]);  /* Close write end of stderr */
        
        /* Store handles */
        proc->process = pid;
        proc->stdin_pipe = stdin_pipe[1];
        proc->stdout_pipe = stdout_pipe[0];
        proc->stderr_pipe = stderr_pipe[0];
        
        /* Make pipes non-blocking */
        fcntl(proc->stdout_pipe, F_SETFL, O_NONBLOCK);
        fcntl(proc->stderr_pipe, F_SETFL, O_NONBLOCK);
    }
#endif
    
    proc->running = true;
    lisp_log_message("Enhanced SBCL process started successfully");
    
    /* SBCL is now ready with default settings - no extra initialization needed */
    
    lisp_log_message("SBCL environment initialized with Phase 2 enhancements");
    return true;
}

/* Stop SBCL process */
static void stop_sbcl_process(sbcl_process_t* proc) {
    if (!proc->running) return;
    
    lisp_log_message("Stopping SBCL process...");
    
#ifdef _WIN32
    if (proc->process != INVALID_PROCESS_VALUE) {
        TerminateProcess(proc->process, 0);
        WaitForSingleObject(proc->process, 5000);  /* Wait up to 5 seconds */
        CloseHandle(proc->process);
        proc->process = INVALID_PROCESS_VALUE;
    }
    
    if (proc->stdin_pipe != INVALID_PIPE_VALUE) {
        CloseHandle(proc->stdin_pipe);
        proc->stdin_pipe = INVALID_PIPE_VALUE;
    }
    if (proc->stdout_pipe != INVALID_PIPE_VALUE) {
        CloseHandle(proc->stdout_pipe);
        proc->stdout_pipe = INVALID_PIPE_VALUE;
    }
    if (proc->stderr_pipe != INVALID_PIPE_VALUE) {
        CloseHandle(proc->stderr_pipe);
        proc->stderr_pipe = INVALID_PIPE_VALUE;
    }
#else
    if (proc->process != INVALID_PROCESS_VALUE) {
        kill(proc->process, SIGTERM);
        
        /* Wait for process to terminate */
        int status;
        int result = waitpid(proc->process, &status, WNOHANG);
        if (result == 0) {
            /* Process still running, force kill */
            sleep(2);
            kill(proc->process, SIGKILL);
            waitpid(proc->process, &status, 0);
        }
        proc->process = INVALID_PROCESS_VALUE;
    }
    
    if (proc->stdin_pipe != INVALID_PIPE_VALUE) {
        close(proc->stdin_pipe);
        proc->stdin_pipe = INVALID_PIPE_VALUE;
    }
    if (proc->stdout_pipe != INVALID_PIPE_VALUE) {
        close(proc->stdout_pipe);
        proc->stdout_pipe = INVALID_PIPE_VALUE;
    }
    if (proc->stderr_pipe != INVALID_PIPE_VALUE) {
        close(proc->stderr_pipe);
        proc->stderr_pipe = INVALID_PIPE_VALUE;
    }
#endif
    
    proc->running = false;
    lisp_log_message("SBCL process stopped");
}

/* Send command to SBCL */
static bool read_from_sbcl(char* buffer, size_t buffer_size, int timeout_ms) {
    if (!g_lisp_state->sbcl.running) {
        return false;
    }
    
#ifdef _WIN32
    DWORD bytes_available = 0;
    if (!PeekNamedPipe(g_lisp_state->sbcl.stdout_pipe, NULL, 0, NULL, &bytes_available, NULL)) {
        return false;
    }
    
    if (bytes_available > 0) {
        DWORD bytes_read;
        DWORD to_read = (bytes_available < buffer_size - 1) ? bytes_available : (DWORD)(buffer_size - 1);
        if (ReadFile(g_lisp_state->sbcl.stdout_pipe, buffer, to_read, &bytes_read, NULL)) {
            buffer[bytes_read] = '\0';
            return bytes_read > 0;
        }
    }
#else
    /* Non-blocking read on Unix */
    fd_set readfds;
    struct timeval tv;
    
    FD_ZERO(&readfds);
    FD_SET(g_lisp_state->sbcl.stdout_pipe, &readfds);
    
    tv.tv_sec = timeout_ms / 1000;
    tv.tv_usec = (timeout_ms % 1000) * 1000;
    
    int ready = select(g_lisp_state->sbcl.stdout_pipe + 1, &readfds, NULL, NULL, &tv);
    if (ready > 0 && FD_ISSET(g_lisp_state->sbcl.stdout_pipe, &readfds)) {
        ssize_t bytes_read = read(g_lisp_state->sbcl.stdout_pipe, buffer, buffer_size - 1);
        if (bytes_read > 0) {
            buffer[bytes_read] = '\0';
            return true;
        }
    }
#endif
    
    return false;
}

static bool send_to_sbcl(const char* command) {
    if (!g_lisp_state->sbcl.running) {
        lisp_log_message("ERROR: SBCL process not running");
        return false;
    }
    
    char full_command[1024];
    snprintf(full_command, sizeof(full_command), "%s\n", command);
    
#ifdef _WIN32
    DWORD bytes_written;
    if (!WriteFile(g_lisp_state->sbcl.stdin_pipe, full_command, 
                   (DWORD)strlen(full_command), &bytes_written, NULL)) {
        lisp_log_message("ERROR: Failed to write to SBCL process");
        return false;
    }
#else
    ssize_t bytes_written = write(g_lisp_state->sbcl.stdin_pipe, full_command, strlen(full_command));
    if (bytes_written == -1) {
        lisp_log_message("ERROR: Failed to write to SBCL process");
        return false;
    }
#endif
    
    return true;
}

static bool send_to_sbcl_silent(const char* command) {
    if (!g_lisp_state->sbcl.running) {
        lisp_log_message("ERROR: SBCL process not running");
        return false;
    }
    
    char full_command[1024];
    snprintf(full_command, sizeof(full_command), "%s\n", command);
    
#ifdef _WIN32
    DWORD bytes_written;
    if (!WriteFile(g_lisp_state->sbcl.stdin_pipe, full_command, strlen(full_command), &bytes_written, NULL)) {
        lisp_log_message("ERROR: Failed to write to SBCL process");
        return false;
    }
#else
    ssize_t bytes_written = write(g_lisp_state->sbcl.stdin_pipe, full_command, strlen(full_command));
    if (bytes_written == -1) {
        lisp_log_message("ERROR: Failed to write to SBCL process");
        return false;
    }
#endif
    
    /* Sleep briefly to let SBCL process the command but don't read the output */
#ifdef _WIN32
    Sleep(50);
#else
    usleep(50000);
#endif
    
    return true;
}

/* Enhanced SBCL output reading with multi-line support */
static void read_sbcl_output(void) {
    if (!g_lisp_state->sbcl.running) return;
    
    char temp_buffer[4096];
    sbcl_process_t* proc = &g_lisp_state->sbcl;
    
#ifdef _WIN32
    DWORD bytes_available = 0;
    DWORD bytes_read = 0;
    
    /* Check stdout with enhanced buffering */
    if (PeekNamedPipe(proc->stdout_pipe, NULL, 0, NULL, &bytes_available, NULL) && bytes_available > 0) {
        if (ReadFile(proc->stdout_pipe, temp_buffer, sizeof(temp_buffer) - 1, &bytes_read, NULL) && bytes_read > 0) {
            temp_buffer[bytes_read] = '\0';
            
            /* Append to read buffer */
            size_t remaining = sizeof(proc->read_buffer) - proc->buffer_pos - 1;
            if (remaining > 0) {
                size_t to_copy = bytes_read < remaining ? bytes_read : remaining;
                memcpy(proc->read_buffer + proc->buffer_pos, temp_buffer, to_copy);
                proc->buffer_pos += to_copy;
                proc->read_buffer[proc->buffer_pos] = '\0';
            }
        }
    }
    
    /* Check stderr */
    if (PeekNamedPipe(proc->stderr_pipe, NULL, 0, NULL, &bytes_available, NULL) && bytes_available > 0) {
        if (ReadFile(proc->stderr_pipe, temp_buffer, sizeof(temp_buffer) - 1, &bytes_read, NULL) && bytes_read > 0) {
            temp_buffer[bytes_read] = '\0';
            
            /* Create error message */
            if (g_lisp_state->buffer_count > 0) {
                lisp_buffer_t* buffer = g_lisp_state->buffers[0];
                if (buffer->message_count < 5000) {
                    lisp_message_t* msg = &buffer->messages[buffer->message_count++];
                    get_timestamp(msg->timestamp, sizeof(msg->timestamp));
                    strncpy(msg->content, temp_buffer, sizeof(msg->content) - 1);
                    msg->content[sizeof(msg->content) - 1] = '\0';
                    msg->type = LISP_MSG_ERROR;
                    msg->colour = (vizero_colour_t){255, 100, 100, 255};  /* Red for errors */
                    msg->severity = 2;  /* Error level */
                }
            }
        }
    }
#else
    ssize_t bytes_read;
    
    /* Enhanced stdout reading */
    bytes_read = read(proc->stdout_pipe, temp_buffer, sizeof(temp_buffer) - 1);
    if (bytes_read > 0) {
        temp_buffer[bytes_read] = '\0';
        
        /* Append to read buffer */
        size_t remaining = sizeof(proc->read_buffer) - proc->buffer_pos - 1;
        if (remaining > 0) {
            size_t to_copy = bytes_read < remaining ? bytes_read : remaining;
            memcpy(proc->read_buffer + proc->buffer_pos, temp_buffer, to_copy);
            proc->buffer_pos += to_copy;
            proc->read_buffer[proc->buffer_pos] = '\0';
        }
    }
    
    /* Enhanced stderr reading */
    bytes_read = read(proc->stderr_pipe, temp_buffer, sizeof(temp_buffer) - 1);
    if (bytes_read > 0) {
        temp_buffer[bytes_read] = '\0';
        
        /* Create error message */
        if (g_lisp_state->buffer_count > 0) {
            lisp_buffer_t* buffer = g_lisp_state->buffers[0];
            if (buffer->message_count < 5000) {
                lisp_message_t* msg = &buffer->messages[buffer->message_count++];
                get_timestamp(msg->timestamp, sizeof(msg->timestamp));
                strncpy(msg->content, temp_buffer, sizeof(msg->content) - 1);
                msg->content[sizeof(msg->content) - 1] = '\0';
                msg->type = LISP_MSG_ERROR;
                msg->colour = (vizero_colour_t){255, 100, 100, 255};  /* Red for errors */
                msg->severity = 2;  /* Error level */
            }
        }
    }
#endif

    /* Process accumulated output for complete messages */
    if (proc->buffer_pos > 0) {
        /* Look for complete lines or expressions */
        char* line_start = proc->read_buffer;
        char* line_end;
        
        while ((line_end = strchr(line_start, '\n')) != NULL) {
            *line_end = '\0';  /* Terminate line */
            
            /* Process complete line */
            if (strlen(line_start) > 0) {
                lisp_log_message(line_start);
            }
            
            line_start = line_end + 1;
        }
        
        /* Move remaining partial line to beginning of buffer */
        if (line_start < proc->read_buffer + proc->buffer_pos) {
            size_t remaining = strlen(line_start);
            memmove(proc->read_buffer, line_start, remaining + 1);
            proc->buffer_pos = remaining;
        } else {
            proc->buffer_pos = 0;
            proc->read_buffer[0] = '\0';
        }
    }
}

/* Create REPL buffer */
static lisp_buffer_t* create_lisp_buffer(const char* name, const char* display_name) {
    lisp_buffer_t* buffer = calloc(1, sizeof(lisp_buffer_t));
    if (!buffer) return NULL;
    
    strncpy(buffer->name, name, sizeof(buffer->name) - 1);
    strncpy(buffer->display_name, display_name, sizeof(buffer->display_name) - 1);
    buffer->message_count = 0;
    buffer->scroll_offset = 0;
    buffer->input_cursor = 0;
    buffer->eval_counter = 0;
    buffer->multiline_input = false;
    buffer->paren_depth = 0;
    strcpy(buffer->current_package, "CL-USER");
    
    /* Initialize with welcome message */
    lisp_message_t* welcome = &buffer->messages[buffer->message_count++];
    get_timestamp(welcome->timestamp, sizeof(welcome->timestamp));
    strcpy(welcome->content, "Lisp REPL - Phase 1 (SBCL Integration)");
    welcome->type = LISP_MSG_INFO;
    welcome->colour = (vizero_colour_t){100, 200, 100, 255};
    
    return buffer;
}

/**
 * @brief Command handler for :lisp-connect
 * 
 * Handles the :lisp-connect command to establish connection to SBCL and enter interactive mode.
 * Detects SBCL installation, starts the subprocess, creates the REPL buffer, and enables
 * interactive typing mode with automatic expression evaluation.
 * 
 * @param editor Pointer to the Vizero editor instance
 * @param args Command arguments (unused)
 * @return 0 on success, -1 on failure
 */
static int lisp_cmd_connect(vizero_editor_t* editor, const char* args) {
    if (g_lisp_state->sbcl.running) {
        lisp_log_message("SBCL is already running");
        return 0;
    }
    
    /* Detect SBCL if not already found */
    if (!g_lisp_state->sbcl.sbcl_path[0]) {
        if (!detect_sbcl_installation(g_lisp_state->sbcl.sbcl_path, sizeof(g_lisp_state->sbcl.sbcl_path))) {
            lisp_log_message("ERROR: SBCL not found. Please install SBCL or add it to PATH");
            return -1;
        }
        
        char msg[1024];
        snprintf(msg, sizeof(msg), "Found SBCL at: %s", g_lisp_state->sbcl.sbcl_path);
        lisp_log_message(msg);
    }
    
    /* Start SBCL process */
    if (!start_sbcl_process(&g_lisp_state->sbcl)) {
        return -1;
    }
    
    /* Save reference to current buffer BEFORE creating new one */
    if (g_lisp_state->api && g_lisp_state->api->get_current_buffer) {
        g_lisp_state->original_buffer = g_lisp_state->api->get_current_buffer(editor);
        const char* orig_filename = NULL;
        if (g_lisp_state->api->get_buffer_filename) {
            orig_filename = g_lisp_state->api->get_buffer_filename(g_lisp_state->original_buffer);
        }
        printf("[LISP] Saved reference to original buffer: %s\n", orig_filename ? orig_filename : "<unnamed>");
    }
    
    /* Create a new empty buffer using enew command */
    if (g_lisp_state->api && g_lisp_state->api->execute_command) {
        printf("[LISP] Executing enew command to create new buffer...\n");
        int result = g_lisp_state->api->execute_command(editor, "enew");
        printf("[LISP] enew returned: %d\n", result);
        
        /* Check what buffer we're currently in immediately after enew */
        vizero_buffer_t* current_buffer = g_lisp_state->api->get_current_buffer(editor);
        const char* current_filename = NULL;
        if (g_lisp_state->api->get_buffer_filename) {
            current_filename = g_lisp_state->api->get_buffer_filename(current_buffer);
        }
        printf("[LISP] Buffer immediately after enew: %s\n", current_filename ? current_filename : "<null>");
        
        /* If we're still in the original buffer, try switching by name */
        if (current_buffer == g_lisp_state->original_buffer) {
            
            printf("[LISP] Still in original buffer, trying name-based switching...\n");
            
            /* Try different buffer name patterns without brackets */
            result = g_lisp_state->api->execute_command(editor, "buffer No Name");
            printf("[LISP] 'buffer No Name' returned: %d\n", result);
            
            if (result != 0) {
                result = g_lisp_state->api->execute_command(editor, "buffer unnamed");
                printf("[LISP] 'buffer unnamed' returned: %d\n", result);
            }
            
            if (result != 0) {
                result = g_lisp_state->api->execute_command(editor, "buffer noname");
                printf("[LISP] 'buffer noname' returned: %d\n", result);
            }
            
            if (result != 0) {
                result = g_lisp_state->api->execute_command(editor, "b No Name");
                printf("[LISP] 'b No Name' returned: %d\n", result);
            }
            
            if (result != 0) {
                result = g_lisp_state->api->execute_command(editor, "edit No Name");
                printf("[LISP] 'edit No Name' returned: %d\n", result);
            }
        }
        
        /* Check what buffer we're now in */
        if (g_lisp_state->api->get_current_buffer) {
            vizero_buffer_t* current_buffer = g_lisp_state->api->get_current_buffer(editor);
            const char* current_filename = NULL;
            if (g_lisp_state->api->get_buffer_filename) {
                current_filename = g_lisp_state->api->get_buffer_filename(current_buffer);
            }
            
            printf("[LISP] Current buffer after bn: %s\n", current_filename ? current_filename : "<null>");
            printf("[LISP] Buffer pointer: %p, Original pointer: %p\n", 
                   (void*)current_buffer, (void*)g_lisp_state->original_buffer);
            
            /* Set this as our REPL buffer regardless and add content */
            g_lisp_state->repl_buffer = current_buffer;
            
            /* Always try to add REPL content */
            if (g_lisp_state->api->insert_text) {
                printf("[LISP] Inserting REPL welcome text...\n");
                vizero_position_t pos = {0, 0};
                int insert_result;
                if (g_lisp_state->api->insert_text_multiline) {
                    insert_result = g_lisp_state->api->insert_text_multiline(current_buffer, pos, 
                        "; Vizero Lisp REPL - Connected to SBCL\n");
                } else {
                    insert_result = g_lisp_state->api->insert_text(current_buffer, pos, 
                        "; Vizero Lisp REPL - Connected to SBCL\n");
                }
                printf("[LISP] insert_text returned: %d\n", insert_result);
            }
            
            printf("[LISP] REPL setup complete - check noname buffer for content\n");
        }
    }
    
    /* Send initial setup commands to SBCL */
    send_to_sbcl("(force-output)");
    
    /* Wait for and read SBCL response */
#ifdef _WIN32
    Sleep(200); /* Wait 200ms for SBCL to process and respond */
#else
    usleep(200000); /* Wait 200ms for SBCL to process and respond */
#endif
    
    char sbcl_response[1024];
    if (read_from_sbcl(sbcl_response, sizeof(sbcl_response), 1000)) {
        /* Clean up SBCL response - remove control characters and fix line endings */
        char cleaned_response[1024];
        size_t cleaned_len = 0;
        bool last_was_cr = false;
        
        printf("[LISP] Raw SBCL startup response (%zu chars): ", strlen(sbcl_response));
        for (size_t j = 0; j < strlen(sbcl_response) && j < 20; j++) {
            printf("%02X ", (unsigned char)sbcl_response[j]);
        }
        printf("\n");
        
        for (size_t i = 0; i < strlen(sbcl_response) && cleaned_len < sizeof(cleaned_response) - 1; i++) {
            unsigned char c = (unsigned char)sbcl_response[i];
            
            if (c == '\r') {
                /* Windows line ending - convert to \n and skip following \n if present */
                if (i + 1 < strlen(sbcl_response) && sbcl_response[i + 1] == '\n') {
                    i++; /* Skip the \n after \r */
                }
                cleaned_response[cleaned_len++] = '\n';
                last_was_cr = false;
            } else if (c == '\n') {
                /* Unix line ending */
                cleaned_response[cleaned_len++] = '\n';
                last_was_cr = false;
            } else if ((c >= 32 && c <= 126) || c == '\t') {
                /* Printable ASCII characters and tab */
                cleaned_response[cleaned_len++] = c;
                last_was_cr = false;
            }
            /* Skip all other control characters (including escape sequences) */
        }
        cleaned_response[cleaned_len] = '\0';
        
        /* Append cleaned SBCL response to the REPL buffer */
        if (g_lisp_state->repl_buffer && g_lisp_state->api && g_lisp_state->api->insert_text) {
            size_t line_count = g_lisp_state->api->get_buffer_line_count(g_lisp_state->repl_buffer);
            vizero_position_t append_pos;
            append_pos.line = line_count > 0 ? line_count - 1 : 0;
            append_pos.column = 0;
            
            /* Find the end of the last line */
            if (line_count > 0 && g_lisp_state->api->get_buffer_line_length) {
                append_pos.column = g_lisp_state->api->get_buffer_line_length(g_lisp_state->repl_buffer, line_count - 1);
            }
            
            g_lisp_state->inserting_sbcl_output = true;
            if (g_lisp_state->api->insert_text_multiline) {
                g_lisp_state->api->insert_text_multiline(g_lisp_state->repl_buffer, append_pos, cleaned_response);
            } else {
                g_lisp_state->api->insert_text(g_lisp_state->repl_buffer, append_pos, cleaned_response);
            }
            g_lisp_state->inserting_sbcl_output = false;
            printf("[LISP] SBCL response appended (%zu chars): '%s'\n", strlen(cleaned_response), cleaned_response);
            printf("[LISP] Original response had %zu chars, cleaned has %zu chars\n", strlen(sbcl_response), strlen(cleaned_response));
            
            /* Position cursor at the end of the inserted text */
            if (g_lisp_state->api->get_current_cursor && g_lisp_state->api->set_cursor_position) {
                vizero_cursor_t* cursor = g_lisp_state->api->get_current_cursor(editor);
                if (cursor) {
                    size_t final_line_count = g_lisp_state->api->get_buffer_line_count(g_lisp_state->repl_buffer);
                    vizero_position_t cursor_pos;
                    cursor_pos.line = final_line_count > 0 ? final_line_count - 1 : 0;
                    cursor_pos.column = 0;
                    
                    /* Find the end of the last line for cursor positioning */
                    if (final_line_count > 0 && g_lisp_state->api->get_buffer_line_length) {
                        cursor_pos.column = g_lisp_state->api->get_buffer_line_length(g_lisp_state->repl_buffer, cursor_pos.line);
                    }
                    
                    int cursor_result = g_lisp_state->api->set_cursor_position(cursor, cursor_pos);
                    printf("[LISP] Cursor positioned at line %zu, column %zu (result: %d)\n", cursor_pos.line, cursor_pos.column, cursor_result);
                }
            }
        }
    }
    
    /* Enable interactive REPL mode */
    g_lisp_state->interactive_mode = true;
    g_lisp_state->user_exited_interactive = false;
    g_lisp_state->command_mode_active = false;
    g_lisp_state->inserting_sbcl_output = false;
    g_lisp_state->paren_balance = 0;
    
    /* Set prompt start position and cursor at the very end of the buffer */
    if (g_lisp_state->repl_buffer && g_lisp_state->api->get_buffer_line_count) {
        size_t line_count = g_lisp_state->api->get_buffer_line_count(g_lisp_state->repl_buffer);
        if (line_count > 0) {
            size_t last_line = line_count - 1;
            size_t last_line_length = 0;
            
            /* Get the actual length of the last line */
            if (g_lisp_state->api->get_buffer_line_length) {
                last_line_length = g_lisp_state->api->get_buffer_line_length(g_lisp_state->repl_buffer, last_line);
            }
            
            g_lisp_state->prompt_start.line = last_line;
            g_lisp_state->prompt_start.column = last_line_length;
            
            /* Position cursor at the very end of the buffer */
            if (g_lisp_state->api->get_current_cursor && g_lisp_state->api->set_cursor_position) {
                vizero_cursor_t* cursor = g_lisp_state->api->get_current_cursor(editor);
                if (cursor) {
                    vizero_position_t cursor_pos;
                    cursor_pos.line = last_line;
                    cursor_pos.column = last_line_length;
                    int cursor_result = g_lisp_state->api->set_cursor_position(cursor, cursor_pos);
                    printf("[LISP] Cursor positioned at end of buffer (%zu, %zu), result: %d\n", 
                           last_line, last_line_length, cursor_result);
                }
            }
            
            printf("[LISP] Interactive mode enabled, cursor at end of buffer (%zu, %zu)\n", 
                   last_line, last_line_length);
        }
    }
    
    return 0;
}

/**
 * @brief Command handler for :lisp-disconnect
 * 
 * Handles the :lisp-disconnect command to cleanly disconnect from SBCL and exit interactive mode.
 * Sends graceful quit command to SBCL, terminates the subprocess, and restores normal editing mode.
 * 
 * @param editor Pointer to the Vizero editor instance
 * @param args Command arguments (unused)
 * @return 0 on success, -1 on failure
 */
static int lisp_cmd_disconnect(vizero_editor_t* editor, const char* args) {
    if (!g_lisp_state->sbcl.running) {
        lisp_log_message("SBCL is not running");
        return 0;
    }
    
    /* Send quit command to SBCL */
    send_to_sbcl("(quit)");
    
    /* Wait a moment then force stop */
#ifdef _WIN32
    Sleep(1000);
#else
    sleep(1);
#endif
    
    stop_sbcl_process(&g_lisp_state->sbcl);
    return 0;
}

static int lisp_cmd_eval(vizero_editor_t* editor, const char* args) {
    if (!g_lisp_state->sbcl.running) {
        lisp_log_message("ERROR: SBCL not running. Use /lisp-connect first");
        return -1;
    }
    
    if (!args || !args[0]) {
        lisp_log_message("Usage: /lisp-eval <lisp-expression>");
        return -1;
    }
    
    /* Display the input in the REPL buffer with prompt */
    if (g_lisp_state && g_lisp_state->repl_buffer && g_lisp_state->api && g_lisp_state->api->insert_text) {
        size_t line_count = 0;
        if (g_lisp_state->api->get_buffer_line_count) {
            line_count = g_lisp_state->api->get_buffer_line_count(g_lisp_state->repl_buffer);
        }
        
        printf("[LISP] Input insertion: line_count=%zu, repl_buffer=%p\n", line_count, (void*)g_lisp_state->repl_buffer);
        
        char input_display[2048];
        snprintf(input_display, sizeof(input_display), "CL-USER> %s", args);
        
        /* Simple append at end of buffer with newline prefix */
        size_t end_line = (line_count > 0) ? line_count - 1 : 0;
        size_t end_col = 0;
        if (line_count > 0 && g_lisp_state->api->get_buffer_line_length) {
            end_col = g_lisp_state->api->get_buffer_line_length(g_lisp_state->repl_buffer, end_line);
        }
        
        char prefixed_input[2048];
        if (line_count > 0) {
            snprintf(prefixed_input, sizeof(prefixed_input), "\n%s", input_display);
        } else {
            snprintf(prefixed_input, sizeof(prefixed_input), "%s", input_display);
        }
        
        vizero_position_t pos = {end_line, end_col};
        int insert_result = g_lisp_state->api->insert_text_multiline(g_lisp_state->repl_buffer, pos, prefixed_input);
        printf("[LISP] Input insert_text_multiline at position (%zu, %zu) with text '%s' returned: %d\n", 
               pos.line, pos.column, prefixed_input, insert_result);
    }
    
    /* Log the evaluation */
    char log_msg[1024];
    snprintf(log_msg, sizeof(log_msg), "Evaluating: %s", args);
    printf("[LISP] %s\n", log_msg); /* Only log to console, not to buffer */
    
    /* Send to SBCL */
    if (!send_to_sbcl(args)) {
        return -1;
    }
    
    /* Wait a bit and try to read the result */
#ifdef _WIN32
    Sleep(100); /* Wait 100ms for SBCL to process */
#else
    usleep(100000); /* Wait 100ms for SBCL to process */
#endif
    
    char result[2048];
    if (read_from_sbcl(result, sizeof(result), 500)) {
        /* Display the result in the REPL buffer */
        if (g_lisp_state && g_lisp_state->repl_buffer && g_lisp_state->api && g_lisp_state->api->insert_text) {
            size_t line_count = 0;
            if (g_lisp_state->api->get_buffer_line_count) {
                line_count = g_lisp_state->api->get_buffer_line_count(g_lisp_state->repl_buffer);
            }
            
            printf("[LISP] Result insertion: line_count=%zu, result_length=%zu\n", line_count, strlen(result));
            printf("[LISP] Raw SBCL output: '%s'\n", result);
            
            /* Clean up the result - extract just the final result, not the whole SBCL output */
            char clean_result[256];
            
            /* The result appears BEFORE the final "* " prompt, so we need to find the last line before it */
            char* last_prompt = strrchr(result, '*');
            printf("[LISP] Parsing result: last_prompt=%p\n", (void*)last_prompt);
            if (last_prompt) {
                printf("[LISP] Found '*' at: '%.20s'\n", last_prompt);
            }
            
            if (last_prompt && last_prompt[1] == ' ') {
                /* Found the final "* " prompt, now find the result line before it */
                printf("[LISP] Found final '* ' prompt, looking for result before it...\n");
                
                /* Go backwards to find the start of the result line */
                char* result_end = last_prompt - 1;
                while (result_end > result && (*result_end == '\n' || *result_end == '\r')) {
                    result_end--; /* Skip trailing newlines */
                }
                
                if (result_end > result) {
                    /* Find the start of this line */
                    char* result_start = result_end;
                    while (result_start > result && *result_start != '\n' && *result_start != '\r') {
                        result_start--;
                    }
                    if (*result_start == '\n' || *result_start == '\r') {
                        result_start++; /* Move past the newline */
                    }
                    
                    /* Skip the "* " prefix if present */
                    if (result_start[0] == '*' && result_start[1] == ' ') {
                        result_start += 2;
                    }
                    
                    /* Extract the result */
                    size_t len = result_end - result_start + 1;
                    if (len < sizeof(clean_result) - 1) {
                        strncpy(clean_result, result_start, len);
                        clean_result[len] = '\0';
                    } else {
                        strcpy(clean_result, "Result too long");
                    }
                    printf("[LISP] Extracted clean result: '%s'\n", clean_result);
                } else {
                    strcpy(clean_result, "Could not parse result");
                }
            } else {
                /* Fallback - just show "Evaluated" */
                printf("[LISP] No final '* ' prompt found, using fallback\n");
                strcpy(clean_result, "Evaluated");
            }
            
            char result_display[512];
            snprintf(result_display, sizeof(result_display), "%s\nCL-USER> ", clean_result);
            
            /* Append result at end of buffer with newline prefix */
            line_count = g_lisp_state->api->get_buffer_line_count(g_lisp_state->repl_buffer);
            size_t end_line = (line_count > 0) ? line_count - 1 : 0;
            size_t end_col = 0;
            if (line_count > 0 && g_lisp_state->api->get_buffer_line_length) {
                end_col = g_lisp_state->api->get_buffer_line_length(g_lisp_state->repl_buffer, end_line);
            }
            
            char prefixed_result[1024];
            snprintf(prefixed_result, sizeof(prefixed_result), "\n%s", result_display);
            
            vizero_position_t pos = {end_line, end_col};
            int insert_result = g_lisp_state->api->insert_text_multiline(g_lisp_state->repl_buffer, pos, prefixed_result);
            printf("[LISP] Result insert_text_multiline at position (%zu, %zu) with text '%s' returned: %d (clean result: '%s')\n", 
                   pos.line, pos.column, prefixed_result, insert_result, clean_result);
            
            /* Position cursor at the end of the inserted text (after "CL-USER> ") */
            if (insert_result == 0 && g_lisp_state->api->set_cursor_position && g_lisp_state->api->get_current_cursor) {
                /* Get the current cursor for the editor */
                vizero_cursor_t* cursor = g_lisp_state->api->get_current_cursor(editor);
                if (cursor) {
                    /* Get the final buffer line count after insertion */
                    size_t final_line_count = g_lisp_state->api->get_buffer_line_count(g_lisp_state->repl_buffer);
                    if (final_line_count > 0) {
                        size_t last_line = final_line_count - 1;
                        size_t last_line_length = 0;
                        if (g_lisp_state->api->get_buffer_line_length) {
                            last_line_length = g_lisp_state->api->get_buffer_line_length(g_lisp_state->repl_buffer, last_line);
                        }
                        
                        vizero_position_t cursor_pos = {last_line, last_line_length};
                        int cursor_result = g_lisp_state->api->set_cursor_position(cursor, cursor_pos);
                        printf("[LISP] Positioned cursor at end of prompt: line %zu, column %zu (result: %d)\n", 
                               cursor_pos.line, cursor_pos.column, cursor_result);
                        
                        /* Update prompt start position for interactive mode */
                        if (g_lisp_state->interactive_mode) {
                            g_lisp_state->prompt_start.line = last_line;
                            g_lisp_state->prompt_start.column = last_line_length;
                            printf("[LISP] Updated prompt start to (%zu, %zu)\n", 
                                   last_line, last_line_length);
                            
                            /* Re-enter insert mode for continued interaction */
                            if (g_lisp_state->api->execute_command) {
                                g_lisp_state->api->execute_command(editor, "i");
                                printf("[LISP] Re-entered insert mode after evaluation\n");
                            }
                        }
                    }
                }
            }
        }
        
        /* Also log to console for debugging */
        char display_msg[2048];
        snprintf(display_msg, sizeof(display_msg), "Result: %s", result);
        printf("[LISP] %s\n", display_msg);
    } else {
        /* Display timeout message in REPL buffer */
        if (g_lisp_state && g_lisp_state->repl_buffer && g_lisp_state->api && g_lisp_state->api->insert_text) {
            size_t line_count = 0;
            if (g_lisp_state->api->get_buffer_line_count) {
                line_count = g_lisp_state->api->get_buffer_line_count(g_lisp_state->repl_buffer);
            }
            
            printf("[LISP] Timeout insertion: line_count=%zu\n", line_count);
            
            /* Insert at the beginning of a new line after all existing content */
            vizero_position_t pos = {line_count, 0};
            int insert_result = g_lisp_state->api->insert_text(g_lisp_state->repl_buffer, pos, "(No output or timeout)\nCL-USER> ");
            printf("[LISP] Timeout insert_text at position (%zu, %zu) returned: %d\n", pos.line, pos.column, insert_result);
        }
        
        printf("[LISP] (No output or timeout reading from SBCL)\n");
    }
    
    return 0;
}

static int lisp_cmd_package(vizero_editor_t* editor, const char* args) {
    if (!g_lisp_state->sbcl.running) {
        lisp_log_message("ERROR: SBCL not running. Use /lisp-connect first");
        return -1;
    }
    
    if (!args || !args[0]) {
        /* Show current package */
        send_to_sbcl("(package-name *package*)");
        return 0;
    }
    
    /* Change package */
    char pkg_cmd[256];
    snprintf(pkg_cmd, sizeof(pkg_cmd), "(in-package :%s)", args);
    send_to_sbcl(pkg_cmd);
    
    /* Update local state */
    if (g_lisp_state->buffer_count > 0) {
        strncpy(g_lisp_state->buffers[0]->current_package, args, 
                sizeof(g_lisp_state->buffers[0]->current_package) - 1);
    }
    
    return 0;
}

/**
 * @brief Command handler for :lisp-status
 * 
 * Handles the :lisp-status command to display comprehensive REPL status information.
 * Shows SBCL process state, executable path, interactive mode status, and provides
 * helpful guidance for SBCL installation when not available.
 * 
 * @param editor Pointer to the Vizero editor instance
 * @param args Command arguments (unused)
 * @return 0 on success, -1 on failure
 */
static int lisp_cmd_status(vizero_editor_t* editor, const char* args) {
    char status_msg[1024];
    if (g_lisp_state->sbcl.running) {
        snprintf(status_msg, sizeof(status_msg), 
                "SBCL Status: Running | Path: %s | Package: %s\n"
                "Dynamic Space: %dMB | Swank: %s | Evaluations: %d\n"
                "Features: Completion=%s, Inspection=%s, Debugger=%s", 
                g_lisp_state->sbcl.sbcl_path,
                g_lisp_state->buffer_count > 0 ? g_lisp_state->buffers[0]->current_package : "Unknown",
                g_lisp_state->sbcl.dynamic_space_size,
                g_lisp_state->sbcl.swank_server_running ? "Running" : "Stopped",
                g_lisp_state->total_evaluations,
                g_lisp_state->enable_completion ? "ON" : "OFF",
                g_lisp_state->enable_inspection ? "ON" : "OFF",
                g_lisp_state->enable_debugger ? "ON" : "OFF");
    } else {
        snprintf(status_msg, sizeof(status_msg), "SBCL Status: Not running | Phase 2 features ready");
    }
    
    lisp_log_message(status_msg);
    return 0;
}

/* New Phase 2 Commands */

static int lisp_cmd_complete(vizero_editor_t* editor, const char* args) {
    if (!g_lisp_state->sbcl.running) {
        lisp_log_message("ERROR: SBCL not running. Use /lisp-connect first");
        return -1;
    }
    
    if (!args || !args[0]) {
        lisp_log_message("Usage: /lisp-complete <partial-symbol>");
        return -1;
    }
    
    /* Generate completion request */
    char completion_expr[512];
    snprintf(completion_expr, sizeof(completion_expr),
             "(let ((completions nil)) "
             "(do-symbols (sym (find-package \"CL-USER\")) "
             "(when (and (>= (length (symbol-name sym)) %zu) "
             "(string= \"%s\" (symbol-name sym) :end2 %zu)) "
             "(push (symbol-name sym) completions))) "
             "(sort completions #'string<))",
             strlen(args), args, strlen(args));
    
    send_to_sbcl(completion_expr);
    lisp_log_message("Generating completions...");
    return 0;
}

static int lisp_cmd_inspect(vizero_editor_t* editor, const char* args) {
    if (!g_lisp_state->sbcl.running) {
        lisp_log_message("ERROR: SBCL not running. Use /lisp-connect first");
        return -1;
    }
    
    if (!args || !args[0]) {
        lisp_log_message("Usage: /lisp-inspect <symbol-or-expression>");
        return -1;
    }
    
    /* Generate inspection expression */
    char inspect_expr[1024];
    snprintf(inspect_expr, sizeof(inspect_expr),
             "(let ((obj %s)) "
             "(format t \"Object: ~A~%%Type: ~A~%%\" obj (type-of obj)) "
             "(when (fboundp '%s) "
             "(format t \"Function: ~A~%%\" (documentation '%s 'function))) "
             "(when (boundp '%s) "
             "(format t \"Variable: ~A~%%\" (documentation '%s 'variable))) "
             "(force-output))",
             args, args, args, args, args);
    
    send_to_sbcl(inspect_expr);
    
    char log_msg[512];
    snprintf(log_msg, sizeof(log_msg), "Inspecting: %s", args);
    lisp_log_message(log_msg);
    
    return 0;
}

static int lisp_cmd_trace(vizero_editor_t* editor, const char* args) {
    if (!g_lisp_state->sbcl.running) {
        lisp_log_message("ERROR: SBCL not running. Use /lisp-connect first");
        return -1;
    }
    
    if (!args || !args[0]) {
        /* Show currently traced functions */
        send_to_sbcl("(trace)");
        lisp_log_message("Showing currently traced functions");
        return 0;
    }
    
    /* Trace specified function */
    char trace_expr[256];
    snprintf(trace_expr, sizeof(trace_expr), "(trace %s)", args);
    send_to_sbcl(trace_expr);
    
    char log_msg[512];
    snprintf(log_msg, sizeof(log_msg), "Tracing function: %s", args);
    lisp_log_message(log_msg);
    
    return 0;
}

static int lisp_cmd_untrace(vizero_editor_t* editor, const char* args) {
    if (!g_lisp_state->sbcl.running) {
        lisp_log_message("ERROR: SBCL not running. Use /lisp-connect first");
        return -1;
    }
    
    if (!args || !args[0]) {
        /* Untrace all functions */
        send_to_sbcl("(untrace)");
        lisp_log_message("Untracing all functions");
    } else {
        /* Untrace specific function */
        char untrace_expr[256];
        snprintf(untrace_expr, sizeof(untrace_expr), "(untrace %s)", args);
        send_to_sbcl(untrace_expr);
        
        char log_msg[512];
        snprintf(log_msg, sizeof(log_msg), "Untracing function: %s", args);
        lisp_log_message(log_msg);
    }
    
    return 0;
}

static int lisp_cmd_load(vizero_editor_t* editor, const char* args) {
    if (!g_lisp_state->sbcl.running) {
        lisp_log_message("ERROR: SBCL not running. Use /lisp-connect first");
        return -1;
    }
    
    if (!args || !args[0]) {
        lisp_log_message("Usage: /lisp-load <filename>");
        return -1;
    }
    
    /* Load Lisp file */
    char load_expr[512];
    snprintf(load_expr, sizeof(load_expr), "(load \"%s\")", args);
    send_to_sbcl(load_expr);
    
    char log_msg[1024];
    snprintf(log_msg, sizeof(log_msg), "Loading file: %s", args);
    lisp_log_message(log_msg);
    
    return 0;
}

static int lisp_cmd_compile(vizero_editor_t* editor, const char* args) {
    if (!g_lisp_state->sbcl.running) {
        lisp_log_message("ERROR: SBCL not running. Use /lisp-connect first");
        return -1;
    }
    
    if (!args || !args[0]) {
        lisp_log_message("Usage: /lisp-compile <filename>");
        return -1;
    }
    
    /* Compile Lisp file */
    char compile_expr[512];
    snprintf(compile_expr, sizeof(compile_expr), "(compile-file \"%s\")", args);
    send_to_sbcl(compile_expr);
    
    char log_msg[1024];
    snprintf(log_msg, sizeof(log_msg), "Compiling file: %s", args);
    lisp_log_message(log_msg);
    
    return 0;
}

static int lisp_cmd_quicklisp(vizero_editor_t* editor, const char* args) {
    if (!g_lisp_state->sbcl.running) {
        lisp_log_message("ERROR: SBCL not running. Use /lisp-connect first");
        return -1;
    }
    
    if (!args || !args[0]) {
        /* Show Quicklisp status */
        send_to_sbcl("(if (find-package :quicklisp) \"Quicklisp loaded\" \"Quicklisp not available\")");
        return 0;
    }
    
    /* Load Quicklisp system */
    char ql_expr[256];
    snprintf(ql_expr, sizeof(ql_expr), "(ql:quickload :%s)", args);
    send_to_sbcl(ql_expr);
    
    char log_msg[512];
    snprintf(log_msg, sizeof(log_msg), "Loading Quicklisp system: %s", args);
    lisp_log_message(log_msg);
    
    return 0;
}

static int lisp_cmd_help(vizero_editor_t* editor, const char* args) {
    lisp_log_message("=== Lisp REPL Plugin Phase 2 Commands ===");
    lisp_log_message("Basic Commands:");
    lisp_log_message("  /lisp-connect       - Connect to SBCL REPL");
    lisp_log_message("  /lisp-disconnect    - Disconnect from SBCL");
    lisp_log_message("  /lisp-eval <expr>   - Evaluate Lisp expression");
    lisp_log_message("  /lisp-package [pkg] - Change/show current package");
    lisp_log_message("  /lisp-status        - Show connection status");
    lisp_log_message("");
    lisp_log_message("Advanced Commands:");
    lisp_log_message("  /lisp-complete <sym> - Show symbol completions");
    lisp_log_message("  /lisp-inspect <obj>  - Inspect object/symbol");
    lisp_log_message("  /lisp-trace [fn]     - Trace function calls");
    lisp_log_message("  /lisp-untrace [fn]   - Stop tracing");
    lisp_log_message("  /lisp-load <file>    - Load Lisp source file");
    lisp_log_message("  /lisp-compile <file> - Compile Lisp file");
    lisp_log_message("  /lisp-quicklisp [sys] - Quicklisp operations");
    lisp_log_message("  /lisp-help          - Show this help");
    lisp_log_message("  /lisp-interactive   - Re-enable interactive REPL mode");
    lisp_log_message("  /lisp-return        - Return to original buffer");
    
    return 0;
}

/**
 * @brief Return to the original buffer before REPL was opened
 * @param editor Editor instance
 * @param args Command arguments (unused)
 * @return 0 on success, -1 on error
 */
static int lisp_cmd_interactive(vizero_editor_t* editor, const char* args) {
    if (!g_lisp_state) {
        lisp_log_message("LISP REPL not initialized");
        return -1;
    }
    
    if (!g_lisp_state->sbcl.running) {
        lisp_log_message("SBCL not running. Use /lisp-connect first.");
        return -1;
    }
    
    /* Re-enable interactive mode */
    g_lisp_state->interactive_mode = true;
    g_lisp_state->user_exited_interactive = false;
    g_lisp_state->paren_balance = 0;
    
    lisp_log_message("Interactive REPL mode re-enabled. Type Lisp expressions to evaluate them.");
    return 0;
}

static int lisp_cmd_return(vizero_editor_t* editor, const char* args) {
    if (!g_lisp_state) {
        printf("[LISP] Plugin not initialized\n");
        return -1;
    }
    
    if (!g_lisp_state->original_buffer) {
        lisp_log_message("No original buffer to return to. Use :bp to navigate buffers manually.");
        return -1;
    }
    
    /* Use buffer previous command to return to original buffer */
    if (g_lisp_state->api && g_lisp_state->api->execute_command) {
        g_lisp_state->api->execute_command(editor, "bp");
        lisp_log_message("Returned to original file buffer. Use :bn/:bp to navigate buffers.");
        printf("[LISP] Switched back to original file buffer\n");
        return 0;
    }
    
    lisp_log_message("Unable to switch buffers - API not available");
    return -1;
}

/* Buffer management */
static void lisp_set_active_buffer(const char* buffer_name) {
    strncpy(g_lisp_state->current_buffer, buffer_name, sizeof(g_lisp_state->current_buffer) - 1);
    g_lisp_state->current_buffer[sizeof(g_lisp_state->current_buffer) - 1] = '\0';
    g_lisp_state->wants_full_window = false; /* Disable full window mode to avoid rendering conflicts */
    
    char msg[128];
    snprintf(msg, sizeof(msg), "Switched to buffer: %s", buffer_name);
    lisp_log_message(msg);
}

/* Enhanced Phase 2 Rendering System */
static int lisp_wants_full_window(vizero_editor_t* editor) {
    /* Disable full window mode to avoid rendering conflicts with other plugins */
    return 0;
}

static int lisp_render_full_window(vizero_editor_t* editor, vizero_renderer_t* renderer, int width, int height) {
    if (!g_lisp_state || g_lisp_state->buffer_count == 0) return 0;
    
    lisp_buffer_t* buffer = g_lisp_state->buffers[0];
    if (!buffer) return 0;
    
    /* Update buffer dimensions */
    buffer->window_width = width;
    buffer->window_height = height;
    buffer->line_height = 16;  /* Assume 16px line height */
    buffer->char_width = 8;    /* Assume 8px char width */
    
    /* Phase 2: Enhanced custom rendering */
    if (g_lisp_state->custom_rendering) {
        /* Render REPL header */
        char header[256];
        snprintf(header, sizeof(header), "Lisp REPL Phase 2 - %s - %s", 
                buffer->current_package, 
                g_lisp_state->sbcl.running ? "Connected" : "Disconnected");
        
        /* Enhanced status display */
        if (g_lisp_state->api && g_lisp_state->api->set_status_message) {
            char enhanced_status[1024];
            
            if (g_lisp_state->ui_mode == 1 && buffer->showing_completions) {
                /* Completion mode */
                snprintf(enhanced_status, sizeof(enhanced_status), 
                        "COMPLETION MODE | %zu matches | Use Tab/Arrow keys to navigate", 
                        buffer->completion_count);
            } else if (g_lisp_state->ui_mode == 2 && buffer->showing_inspection) {
                /* Inspection mode */
                snprintf(enhanced_status, sizeof(enhanced_status), 
                        "INSPECTION: %s | Type: %s", 
                        buffer->current_symbol.symbol, 
                        buffer->current_symbol.type);
            } else if (g_lisp_state->ui_mode == 3 && buffer->in_debugger) {
                /* Debugger mode */
                snprintf(enhanced_status, sizeof(enhanced_status), 
                        "DEBUGGER | Stack depth: %d | %s", 
                        buffer->stack_depth, 
                        buffer->debugger_prompt);
            } else {
                /* Normal REPL mode */
                if (buffer->message_count > 0) {
                    lisp_message_t* last_msg = &buffer->messages[buffer->message_count - 1];
                    const char* type_str = "";
                    switch (last_msg->type) {
                        case LISP_MSG_RESULT: type_str = "RESULT"; break;
                        case LISP_MSG_ERROR: type_str = "ERROR"; break;
                        case LISP_MSG_OUTPUT: type_str = "OUTPUT"; break;
                        default: type_str = "INFO"; break;
                    }
                    snprintf(enhanced_status, sizeof(enhanced_status), 
                            "LISP REPL Phase 2 | %s: %.60s%s | Evals: %d", 
                            type_str, last_msg->content,
                            strlen(last_msg->content) > 60 ? "..." : "",
                            g_lisp_state->total_evaluations);
                } else {
                    snprintf(enhanced_status, sizeof(enhanced_status), 
                            "LISP REPL Phase 2 Ready | Use /lisp-help for commands | Package: %s", 
                            buffer->current_package);
                }
            }
            
            g_lisp_state->api->set_status_message(editor, enhanced_status);
        }
        
        /* Render completion popup if active */
        if (buffer->showing_completions && buffer->completion_count > 0) {
            /* This would render a completion popup in a full implementation */
            /* For now, we'll just update the status message above */
        }
        
        /* Render inspection details if active */
        if (buffer->showing_inspection) {
            /* This would render detailed inspection information */
            /* For now, we'll display via status message */
        }
        
        /* Animate cursor blinking */
        unsigned int current_time = (unsigned int)time(NULL) * 1000;  /* Rough milliseconds */
        if (current_time - g_lisp_state->last_blink_time > 500) {  /* 500ms blink rate */
            g_lisp_state->animate_cursor = !g_lisp_state->animate_cursor;
            g_lisp_state->last_blink_time = current_time;
        }
        
    } else {
        /* Fallback to Phase 1 style status display */
        if (g_lisp_state->api && g_lisp_state->api->set_status_message) {
            char status[512];
            if (buffer->message_count > 0) {
                lisp_message_t* last_msg = &buffer->messages[buffer->message_count - 1];
                snprintf(status, sizeof(status), "LISP REPL: %s", last_msg->content);
            } else {
                snprintf(status, sizeof(status), "LISP REPL: Ready (use /lisp-connect to start SBCL)");
            }
            g_lisp_state->api->set_status_message(editor, status);
        }
    }
    
    return 0;
}

/**
 * @brief Handles interactive input in REPL mode
 * 
 * Processes keyboard input when the REPL is in interactive mode. Handles special keys
 * like Enter for expression evaluation, Escape for command mode transition, and
 * regular character input for expression building. Manages parentheses balancing
 * and provides real-time feedback.
 * 
 * @param editor Pointer to the Vizero editor instance
 * @param key Key code of the pressed key
 * @param modifiers Modifier keys (Ctrl, Alt, Shift) bitmask
 * @return 0 on successful handling, -1 on error
 */
static int lisp_handle_interactive_input(vizero_editor_t* editor, uint32_t key, uint32_t modifiers) {
    printf("[LISP] Interactive key input: key=%u, modifiers=%u\n", key, modifiers);
    
    /* Handle special keys */
    switch (key) {
        case 27: /* Escape - exit interactive mode */
        case 1073742053: /* Escape with modifiers on Windows */
            printf("[LISP] Escape pressed (key=%u), exiting interactive mode\n", key);
            g_lisp_state->interactive_mode = false;
            g_lisp_state->user_exited_interactive = true; /* Remember user explicitly exited */
            g_lisp_state->paren_balance = 0; /* Reset paren balance */
            return 0; /* Let Vizero handle the Escape key for command mode */
            

            
        case 13: /* Enter key */
        case 10: /* LF */
            return lisp_handle_enter_key(editor);
            
        case 40: /* '(' */
            g_lisp_state->paren_balance++;
            printf("[LISP] Paren balance: %d (open)\n", g_lisp_state->paren_balance);
            return 0; /* Let normal input processing handle the character */
            
        case 41: /* ')' */
            g_lisp_state->paren_balance--;
            printf("[LISP] Paren balance: %d (close)\n", g_lisp_state->paren_balance);
            
            /* Check if expression is balanced and ready for evaluation */
            if (g_lisp_state->paren_balance == 0) {
                printf("[LISP] Expression balanced, auto-evaluating after character insertion\n");
                /* Schedule evaluation after the ')' character is inserted */
                /* We'll handle this in a separate function to avoid interfering with input */
                return 0; /* Let normal processing insert the ')' first */
            }
            return 0;
            
        default:
            return 0; /* Let normal input processing handle other keys */
    }
}

/* Evaluate input for interactive REPL (without duplicating input display) */
static int lisp_evaluate_interactive(vizero_editor_t* editor, const char* input) {
    if (!g_lisp_state->sbcl.running) {
        printf("[LISP] SBCL not running, cannot evaluate\n");
        return -1;
    }
    
    printf("[LISP] Interactive evaluation: '%s'\n", input);
    
    /* Send to SBCL */
    if (!send_to_sbcl(input)) {
        return -1;
    }
    
    /* Wait for SBCL response */
#ifdef _WIN32
    Sleep(200); /* Wait 200ms for SBCL to process */
#else
    usleep(200000); /* Wait 200ms for SBCL to process */
#endif
    
    char result[2048];
    if (read_from_sbcl(result, sizeof(result), 1000)) {
        printf("[LISP] Raw SBCL result (%zu chars): '%s'\n", strlen(result), result);
        
        /* Clean up the result - remove control characters */
        char cleaned_result[2048];
        size_t cleaned_len = 0;
        for (size_t i = 0; i < strlen(result) && cleaned_len < sizeof(cleaned_result) - 1; i++) {
            unsigned char c = (unsigned char)result[i];
            /* Keep printable characters, newlines, and spaces */
            if ((c >= 32 && c <= 126) || c == '\n' || c == '\r' || c == '\t') {
                cleaned_result[cleaned_len++] = c;
            }
        }
        cleaned_result[cleaned_len] = '\0';
        printf("[LISP] Cleaned result (%zu chars): '%s'\n", cleaned_len, cleaned_result);
        
        /* Prepend newline to result for proper formatting */
        char formatted_result[2048];
        snprintf(formatted_result, sizeof(formatted_result), "\n%s", cleaned_result);
        printf("[LISP] Formatted result (%zu chars): '%s'\n", strlen(formatted_result), formatted_result);
        
        /* Append result to the REPL buffer */
        if (g_lisp_state->repl_buffer && g_lisp_state->api && g_lisp_state->api->insert_text) {
            size_t line_count = g_lisp_state->api->get_buffer_line_count(g_lisp_state->repl_buffer);
            vizero_position_t append_pos;
            append_pos.line = line_count > 0 ? line_count - 1 : 0;
            append_pos.column = 0;
            
            /* Find the end of the last line */
            if (line_count > 0 && g_lisp_state->api->get_buffer_line_length) {
                append_pos.column = g_lisp_state->api->get_buffer_line_length(g_lisp_state->repl_buffer, line_count - 1);
            }
            
            g_lisp_state->inserting_sbcl_output = true;
            if (g_lisp_state->api->insert_text_multiline) {
                g_lisp_state->api->insert_text_multiline(g_lisp_state->repl_buffer, append_pos, formatted_result);
            } else {
                g_lisp_state->api->insert_text(g_lisp_state->repl_buffer, append_pos, formatted_result);
            }
            g_lisp_state->inserting_sbcl_output = false;
            printf("[LISP] Interactive result appended (%zu chars)\n", strlen(formatted_result));
        }
    } else {
        printf("[LISP] Failed to read result from SBCL\n");
    }
    
    return 0;
}

/**
 * @brief Handles Enter key press in interactive REPL mode
 * 
 * Processes Enter key when in interactive REPL mode. Determines if the current expression
 * has balanced parentheses and is ready for evaluation. If balanced, sends the expression
 * to SBCL for evaluation and displays the result. If unbalanced, allows multi-line input.
 * 
 * @param editor Pointer to the Vizero editor instance
 * @return 0 on successful handling, -1 on error
 */
static int lisp_handle_enter_key(vizero_editor_t* editor) {
    printf("[LISP] Enter pressed in interactive REPL\n");
    
    /* Get current cursor position */
    vizero_cursor_t* cursor = g_lisp_state->api->get_current_cursor(editor);
    if (!cursor) return 0;
    
    vizero_position_t cursor_pos = g_lisp_state->api->get_cursor_position(cursor);
    
    /* Extract current input to check if it's balanced */
    char current_input[2048];
    if (lisp_extract_current_input(current_input, sizeof(current_input)) == 0) {
        int actual_balance = count_parens(current_input);
        printf("[LISP] Checking balance: live_balance=%d, actual_balance=%d\n", 
               g_lisp_state->paren_balance, actual_balance);
        
        if (actual_balance == 0) {
            /* Expression is complete, evaluate it */
            printf("[LISP] Complete expression, evaluating...\n");
            return lisp_evaluate_current_input(editor);
        } else {
            /* Expression incomplete, just insert newline and continue */
            printf("[LISP] Incomplete expression (balance: %d), continuing on next line\n", actual_balance);
            return 0; /* Let normal newline processing happen */
        }
    } else {
        printf("[LISP] Failed to extract input for balance check\n");
        return 0;
    }
}

/* Evaluate the current input in interactive mode */
static int lisp_evaluate_current_input(vizero_editor_t* editor) {
    if (!g_lisp_state->sbcl.running) {
        printf("[LISP] SBCL not running, cannot evaluate\n");
        return 0;
    }
    
    /* The newline will be added before the result in lisp_evaluate_interactive */
    
    /* Extract text from prompt position to cursor */
    char input_text[2048];
    if (lisp_extract_current_input(input_text, sizeof(input_text)) != 0) {
        printf("[LISP] Failed to extract current input\n");  
        return 0;
    }
    
    printf("[LISP] Extracted input for evaluation: '%s' (length: %zu)\n", input_text, strlen(input_text));
    
    /* Evaluate the input (interactive version - don't duplicate input display) */
    lisp_evaluate_interactive(editor, input_text);
    
    /* Reset parentheses balance for next input */
    printf("[LISP] Resetting parentheses balance to 0 (was %d)\n", g_lisp_state->paren_balance);
    g_lisp_state->paren_balance = 0;
    
    /* Position cursor at the end of the buffer after evaluation */
    if (g_lisp_state->repl_buffer && g_lisp_state->api->get_buffer_line_count) {
        size_t line_count = g_lisp_state->api->get_buffer_line_count(g_lisp_state->repl_buffer);
        if (line_count > 0) {
            size_t last_line = line_count - 1;
            size_t last_line_length = 0;
            
            if (g_lisp_state->api->get_buffer_line_length) {
                last_line_length = g_lisp_state->api->get_buffer_line_length(g_lisp_state->repl_buffer, last_line);
            }
            
            /* Position cursor at the end of the buffer */
            if (g_lisp_state->api->get_current_cursor && g_lisp_state->api->set_cursor_position) {
                vizero_cursor_t* cursor = g_lisp_state->api->get_current_cursor(editor);
                if (cursor) {
                    vizero_position_t cursor_pos;
                    cursor_pos.line = last_line;
                    cursor_pos.column = last_line_length;
                    int cursor_result = g_lisp_state->api->set_cursor_position(cursor, cursor_pos);
                    printf("[LISP] Post-evaluation cursor positioned at (%zu, %zu), result: %d\n", 
                           last_line, last_line_length, cursor_result);
                }
            }
        }
    }
    
    return 1; /* We handled the Enter key */
}

/* Extract current input from the REPL buffer */
static int lisp_extract_current_input(char* buffer, size_t buffer_size) {
    if (!g_lisp_state->repl_buffer || !g_lisp_state->api->get_buffer_line_count || !g_lisp_state->api->get_buffer_line) {
        printf("[LISP] Missing required API functions for input extraction\n");
        return -1;
    }
    
    /* Get line count */
    size_t line_count = g_lisp_state->api->get_buffer_line_count(g_lisp_state->repl_buffer);
    if (line_count == 0) {
        printf("[LISP] Buffer is empty\n");
        return -1;
    }
    
    /* Search backwards from the last line to find the most recent SBCL * prompt */
    for (size_t i = line_count; i > 0; i--) {
        const char* line_text = g_lisp_state->api->get_buffer_line(g_lisp_state->repl_buffer, i - 1);
        if (line_text) {
            printf("[LISP] Checking line %zu: '%s'\n", i - 1, line_text);
            
            /* Look for SBCL prompt "* " at the beginning of the line */
            const char* prompt_pos = NULL;
            if (strncmp(line_text, "* ", 2) == 0) {
                prompt_pos = line_text;
            }
            
            if (prompt_pos) {
                /* Found the prompt at the beginning of this line, extract everything after it */
                const char* input_start = prompt_pos + 2;
                
                /* Check if there's actually input after the prompt */
                if (strlen(input_start) > 0) {
                    strncpy(buffer, input_start, buffer_size - 1);
                    buffer[buffer_size - 1] = '\0';
                    
                    /* Remove any trailing whitespace */
                    size_t len = strlen(buffer);
                    while (len > 0 && (buffer[len-1] == '\n' || buffer[len-1] == '\r' || buffer[len-1] == ' ' || buffer[len-1] == '\t')) {
                        buffer[--len] = '\0';
                    }
                    
                    printf("[LISP] Extracted input from line %zu: '%s' (length: %zu)\n", i - 1, buffer, strlen(buffer));
                    return 0;
                }
            }
        }
    }
    
    printf("[LISP] Could not find SBCL * prompt in buffer\n");
    return -1;
}

/**
 * @brief Main keyboard input handler for LISP REPL plugin
 * 
 * Central input processing function that routes keyboard input based on current REPL state.
 * Handles buffer switching detection, interactive mode management, command mode transitions,
 * and delegates to appropriate specialized input handlers. Provides comprehensive state
 * management across buffer switches with automatic restoration.
 * 
 * @param editor Pointer to the Vizero editor instance
 * @param key Key code of the pressed key
 * @param modifiers Modifier keys (Ctrl, Alt, Shift) bitmask
 * @return 0 if key handled by plugin, -1 to pass through to editor
 */
static int lisp_on_key_input(vizero_editor_t* editor, uint32_t key, uint32_t modifiers) {
    if (!g_lisp_state) {
        return 0;  /* Let Vizero handle input */
    }
    
    /* Check if we're in the REPL buffer */
    int in_repl_buffer = 0;
    static vizero_buffer_t* last_buffer = NULL;
    if (g_lisp_state->api && g_lisp_state->api->get_current_buffer && g_lisp_state->repl_buffer) {
        vizero_buffer_t* current = g_lisp_state->api->get_current_buffer(editor);
        in_repl_buffer = (current == g_lisp_state->repl_buffer);
        
        /* Debug buffer switches */
        if (current != last_buffer) {
            printf("[LISP] Buffer switch detected: from %p to %p (REPL buffer: %p)\n", 
                   last_buffer, current, g_lisp_state->repl_buffer);
            if (current == g_lisp_state->repl_buffer) {
                printf("[LISP] Switched back to REPL buffer - interactive_mode=%d, user_exited=%d\n", 
                       g_lisp_state->interactive_mode, g_lisp_state->user_exited_interactive);
                
                /* Auto-enable interactive mode when switching back to REPL buffer */
                if (!g_lisp_state->interactive_mode && g_lisp_state->sbcl.running) {
                    printf("[LISP] Auto-enabling interactive mode on buffer switch\n");
                    g_lisp_state->interactive_mode = true;
                    g_lisp_state->user_exited_interactive = false;
                    g_lisp_state->command_mode_active = false;
                    g_lisp_state->paren_balance = 0;
                }
            }
            last_buffer = current;
        }
    }
    
    /* Handle special cases when user recently exited interactive mode */
    if (in_repl_buffer && g_lisp_state->user_exited_interactive && !g_lisp_state->command_mode_active) {
        /* If user pressed ':', let Vizero handle command mode */
        if (key == 58) { /* ':' key */
            printf("[LISP] Colon pressed after Escape, activating command mode\n");
            g_lisp_state->command_mode_active = true;
            return 0; /* Let Vizero handle the colon for command mode */
        }
        
        /* If user is typing Lisp-like characters, re-enable interactive mode */
        /* Only re-enable for characters that start Lisp expressions: letters, digits, (, +, -, *, / */
        if (g_lisp_state->sbcl.running &&
            ((key >= 'a' && key <= 'z') || (key >= 'A' && key <= 'Z') || 
             (key >= '0' && key <= '9') || key == '(' || key == '+' || 
             key == '-' || key == '*' || key == '/')) {
            printf("[LISP] User typing Lisp expression (key=%u), re-enabling interactive mode\n", key);
            g_lisp_state->interactive_mode = true;
            g_lisp_state->user_exited_interactive = false;
            g_lisp_state->paren_balance = 0;
        } else {
            printf("[LISP] Auto-re-enable conditions not met: sbcl_running=%d, key=%u\n", 
                   g_lisp_state->sbcl.running, key);
        }
    }
    
    /* Reset command mode when Enter is pressed (command completed) */
    if (g_lisp_state->command_mode_active && key == 13) { /* Enter key */
        printf("[LISP] Command completed, resetting flags\n");
        g_lisp_state->command_mode_active = false;
        g_lisp_state->user_exited_interactive = false;
    }
    
    /* Handle interactive REPL mode */
    if (in_repl_buffer && g_lisp_state->interactive_mode && !g_lisp_state->inserting_sbcl_output) {
        printf("[LISP] Routing key %u to interactive handler (interactive_mode=%d, user_exited=%d)\n", 
               key, g_lisp_state->interactive_mode, g_lisp_state->user_exited_interactive);
        return lisp_handle_interactive_input(editor, key, modifiers);
    }
    
    /* Only handle input if we're in the REPL buffer AND in a special REPL mode */
    if (!in_repl_buffer) {
        return 0;  /* Let Vizero handle input normally */
    }
    
    /* If we're in the REPL buffer but not in a special mode, let Vizero handle commands */
    if (g_lisp_state->ui_mode == 0) {
        return 0;  /* Let Vizero handle normal commands like :lisp-eval */
    }
    
    lisp_buffer_t* buffer = g_lisp_state->buffer_count > 0 ? g_lisp_state->buffers[0] : NULL;
    if (!buffer) return 0;
    
    /* Handle completion mode */
    if (buffer->showing_completions) {
        switch (key) {
            case 9:  /* Tab - select completion */
                if (buffer->completion_count > 0 && buffer->selected_completion >= 0) {
                    lisp_completion_t* completion = &buffer->completions[buffer->selected_completion];
                    /* Replace current symbol with completion */
                    strncpy(g_lisp_state->input_buffer, completion->text, sizeof(g_lisp_state->input_buffer) - 1);
                    g_lisp_state->input_buffer[sizeof(g_lisp_state->input_buffer) - 1] = '\0';
                }
                buffer->showing_completions = false;
                g_lisp_state->ui_mode = 0;
                return 1;
                
            case 27: /* Escape - cancel completion */
                buffer->showing_completions = false;
                g_lisp_state->ui_mode = 0;
                return 1;
                
            case 72: case 38: /* Up arrow */
                if (buffer->selected_completion > 0) {
                    buffer->selected_completion--;
                }
                return 1;
                
            case 80: case 40: /* Down arrow */
                if (buffer->selected_completion < (int)buffer->completion_count - 1) {
                    buffer->selected_completion++;
                }
                return 1;
        }
        return 1; /* Consume all input in completion mode */
    }
    
    /* Handle inspection mode */
    if (buffer->showing_inspection) {
        if (key == 27) { /* Escape - close inspection */
            buffer->showing_inspection = false;
            g_lisp_state->ui_mode = 0;
            return 1;
        }
        return 1; /* Consume input in inspection mode */
    }
    
    /* Handle debugger mode */
    if (buffer->in_debugger) {
        /* Enhanced debugger input handling would go here */
        /* For now, just allow escape to exit */
        if (key == 27) {
            buffer->in_debugger = false;
            g_lisp_state->ui_mode = 0;
            return 1;
        }
        return 0; /* Let normal command processing handle debugger commands */
    }
    
    /* Normal REPL input handling */
    if (key >= 32 && key <= 126) { /* Printable characters */
        size_t len = strlen(g_lisp_state->input_buffer);
        if (len < sizeof(g_lisp_state->input_buffer) - 1) {
            g_lisp_state->input_buffer[len] = (char)key;
            g_lisp_state->input_buffer[len + 1] = '\0';
            
            /* Auto-completion trigger on Ctrl+Space */
            if (modifiers & 4 && key == 32) { /* Ctrl+Space */
                char symbol[128];
                if (extract_symbol_at_cursor(g_lisp_state->input_buffer, len, symbol, sizeof(symbol))) {
                    /* Trigger completion */
                    char complete_cmd[256];
                    snprintf(complete_cmd, sizeof(complete_cmd), "%s", symbol);
                    lisp_cmd_complete(editor, complete_cmd);
                    buffer->showing_completions = true;
                    g_lisp_state->ui_mode = 1;
                }
                return 1;
            }
        }
        return 1; /* Consumed */
    }
    
    /* Special key handling */
    switch (key) {
        case 13: /* Enter */
            if (strlen(g_lisp_state->input_buffer) > 0) {
                /* Add to history */
                if (buffer->history_count < 50) {
                    strncpy(buffer->input_history[buffer->history_count], g_lisp_state->input_buffer, 511);
                    buffer->input_history[buffer->history_count][511] = '\0';
                    buffer->history_count++;
                }
                buffer->history_index = buffer->history_count;
                
                /* Check for balanced parentheses */
                int paren_balance = count_parens(g_lisp_state->input_buffer);
                if (paren_balance > 0) {
                    /* Multi-line input - add newline and continue */
                    size_t len = strlen(g_lisp_state->input_buffer);
                    if (len < sizeof(g_lisp_state->input_buffer) - 2) {
                        g_lisp_state->input_buffer[len] = '\n';
                        g_lisp_state->input_buffer[len + 1] = '\0';
                        buffer->multiline_input = true;
                        buffer->paren_depth = paren_balance;
                    }
                    return 1;
                }
                
                /* Complete expression - evaluate */
                if (g_lisp_state->input_buffer[0] == '/' || g_lisp_state->input_buffer[0] == ':') {
                    /* Let Vizero handle commands */
                    return 0;
                } else {
                    /* Evaluate Lisp expression */
                    if (g_lisp_state->sbcl.running) {
                        send_to_sbcl(g_lisp_state->input_buffer);
                        g_lisp_state->total_evaluations++;
                    } else {
                        lisp_log_message("SBCL not running. Use /lisp-connect to start.");
                    }
                }
                
                /* Clear input */
                g_lisp_state->input_buffer[0] = '\0';
                buffer->multiline_input = false;
                buffer->paren_depth = 0;
            }
            return 1;
            
        case 8: /* Backspace */
            {
                size_t len = strlen(g_lisp_state->input_buffer);
                if (len > 0) {
                    g_lisp_state->input_buffer[len - 1] = '\0';
                    /* Update parentheses tracking */
                    buffer->paren_depth = count_parens(g_lisp_state->input_buffer);
                    if (buffer->paren_depth <= 0) {
                        buffer->multiline_input = false;
                    }
                }
            }
            return 1;
            
        case 72: case 38: /* Up arrow - command history */
            if (buffer->history_count > 0 && buffer->history_index > 0) {
                buffer->history_index--;
                strncpy(g_lisp_state->input_buffer, buffer->input_history[buffer->history_index], 
                       sizeof(g_lisp_state->input_buffer) - 1);
                g_lisp_state->input_buffer[sizeof(g_lisp_state->input_buffer) - 1] = '\0';
            }
            return 1;
            
        case 80: case 40: /* Down arrow - command history */
            if (buffer->history_index < buffer->history_count - 1) {
                buffer->history_index++;
                strncpy(g_lisp_state->input_buffer, buffer->input_history[buffer->history_index], 
                       sizeof(g_lisp_state->input_buffer) - 1);
                g_lisp_state->input_buffer[sizeof(g_lisp_state->input_buffer) - 1] = '\0';
            } else if (buffer->history_index == buffer->history_count - 1) {
                buffer->history_index = buffer->history_count;
                g_lisp_state->input_buffer[0] = '\0';
            }
            return 1;
            
        case 27: /* Escape - exit REPL mode */
            g_lisp_state->wants_full_window = false;
            g_lisp_state->custom_rendering = false;
            return 1;
    }
    
    return 0;  /* Let Vizero handle other keys */
}

/* Enhanced Phase 2 Command Registration */
static vizero_plugin_command_t lisp_commands[] = {
    /* Basic Commands */
    {
        .command = "lisp-connect",
        .description = "Connect to enhanced SBCL REPL with Phase 2 features",
        .handler = lisp_cmd_connect,
        .user_data = NULL
    },
    {
        .command = "lisp-disconnect", 
        .description = "Disconnect from SBCL REPL and cleanup resources",
        .handler = lisp_cmd_disconnect,
        .user_data = NULL
    },
    {
        .command = "lisp-eval",
        .description = "Evaluate Lisp expression with enhanced formatting",
        .handler = lisp_cmd_eval,
        .user_data = NULL
    },
    {
        .command = "lisp-package",
        .description = "Change or show current Lisp package",
        .handler = lisp_cmd_package,
        .user_data = NULL
    },
    {
        .command = "lisp-status",
        .description = "Show detailed SBCL connection and feature status",
        .handler = lisp_cmd_status,
        .user_data = NULL
    },
    
    /* Advanced Phase 2 Commands */
    {
        .command = "lisp-complete",
        .description = "Show symbol completions for partial input",
        .handler = lisp_cmd_complete,
        .user_data = NULL
    },
    {
        .command = "lisp-inspect",
        .description = "Inspect Lisp object or symbol with detailed information",
        .handler = lisp_cmd_inspect,
        .user_data = NULL
    },
    {
        .command = "lisp-trace",
        .description = "Trace function calls for debugging",
        .handler = lisp_cmd_trace,
        .user_data = NULL
    },
    {
        .command = "lisp-untrace",
        .description = "Stop tracing function calls",
        .handler = lisp_cmd_untrace,
        .user_data = NULL
    },
    {
        .command = "lisp-load",
        .description = "Load Lisp source file into REPL",
        .handler = lisp_cmd_load,
        .user_data = NULL
    },
    {
        .command = "lisp-compile",
        .description = "Compile Lisp source file",
        .handler = lisp_cmd_compile,
        .user_data = NULL
    },
    {
        .command = "lisp-quicklisp",
        .description = "Load Quicklisp system or show status",
        .handler = lisp_cmd_quicklisp,
        .user_data = NULL
    },
    {
        .command = "lisp-help",
        .description = "Show comprehensive help for all REPL commands",
        .handler = lisp_cmd_help,
        .user_data = NULL
    },
    /* TODO Phase 3: Add SLIME protocol integration
    {
        .command = "lisp-slime-connect",
        .description = "Connect to SBCL via SLIME protocol for advanced features",
        .handler = lisp_cmd_slime_connect,
        .user_data = NULL
    },
    */
    {
        .command = "lisp-interactive",
        .description = "Re-enable interactive REPL mode for direct expression typing",
        .handler = lisp_cmd_interactive,
        .user_data = NULL
    },
    {
        .command = "lisp-return",
        .description = "Return to the original buffer before REPL was opened",
        .handler = lisp_cmd_return,
        .user_data = NULL
    }
};

/**
 * @brief Plugin information structure for LISP REPL
 * 
 * Defines metadata for the Interactive LISP REPL plugin including version,
 * author, description, and plugin type. Used by Vizero's plugin manager
 * for loading and identification purposes.
 */
VIZERO_PLUGIN_DEFINE_INFO(
    "lisp_repl",                                          /* name */
    "2.0.0",                                              /* version */
    "Vizero Team",                                        /* author */
    "Interactive LISP REPL with SBCL integration",       /* description */
    VIZERO_PLUGIN_TYPE_GENERIC                           /* type */
);


/**
 * @brief Initialize the LISP REPL plugin
 * 
 * Entry point for plugin initialization. Allocates plugin state, sets up command handlers,
 * configures callbacks for keyboard input processing, and initializes SBCL detection.
 * Registers plugin commands (:lisp-connect, :lisp-disconnect, :lisp-status) with the editor.
 * 
 * @param plugin Pointer to the plugin structure to initialize
 * @param editor Pointer to the Vizero editor instance
 * @param api Pointer to the editor API function table
 * @return 0 on successful initialization, -1 on failure
 */
int vizero_plugin_init(vizero_plugin_t* plugin, vizero_editor_t* editor, const vizero_editor_api_t* api) {
    /* Allocate plugin state */
    g_lisp_state = calloc(1, sizeof(lisp_repl_state_t));
    if (!g_lisp_state) {
        return -1;
    }
    
    /* Initialize Phase 2 enhanced state */
    g_lisp_state->editor = editor;
    g_lisp_state->api = api;
    g_lisp_state->buffer_count = 0;
    g_lisp_state->wants_full_window = false;
    g_lisp_state->custom_rendering = true;  /* Enable Phase 2 custom UI */
    g_lisp_state->repl_buffer = NULL;       /* Will be set when REPL connects */
    g_lisp_state->original_buffer = NULL;   /* Will be set when REPL connects */
    g_lisp_state->in_vi_command = false;
    g_lisp_state->input_buffer[0] = '\0';
    strcpy(g_lisp_state->current_buffer, "");
    
    /* Initialize SLIME protocol state */
    g_lisp_state->pending_count = 0;
    g_lisp_state->next_message_id = 1;
    
    /* Initialize UI state */
    g_lisp_state->ui_mode = 0;  /* Normal mode */
    g_lisp_state->show_welcome = true;
    g_lisp_state->animate_cursor = false;
    g_lisp_state->last_blink_time = 0;
    
    /* Initialize theme colours */
    g_lisp_state->prompt_colour = (vizero_colour_t){100, 255, 100, 255};      /* Green */
    g_lisp_state->input_colour = (vizero_colour_t){255, 255, 255, 255};       /* White */
    g_lisp_state->output_colour = (vizero_colour_t){200, 200, 255, 255};      /* Light blue */
    g_lisp_state->error_colour = (vizero_colour_t){255, 100, 100, 255};       /* Red */
    g_lisp_state->completion_colour = (vizero_colour_t){255, 255, 100, 255};  /* Yellow */
    
    /* Initialize feature flags */
    g_lisp_state->enable_completion = true;
    g_lisp_state->enable_inspection = true;
    g_lisp_state->enable_debugger = true;
    g_lisp_state->enable_tracing = true;
    g_lisp_state->auto_indent = true;
    
    /* Initialize performance tracking */
    g_lisp_state->total_evaluations = 0;
    g_lisp_state->total_eval_time = 0.0;
    
    /* Initialize enhanced SBCL process state */
    g_lisp_state->sbcl.process = INVALID_PROCESS_VALUE;
    g_lisp_state->sbcl.stdin_pipe = INVALID_PIPE_VALUE;
    g_lisp_state->sbcl.stdout_pipe = INVALID_PIPE_VALUE;
    g_lisp_state->sbcl.stderr_pipe = INVALID_PIPE_VALUE;
    g_lisp_state->sbcl.running = false;
    g_lisp_state->sbcl.sbcl_path[0] = '\0';
    g_lisp_state->sbcl.working_directory[0] = '\0';
    
    /* Initialize Phase 2 SBCL features */
    g_lisp_state->sbcl.swank_server_running = false;
    g_lisp_state->sbcl.swank_port = 4005;
    g_lisp_state->sbcl.slime_connected = false;
    g_lisp_state->sbcl.read_buffer[0] = '\0';
    g_lisp_state->sbcl.buffer_pos = 0;
    g_lisp_state->sbcl.in_multiline_read = false;
    g_lisp_state->sbcl.paren_depth = 0;
    g_lisp_state->sbcl.dynamic_space_size = 512;
    g_lisp_state->sbcl.enable_debugger = false;  /* Disabled for cleaner output */
    g_lisp_state->sbcl.load_quicklisp = true;
    g_lisp_state->sbcl.init_file[0] = '\0';
    
    /* Detect SBCL installation */
    if (detect_sbcl_installation(g_lisp_state->sbcl.sbcl_path, sizeof(g_lisp_state->sbcl.sbcl_path))) {
        char msg[1024];
        snprintf(msg, sizeof(msg), "Detected SBCL at: %s", g_lisp_state->sbcl.sbcl_path);
        lisp_log_message(msg);
    } else {
        lisp_log_message("SBCL not found in common locations. Use /lisp-status to check later.");
    }
    
    /* Create enhanced main REPL buffer */
    lisp_buffer_t* main_buffer = create_lisp_buffer("*lisp-repl*", "Lisp REPL Phase 2");
    if (main_buffer) {
        /* Initialize Phase 2 buffer features */
        main_buffer->history_count = 0;
        main_buffer->history_index = 0;
        main_buffer->completion_count = 0;
        main_buffer->selected_completion = 0;
        main_buffer->showing_completions = false;
        main_buffer->showing_inspection = false;
        main_buffer->syntax_highlighting = true;
        main_buffer->show_line_numbers = false;
        main_buffer->in_debugger = false;
        main_buffer->stack_depth = 0;
        main_buffer->debugger_prompt[0] = '\0';
        main_buffer->current_restart[0] = '\0';
        
        g_lisp_state->buffers[g_lisp_state->buffer_count++] = main_buffer;
    }
    
    /* Store original buffer */
    if (api && api->get_current_buffer) {
        g_lisp_state->original_buffer = api->get_current_buffer(editor);
    }
    
    /* Store state in plugin */
    plugin->user_data = g_lisp_state;
    
    /* Register callbacks */
    plugin->callbacks.commands = lisp_commands;
    plugin->callbacks.command_count = sizeof(lisp_commands) / sizeof(lisp_commands[0]);
    plugin->callbacks.render_full_window = lisp_render_full_window;
    plugin->callbacks.wants_full_window = lisp_wants_full_window;
    plugin->callbacks.on_key_input = lisp_on_key_input;
    
    lisp_log_message("=== Lisp REPL Plugin Phase 2 Initialized ===");
    lisp_log_message("Enhanced features: Completion, Inspection, Tracing, Advanced UI");
    
    /* Log available commands by category */
    lisp_log_message("Basic Commands: /lisp-connect, /lisp-disconnect, /lisp-eval, /lisp-package, /lisp-status");
    lisp_log_message("Advanced Commands: /lisp-complete, /lisp-inspect, /lisp-trace, /lisp-load, /lisp-compile");
    lisp_log_message("Utility Commands: /lisp-quicklisp, /lisp-help");
    
    char feature_msg[512];
    snprintf(feature_msg, sizeof(feature_msg), 
            "Registered %zu commands | Features: %s%s%s%s%s",
            plugin->callbacks.command_count,
            g_lisp_state->enable_completion ? "Completion " : "",
            g_lisp_state->enable_inspection ? "Inspection " : "",
            g_lisp_state->enable_debugger ? "Debugger " : "",
            g_lisp_state->enable_tracing ? "Tracing " : "",
            g_lisp_state->custom_rendering ? "CustomUI " : "");
    lisp_log_message(feature_msg);
    
    return 0;
}

/**
 * @brief Clean up and shutdown the LISP REPL plugin
 * 
 * Performs comprehensive cleanup when the plugin is being unloaded. Stops any running
 * SBCL processes gracefully, frees allocated buffers and message histories, and deallocates
 * the plugin state structure. Ensures no memory leaks or zombie processes remain.
 * 
 * @param plugin Pointer to the plugin structure being cleaned up
 */
void vizero_plugin_cleanup(vizero_plugin_t* plugin) {
    if (g_lisp_state) {
        /* Stop SBCL if running */
        if (g_lisp_state->sbcl.running) {
            stop_sbcl_process(&g_lisp_state->sbcl);
        }
        
        /* Clean up buffers */
        for (int i = 0; i < g_lisp_state->buffer_count; i++) {
            if (g_lisp_state->buffers[i]) {
                free(g_lisp_state->buffers[i]);
            }
        }
        
        /* Free state */
        free(g_lisp_state);
        g_lisp_state = NULL;
    }
    
    printf("[LISP] Lisp REPL plugin cleaned up\n");
}