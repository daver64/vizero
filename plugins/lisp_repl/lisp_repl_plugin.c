/* Lisp REPL Plugin for Vizero - Phase 1: Basic SBCL Integration
 * Features: SBCL detection, process management, basic SLIME protocol
 * Commands: /lisp-connect, /lisp-eval, /lisp-package, /lisp-disconnect
 */

#include "vizero/plugin_interface.h"
#include "vizero/renderer.h"
#include "vizero/colour_theme.h"
#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <time.h>

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

/* Lisp REPL Message structure */
typedef struct {
    char timestamp[16];
    char content[1024];
    enum {
        LISP_MSG_INPUT,     /* User input */
        LISP_MSG_OUTPUT,    /* SBCL output */
        LISP_MSG_ERROR,     /* Error messages */
        LISP_MSG_RESULT,    /* Evaluation results */
        LISP_MSG_INFO,      /* System info */
        LISP_MSG_DEBUG      /* Debug information */
    } type;
    vizero_colour_t colour;
} lisp_message_t;

/* SBCL Process Management */
typedef struct {
    process_t process;
    pipe_t stdin_pipe;
    pipe_t stdout_pipe;
    pipe_t stderr_pipe;
    bool running;
    char sbcl_path[512];
    char working_directory[512];
} sbcl_process_t;

/* Lisp REPL Buffer */
typedef struct {
    char name[64];
    char display_name[64];
    vizero_buffer_t* text_buffer;
    lisp_message_t messages[2000];
    size_t message_count;
    size_t scroll_offset;
    
    /* Input buffer */
    char input_text[1024];
    size_t input_cursor;
    
    /* REPL state */
    char current_package[64];
    int eval_counter;
    bool multiline_input;
    int paren_depth;
} lisp_buffer_t;

/* Main REPL State */
typedef struct {
    /* Core state */
    vizero_editor_t* editor;
    const vizero_editor_api_t* api;
    
    /* SBCL process */
    sbcl_process_t sbcl;
    
    /* Buffers */
    lisp_buffer_t* buffers[16];
    int buffer_count;
    char current_buffer[64];
    
    /* Input handling */
    char input_buffer[1024];
    bool in_vi_command;
    bool wants_full_window;
    
    /* Original editor state */
    vizero_buffer_t* original_buffer;
    
} lisp_repl_state_t;

/* Global plugin state */
static lisp_repl_state_t* g_lisp_state = NULL;

/* Utility: Get current timestamp */
static void get_timestamp(char* buffer, size_t size) {
    time_t now = time(NULL);
    struct tm* tm_info = localtime(&now);
    strftime(buffer, size, "%H:%M:%S", tm_info);
}

/* Utility: Log message to main REPL buffer */
static void lisp_log_message(const char* message) {
    if (!g_lisp_state || g_lisp_state->buffer_count == 0) {
        printf("[LISP] %s\n", message);
        return;
    }
    
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
    
    printf("[LISP] %s\n", message);
}

/* SBCL Detection */
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

/* SBCL Process Management */
static bool start_sbcl_process(sbcl_process_t* proc) {
    if (!proc->sbcl_path[0]) {
        lisp_log_message("ERROR: No SBCL path configured");
        return false;
    }
    
    lisp_log_message("Starting SBCL process...");
    
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
    
    /* Create SBCL command line */
    char cmdline[1024];
    snprintf(cmdline, sizeof(cmdline), "\"%s\" --dynamic-space-size 512", proc->sbcl_path);
    
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
        
        /* Execute SBCL */
        execl(proc->sbcl_path, "sbcl", "--dynamic-space-size", "512", NULL);
        
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
    lisp_log_message("SBCL process started successfully");
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

/* Read output from SBCL (non-blocking) */
static void read_sbcl_output(void) {
    if (!g_lisp_state->sbcl.running) return;
    
    char buffer[4096];
    
#ifdef _WIN32
    DWORD bytes_available = 0;
    DWORD bytes_read = 0;
    
    /* Check stdout */
    if (PeekNamedPipe(g_lisp_state->sbcl.stdout_pipe, NULL, 0, NULL, &bytes_available, NULL) && bytes_available > 0) {
        if (ReadFile(g_lisp_state->sbcl.stdout_pipe, buffer, sizeof(buffer) - 1, &bytes_read, NULL) && bytes_read > 0) {
            buffer[bytes_read] = '\0';
            lisp_log_message(buffer);
        }
    }
    
    /* Check stderr */
    if (PeekNamedPipe(g_lisp_state->sbcl.stderr_pipe, NULL, 0, NULL, &bytes_available, NULL) && bytes_available > 0) {
        if (ReadFile(g_lisp_state->sbcl.stderr_pipe, buffer, sizeof(buffer) - 1, &bytes_read, NULL) && bytes_read > 0) {
            buffer[bytes_read] = '\0';
            lisp_log_message(buffer);
        }
    }
#else
    ssize_t bytes_read;
    
    /* Check stdout */
    bytes_read = read(g_lisp_state->sbcl.stdout_pipe, buffer, sizeof(buffer) - 1);
    if (bytes_read > 0) {
        buffer[bytes_read] = '\0';
        lisp_log_message(buffer);
    }
    
    /* Check stderr */
    bytes_read = read(g_lisp_state->sbcl.stderr_pipe, buffer, sizeof(buffer) - 1);
    if (bytes_read > 0) {
        buffer[bytes_read] = '\0';
        lisp_log_message(buffer);
    }
#endif
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

/* Command handlers */
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
    
    /* Switch to REPL buffer */
    lisp_set_active_buffer("*lisp-repl*");
    
    /* Send initial setup commands to SBCL */
    send_to_sbcl("(format t \"~&; Vizero Lisp REPL ready~%\")");
    send_to_sbcl("(force-output)");
    
    return 0;
}

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
    
    /* Log the evaluation */
    char log_msg[1024];
    snprintf(log_msg, sizeof(log_msg), "Evaluating: %s", args);
    lisp_log_message(log_msg);
    
    /* Send to SBCL */
    if (!send_to_sbcl(args)) {
        return -1;
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

static int lisp_cmd_status(vizero_editor_t* editor, const char* args) {
    char status_msg[512];
    if (g_lisp_state->sbcl.running) {
        snprintf(status_msg, sizeof(status_msg), 
                "SBCL Status: Running (PID: %s)\nSBCL Path: %s\nCurrent Package: %s", 
#ifdef _WIN32
                "Windows Process",
#else
                g_lisp_state->sbcl.process != INVALID_PROCESS_VALUE ? "Active" : "Unknown",
#endif
                g_lisp_state->sbcl.sbcl_path,
                g_lisp_state->buffer_count > 0 ? g_lisp_state->buffers[0]->current_package : "Unknown");
    } else {
        snprintf(status_msg, sizeof(status_msg), "SBCL Status: Not running");
    }
    
    lisp_log_message(status_msg);
    return 0;
}

/* Buffer management */
static void lisp_set_active_buffer(const char* buffer_name) {
    strncpy(g_lisp_state->current_buffer, buffer_name, sizeof(g_lisp_state->current_buffer) - 1);
    g_lisp_state->current_buffer[sizeof(g_lisp_state->current_buffer) - 1] = '\0';
    g_lisp_state->wants_full_window = true;
    
    char msg[128];
    snprintf(msg, sizeof(msg), "Switched to buffer: %s", buffer_name);
    lisp_log_message(msg);
}

/* Rendering - simple text-based for Phase 1 */
static int lisp_wants_full_window(vizero_editor_t* editor) {
    return g_lisp_state && g_lisp_state->wants_full_window ? 1 : 0;
}

static int lisp_render_full_window(vizero_editor_t* editor, vizero_renderer_t* renderer, int width, int height) {
    if (!g_lisp_state || g_lisp_state->buffer_count == 0) return 0;
    
    lisp_buffer_t* buffer = g_lisp_state->buffers[0];
    if (!buffer) return 0;
    
    /* For Phase 1, just show messages in the status area */
    /* Full custom rendering will be implemented in Phase 2 */
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
    
    return 0;
}

/* Input handling */
static int lisp_on_key_input(vizero_editor_t* editor, uint32_t key, uint32_t modifiers) {
    /* For Phase 1, we'll handle REPL input through commands only */
    /* Custom key handling will be implemented in Phase 2 */
    return 0;  /* Let Vizero handle all input */
}

/* Command registration */
static vizero_plugin_command_t lisp_commands[] = {
    {
        .command = "lisp-connect",
        .description = "Connect to SBCL REPL",
        .handler = lisp_cmd_connect,
        .user_data = NULL
    },
    {
        .command = "lisp-disconnect", 
        .description = "Disconnect from SBCL REPL",
        .handler = lisp_cmd_disconnect,
        .user_data = NULL
    },
    {
        .command = "lisp-eval",
        .description = "Evaluate Lisp expression",
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
        .description = "Show SBCL connection status",
        .handler = lisp_cmd_status,
        .user_data = NULL
    }
};

/* Plugin information */
VIZERO_PLUGIN_DEFINE_INFO(
    "lisp_repl",                              /* name */
    "1.0.0",                                  /* version */
    "Vizero Team",                            /* author */
    "Lisp REPL Plugin - SBCL Integration",   /* description */
    VIZERO_PLUGIN_TYPE_GENERIC               /* type */
);

/* Plugin entry points */
int vizero_plugin_init(vizero_plugin_t* plugin, vizero_editor_t* editor, const vizero_editor_api_t* api) {
    /* Allocate plugin state */
    g_lisp_state = calloc(1, sizeof(lisp_repl_state_t));
    if (!g_lisp_state) {
        return -1;
    }
    
    /* Initialize state */
    g_lisp_state->editor = editor;
    g_lisp_state->api = api;
    g_lisp_state->buffer_count = 0;
    g_lisp_state->wants_full_window = false;
    g_lisp_state->in_vi_command = false;
    g_lisp_state->input_buffer[0] = '\0';
    strcpy(g_lisp_state->current_buffer, "");
    
    /* Initialize SBCL process state */
    g_lisp_state->sbcl.process = INVALID_PROCESS_VALUE;
    g_lisp_state->sbcl.stdin_pipe = INVALID_PIPE_VALUE;
    g_lisp_state->sbcl.stdout_pipe = INVALID_PIPE_VALUE;
    g_lisp_state->sbcl.stderr_pipe = INVALID_PIPE_VALUE;
    g_lisp_state->sbcl.running = false;
    g_lisp_state->sbcl.sbcl_path[0] = '\0';
    g_lisp_state->sbcl.working_directory[0] = '\0';
    
    /* Detect SBCL installation */
    if (detect_sbcl_installation(g_lisp_state->sbcl.sbcl_path, sizeof(g_lisp_state->sbcl.sbcl_path))) {
        char msg[1024];
        snprintf(msg, sizeof(msg), "Detected SBCL at: %s", g_lisp_state->sbcl.sbcl_path);
        lisp_log_message(msg);
    } else {
        lisp_log_message("SBCL not found in common locations. Use /lisp-status to check later.");
    }
    
    /* Create main REPL buffer */
    lisp_buffer_t* main_buffer = create_lisp_buffer("*lisp-repl*", "Lisp REPL");
    if (main_buffer) {
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
    
    lisp_log_message("Lisp REPL Plugin Phase 1 initialized");
    
    /* Log available commands */
    for (size_t i = 0; i < plugin->callbacks.command_count; i++) {
        char cmd_msg[256];
        snprintf(cmd_msg, sizeof(cmd_msg), "Command: /%s - %s", 
                lisp_commands[i].command, lisp_commands[i].description);
        lisp_log_message(cmd_msg);
    }
    
    return 0;
}

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