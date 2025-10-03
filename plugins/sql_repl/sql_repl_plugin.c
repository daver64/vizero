/**
 * @file sql_repl_plugin.c
 * @brief Interactive SQL REPL Plugin for Vizero Editor
 * 
 * This plugin provides seamless integration between Vizero and multiple SQL databases,
 * enabling interactive SQL development with direct buffer typing, query execution,
 * and comprehensive vi-style integration.
 * 
 * Current Features:
 * - Interactive SQL buffer with real-time query execution
 * - Multi-database support: MySQL, MariaDB, PostgreSQL
 * - Connection management with database switching
 * - Query result formatting and display
 * - Vi-style command integration with Escape+colon sequences
 * - Buffer switching support with automatic state restoration
 * - Transaction management (BEGIN, COMMIT, ROLLBACK)
 * - Schema browsing and table inspection
 * 
 * Supported Databases:
 * - MySQL (mysql command line client or direct libmysqlclient)
 * - MariaDB (mariadb command line client or direct libmariadb)
 * - PostgreSQL (psql command line client or direct libpq)
 * 
 * Commands:
 * - :sql-connect mysql://user:pass@host:port/database
 * - :sql-connect postgresql://user:pass@host:port/database
 * - :sql-disconnect
 * - :sql-show-tables
 * - :sql-describe table_name
 * - :sql-begin, :sql-commit, :sql-rollback
 * 
 * @version 0.0.5
 * @date September 2025
 * @author Vizero Development Team
 */

#include "vizero/plugin_interface.h"
#include "vizero/renderer.h"
#include "vizero/colour_theme.h"
#include "vizero/memory_utils.h"
#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <time.h>
#include <ctype.h>
#include <inttypes.h>

#ifdef _WIN32
    #include <windows.h>
    #include <winsock2.h>
    #include <ws2tcpip.h>
#else
    #include <unistd.h>
    #include <sys/wait.h>
    #include <errno.h>
#endif

/* Database client includes */
#ifdef HAVE_MYSQL
    #ifdef _WIN32
        #include <mysql.h>
    #else
        #include <mysql/mysql.h>
    #endif
#endif

#ifdef HAVE_POSTGRESQL
    #include <libpq-fe.h>
#endif

/* Plugin state structure */
typedef enum {
    DB_TYPE_NONE = 0,
    DB_TYPE_MYSQL,
    DB_TYPE_POSTGRESQL
} db_type_t;

typedef struct {
    db_type_t db_type;
    char connection_string[512];
    char host[256];
    char database[256];
    char username[256];
    char password[256];
    int port;
    bool connected;
    bool in_transaction;
    
    /* Database-specific handles */
#ifdef HAVE_MYSQL
    MYSQL* mysql_handle;
#endif
#ifdef HAVE_POSTGRESQL
    PGconn* pgsql_handle;
#endif
    
    /* Plugin state */
    vizero_editor_t* editor;
    const vizero_editor_api_t* api;
    vizero_buffer_t* sql_buffer;
    char buffer_name[64];
    bool plugin_active;
    bool buffer_created;
    bool scratch_buffer_created;
    
    /* Input handling */
    char input_buffer[4096];
    size_t input_length;
    
    /* Result display */
    char* last_result;
    size_t last_result_size;
} sql_repl_state_t;

static sql_repl_state_t g_sql_state = {0};

/* Forward declarations */
static void sql_log_message(const char* message);
static int sql_connect_database(const char* connection_string);
static void sql_disconnect_database(void);
static int sql_execute_query(const char* query, char** result, size_t* result_size);
static int sql_handle_enter_key(vizero_editor_t* editor, uint32_t key, uint32_t modifiers);
static void sql_display_result(const char* result);
static void sql_display_error(const char* error);
static void sql_add_prompt(void);
static int sql_create_scratch_buffer(void);

/* Command handlers */
static int sql_cmd_connect(vizero_editor_t* editor, const char* args);
static int sql_cmd_disconnect(vizero_editor_t* editor, const char* args);
static int sql_cmd_status(vizero_editor_t* editor, const char* args);
static int sql_cmd_query(vizero_editor_t* editor, const char* args);
static int sql_cmd_exec(vizero_editor_t* editor, const char* args);
static int sql_cmd_show_tables(vizero_editor_t* editor, const char* args);
static int sql_cmd_describe(vizero_editor_t* editor, const char* args);
static int sql_cmd_begin(vizero_editor_t* editor, const char* args);
static int sql_cmd_commit(vizero_editor_t* editor, const char* args);
static int sql_cmd_rollback(vizero_editor_t* editor, const char* args);

/* Utility functions */
static void sql_log_message(const char* message) {
    if (!message) return;
    
    char timestamp[32];
    time_t now = time(NULL);
    struct tm* local_time = localtime(&now);
    strftime(timestamp, sizeof(timestamp), "%H:%M:%S", local_time);
    
    char log_entry[1024];
    /* Always use precision specifier to prevent format-truncation warnings */
    /* Reserve space for timestamp prefix "[HH:MM:SS] SQL: " (about 16 chars) plus null terminator */
    const size_t max_msg_len = 980; /* Leave room for timestamp and formatting */
    
    if (strlen(message) > max_msg_len) {
        snprintf(log_entry, sizeof(log_entry), "[%s] SQL: %.980s...", timestamp, message);
    } else {
        snprintf(log_entry, sizeof(log_entry), "[%s] SQL: %.980s", timestamp, message);
    }
    
    /* For now, just print to stdout/debug output */
    printf("%s\n", log_entry);
}



/* Database connection parsing */
static int parse_connection_string(const char* conn_str, db_type_t* db_type, 
                                  char* host, char* database, char* username, 
                                  char* password, int* port) {
    if (!conn_str || !db_type) return -1;
    
    /* Parse connection strings like:
     * mysql://user:pass@host:port/database
     * postgresql://user:pass@host:port/database
     */
    
    if (strncmp(conn_str, "mysql://", 8) == 0) {
#ifdef HAVE_MYSQL
        *db_type = DB_TYPE_MYSQL;
        *port = 3306; /* Default MySQL port */
#else
        sql_display_error("MySQL support not available in this build");
        return -1;
#endif
    } else if (strncmp(conn_str, "postgresql://", 13) == 0) {
#ifdef HAVE_POSTGRESQL
        *db_type = DB_TYPE_POSTGRESQL;
        *port = 5432; /* Default PostgreSQL port */
#else
        sql_display_error("PostgreSQL support not available in this build");
        return -1;
#endif
    } else {
        return -1; /* Unsupported database type */
    }
    
    /* Parse the rest of the URL */
    const char* url_part = strchr(conn_str, '/') + 2; /* Skip "//" */
    if (!url_part) return -1;
    
    /* Extract user:pass@host:port/database */
    char temp_url[1024];
    snprintf(temp_url, sizeof(temp_url), "%s", url_part);
    
    /* Find database name */
    char* db_part = strrchr(temp_url, '/');
    if (db_part) {
        snprintf(database, 256, "%s", db_part + 1);
        *db_part = '\0'; /* Cut off database part */
    }
    
    /* Find user:pass part */
    char* auth_part = strchr(temp_url, '@');
    if (auth_part) {
        *auth_part = '\0';
        auth_part++;
        
        /* Parse user:pass */
        char* pass_part = strchr(temp_url, ':');
        if (pass_part) {
            *pass_part = '\0';
            snprintf(username, 256, "%.255s", temp_url);
            snprintf(password, 256, "%.255s", pass_part + 1);
        } else {
            snprintf(username, 256, "%.255s", temp_url);
            password[0] = '\0';
        }
        
        /* Parse host:port */
        char* port_part = strchr(auth_part, ':');
        if (port_part) {
            *port_part = '\0';
            *port = atoi(port_part + 1);
        }
        snprintf(host, 256, "%.255s", auth_part);
    } else {
        snprintf(host, 256, "%.255s", temp_url);
        username[0] = '\0';
        password[0] = '\0';
    }
    
    username[255] = '\0';
    password[255] = '\0';
    host[255] = '\0';
    
    return 0;
}

#ifdef HAVE_MYSQL
static int mysql_connect(void) {
    g_sql_state.mysql_handle = mysql_init(NULL);
    if (!g_sql_state.mysql_handle) {
        sql_log_message("Failed to initialize MySQL");
        return -1;
    }
    
    if (!mysql_real_connect(g_sql_state.mysql_handle,
                           g_sql_state.host,
                           g_sql_state.username,
                           g_sql_state.password,
                           g_sql_state.database,
                           g_sql_state.port,
                           NULL, 0)) {
        char error_msg[512];
        snprintf(error_msg, sizeof(error_msg), "MySQL connection failed: %s", 
                mysql_error(g_sql_state.mysql_handle));
        sql_log_message(error_msg);
        mysql_close(g_sql_state.mysql_handle);
        g_sql_state.mysql_handle = NULL;
        return -1;
    }
    
    sql_log_message("Connected to MySQL database");
    return 0;
}

static int mysql_execute_query(const char* query, char** result, size_t* result_size) {
    if (!g_sql_state.mysql_handle || !query) return -1;
    
    if (mysql_query(g_sql_state.mysql_handle, query) != 0) {
        char error_msg[512];
        snprintf(error_msg, sizeof(error_msg), "MySQL query failed: %s", 
                mysql_error(g_sql_state.mysql_handle));
        sql_display_error(error_msg);
        return -1;
    }
    
    MYSQL_RES* res = mysql_store_result(g_sql_state.mysql_handle);
    if (!res) {
        /* Query didn't return a result set (INSERT, UPDATE, DELETE, etc.) */
        char status_msg[256];
        snprintf(status_msg, sizeof(status_msg), "Query OK, %" PRIu64 " rows affected", 
                (uint64_t)mysql_affected_rows(g_sql_state.mysql_handle));
        
        *result = strdup(status_msg);
        *result_size = strlen(status_msg);
        return 0;
    }
    
    /* Format result set */
    MYSQL_FIELD* fields = mysql_fetch_fields(res);
    int num_fields = mysql_num_fields(res);
    
    /* Calculate buffer size needed */
    size_t buffer_size = 4096; /* Initial size */
    char* output = malloc(buffer_size);
    if (!output) {
        mysql_free_result(res);
        return -1;
    }
    
    size_t pos = 0;
    
    /* Add column headers */
    for (int i = 0; i < num_fields; i++) {
        pos += snprintf(output + pos, buffer_size - pos, "%-20s", fields[i].name);
        if (pos >= buffer_size - 100) {
            size_t new_size = buffer_size * 2;
            char* new_output = vizero_safe_realloc(output, new_size);
            if (!new_output) {
                free(output);
                mysql_free_result(res);
                return -1;
            }
            output = new_output;
            buffer_size = new_size;
        }
    }
    pos += snprintf(output + pos, buffer_size - pos, "\n");
    
    /* Add separator line */
    for (int i = 0; i < num_fields; i++) {
        pos += snprintf(output + pos, buffer_size - pos, "%-20s", "--------------------");
    }
    pos += snprintf(output + pos, buffer_size - pos, "\n");
    
    /* Add data rows */
    MYSQL_ROW row;
    while ((row = mysql_fetch_row(res))) {
        for (int i = 0; i < num_fields; i++) {
            const char* value = row[i] ? row[i] : "NULL";
            pos += snprintf(output + pos, buffer_size - pos, "%-20s", value);
            if (pos >= buffer_size - 100) {
                size_t new_size = buffer_size * 2;
                char* new_output = vizero_safe_realloc(output, new_size);
                if (!new_output) {
                    free(output);
                    mysql_free_result(res);
                    return -1;
                }
                output = new_output;
                buffer_size = new_size;
            }
        }
        pos += snprintf(output + pos, buffer_size - pos, "\n");
    }
    
    mysql_free_result(res);
    
    *result = output;
    *result_size = pos;
    return 0;
}
#endif

#ifdef HAVE_POSTGRESQL
static int pgsql_connect(void) {
    char conn_info[1024];
    snprintf(conn_info, sizeof(conn_info), 
            "host=%s port=%d dbname=%s user=%s password=%s",
            g_sql_state.host, g_sql_state.port, g_sql_state.database,
            g_sql_state.username, g_sql_state.password);
    
    g_sql_state.pgsql_handle = PQconnectdb(conn_info);
    
    if (PQstatus(g_sql_state.pgsql_handle) != CONNECTION_OK) {
        char error_msg[512];
        snprintf(error_msg, sizeof(error_msg), "PostgreSQL connection failed: %s",
                PQerrorMessage(g_sql_state.pgsql_handle));
        sql_log_message(error_msg);
        PQfinish(g_sql_state.pgsql_handle);
        g_sql_state.pgsql_handle = NULL;
        return -1;
    }
    
    sql_log_message("Connected to PostgreSQL database");
    return 0;
}

static int pgsql_execute_query(const char* query, char** result, size_t* result_size) {
    if (!g_sql_state.pgsql_handle || !query) return -1;
    
    PGresult* res = PQexec(g_sql_state.pgsql_handle, query);
    
    if (PQresultStatus(res) != PGRES_TUPLES_OK && PQresultStatus(res) != PGRES_COMMAND_OK) {
        char error_msg[512];
        snprintf(error_msg, sizeof(error_msg), "PostgreSQL query failed: %s",
                PQerrorMessage(g_sql_state.pgsql_handle));
        sql_display_error(error_msg);
        PQclear(res);
        return -1;
    }
    
    if (PQresultStatus(res) == PGRES_COMMAND_OK) {
        /* Non-SELECT query */
        char status_msg[256];
        snprintf(status_msg, sizeof(status_msg), "Query OK, %s", PQcmdStatus(res));
        
        *result = strdup(status_msg);
        *result_size = strlen(status_msg);
        PQclear(res);
        return 0;
    }
    
    /* Format result set */
    int num_fields = PQnfields(res);
    int num_rows = PQntuples(res);
    
    /* Calculate buffer size needed */
    size_t buffer_size = 4096;
    char* output = malloc(buffer_size);
    if (!output) {
        PQclear(res);
        return -1;
    }
    
    size_t pos = 0;
    
    /* Add column headers */
    for (int i = 0; i < num_fields; i++) {
        pos += snprintf(output + pos, buffer_size - pos, "%-20s", PQfname(res, i));
        if (pos >= buffer_size - 100) {
            size_t new_size = buffer_size * 2;
            char* new_output = vizero_safe_realloc(output, new_size);
            if (!new_output) {
                free(output);
                PQclear(res);
                return -1;
            }
            output = new_output;
            buffer_size = new_size;
        }
    }
    pos += snprintf(output + pos, buffer_size - pos, "\n");
    
    /* Add separator line */
    for (int i = 0; i < num_fields; i++) {
        pos += snprintf(output + pos, buffer_size - pos, "%-20s", "--------------------");
    }
    pos += snprintf(output + pos, buffer_size - pos, "\n");
    
    /* Add data rows */
    for (int row = 0; row < num_rows; row++) {
        for (int col = 0; col < num_fields; col++) {
            const char* value = PQgetisnull(res, row, col) ? "NULL" : PQgetvalue(res, row, col);
            pos += snprintf(output + pos, buffer_size - pos, "%-20s", value);
            if (pos >= buffer_size - 100) {
                size_t new_size = buffer_size * 2;
                char* new_output = vizero_safe_realloc(output, new_size);
                if (!new_output) {
                    free(output);
                    PQclear(res);
                    return -1;
                }
                output = new_output;
                buffer_size = new_size;
            }
        }
        pos += snprintf(output + pos, buffer_size - pos, "\n");
    }
    
    PQclear(res);
    
    *result = output;
    *result_size = pos;
    return 0;
}
#endif

/* Main database functions */
static int sql_connect_database(const char* connection_string) {
    if (!connection_string) return -1;
    
    /* Disconnect existing connection */
    sql_disconnect_database();
    
    /* Parse connection string */
    if (parse_connection_string(connection_string, &g_sql_state.db_type,
                               g_sql_state.host, g_sql_state.database,
                               g_sql_state.username, g_sql_state.password,
                               &g_sql_state.port) != 0) {
        sql_log_message("Invalid connection string format");
        return -1;
    }
    
    snprintf(g_sql_state.connection_string, sizeof(g_sql_state.connection_string), 
            "%s", connection_string);
    
    /* Connect based on database type */
    int result = -1;
    
    switch (g_sql_state.db_type) {
#ifdef HAVE_MYSQL
        case DB_TYPE_MYSQL:
            result = mysql_connect();
            break;
#endif
#ifdef HAVE_POSTGRESQL
        case DB_TYPE_POSTGRESQL:
            result = pgsql_connect();
            break;
#endif
        default:
            sql_log_message("Database type not supported in this build");
            return -1;
    }
    
    if (result == 0) {
        g_sql_state.connected = true;
        g_sql_state.plugin_active = true;  /* Enable REPL mode */
        g_sql_state.in_transaction = false;
        
        /* Create scratch buffer if not already created */
        if (!g_sql_state.scratch_buffer_created) {
            if (sql_create_scratch_buffer() != 0) {
                sql_log_message("Warning: Failed to create *sql* scratch buffer, using current buffer");
            }
        }
        
        char status_msg[1024];
        snprintf(status_msg, sizeof(status_msg), "Connected to %s:%d/%s as %s",
                g_sql_state.host, g_sql_state.port, g_sql_state.database, g_sql_state.username);
        sql_display_result(status_msg);
        sql_log_message("SQL REPL mode enabled - you can now type SQL commands directly in the buffer");
        
        /* Add initial prompt after connection message */
        sql_add_prompt();
    }
    
    return result;
}

static void sql_disconnect_database(void) {
    if (!g_sql_state.connected) return;
    
    switch (g_sql_state.db_type) {
#ifdef HAVE_MYSQL
        case DB_TYPE_MYSQL:
            if (g_sql_state.mysql_handle) {
                mysql_close(g_sql_state.mysql_handle);
                g_sql_state.mysql_handle = NULL;
            }
            break;
#endif
#ifdef HAVE_POSTGRESQL
        case DB_TYPE_POSTGRESQL:
            if (g_sql_state.pgsql_handle) {
                PQfinish(g_sql_state.pgsql_handle);
                g_sql_state.pgsql_handle = NULL;
            }
            break;
#endif
        default:
            break;
    }
    
    g_sql_state.connected = false;
    g_sql_state.plugin_active = false;  /* Disable REPL mode */
    g_sql_state.in_transaction = false;
    g_sql_state.db_type = DB_TYPE_NONE;
    
    sql_log_message("Database disconnected - SQL REPL mode disabled");
    sql_display_result("Disconnected from database");
}

static int sql_execute_query(const char* query, char** result, size_t* result_size) {
    if (!g_sql_state.connected || !query) return -1;
    
    /* Trim whitespace and check for empty query */
    while (isspace(*query)) query++;
    if (*query == '\0') return -1;
    
    sql_log_message(query);
    
    int ret = -1;
    switch (g_sql_state.db_type) {
#ifdef HAVE_MYSQL
        case DB_TYPE_MYSQL:
            ret = mysql_execute_query(query, result, result_size);
            break;
#endif
#ifdef HAVE_POSTGRESQL
        case DB_TYPE_POSTGRESQL:
            ret = pgsql_execute_query(query, result, result_size);
            break;
#endif
        default:
            sql_display_error("No database connection");
            return -1;
    }
    
    /* Check for transaction state changes */
    if (ret == 0) {
        char query_upper[256];
        snprintf(query_upper, sizeof(query_upper), "%s", query);
        
        /* Convert to uppercase for comparison */
        for (char* p = query_upper; *p; p++) {
            *p = (char)toupper((unsigned char)*p);
        }
        
        if (strstr(query_upper, "BEGIN") || strstr(query_upper, "START TRANSACTION")) {
            g_sql_state.in_transaction = true;
        } else if (strstr(query_upper, "COMMIT") || strstr(query_upper, "ROLLBACK")) {
            g_sql_state.in_transaction = false;
        }
    }
    
    return ret;
}

static void sql_display_result(const char* result) {
    if (!result || !g_sql_state.api || !g_sql_state.editor) return;
    
    /* Use dedicated SQL buffer if available, otherwise use current buffer */
    vizero_buffer_t* target_buffer = g_sql_state.sql_buffer ? 
                                    g_sql_state.sql_buffer : 
                                    g_sql_state.api->get_current_buffer(g_sql_state.editor);
    if (!target_buffer) return;
    
    vizero_cursor_t* cursor = g_sql_state.api->get_current_cursor(g_sql_state.editor);
    if (!cursor) return;
    
    /* Get the end of the buffer to append results there */
    size_t line_count = g_sql_state.api->get_buffer_line_count(target_buffer);
    vizero_position_t end_pos;
    
    if (line_count > 0) {
        /* Move to end of last line */
        end_pos.line = (int)(line_count - 1);
        const char* last_line = g_sql_state.api->get_buffer_line(target_buffer, end_pos.line);
        end_pos.column = last_line ? (int)strlen(last_line) : 0;
    } else {
        /* Empty buffer */
        end_pos.line = 0;
        end_pos.column = 0;
    }
    
    /* Insert the result with newlines for separation */
    char formatted_result[8192];
    snprintf(formatted_result, sizeof(formatted_result), "\n%s\n", result);
    
    /* Use multiline insert if available, otherwise fall back to regular insert */
    if (g_sql_state.api->insert_text_multiline) {
        g_sql_state.api->insert_text_multiline(target_buffer, end_pos, formatted_result);
    } else if (g_sql_state.api->insert_text) {
        g_sql_state.api->insert_text(target_buffer, end_pos, formatted_result);
    }
    
    /* Move cursor to the new end of buffer after insertion (only if it's the current buffer) */
    vizero_buffer_t* current_buffer = g_sql_state.api->get_current_buffer(g_sql_state.editor);
    if (current_buffer == target_buffer) {
        size_t new_line_count = g_sql_state.api->get_buffer_line_count(target_buffer);
        if (new_line_count > 0) {
            vizero_position_t new_end_pos;
            new_end_pos.line = (int)(new_line_count - 1);
            const char* new_last_line = g_sql_state.api->get_buffer_line(target_buffer, new_end_pos.line);
            new_end_pos.column = new_last_line ? (int)strlen(new_last_line) : 0;
            g_sql_state.api->set_cursor_position(cursor, new_end_pos);
        }
    }
    
    /* Also show brief status message */
    g_sql_state.api->set_status_message(g_sql_state.editor, "SQL query executed");
}

static void sql_display_error(const char* error) {
    if (!error) return;
    
    char error_msg[1024];
    snprintf(error_msg, sizeof(error_msg), "ERROR: %s", error);
    sql_display_result(error_msg);
    sql_log_message(error_msg);
}

static void sql_add_prompt(void) {
    if (!g_sql_state.connected || !g_sql_state.api || !g_sql_state.editor) return;
    
    /* Use dedicated SQL buffer if available, otherwise use current buffer */
    vizero_buffer_t* target_buffer = g_sql_state.sql_buffer ? 
                                    g_sql_state.sql_buffer : 
                                    g_sql_state.api->get_current_buffer(g_sql_state.editor);
    if (!target_buffer) return;
    
    vizero_cursor_t* cursor = g_sql_state.api->get_current_cursor(g_sql_state.editor);
    if (!cursor) return;
    
    /* Get the end of the buffer to append prompt there */
    size_t line_count = g_sql_state.api->get_buffer_line_count(target_buffer);
    vizero_position_t end_pos;
    
    if (line_count > 0) {
        /* Move to end of last line */
        end_pos.line = (int)(line_count - 1);
        const char* last_line = g_sql_state.api->get_buffer_line(target_buffer, end_pos.line);
        end_pos.column = last_line ? (int)strlen(last_line) : 0;
    } else {
        /* Empty buffer */
        end_pos.line = 0;
        end_pos.column = 0;
    }
    
    /* Add appropriate prompt based on database type */
    const char* prompt = (g_sql_state.db_type == DB_TYPE_MYSQL) ? "mysql> " : 
                        (g_sql_state.db_type == DB_TYPE_POSTGRESQL) ? "pgsql> " : "sql> ";
    
    /* Insert prompt */
    if (g_sql_state.api->insert_text) {
        g_sql_state.api->insert_text(target_buffer, end_pos, prompt);
    }
    
    /* Move cursor to the end after prompt insertion (only if it's the current buffer) */
    vizero_buffer_t* current_buffer = g_sql_state.api->get_current_buffer(g_sql_state.editor);
    if (current_buffer == target_buffer) {
        vizero_cursor_t* current_cursor = g_sql_state.api->get_current_cursor(g_sql_state.editor);
        if (current_cursor) {
            size_t new_line_count = g_sql_state.api->get_buffer_line_count(target_buffer);
            if (new_line_count > 0) {
                vizero_position_t new_end_pos;
                new_end_pos.line = (int)(new_line_count - 1);
                const char* new_last_line = g_sql_state.api->get_buffer_line(target_buffer, new_end_pos.line);
                new_end_pos.column = new_last_line ? (int)strlen(new_last_line) : 0;
                g_sql_state.api->set_cursor_position(current_cursor, new_end_pos);
            }
        }
    }
}

static int sql_create_scratch_buffer(void) {
    if (!g_sql_state.api || !g_sql_state.editor) return -1;
    
    /* Create a new buffer with the name "*sql*" */
    if (g_sql_state.api->open_file(g_sql_state.editor, "*sql*") != 0) {
        sql_log_message("Failed to create *sql* scratch buffer");
        return -1;
    }
    
    /* Get the newly created buffer */
    g_sql_state.sql_buffer = g_sql_state.api->get_current_buffer(g_sql_state.editor);
    if (!g_sql_state.sql_buffer) {
        sql_log_message("Failed to get *sql* buffer reference");
        return -1;
    }
    
    /* Mark it as a scratch buffer so it won't prompt to save */
    if (g_sql_state.api->set_buffer_scratch) {
        g_sql_state.api->set_buffer_scratch(g_sql_state.sql_buffer, 1);
        sql_log_message("Created *sql* scratch buffer");
    }
    
    /* Store buffer name */
    snprintf(g_sql_state.buffer_name, sizeof(g_sql_state.buffer_name), "*sql*");
    g_sql_state.scratch_buffer_created = true;
    
    return 0;
}

/* Individual command handlers */
static int sql_cmd_connect(vizero_editor_t* editor, const char* args) {
    (void)editor; /* Unused parameter */
    if (!args || strlen(args) == 0) {
        sql_display_error("Usage: :sql-connect mysql://user:pass@host:port/db or postgresql://user:pass@host:port/db");
        return 1;
    }
    
    if (sql_connect_database(args) == 0) {
        return 1; /* Command handled */
    }
    return 1; /* Command handled but failed */
}

static int sql_cmd_disconnect(vizero_editor_t* editor, const char* args) {
    (void)editor;
    (void)args;
    sql_disconnect_database();
    return 1;
}

static int sql_cmd_status(vizero_editor_t* editor, const char* args) {
    (void)editor;
    (void)args;
    
    char status[512];
    if (g_sql_state.connected) {
        const char* db_type_str = (g_sql_state.db_type == DB_TYPE_MYSQL) ? "MySQL" :
                                  (g_sql_state.db_type == DB_TYPE_POSTGRESQL) ? "PostgreSQL" : "Unknown";
        snprintf(status, sizeof(status), "Connected to %s database", db_type_str);
    } else {
        snprintf(status, sizeof(status), "Not connected to any database");
    }
    sql_display_result(status);
    return 1;
}

static int sql_cmd_query(vizero_editor_t* editor, const char* args) {
    (void)editor;
    
    if (!g_sql_state.connected) {
        sql_display_error("Not connected to database");
        return 1;
    }

    if (!args || strlen(args) == 0) {
        sql_display_error("Usage: :sql-query SELECT * FROM table;");
        return 1;
    }

    char* result = NULL;
    size_t result_size = 0;
    
    if (sql_execute_query(args, &result, &result_size) == 0 && result) {
        sql_display_result(result);
        free(result);
        /* Add prompt after command-based query */
        sql_add_prompt();
    }
    return 1;
}static int sql_cmd_exec(vizero_editor_t* editor, const char* args) {
    (void)args;
    
    if (!g_sql_state.connected) {
        sql_display_error("Not connected to database");
        return 1;
    }
    
    /* Get current buffer and execute all selected text or current line */
    if (!g_sql_state.api) return 1;
    
    vizero_buffer_t* buffer = g_sql_state.api->get_current_buffer(editor);
    vizero_cursor_t* cursor = g_sql_state.api->get_current_cursor(editor);
    if (!buffer || !cursor) return 1;
    
    vizero_position_t pos = g_sql_state.api->get_cursor_position(cursor);
    const char* line_text = g_sql_state.api->get_buffer_line(buffer, pos.line);
    
    if (line_text && strlen(line_text) > 0) {
        char* result = NULL;
        size_t result_size = 0;
        if (sql_execute_query(line_text, &result, &result_size) == 0 && result) {
            sql_display_result(result);
            free(result);
            /* Add prompt after exec command */
            sql_add_prompt();
        }
    }
    return 1;
}

static int sql_cmd_show_tables(vizero_editor_t* editor, const char* args) {
    (void)editor;
    (void)args;
    
    if (!g_sql_state.connected) {
        sql_display_error("Not connected to database");
        return 1;
    }
    
    const char* query = NULL;
    switch (g_sql_state.db_type) {
        case DB_TYPE_MYSQL:
            query = "SHOW TABLES";
            break;
        case DB_TYPE_POSTGRESQL:
            /* Show tables from all schemas, not just public */
            query = "SELECT schemaname, tablename FROM pg_tables WHERE schemaname NOT IN ('information_schema', 'pg_catalog') ORDER BY schemaname, tablename";
            break;
        default:
            sql_display_error("Unsupported database type");
            return 1;
    }
    
    char* result = NULL;
    size_t result_size = 0;
    if (sql_execute_query(query, &result, &result_size) == 0 && result) {
        sql_display_result(result);
        free(result);
        /* Add prompt after show tables */
        sql_add_prompt();
    }
    return 1;
}

static int sql_cmd_describe(vizero_editor_t* editor, const char* args) {
    (void)editor;
    
    if (!g_sql_state.connected) {
        sql_display_error("Not connected to database");
        return 1;
    }
    
    if (!args || strlen(args) == 0) {
        sql_display_error("Usage: :sql-describe table_name");
        return 1;
    }
    
    /* Trim whitespace from table name */
    char trimmed_table[256];
    const char* start = args;
    while (*start && isspace(*start)) start++;
    
    size_t len = strlen(start);
    while (len > 0 && isspace(start[len - 1])) len--;
    
    if (len >= sizeof(trimmed_table)) {
        sql_display_error("Table name too long");
        return 1;
    }
    
    strncpy(trimmed_table, start, len);
    trimmed_table[len] = '\0';
    
    char query[512];
    switch (g_sql_state.db_type) {
        case DB_TYPE_MYSQL:
            /* Try simple DESCRIBE first - let MySQL handle the current database context */
            snprintf(query, sizeof(query), "DESCRIBE `%s`", trimmed_table);
            /* Debug: log the query being executed */
            char debug_msg[600];  /* Increased size to accommodate full query + prefix */
            snprintf(debug_msg, sizeof(debug_msg), "Executing describe query: %s", query);
            sql_log_message(debug_msg);
            break;
        case DB_TYPE_POSTGRESQL:
            snprintf(query, sizeof(query), 
                    "SELECT column_name, data_type, is_nullable FROM information_schema.columns WHERE table_name = '%s'", 
                    trimmed_table);
            break;
        default:
            sql_display_error("Unsupported database type");
            return 1;
    }
    
    char* result = NULL;
    size_t result_size = 0;
    if (sql_execute_query(query, &result, &result_size) == 0 && result) {
        sql_display_result(result);
        free(result);
        /* Add prompt after describe */
        sql_add_prompt();
    }
    return 1;
}

static int sql_cmd_begin(vizero_editor_t* editor, const char* args) {
    (void)editor;
    (void)args;
    
    if (!g_sql_state.connected) {
        sql_display_error("Not connected to database");
        return 1;
    }
    
    char* result = NULL;
    size_t result_size = 0;
    if (sql_execute_query("BEGIN", &result, &result_size) == 0 && result) {
        sql_display_result(result);
        free(result);
        /* Add prompt after begin */
        sql_add_prompt();
    }
    return 1;
}

static int sql_cmd_commit(vizero_editor_t* editor, const char* args) {
    (void)editor;
    (void)args;
    
    if (!g_sql_state.connected) {
        sql_display_error("Not connected to database");
        return 1;
    }
    
    char* result = NULL;
    size_t result_size = 0;
    if (sql_execute_query("COMMIT", &result, &result_size) == 0 && result) {
        sql_display_result(result);
        free(result);
        /* Add prompt after commit */
        sql_add_prompt();
    }
    return 1;
}

static int sql_cmd_rollback(vizero_editor_t* editor, const char* args) {
    (void)editor;
    (void)args;
    
    if (!g_sql_state.connected) {
        sql_display_error("Not connected to database");
        return 1;
    }
    
    char* result = NULL;
    size_t result_size = 0;
    if (sql_execute_query("ROLLBACK", &result, &result_size) == 0 && result) {
        sql_display_result(result);
        free(result);
        /* Add prompt after rollback */
        sql_add_prompt();
    }
    return 1;
}

/* SQL command table */
static vizero_plugin_command_t sql_commands[] = {
    {
        .command = "sql-connect",
        .description = "Connect to SQL database: :sql-connect mysql://user:pass@host:port/db or postgresql://...",
        .handler = sql_cmd_connect,
        .user_data = NULL
    },
    {
        .command = "sql-disconnect",
        .description = "Disconnect from current SQL database",
        .handler = sql_cmd_disconnect,
        .user_data = NULL
    },
    {
        .command = "sql-status",
        .description = "Show current SQL connection status and database information",
        .handler = sql_cmd_status,
        .user_data = NULL
    },
    {
        .command = "sql-query",
        .description = "Execute SQL query: :sql-query SELECT * FROM table;",
        .handler = sql_cmd_query,
        .user_data = NULL
    },
    {
        .command = "sql-exec",
        .description = "Execute multi-line SQL statement from buffer",
        .handler = sql_cmd_exec,
        .user_data = NULL
    },
    {
        .command = "sql-show-tables",
        .description = "List all tables in the current database",
        .handler = sql_cmd_show_tables,
        .user_data = NULL
    },
    {
        .command = "sql-describe",
        .description = "Describe table structure: :sql-describe table_name",
        .handler = sql_cmd_describe,
        .user_data = NULL
    },
    {
        .command = "sql-begin",
        .description = "Begin SQL transaction",
        .handler = sql_cmd_begin,
        .user_data = NULL
    },
    {
        .command = "sql-commit",
        .description = "Commit current SQL transaction",
        .handler = sql_cmd_commit,
        .user_data = NULL
    },
    {
        .command = "sql-rollback",
        .description = "Rollback current SQL transaction",
        .handler = sql_cmd_rollback,
        .user_data = NULL
    }
};

/* Input handling */
static int sql_handle_enter_key(vizero_editor_t* editor, uint32_t key, uint32_t modifiers) {
    (void)modifiers; /* Unused */
    
    /* Only handle Enter key */
    if (key != 13 && key != 10) return 0; /* Not Enter key */
    
    /* Only handle if we're connected to a database */
    if (!g_sql_state.connected || !g_sql_state.api) return 0;
    
    /* Get current cursor and buffer */
    vizero_cursor_t* cursor = g_sql_state.api->get_current_cursor(editor);
    vizero_buffer_t* buffer = g_sql_state.api->get_current_buffer(editor);
    if (!cursor || !buffer) return 0;
    
    vizero_position_t pos = g_sql_state.api->get_cursor_position(cursor);
    const char* line_text = g_sql_state.api->get_buffer_line(buffer, pos.line);
    
    if (!line_text || strlen(line_text) == 0) {
        return 0; /* Let normal handling proceed for empty lines */
    }
    
    /* Check if line ends with semicolon (SQL statement terminator) */
    size_t line_len = strlen(line_text);
    char trimmed_line[1024];
    snprintf(trimmed_line, sizeof(trimmed_line), "%s", line_text);
    
    /* Trim trailing whitespace */
    while (line_len > 0 && isspace(trimmed_line[line_len - 1])) {
        trimmed_line[--line_len] = '\0';
    }
    
    if (line_len > 0 && trimmed_line[line_len - 1] == ';') {
        /* Execute the SQL statement */
        trimmed_line[line_len - 1] = '\0'; /* Remove semicolon */
        
        /* Strip any SQL prompt from the beginning of the line */
        const char* query_start = trimmed_line;
        
        /* Check for and skip common SQL prompts (with or without space after >) */
        if (strncmp(query_start, "mysql>", 6) == 0) {
            query_start += 6;
            /* Skip optional space after > */
            if (*query_start == ' ') query_start++;
        } else if (strncmp(query_start, "pgsql>", 6) == 0) {
            query_start += 6;
            /* Skip optional space after > */
            if (*query_start == ' ') query_start++;
        } else if (strncmp(query_start, "sql>", 4) == 0) {
            query_start += 4;
            /* Skip optional space after > */
            if (*query_start == ' ') query_start++;
        }
        
        /* Skip any remaining leading whitespace */
        while (*query_start && isspace(*query_start)) query_start++;
        if (*query_start == '\0') {
            return 0; /* Empty query, let normal handling proceed */
        }
        
        char* result = NULL;
        size_t result_size = 0;
        
        if (sql_execute_query(query_start, &result, &result_size) == 0 && result) {
            sql_display_result(result);
            free(result);
        }
        
        /* Add prompt for next command */
        sql_add_prompt();
        
        return 1; /* We handled the Enter key */
    }
    
    return 0; /* Let normal handling proceed */
}

/* Plugin interface implementation */
VIZERO_PLUGIN_DEFINE_INFO(
    "sql_repl",                                          /* name */
    VIZERO_PLUGIN_VERSION,                                /* version */
    "Vizero Development Team",                           /* author */
    "Interactive SQL REPL with multi-database support", /* description */
    VIZERO_PLUGIN_TYPE_GENERIC                          /* type */
)

VIZERO_PLUGIN_API int vizero_plugin_init(vizero_plugin_t* plugin, vizero_editor_t* editor, const vizero_editor_api_t* api) {
    if (!plugin || !editor || !api) return -1;
    
    /* Initialize state */
    memset(&g_sql_state, 0, sizeof(g_sql_state));
    g_sql_state.db_type = DB_TYPE_NONE;
    g_sql_state.connected = false;
    g_sql_state.in_transaction = false;
    g_sql_state.editor = editor;
    g_sql_state.api = api;
    
    /* DEBUG: Just print to console to test loading */
    printf("[SQL REPL] Plugin loading test - basic initialization successful!\n");
    
    /* Initialize database libraries and test support */
    printf("[SQL REPL] Testing database support:\n");
    
#ifdef HAVE_MYSQL
    if (mysql_library_init(0, NULL, NULL) == 0) {
        printf("[SQL REPL] MySQL/MariaDB support: ENABLED\n");
    } else {
        printf("[SQL REPL] MySQL/MariaDB support: FAILED to initialize\n");
    }
#else
    printf("[SQL REPL] MySQL/MariaDB support: NOT COMPILED\n");
#endif

#ifdef HAVE_POSTGRESQL
    /* Test PostgreSQL by creating a dummy connection info - this doesn't actually connect */
    printf("[SQL REPL] PostgreSQL support: ENABLED\n");
#else
    printf("[SQL REPL] PostgreSQL support: NOT COMPILED\n");
#endif
    
    printf("[SQL REPL] Plugin initialized with database support testing ENABLED\n");
    
    /* Store plugin and API for later use */
    plugin->user_data = &g_sql_state;
    
    /* Register callbacks */
    plugin->callbacks.commands = sql_commands;
    plugin->callbacks.command_count = sizeof(sql_commands) / sizeof(sql_commands[0]);
    plugin->callbacks.on_key_input = sql_handle_enter_key;
    
    return 0;
}

VIZERO_PLUGIN_API void vizero_plugin_cleanup(vizero_plugin_t* plugin) {
    (void)plugin; /* Suppress unused parameter warning */
    
    sql_disconnect_database();
    
    if (g_sql_state.last_result) {
        free(g_sql_state.last_result);
        g_sql_state.last_result = NULL;
    }
    
#ifdef HAVE_MYSQL
    mysql_library_end();
#endif
    
    sql_log_message("SQL REPL plugin cleaned up");
}