/* Claude LLM Plugin for Vizero
 * Provides Claude Sonnet integration for code completion, chat, and code explanation
 * Uses libcurl for HTTP requests and nlohmann JSON for response parsing
 */

#include "vizero/plugin_interface.h"
#include "vizero/json_parser.h"
#include "vizero/memory_utils.h"
#include "vizero/settings.h"
#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <stdarg.h>
#include <time.h>
#include <ctype.h>
#include <curl/curl.h>

/* Forward declarations */
static int handle_llm_test_command(vizero_editor_t* editor, const char* args);
static int handle_claude_chat_command(vizero_editor_t* editor, const char* args);
static int handle_claude_ask_command(vizero_editor_t* editor, const char* args);
static int handle_claude_clear_command(vizero_editor_t* editor, const char* args);
static int claude_handle_enter_key(vizero_editor_t* editor, uint32_t key, uint32_t modifiers);
static int create_claude_buffer(vizero_editor_t* editor);
static void append_to_claude_buffer(const char* text);
static void display_claude_response(const char* response);
static void claude_add_prompt(void);
static char* escape_json_string(const char* str);

/* Windows doesn't have asprintf, so we implement it */
#ifdef _WIN32
static int _asprintf(char** strp, const char* fmt, ...) {
    va_list args;
    va_start(args, fmt);
    int len = _vscprintf(fmt, args);
    if (len < 0) {
        va_end(args);
        return -1;
    }
    *strp = (char*)malloc(len + 1);
    if (!*strp) {
        va_end(args);
        return -1;
    }
    int result = vsprintf_s(*strp, len + 1, fmt, args);
    va_end(args);
    return result;
}
#define asprintf _asprintf
#endif

/* Plugin metadata */
VIZERO_PLUGIN_DEFINE_INFO(
    "Claude LLM",
    "1.0.0",
    "Vizero Team",
    "Claude Sonnet integration for code completion and AI assistance",
    VIZERO_PLUGIN_TYPE_LANGUAGE_SERVER
)

/* Plugin state structure */
typedef struct {
    /* API and editor references */
    const vizero_editor_api_t* api;
    vizero_editor_t* editor;
    
    /* Configuration */
    char* api_token;
    char* endpoint_url;
    char* model_name;
    
    /* HTTP client state */
    CURL* curl_handle;
    
    /* Chat buffer state */
    vizero_buffer_t* claude_buffer;
    bool in_claude_mode;
    char* conversation_history;
    size_t history_length;
    
    /* Plugin status */
    bool initialized;
    bool curl_global_init_done;
} github_llm_state_t;

/* Global plugin state */
static github_llm_state_t* g_llm_state = NULL;

/* HTTP response structure for curl callbacks */
typedef struct {
    char* data;
    size_t size;
} http_response_t;

/* libcurl write callback function */
static size_t write_callback(void* contents, size_t size, size_t nmemb, http_response_t* response) {
    size_t realsize = size * nmemb;
    
    char* ptr = (char*)vizero_safe_realloc(response->data, response->size + realsize + 1);
    if (!ptr) {
        /* Out of memory - return 0 to signal error to curl */
        return 0;
    }
    
    response->data = ptr;
    memcpy(&(response->data[response->size]), contents, realsize);
    response->size += realsize;
    response->data[response->size] = '\0'; /* Null terminate */
    
    return realsize;
}

/* Load configuration from claude-key.txt file in settings directory */
static int load_llm_config(void) {
    if (!g_llm_state) return -1;
    
    /* Get the config directory path */
    const char* config_dir = vizero_settings_get_config_directory();
    if (!config_dir) {
        printf("[Claude LLM] Error: Could not get settings directory\n");
        return -1;
    }
    
    /* Build the full path to claude-key.txt in settings directory */
    char key_file_path[512];
#ifdef _WIN32
    snprintf(key_file_path, sizeof(key_file_path), "%s\\claude-key.txt", config_dir);
#else
    snprintf(key_file_path, sizeof(key_file_path), "%s/claude-key.txt", config_dir);
#endif
    
    /* Load API token from claude-key.txt file in settings directory */
    const char* token = NULL;
    FILE* key_file = fopen(key_file_path, "r");
    if (key_file) {
        static char file_token[512];
        if (fgets(file_token, sizeof(file_token), key_file)) {
            /* Remove newline if present */
            char* newline = strchr(file_token, '\n');
            if (newline) *newline = '\0';
            /* Remove carriage return if present */
            char* cr = strchr(file_token, '\r');
            if (cr) *cr = '\0';
            token = file_token;
            printf("[Claude LLM] Found API key in %s\n", key_file_path);
        }
        fclose(key_file);
    }
    
    if (token) {
        g_llm_state->api_token = vizero_safe_strdup(token);
        if (!g_llm_state->api_token) {
            return -1;
        }
    } else {
        printf("[Claude LLM] Error: No API token found in %s\n", key_file_path);
        printf("  Please create claude-key.txt file in your Vizero settings directory\n");
        printf("  Settings directory: %s\n", config_dir);
        return -1;
    }
    
    /* Set default endpoint for Claude API */
    const char* endpoint = getenv("LLM_ENDPOINT");
    if (!endpoint) {
        endpoint = "https://api.anthropic.com/v1/messages"; /* Default to Claude API */
    }
    
    g_llm_state->endpoint_url = vizero_safe_strdup(endpoint);
    if (!g_llm_state->endpoint_url) {
        return -1;
    }
    
    /* Set default model */
    const char* model = getenv("LLM_MODEL");
    if (!model) {
        model = "claude-3-haiku-20240307"; /* Claude 3 Haiku model */
    }
    
    g_llm_state->model_name = vizero_safe_strdup(model);
    if (!g_llm_state->model_name) {
        return -1;
    }
    
    printf("[Claude LLM] Configuration loaded: endpoint=%s, model=%s\n", 
           g_llm_state->endpoint_url, g_llm_state->model_name);
    
    return 0;
}

/* Escape a string for safe inclusion in JSON */
static char* escape_json_string(const char* str) {
    if (!str) return NULL;
    
    size_t len = strlen(str);
    /* Worst case: every character needs escaping (2x expansion) plus null terminator */
    char* escaped = vizero_safe_malloc(len * 2 + 1);
    if (!escaped) return NULL;
    
    char* dst = escaped;
    for (const char* src = str; *src; src++) {
        switch (*src) {
            case '"':  *dst++ = '\\'; *dst++ = '"'; break;
            case '\\': *dst++ = '\\'; *dst++ = '\\'; break;
            case '/':  *dst++ = '\\'; *dst++ = '/'; break;
            case '\b': *dst++ = '\\'; *dst++ = 'b'; break;
            case '\f': *dst++ = '\\'; *dst++ = 'f'; break;
            case '\n': *dst++ = '\\'; *dst++ = 'n'; break;
            case '\r': *dst++ = '\\'; *dst++ = 'r'; break;
            case '\t': *dst++ = '\\'; *dst++ = 't'; break;
            default:
                if ((unsigned char)*src < 32) {
                    /* Escape other control characters as \uXXXX */
                    sprintf(dst, "\\u%04x", (unsigned char)*src);
                    dst += 6;
                } else {
                    *dst++ = *src;
                }
                break;
        }
    }
    *dst = '\0';
    return escaped;
}

/* Send HTTP request to LLM API */
static int send_llm_request(const char* prompt, const char* system_prompt, http_response_t* response) {
    if (!g_llm_state || !g_llm_state->curl_handle || !prompt || !response) {
        return -1;
    }
    
    /* Initialize response */
    response->data = NULL;
    response->size = 0;
    
    /* Build JSON payload for Claude API */
    char* json_payload;
    int json_len;
    
    /* Escape the strings for JSON */
    char* escaped_prompt = escape_json_string(prompt);
    char* escaped_system = system_prompt ? escape_json_string(system_prompt) : NULL;
    
    if (!escaped_prompt) {
        printf("[Claude LLM] Failed to escape prompt string\n");
        return -1;
    }
    
    if (escaped_system) {
        json_len = asprintf(&json_payload,
            "{"
            "\"model\":\"%s\","
            "\"max_tokens\":1024,"
            "\"temperature\":0.7,"
            "\"system\":\"%s\","
            "\"messages\":["
                "{\"role\":\"user\",\"content\":\"%s\"}"
            "]"
            "}",
            g_llm_state->model_name, escaped_system, escaped_prompt);
    } else {
        json_len = asprintf(&json_payload,
            "{"
            "\"model\":\"%s\","
            "\"max_tokens\":1024,"
            "\"temperature\":0.7,"
            "\"messages\":["
                "{\"role\":\"user\",\"content\":\"%s\"}"
            "]"
            "}",
            g_llm_state->model_name, escaped_prompt);
    }
    
    /* Clean up escaped strings */
    vizero_safe_free(escaped_prompt);
    if (escaped_system) vizero_safe_free(escaped_system);
    
    if (json_len < 0 || !json_payload) {
        return -1;
    }
    
    /* Set curl options */
    curl_easy_setopt(g_llm_state->curl_handle, CURLOPT_URL, g_llm_state->endpoint_url);
    curl_easy_setopt(g_llm_state->curl_handle, CURLOPT_POSTFIELDS, json_payload);
    curl_easy_setopt(g_llm_state->curl_handle, CURLOPT_WRITEFUNCTION, write_callback);
    curl_easy_setopt(g_llm_state->curl_handle, CURLOPT_WRITEDATA, response);
    
    /* Enable HTTPS/SSL support */
    curl_easy_setopt(g_llm_state->curl_handle, CURLOPT_SSL_VERIFYPEER, 1L);
    curl_easy_setopt(g_llm_state->curl_handle, CURLOPT_SSL_VERIFYHOST, 2L);
    curl_easy_setopt(g_llm_state->curl_handle, CURLOPT_FOLLOWLOCATION, 1L);
    curl_easy_setopt(g_llm_state->curl_handle, CURLOPT_TIMEOUT, 30L);
    
    /* Add debug info */
    curl_easy_setopt(g_llm_state->curl_handle, CURLOPT_VERBOSE, 1L);
    
    /* Set headers for Claude API */
    struct curl_slist* headers = NULL;
    char auth_header[512];
    snprintf(auth_header, sizeof(auth_header), "x-api-key: %s", g_llm_state->api_token);
    
    headers = curl_slist_append(headers, "Content-Type: application/json");
    headers = curl_slist_append(headers, "anthropic-version: 2023-06-01");
    headers = curl_slist_append(headers, auth_header);
    curl_easy_setopt(g_llm_state->curl_handle, CURLOPT_HTTPHEADER, headers);
    
    /* Perform the request */
    CURLcode res = curl_easy_perform(g_llm_state->curl_handle);
    
    /* Cleanup */
    curl_slist_free_all(headers);
    free(json_payload);
    
    if (res != CURLE_OK) {
        printf("[Claude LLM] Request failed: %s (Error code: %d)\n", curl_easy_strerror(res), res);
        printf("[Claude LLM] URL was: %s\n", g_llm_state->endpoint_url);
        if (response->data) {
            vizero_safe_free(response->data);
            response->data = NULL;
            response->size = 0;
        }
        return -1;
    }
    
    /* Check HTTP response code */
    long response_code;
    curl_easy_getinfo(g_llm_state->curl_handle, CURLINFO_RESPONSE_CODE, &response_code);
    
    if (response_code != 200) {
        printf("[GitHub LLM] HTTP error: %ld\n", response_code);
        if (response->data) {
            printf("[GitHub LLM] Response: %s\n", response->data);
        }
        return -1;
    }
    
    return 0;
}

/* Extract content from LLM response JSON */
static char* extract_llm_content(const char* json_response) {
    if (!json_response) return NULL;
    
    vizero_json_t* json = vizero_json_parse(json_response, strlen(json_response));
    if (!json) {
        printf("[Claude LLM] Failed to parse JSON response\n");
        return NULL;
    }
    
    char* content = NULL;
    
    /* Parse Claude-style response: content[0].text */
    vizero_json_t* content_array = vizero_json_get_array(json, "content");
    if (content_array && vizero_json_array_size(content_array) > 0) {
        vizero_json_t* first_content = vizero_json_array_get(content_array, 0);
        if (first_content) {
            content = vizero_json_get_string(first_content, "text");
        }
    }
    
    vizero_json_free(json);
    return content; /* Caller must free this */
}

/* Command handlers */
static int handle_llm_test_command(vizero_editor_t* editor, const char* args) {
    (void)args; /* Unused for now */
    
    if (!g_llm_state || !g_llm_state->initialized) {
        if (g_llm_state->api && g_llm_state->api->set_status_message) {
            g_llm_state->api->set_status_message(editor, "LLM plugin not initialized");
        }
        return -1;
    }

    /* Try different Claude model names to find one that works */
    const char* models_to_try[] = {
        "claude-3-5-sonnet-20241022",
        "claude-3-5-sonnet-20240620",
        "claude-3-5-sonnet",
        "claude-3-opus-20240229",
        "claude-3-sonnet-20240229",
        "claude-3-haiku-20240307",
        "claude-instant-1.2",
        "claude-2.1",
        "claude-2.0",
        NULL
    };
    
    /* Save original model */
    char* original_model = g_llm_state->model_name ? vizero_safe_strdup(g_llm_state->model_name) : NULL;
    
    for (int i = 0; models_to_try[i]; i++) {
        printf("[Claude LLM] Trying model: %s\n", models_to_try[i]);
        
        /* Update model for this test */
        if (g_llm_state->model_name) {
            vizero_safe_free(g_llm_state->model_name);
        }
        g_llm_state->model_name = vizero_safe_strdup(models_to_try[i]);
        
        /* Simple test request */
        http_response_t response = {0};
        int result = send_llm_request("Hello, respond with 'LLM is working!'", 
                                      "You are a helpful assistant.", &response);
        
        if (result == 0 && response.data) {
            char* content = extract_llm_content(response.data);
            if (content) {
                char status_msg[256];
                snprintf(status_msg, sizeof(status_msg), "SUCCESS with %s: %.150s", models_to_try[i], content);
                if (g_llm_state->api && g_llm_state->api->set_status_message) {
                    g_llm_state->api->set_status_message(editor, status_msg);
                }
                printf("[Claude LLM] SUCCESS! Working model: %s\n", models_to_try[i]);
                vizero_safe_free(content);
                vizero_safe_free(response.data);
                
                /* Keep the working model */
                if (original_model) vizero_safe_free(original_model);
                return 0;
            }
            vizero_safe_free(response.data);
        }
        
        printf("[Claude LLM] Model %s failed, trying next...\n", models_to_try[i]);
    }
    
    /* Restore original model if all failed */
    if (g_llm_state->model_name) {
        vizero_safe_free(g_llm_state->model_name);
    }
    g_llm_state->model_name = original_model;
    
    if (g_llm_state->api && g_llm_state->api->set_status_message) {
        g_llm_state->api->set_status_message(editor, "All Claude models failed - check API key or account");
    }
    printf("[Claude LLM] All models failed. Check your API key and account access.\n");
    
    return -1;
}

/* Create or switch to Claude chat buffer */
static int create_claude_buffer(vizero_editor_t* editor) {
    if (!g_llm_state || !g_llm_state->api) {
        return -1;
    }
    
    /* Create dedicated Claude buffer using enew with name parameter */
    if (g_llm_state->api->execute_command) {
        int enew_result = g_llm_state->api->execute_command(editor, "enew *claude-haiku*");
        if (enew_result == 0) {
            /* Try to find the Claude buffer */
            vizero_buffer_t* claude_buffer_found = NULL;
            
            for (int buf_num = 1; buf_num <= 10; buf_num++) {
                char cmd[32];
                snprintf(cmd, sizeof(cmd), "b%d", buf_num);
                
                int switch_result = g_llm_state->api->execute_command(editor, cmd);
                if (switch_result == 0) {
                    if (g_llm_state->api->get_current_buffer && g_llm_state->api->get_buffer_filename) {
                        vizero_buffer_t* current = g_llm_state->api->get_current_buffer(editor);
                        const char* filename = g_llm_state->api->get_buffer_filename(current);
                        
                        if (filename && strcmp(filename, "*claude-haiku*") == 0) {
                            claude_buffer_found = current;
                            printf("[Claude LLM] Found Claude buffer at buffer %d!\n", buf_num);
                            break;
                        }
                    }
                }
            }
            
            if (claude_buffer_found) {
                g_llm_state->claude_buffer = claude_buffer_found;
                g_llm_state->in_claude_mode = true;
                
                /* Mark as scratch buffer */
                if (g_llm_state->api->set_buffer_scratch) {
                    g_llm_state->api->set_buffer_scratch(g_llm_state->claude_buffer, 1);
                }
                
                /* Add welcome message if buffer is empty */
                if (g_llm_state->api->get_buffer_line_count) {
                    int line_count = g_llm_state->api->get_buffer_line_count(g_llm_state->claude_buffer);
                    if (line_count <= 1) {
                        append_to_claude_buffer("=== Claude Haiku Chat ===\n");
                        append_to_claude_buffer("Type your questions after the 'You> ' prompt and press Enter to send.\n");
                        append_to_claude_buffer("Use :claude-clear to clear history.\n\n");
                        
                        /* Add the first prompt */
                        claude_add_prompt();
                    }
                }
                
                return 0;
            }
        }
    }
    
    return -1;
}

/* Append text to Claude buffer */
static void append_to_claude_buffer(const char* text) {
    if (!g_llm_state || !g_llm_state->claude_buffer || !g_llm_state->api) {
        return;
    }
    
    /* Get buffer line count to append at the end */
    if (g_llm_state->api->get_buffer_line_count && g_llm_state->api->insert_text_multiline) {
        int line_count = g_llm_state->api->get_buffer_line_count(g_llm_state->claude_buffer);
        
        /* Position at the end of the last line */
        vizero_position_t pos;
        pos.line = line_count > 0 ? line_count - 1 : 0;
        pos.column = 0;
        
        /* If there are existing lines, get the length of the last line */
        if (line_count > 0 && g_llm_state->api->get_buffer_line_length) {
            pos.column = g_llm_state->api->get_buffer_line_length(g_llm_state->claude_buffer, pos.line);
        }
        
        g_llm_state->api->insert_text_multiline(g_llm_state->claude_buffer, pos, text);
    }
}

/* Display Claude response in the chat buffer */
static void display_claude_response(const char* response) {
    if (!response || !g_llm_state || !g_llm_state->claude_buffer) {
        return;
    }
    
    char timestamp[64];
    time_t now = time(NULL);
    struct tm* tm_info = localtime(&now);
    strftime(timestamp, sizeof(timestamp), "%H:%M:%S", tm_info);
    
    char* formatted_response = vizero_safe_malloc(strlen(response) + 256);
    if (formatted_response) {
        snprintf(formatted_response, strlen(response) + 256, 
                "[%s] Claude: %s\n\n", timestamp, response);
        append_to_claude_buffer(formatted_response);
        vizero_safe_free(formatted_response);
    }
}

/* Handle :claude-chat command */
static int handle_claude_chat_command(vizero_editor_t* editor, const char* args) {
    (void)args; /* Unused */
    
    if (!g_llm_state || !g_llm_state->initialized) {
        if (g_llm_state->api && g_llm_state->api->set_status_message) {
            g_llm_state->api->set_status_message(editor, "Claude plugin not initialized");
        }
        return -1;
    }
    
    int result = create_claude_buffer(editor);
    if (result == 0) {
        if (g_llm_state->api && g_llm_state->api->set_status_message) {
            g_llm_state->api->set_status_message(editor, "Claude chat ready - type your questions!");
        }
    } else {
        if (g_llm_state->api && g_llm_state->api->set_status_message) {
            g_llm_state->api->set_status_message(editor, "Failed to create Claude chat buffer");
        }
    }
    
    return result;
}

/* Handle :claude-ask command */
static int handle_claude_ask_command(vizero_editor_t* editor, const char* args) {
    if (!g_llm_state || !g_llm_state->initialized) {
        if (g_llm_state->api && g_llm_state->api->set_status_message) {
            g_llm_state->api->set_status_message(editor, "Claude plugin not initialized");
        }
        return -1;
    }
    
    if (!args || strlen(args) == 0) {
        if (g_llm_state->api && g_llm_state->api->set_status_message) {
            g_llm_state->api->set_status_message(editor, "Usage: :claude-ask <your question>");
        }
        return -1;
    }
    
    /* Create chat buffer if it doesn't exist */
    if (!g_llm_state->claude_buffer) {
        create_claude_buffer(editor);
    }
    
    /* Display the question in the chat buffer */
    char timestamp[64];
    time_t now = time(NULL);
    struct tm* tm_info = localtime(&now);
    strftime(timestamp, sizeof(timestamp), "%H:%M:%S", tm_info);
    
    char* question_text = vizero_safe_malloc(strlen(args) + 128);
    if (question_text) {
        snprintf(question_text, strlen(args) + 128, "[%s] You: %s\n", timestamp, args);
        append_to_claude_buffer(question_text);
        vizero_safe_free(question_text);
    }
    
    /* Send request to Claude */
    http_response_t response = {0};
    int result = send_llm_request(args, "You are a helpful programming assistant.", &response);
    
    if (result == 0 && response.data) {
        char* content = extract_llm_content(response.data);
        if (content) {
            display_claude_response(content);
            vizero_safe_free(content);
        } else {
            append_to_claude_buffer("[Error] Failed to parse Claude response\n\n");
        }
        vizero_safe_free(response.data);
    } else {
        append_to_claude_buffer("[Error] Failed to get response from Claude\n\n");
    }
    
    /* Add prompt for next question and position cursor */
    claude_add_prompt();
    
    return 0;
}

/* Handle :claude-clear command */
static int handle_claude_clear_command(vizero_editor_t* editor, const char* args) {
    (void)args; /* Unused */
    
    if (!g_llm_state) {
        return -1;
    }
    
    /* Clear conversation history */
    if (g_llm_state->conversation_history) {
        vizero_safe_free(g_llm_state->conversation_history);
        g_llm_state->conversation_history = NULL;
        g_llm_state->history_length = 0;
    }
    
    /* Clear chat buffer if it exists */
    if (g_llm_state->claude_buffer && g_llm_state->api && g_llm_state->api->delete_text) {
        /* Delete all content by creating a range from start to end */
        if (g_llm_state->api->get_buffer_line_count) {
            int line_count = g_llm_state->api->get_buffer_line_count(g_llm_state->claude_buffer);
            if (line_count > 0) {
                vizero_range_t all_range;
                all_range.start.line = 0;
                all_range.start.column = 0;
                all_range.end.line = line_count - 1;
                
                if (g_llm_state->api->get_buffer_line_length) {
                    all_range.end.column = g_llm_state->api->get_buffer_line_length(g_llm_state->claude_buffer, all_range.end.line);
                } else {
                    all_range.end.column = 1000; /* Fallback - large number */
                }
                
                g_llm_state->api->delete_text(g_llm_state->claude_buffer, all_range);
            }
        }
        
        append_to_claude_buffer("=== Claude Haiku Chat ===\n");
        append_to_claude_buffer("Conversation history cleared.\n\n");
        claude_add_prompt();
    }
    
    if (g_llm_state->api && g_llm_state->api->set_status_message) {
        g_llm_state->api->set_status_message(editor, "Claude conversation history cleared");
    }
    
    return 0;
}

/* Add Claude prompt to the chat buffer */
static void claude_add_prompt(void) {
    if (!g_llm_state || !g_llm_state->api || !g_llm_state->editor) return;
    
    /* Use Claude buffer if available, otherwise use current buffer */
    vizero_buffer_t* target_buffer = g_llm_state->claude_buffer ? 
                                    g_llm_state->claude_buffer : 
                                    g_llm_state->api->get_current_buffer(g_llm_state->editor);
    if (!target_buffer) return;
    
    vizero_cursor_t* cursor = g_llm_state->api->get_current_cursor(g_llm_state->editor);
    if (!cursor) return;
    
    /* Get the end of the buffer to append prompt there */
    size_t line_count = g_llm_state->api->get_buffer_line_count(target_buffer);
    vizero_position_t end_pos;
    
    if (line_count > 0) {
        /* Move to end of last line */
        end_pos.line = line_count - 1;
        const char* last_line = g_llm_state->api->get_buffer_line(target_buffer, end_pos.line);
        end_pos.column = last_line ? strlen(last_line) : 0;
    } else {
        /* Empty buffer */
        end_pos.line = 0;
        end_pos.column = 0;
    }
    
    /* Add Claude prompt */
    const char* prompt = "You> ";
    
    /* Insert prompt */
    if (g_llm_state->api->insert_text) {
        g_llm_state->api->insert_text(target_buffer, end_pos, prompt);
    }
    
    /* Move cursor to the end after prompt insertion */
    vizero_buffer_t* current_buffer = g_llm_state->api->get_current_buffer(g_llm_state->editor);
    if (current_buffer == target_buffer) {
        vizero_cursor_t* current_cursor = g_llm_state->api->get_current_cursor(g_llm_state->editor);
        if (current_cursor) {
            size_t new_line_count = g_llm_state->api->get_buffer_line_count(target_buffer);
            if (new_line_count > 0) {
                vizero_position_t new_end_pos;
                new_end_pos.line = new_line_count - 1;
                const char* new_last_line = g_llm_state->api->get_buffer_line(target_buffer, new_end_pos.line);
                new_end_pos.column = new_last_line ? strlen(new_last_line) : 0;
                g_llm_state->api->set_cursor_position(current_cursor, new_end_pos);
            }
        }
    }
}

/* Handle Enter key for interactive Claude chat */
static int claude_handle_enter_key(vizero_editor_t* editor, uint32_t key, uint32_t modifiers) {
    (void)modifiers; /* Unused */
    
    /* Only handle Enter key */
    if (key != 13 && key != 10) return 0; /* Not Enter key */
    
    /* Only handle if we're in Claude mode with initialized plugin */
    if (!g_llm_state || !g_llm_state->initialized || !g_llm_state->in_claude_mode) return 0;
    
    /* Get current cursor and buffer */
    vizero_cursor_t* cursor = g_llm_state->api->get_current_cursor(editor);
    vizero_buffer_t* buffer = g_llm_state->api->get_current_buffer(editor);
    if (!cursor || !buffer || buffer != g_llm_state->claude_buffer) return 0;
    
    vizero_position_t pos = g_llm_state->api->get_cursor_position(cursor);
    const char* line_text = g_llm_state->api->get_buffer_line(buffer, pos.line);
    
    if (!line_text || strlen(line_text) == 0) {
        return 0; /* Let normal handling proceed for empty lines */
    }
    
    /* Check if line starts with "You> " prompt */
    const char* question_start = line_text;
    if (strncmp(question_start, "You> ", 5) == 0) {
        question_start += 5;
    }
    
    /* Skip any remaining leading whitespace */
    while (*question_start && isspace(*question_start)) question_start++;
    if (*question_start == '\0') {
        return 0; /* Empty question, let normal handling proceed */
    }
    
    /* Send the question to Claude */
    http_response_t response = {0};
    int result = send_llm_request(question_start, "You are a helpful programming assistant.", &response);
    
    if (result == 0 && response.data) {
        char* content = extract_llm_content(response.data);
        if (content) {
            /* Add newline and display Claude's response */
            append_to_claude_buffer("\n");
            display_claude_response(content);
            vizero_safe_free(content);
        } else {
            append_to_claude_buffer("\n[Error] Failed to parse Claude response\n\n");
        }
        vizero_safe_free(response.data);
    } else {
        append_to_claude_buffer("\n[Error] Failed to get response from Claude\n\n");
    }
    
    /* Add prompt for next question */
    claude_add_prompt();
    
    return 1; /* We handled the Enter key */
}

/* Plugin commands */
static vizero_plugin_command_t llm_commands[] = {
    {
        .command = "llm-test",
        .description = "Test LLM connection and functionality",
        .handler = handle_llm_test_command,
        .user_data = NULL
    },
    {
        .command = "claude-chat",
        .description = "Open Claude chat buffer for interactive conversation",
        .handler = handle_claude_chat_command,
        .user_data = NULL
    },
    {
        .command = "claude-ask",
        .description = "Ask Claude a question: :claude-ask <question>",
        .handler = handle_claude_ask_command,
        .user_data = NULL
    },
    {
        .command = "claude-clear",
        .description = "Clear Claude conversation history",
        .handler = handle_claude_clear_command,
        .user_data = NULL
    }
};

/* Plugin entry points */

VIZERO_PLUGIN_API int vizero_plugin_init(vizero_plugin_t* plugin, vizero_editor_t* editor, const vizero_editor_api_t* api) {
    if (!plugin || !editor || !api) {
        return -1;
    }
    
    /* Allocate plugin state */
    g_llm_state = (github_llm_state_t*)vizero_safe_malloc(sizeof(github_llm_state_t));
    if (!g_llm_state) {
        return -1;
    }
    
    /* Initialize state */
    memset(g_llm_state, 0, sizeof(github_llm_state_t));
    g_llm_state->api = api;
    g_llm_state->editor = editor;
    
    /* Initialize curl globally (should be done once per process) */
    if (curl_global_init(CURL_GLOBAL_DEFAULT) != CURLE_OK) {
        printf("[Claude LLM] Failed to initialize curl globally\n");
        vizero_safe_free(g_llm_state);
        g_llm_state = NULL;
        return -1;
    }
    g_llm_state->curl_global_init_done = true;
    
    /* Check if HTTPS is supported */
    curl_version_info_data* curl_info = curl_version_info(CURLVERSION_NOW);
    bool https_supported = false;
    for (int i = 0; curl_info->protocols[i]; i++) {
        if (strcmp(curl_info->protocols[i], "https") == 0) {
            https_supported = true;
            break;
        }
    }
    
    if (!https_supported) {
        printf("[Claude LLM] WARNING: HTTPS not supported by this CURL build!\n");
        printf("[Claude LLM] SSL version: %s\n", curl_info->ssl_version ? curl_info->ssl_version : "none");
        printf("[Claude LLM] Cannot connect to Claude API without HTTPS support.\n");
        printf("[Claude LLM] Please install CURL with SSL/TLS support.\n");
        return -1;
    }
    
    /* Create curl handle */
    g_llm_state->curl_handle = curl_easy_init();
    if (!g_llm_state->curl_handle) {
        printf("[GitHub LLM] Failed to initialize curl handle\n");
        curl_global_cleanup();
        vizero_safe_free(g_llm_state);
        g_llm_state = NULL;
        return -1;
    }
    
    /* Load configuration */
    if (load_llm_config() != 0) {
        printf("[GitHub LLM] Failed to load configuration\n");
        curl_easy_cleanup(g_llm_state->curl_handle);
        curl_global_cleanup();
        vizero_safe_free(g_llm_state);
        g_llm_state = NULL;
        return -1;
    }
    
    /* Set up plugin callbacks */
    plugin->callbacks.commands = llm_commands;
    plugin->callbacks.command_count = sizeof(llm_commands) / sizeof(llm_commands[0]);
    plugin->callbacks.on_key_input = claude_handle_enter_key;
    
    g_llm_state->initialized = true;
    
    printf("[GitHub LLM] Plugin initialized successfully\n");
    return 0;
}

VIZERO_PLUGIN_API void vizero_plugin_cleanup(vizero_plugin_t* plugin) {
    (void)plugin; /* Unused */
    
    if (g_llm_state) {
        /* Cleanup curl resources */
        if (g_llm_state->curl_handle) {
            curl_easy_cleanup(g_llm_state->curl_handle);
        }
        
        if (g_llm_state->curl_global_init_done) {
            curl_global_cleanup();
        }
        
        /* Free configuration strings */
        vizero_safe_free(g_llm_state->api_token);
        vizero_safe_free(g_llm_state->endpoint_url);
        vizero_safe_free(g_llm_state->model_name);
        
        /* Free chat state */
        vizero_safe_free(g_llm_state->conversation_history);
        
        /* Free state structure */
        vizero_safe_free(g_llm_state);
        g_llm_state = NULL;
    }
    
    printf("[GitHub LLM] Plugin cleanup completed\n");
}