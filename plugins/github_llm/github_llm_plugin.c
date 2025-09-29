/* GitHub LLM Plugin for Vizero - Phase 1: Foundation
 * Provides LLM integration for code completion, chat, and code explanation
 * Uses libcurl for HTTP requests and nlohmann JSON for response parsing
 */

#include "vizero/plugin_interface.h"
#include "vizero/json_parser.h"
#include "vizero/memory_utils.h"
#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <stdarg.h>
#include <curl/curl.h>

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
    "GitHub LLM",
    "1.0.0",
    "Vizero Team",
    "GitHub LLM integration for code completion and AI assistance",
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

/* Load configuration from environment variables and settings */
static int load_llm_config(void) {
    if (!g_llm_state) return -1;
    
    /* Try to get API token from environment */
    const char* token = getenv("GITHUB_TOKEN");
    if (!token) {
        token = getenv("OPENAI_API_KEY"); /* Fallback for OpenAI-compatible APIs */
    }
    
    if (token) {
        g_llm_state->api_token = vizero_safe_strdup(token);
        if (!g_llm_state->api_token) {
            return -1;
        }
    } else {
        printf("[GitHub LLM] Warning: No API token found. Set GITHUB_TOKEN or OPENAI_API_KEY environment variable.\n");
        return -1;
    }
    
    /* Set default endpoint (this will need to be updated when GitHub's LLM API is available) */
    const char* endpoint = getenv("LLM_ENDPOINT");
    if (!endpoint) {
        endpoint = "https://api.openai.com/v1/chat/completions"; /* Default to OpenAI for now */
    }
    
    g_llm_state->endpoint_url = vizero_safe_strdup(endpoint);
    if (!g_llm_state->endpoint_url) {
        return -1;
    }
    
    /* Set default model */
    const char* model = getenv("LLM_MODEL");
    if (!model) {
        model = "gpt-3.5-turbo"; /* Default model */
    }
    
    g_llm_state->model_name = vizero_safe_strdup(model);
    if (!g_llm_state->model_name) {
        return -1;
    }
    
    printf("[GitHub LLM] Configuration loaded: endpoint=%s, model=%s\n", 
           g_llm_state->endpoint_url, g_llm_state->model_name);
    
    return 0;
}

/* Send HTTP request to LLM API */
static int send_llm_request(const char* prompt, const char* system_prompt, http_response_t* response) {
    if (!g_llm_state || !g_llm_state->curl_handle || !prompt || !response) {
        return -1;
    }
    
    /* Initialize response */
    response->data = NULL;
    response->size = 0;
    
    /* Build JSON payload */
    char* json_payload;
    int json_len;
    
    if (system_prompt) {
        json_len = asprintf(&json_payload,
            "{"
            "\"model\":\"%s\","
            "\"messages\":["
                "{\"role\":\"system\",\"content\":\"%s\"},"
                "{\"role\":\"user\",\"content\":\"%s\"}"
            "],"
            "\"max_tokens\":1024,"
            "\"temperature\":0.7"
            "}",
            g_llm_state->model_name, system_prompt, prompt);
    } else {
        json_len = asprintf(&json_payload,
            "{"
            "\"model\":\"%s\","
            "\"messages\":["
                "{\"role\":\"user\",\"content\":\"%s\"}"
            "],"
            "\"max_tokens\":1024,"
            "\"temperature\":0.7"
            "}",
            g_llm_state->model_name, prompt);
    }
    
    if (json_len < 0 || !json_payload) {
        return -1;
    }
    
    /* Set curl options */
    curl_easy_setopt(g_llm_state->curl_handle, CURLOPT_URL, g_llm_state->endpoint_url);
    curl_easy_setopt(g_llm_state->curl_handle, CURLOPT_POSTFIELDS, json_payload);
    curl_easy_setopt(g_llm_state->curl_handle, CURLOPT_WRITEFUNCTION, write_callback);
    curl_easy_setopt(g_llm_state->curl_handle, CURLOPT_WRITEDATA, response);
    
    /* Set headers */
    struct curl_slist* headers = NULL;
    char auth_header[512];
    snprintf(auth_header, sizeof(auth_header), "Authorization: Bearer %s", g_llm_state->api_token);
    
    headers = curl_slist_append(headers, "Content-Type: application/json");
    headers = curl_slist_append(headers, auth_header);
    curl_easy_setopt(g_llm_state->curl_handle, CURLOPT_HTTPHEADER, headers);
    
    /* Perform the request */
    CURLcode res = curl_easy_perform(g_llm_state->curl_handle);
    
    /* Cleanup */
    curl_slist_free_all(headers);
    free(json_payload);
    
    if (res != CURLE_OK) {
        printf("[GitHub LLM] Request failed: %s\n", curl_easy_strerror(res));
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
        printf("[GitHub LLM] Failed to parse JSON response\n");
        return NULL;
    }
    
    char* content = NULL;
    
    /* Parse OpenAI-style response: choices[0].message.content */
    vizero_json_t* choices = vizero_json_get_array(json, "choices");
    if (choices && vizero_json_array_size(choices) > 0) {
        vizero_json_t* first_choice = vizero_json_array_get(choices, 0);
        if (first_choice) {
            vizero_json_t* message = vizero_json_get_object(first_choice, "message");
            if (message) {
                content = vizero_json_get_string(message, "content");
            }
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
    
    /* Simple test request */
    http_response_t response = {0};
    int result = send_llm_request("Hello, respond with 'LLM is working!'", 
                                  "You are a helpful assistant.", &response);
    
    if (result == 0 && response.data) {
        char* content = extract_llm_content(response.data);
        if (content) {
            char status_msg[256];
            snprintf(status_msg, sizeof(status_msg), "LLM Response: %.200s", content);
            if (g_llm_state->api && g_llm_state->api->set_status_message) {
                g_llm_state->api->set_status_message(editor, status_msg);
            }
            vizero_safe_free(content);
        } else {
            if (g_llm_state->api && g_llm_state->api->set_status_message) {
                g_llm_state->api->set_status_message(editor, "Failed to parse LLM response");
            }
        }
        vizero_safe_free(response.data);
    } else {
        if (g_llm_state->api && g_llm_state->api->set_status_message) {
            g_llm_state->api->set_status_message(editor, "LLM request failed");
        }
    }
    
    return 0;
}

/* Plugin commands */
static vizero_plugin_command_t llm_commands[] = {
    {
        .command = "llm-test",
        .description = "Test LLM connection and functionality",
        .handler = handle_llm_test_command,
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
        printf("[GitHub LLM] Failed to initialize curl globally\n");
        vizero_safe_free(g_llm_state);
        g_llm_state = NULL;
        return -1;
    }
    g_llm_state->curl_global_init_done = true;
    
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
        
        /* Free state structure */
        vizero_safe_free(g_llm_state);
        g_llm_state = NULL;
    }
    
    printf("[GitHub LLM] Plugin cleanup completed\n");
}