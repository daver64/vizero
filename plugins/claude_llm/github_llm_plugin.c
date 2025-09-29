/* Claude LLM Plugin for Vizero
 * Provides Claude Sonnet integration for code completion, chat, and code explanation
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

/* Load configuration from claude-key.txt file only */
static int load_llm_config(void) {
    if (!g_llm_state) return -1;
    
    /* Load API token from claude-key.txt file only */
    const char* token = NULL;
    FILE* key_file = fopen("claude-key.txt", "r");
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
            printf("[Claude LLM] Found API key in claude-key.txt\n");
        }
        fclose(key_file);
    }
    
    if (token) {
        g_llm_state->api_token = vizero_safe_strdup(token);
        if (!g_llm_state->api_token) {
            return -1;
        }
    } else {
        printf("[Claude LLM] Error: No API token found in claude-key.txt\n");
        printf("  Please create claude-key.txt file with your Anthropic API key\n");
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
    
    if (system_prompt) {
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
            g_llm_state->model_name, system_prompt, prompt);
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