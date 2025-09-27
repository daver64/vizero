#include "vizero/json_parser.h"
#include "third_party/json.hpp"
#include <cstring>
#include <cstdlib>

using json = nlohmann::json;

/* Internal JSON wrapper structure */
struct vizero_json_t {
    json* data;
    
    vizero_json_t(json* j) : data(j) {}
    ~vizero_json_t() { delete data; }
};

extern "C" {

vizero_json_t* vizero_json_parse(const char* json_string, size_t length) {
    if (!json_string || length == 0) {
        return nullptr;
    }
    
    try {
        std::string json_str(json_string, length);
        json* j = new json(json::parse(json_str));
        return new vizero_json_t(j);
    } catch (const std::exception&) {
        return nullptr;
    }
}

void vizero_json_free(vizero_json_t* json_obj) {
    delete json_obj;
}

char* vizero_json_get_string(vizero_json_t* json_obj, const char* field_name) {
    if (!json_obj || !field_name || !json_obj->data) {
        return nullptr;
    }
    
    try {
        const json& j = *json_obj->data;
        if (j.contains(field_name) && j[field_name].is_string()) {
            std::string str = j[field_name].get<std::string>();
            char* result = static_cast<char*>(malloc(str.length() + 1));
            if (result) {
                strcpy(result, str.c_str());
            }
            return result;
        }
    } catch (const std::exception&) {
        // Fall through to return nullptr
    }
    
    return nullptr;
}

int vizero_json_get_int(vizero_json_t* json_obj, const char* field_name, int default_value) {
    if (!json_obj || !field_name || !json_obj->data) {
        return default_value;
    }
    
    try {
        const json& j = *json_obj->data;
        if (j.contains(field_name) && j[field_name].is_number_integer()) {
            return j[field_name].get<int>();
        }
    } catch (const std::exception&) {
        // Fall through to return default
    }
    
    return default_value;
}

int vizero_json_get_bool(vizero_json_t* json_obj, const char* field_name, int default_value) {
    if (!json_obj || !field_name || !json_obj->data) {
        return default_value;
    }
    
    try {
        const json& j = *json_obj->data;
        if (j.contains(field_name) && j[field_name].is_boolean()) {
            return j[field_name].get<bool>() ? 1 : 0;
        }
    } catch (const std::exception&) {
        // Fall through to return default
    }
    
    return default_value;
}

int vizero_json_has_field(vizero_json_t* json_obj, const char* field_name) {
    if (!json_obj || !field_name || !json_obj->data) {
        return 0;
    }
    
    try {
        const json& j = *json_obj->data;
        return j.contains(field_name) ? 1 : 0;
    } catch (const std::exception&) {
        return 0;
    }
}

vizero_json_t* vizero_json_get_object(vizero_json_t* json_obj, const char* field_name) {
    if (!json_obj || !field_name || !json_obj->data) {
        return nullptr;
    }
    
    try {
        const json& j = *json_obj->data;
        if (j.contains(field_name) && j[field_name].is_object()) {
            json* obj = new json(j[field_name]);
            return new vizero_json_t(obj);
        }
    } catch (const std::exception&) {
        // Fall through to return nullptr
    }
    
    return nullptr;
}

int vizero_json_array_size(vizero_json_t* json_obj) {
    if (!json_obj || !json_obj->data) {
        return -1;
    }
    
    try {
        const json& j = *json_obj->data;
        if (j.is_array()) {
            return static_cast<int>(j.size());
        }
    } catch (const std::exception&) {
        // Fall through to return -1
    }
    
    return -1;
}

vizero_json_t* vizero_json_array_get(vizero_json_t* json_obj, int index) {
    if (!json_obj || !json_obj->data || index < 0) {
        return nullptr;
    }
    
    try {
        const json& j = *json_obj->data;
        if (j.is_array() && index < static_cast<int>(j.size())) {
            json* element = new json(j[index]);
            return new vizero_json_t(element);
        }
    } catch (const std::exception&) {
        // Fall through to return nullptr
    }
    
    return nullptr;
}

} /* extern "C" */