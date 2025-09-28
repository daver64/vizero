#include "vizero/memory_utils.h"
#include <stdlib.h>
#include <string.h>

char* vizero_safe_strdup(const char* str) {
    if (!str) {
        return NULL;
    }
    
    size_t len = strlen(str);
    char* dup = (char*)malloc(len + 1);
    if (!dup) {
        return NULL;
    }
    
    strcpy(dup, str);
    return dup;
}

void vizero_safe_free(void* ptr) {
    if (ptr) {
        free(ptr);
    }
}

void* vizero_safe_realloc(void* ptr, size_t new_size) {
    if (new_size == 0) {
        vizero_safe_free(ptr);
        return NULL;
    }
    
    void* new_ptr = realloc(ptr, new_size);
    /* On failure, original pointer remains valid and caller must handle */
    return new_ptr;
}

void vizero_completion_item_free(vizero_completion_item_t* item) {
    if (!item) {
        return;
    }
    
    vizero_safe_free(item->label);
    vizero_safe_free(item->detail);
    vizero_safe_free(item->documentation);
    vizero_safe_free(item->insert_text);
    vizero_safe_free(item->filter_text);
    vizero_safe_free(item->sort_text);
    
    /* Clear the item to prevent double-free */
    memset(item, 0, sizeof(*item));
}

void vizero_completion_items_free(vizero_completion_item_t* items, size_t count) {
    if (!items) {
        return;
    }
    
    for (size_t i = 0; i < count; i++) {
        vizero_completion_item_free(&items[i]);
    }
    
    vizero_safe_free(items);
}

void vizero_completion_list_free(vizero_completion_list_t* list) {
    if (!list) {
        return;
    }
    
    vizero_completion_items_free(list->items, list->item_count);
    list->items = NULL;
    list->item_count = 0;
    
    vizero_safe_free(list);
}

void vizero_string_array_safe_free(char** array, size_t count) {
    if (!array) {
        return;
    }
    
    for (size_t i = 0; i < count; i++) {
        vizero_safe_free(array[i]);
    }
    
    vizero_safe_free(array);
}