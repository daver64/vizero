#ifndef VIZERO_FILEWATCH_POLL_H
#define VIZERO_FILEWATCH_POLL_H

#include <stdint.h>
#ifdef __cplusplus
extern "C" {
#endif

/* Returns the last modification time of a file, or 0 on error. */
uint64_t vizero_get_file_mtime(const char* filename);

#ifdef __cplusplus
}
#endif

#endif // VIZERO_FILEWATCH_POLL_H
