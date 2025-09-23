#include "vizero/filewatch_poll.h"
#include <stdint.h>
#include <stdio.h>
#ifdef _WIN32
#include <windows.h>
#else
#include <sys/stat.h>
#endif

uint64_t vizero_get_file_mtime(const char* filename) {
    if (!filename) return 0;
#ifdef _WIN32
    WIN32_FILE_ATTRIBUTE_DATA fad;
    if (!GetFileAttributesExA(filename, GetFileExInfoStandard, &fad))
        return 0;
    ULARGE_INTEGER ull;
    ull.LowPart = fad.ftLastWriteTime.dwLowDateTime;
    ull.HighPart = fad.ftLastWriteTime.dwHighDateTime;
    // Convert to Unix epoch (seconds since 1970)
    return (ull.QuadPart - 116444736000000000ULL) / 10000000ULL;
#else
    struct stat st;
    if (stat(filename, &st) != 0) return 0;
    return (uint64_t)st.st_mtime;
#endif
}
