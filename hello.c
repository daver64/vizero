#include <stdio.h>
#include <stdint.h>

typedef struct tagged_ptr
{
    void *ptr;
    size_t capacity;
    size_t length;
    uint32_t status;
} tagged_ptr;

int main(int argc, char *argv[])
{

    return 0;
}