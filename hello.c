#include <stdio.h>
#include <math.h>
#include <string.h>

typedef struct ans_buffer_t
{
    char *name;
    size_t length;
} ans_buffer_t;

int create_ans_buffer(ans_buffer_t **buffer, const char* name, size_t length)
{
    if(*buffer==NULL)
    {
        (*buffer)=(ans_buffer_t*)malloc(sizeof(ans_buffer_t));
        (*buffer)->name=strdup(name);
        (*buffer)->length=length;
        return 0;
    }
    return 1;
}

int main(int argc, char **argv[])
{
    ans_buffer_t *buf;
    if(create_ans_buffer(&buf,"hello",64))
    {
        printf("buffer created:%s,%zu\n",buf->name,buf->length);
    }
    else
    {
        printf("buffer creation failed\n");
    }
    return 0; 
}