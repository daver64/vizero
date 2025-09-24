#include <stdio.h>
#include <math.h>
#include <string.h>

int get_answer()
{
    return 42;
}

int main(int argc, char **argv[])
{
    int answer=get_answer();
    printf("The answer is %d\n",answer);
    return 0; 
}