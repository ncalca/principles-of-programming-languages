#include<stdio.h>

#define MAX(a,b) (((a)>(b)) ? (a) : (b))

/*
 * What does it prints?
 * /

int main(int argc, char *argv[]){
    int a = 10;
    int b = 20;

    printf("%d\n", MAX(a,b));
    printf("%d\n", MAX(++a, ++b));
    printf("%d\n", b); 

    return 0;
}
