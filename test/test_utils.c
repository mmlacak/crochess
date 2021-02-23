/* Copyright (C) 2011, 2019 Mario Mlačak, mmlacak@gmail.com */

#include <stdio.h>
#include <stdlib.h>
#include <string.h>

#include "test_utils.h"


char * debug_str_ends_str( const char * s1, const char * s2 )
{
    char * rs1;
    char * rs2;
    size_t len1, len2;
    long int i;

    printf( "\n args: 1: %p, 2: %p", (void *)s1, (void *)s2 );
    if ( s1 ) printf( ", 1: '%s'", s1 );
    if ( s2 ) printf( ", 2: '%s'", s2 );

    if ( !s1 ) return NULL;
    if ( !s2 ) return NULL;

    len1 = strlen( s1 );
    printf( ", lens: 1: %lu", len1 );
    len2 = strlen( s2 );
    printf( ", 2: %lu", len2 );
    if ( len1 < len2 ) return NULL;

    rs1 = (char *)s1 + len1;
    printf( ", rev: 1: %p, '%s'", (void *)rs1, rs1 );
    rs2 = (char *)s2 + len2;
    printf( ", 2: %p, '%s' \n\n", (void *)rs2, rs2 );

    for ( i = len2; ( i >= 0 ) && ( *--rs1 == *--rs2 ); --i )
    {
        printf( "i=%li, rev: 1: %p '%s', 2: %p '%s' \n", i, (void *)rs1, rs1, (void *)rs2, rs2 );
    };

    ++rs1;
    printf( "\n end: i=%li, rev: 1: %p, '%s' \n", i, (void *)rs1, rs1 );
    if ( i <= 0 ) return rs1;
    else return NULL;
}

void test_str_ends_str( void )
{
    char * result;

    printf( "\n test: 1. bmp: " );
    result = debug_str_ends_str("/just/for/test/grass1.bmp", ".bmp");
    printf( "%p, '%s' \n", (void *)result, result );

    printf( "\n test: 1. png:" );
    result = debug_str_ends_str("/just/for/test/grass1.png", ".bmp");
    printf( "%p, '%s' \n \n", (void *)result, result );
}

int main( int argc, char *argv[] )
{
    test_str_ends_str();
    return 0;
}
