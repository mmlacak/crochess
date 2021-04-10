// Copyright (c) 2021 Mario Mlačak, mmlacak@gmail.com
// Licensed under 3-clause (modified) BSD license. See LICENSE for details.

#include <stdlib.h>
#include <stdio.h>
#include <string.h>
#include <stdbool.h>


char const TOKEN_SEPARATORS_WHITEPSACE[] = " \t\v\f\r\n";
char const TOKEN_SEPARATORS_PUNCTUATION[] = "!\"#$%%&'()*+,-./";


bool char_in(char c, char const * restrict seps)
{
    if ( !seps ) return false;

    for ( char * x = (char *)seps; *x != '\0'; ++x )
        if ( c == *x ) return true;
    return false;
}

char const * skip_chars(char const * const pos, char const * restrict seps)
{
    if ( !pos ) return pos;
    if ( !seps ) return pos;

    if ( *pos == '\0' ) return pos;
    char const * p = (char const *)pos;

    while ( char_in(*p, seps) )
    {
        if ( *p == '\0' ) return p;
        ++p;
    };

    return p;
}

char const * stop_at(char const * const pos, char const * restrict seps)
{
    if ( !pos ) return pos;
    if ( !seps ) return pos;

    if ( *pos == '\0' ) return pos;
    char const * p = (char const *)pos;

    while ( !char_in(*p, seps) )
    {
        if ( *p == '\0' ) return p;
        ++p;
    };

    return p;
}

char * next_token_alloc(char const * restrict str /* = NULL */, char const * restrict seps /* = NULL */)
{
    static char const * start = NULL;
    static char const * end = NULL;
    static char const * sps = NULL;

    if ( str )
        start = str;
    else
    {
        if ( !end ) return NULL;
        start = end + 1;
    }

    if ( seps ) sps = seps;

    start = skip_chars(start, sps);
    end = stop_at(start, sps);

    if ( end == start ) return NULL;

    size_t len = end - start;
    char * pos = malloc( len + 1 );
    if ( !pos ) return pos; // NULL --> maybe no enough memory.

    strncpy(pos, start, len);
    pos[ len ] = '\0';

    return pos;
}

size_t flush_stdio()
{
    size_t count = 0;
    char c = getchar();

    while ( ( c != EOF ) && ( c != '\0' ) && ( c != '\n' ) )
    {
        ++count;
        c = getchar();
    };

    return count;
}
