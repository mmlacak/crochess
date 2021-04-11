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

char const * traverse_chars(char const * const pos, char const * restrict seps, bool skip_or_stop_at)
{
    if ( !pos ) return NULL;
    if ( !seps ) return pos;

    if ( *pos == '\0' ) return pos;
    char const * p = (char const *)pos;

    while ( skip_or_stop_at == char_in(*p, seps) )
    {
        if ( *p == '\0' ) return p;
        ++p;
    };

    return p;
}

char const * skip_chars(char const * const pos, char const * restrict seps)
{
    return traverse_chars(pos, seps, true);
}

char const * stop_at_chars(char const * const pos, char const * restrict seps)
{
    return traverse_chars(pos, seps, false);
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
    end = stop_at_chars(start, sps);

    if ( end == start ) return NULL;

    size_t len = end - start;
    char * pos = malloc( len + 1 );
    if ( !pos ) return pos; // NULL --> maybe no enough memory.

    strncpy(pos, start, len);
    pos[ len ] = '\0';

    return pos;
}

char * str_trim_alloc( char const * restrict str, char const * restrict chars )
{
    if ( !str ) return NULL;
    if ( !chars ) return NULL;

    char const * start = str;
    char const * end = NULL;

    start = skip_chars(start, chars);
    end = stop_at_chars(start, chars);

    if ( end == start ) return NULL;

    size_t len = end - start;
    char * pos = malloc( len + 1 );
    if ( !pos ) return NULL;

    strncpy(pos, start, len);
    pos[ len ] = '\0';

    return pos;
}

size_t flush_stdin()
{
    // All <stdio.h> getters block, can't flush.

    if ( feof( stdin ) ) return 0;
    if ( ferror( stdin ) ) return 0;

    size_t count = 0;
    int c = fgetc( stdin ); // getchar();

    while ( ( !feof( stdin ) ) && ( !ferror( stdin ) ) && ( c != EOF ) && ( c != '\0' ) && ( c != '\n' ) )
    {
        ++count;
        c = fgetc( stdin ); // getchar();
    };

    return count;
}
