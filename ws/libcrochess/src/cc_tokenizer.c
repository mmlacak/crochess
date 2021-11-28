// Copyright (c) 2021 Mario Mlačak, mmlacak@gmail.com
// Licensed under GNU GPL v3+ license. See LICENSING, COPYING files for details.

#include <stdlib.h>
#include <stdio.h>
#include <string.h>
#include <stdbool.h>

#include "cc_tokenizer.h"

/**
    @file cc_tokenizer.c
    @brief Tokenizer constants, and related functions.
*/


char const CC_TOKEN_SEPARATORS_WHITESPACE[] = " \t\v\f\r\n";
char const CC_TOKEN_SEPARATORS_PUNCTUATION[] = "!\"#$%%&'()*+,-./";


bool cc_char_in( char const c, char const * const restrict seps )
{
    if ( !seps ) return false;

    for ( char * x = (char *)seps; *x != '\0'; ++x )
        if ( c == *x ) return true;

    return false;
}

char const * cc_traverse_chars( char const * const restrict pos,
                                char const * const restrict seps,
                                bool const skip_or_stop_at )
{
    if ( !pos ) return NULL;
    if ( !seps ) return pos;

    if ( *pos == '\0' ) return pos;
    char const * p = (char const *)pos;

    while ( skip_or_stop_at == cc_char_in( *p, seps ) )
    {
        if ( *p == '\0' ) return p;
        ++p;
    }

    return p;
}

char const * cc_skip_chars( char const * const restrict pos,
                            char const * const restrict seps )
{
    return cc_traverse_chars( pos, seps, true );
}

char const * cc_stop_at_chars( char const * const restrict pos,
                               char const * const restrict seps )
{
    return cc_traverse_chars( pos, seps, false );
}

char * cc_next_token_new( char const * const restrict str_s,
                          char const * const restrict seps )
{
    static char const * start = NULL;
    static char const * end = NULL;

    if ( str_s )
        start = str_s;
    else
    {
        if ( !end ) return NULL;
        start = end + 1;
    }

    start = cc_skip_chars( start, seps );
    end = cc_stop_at_chars( start, seps );

    if ( end == start ) return NULL;

    size_t len = end - start;
    char * pos = malloc( len + 1 );
    if ( !pos ) return NULL;

    strncpy( pos, start, len );
    pos[ len ] = '\0';

    return pos;
}

bool cc_next_token_iter_new( char const * const restrict str_s,
                             char const * const restrict seps,
                             char ** const restrict token_o,
                             bool const initialize_iter )
{
    static char const * start = NULL;
    static char const * end = NULL;

    if ( !str_s ) return false;
    if ( !seps ) return false;
    if ( !token_o ) return false;
    if ( *token_o ) return false;

    if ( initialize_iter )
    {
        start = str_s;
    }
    else
    {
        if ( !end )
        {
            start = end = NULL;
            return false;
        }

        start = end + 1;
    }

    start = cc_skip_chars( start, seps );
    end = cc_stop_at_chars( start, seps );

    if ( ( *start == '\0' ) || ( end == start ) )
    {
        start = end = NULL;
        return false;
    }

    size_t len = end - start;
    char * pos = malloc( len + 1 );
    if ( !pos ) return false;

    strncpy( pos, start, len );
    pos[ len ] = '\0';

    *token_o = pos;
    return true;
}

char * cc_str_trim_new( char const * const restrict str,
                        char const * const restrict chars )
{
    if ( !str ) return NULL;
    if ( !chars ) return NULL;

    char const * start = str;
    char const * end = NULL;

    start = cc_skip_chars( start, chars );
    end = cc_stop_at_chars( start, chars );

    if ( end == start ) return NULL;

    size_t len = end - start;
    char * pos = malloc( len + 1 );
    if ( !pos ) return NULL;

    strncpy( pos, start, len );
    pos[ len ] = '\0';

    return pos;
}

// size_t cc_flush_stdin()
// {
//     // All <stdio.h> getters block, can't flush.

//     if ( feof( stdin ) ) return 0;
//     if ( ferror( stdin ) ) return 0;

//     size_t count = 0;
//     int c = fgetc( stdin ); // getchar();

//     while ( ( !feof( stdin ) ) && ( !ferror( stdin ) ) && ( c != EOF ) && ( c != '\0' ) && ( c != '\n' ) )
//     {
//         ++count;
//         c = fgetc( stdin ); // getchar();
//     }

//     return count;
// }
