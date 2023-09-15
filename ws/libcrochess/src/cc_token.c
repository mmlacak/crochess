// Copyright (c) 2021, 2022 Mario Mlačak, mmlacak@gmail.com
// Licensed under GNU GPL v3+ license. See LICENSING, COPYING files for details.

#include <stdlib.h>
#include <stdio.h>
#include <string.h>
#include <stdbool.h>

#include "cc_token.h"

/**
    @file cc_token.c
    @brief Token constants, and related functions.
*/


char const CC_TOKEN_SEPARATORS_WHITESPACE[] = " \t\v\f\r\n";
char const CC_TOKEN_SEPARATORS_PUNCTUATION[] = "!\"#$%%&'()*+,-./";


bool cc_char_in( char c, char const * restrict seps ) {
    if ( !seps ) return false;

    for ( char const * x = (char *)seps; *x != '\0'; ++x )
        if ( c == *x ) return true;

    return false;
}

char const * cc_traverse_chars( char const * restrict pos,
                                char const * restrict seps,
                                bool skip_or_stop_at ) {
    if ( !pos ) return NULL;
    if ( !seps ) return pos;

    if ( *pos == '\0' ) return pos;
    char const * pos__w = pos;

    while ( skip_or_stop_at == cc_char_in( *pos__w, seps ) ) {
        if ( *pos__w == '\0' ) return pos__w;
        ++pos__w;
    }

    return pos__w;
}

char const * cc_skip_chars( char const * restrict pos,
                            char const * restrict seps ) {
    return cc_traverse_chars( pos, seps, true );
}

char const * cc_stop_at_chars( char const * restrict pos,
                               char const * restrict seps ) {
    return cc_traverse_chars( pos, seps, false );
}


bool cc_iter_token( char const * restrict str,
                    char const * restrict seps,
                    char const ** restrict start__io,
                    char const ** restrict end__io ) {
    if ( !str ) return false;
    if ( !seps ) return false;
    if ( !start__io ) return false;
    if ( !end__io ) return false;

    if ( !( *start__io ) && !( *end__io ) )
        *start__io = str;
    else if ( ( *start__io ) && ( *end__io ) )
        *start__io = *end__io;
    else
        return false;

    *start__io = cc_skip_chars( *start__io, seps );
    *end__io = cc_stop_at_chars( *start__io, seps );

    if ( ( **start__io == '\0' ) || ( *end__io == *start__io ) ) {
        *start__io = *end__io = NULL;
        return false;
    }

    return true;
}

char * cc_trim_str__new( char const * restrict str,
                         char const * restrict chars ) {
    if ( !str ) return NULL;
    if ( !chars ) return NULL;

    char const * start = str;
    char const * end = NULL;

    start = cc_skip_chars( start, chars );
    end = cc_stop_at_chars( start, chars );

    if ( end == start ) return NULL;

    size_t len = end - start;
    char * pos__a = malloc( len + 1 );
    if ( !pos__a ) return NULL;

    strncpy( pos__a, start, len );
    pos__a[ len ] = '\0';

    return pos__a;
}


// size_t cc_flush_stdin() {
//     // All <stdio.h> getters block, can't flush.

//     if ( feof( stdin ) ) return 0;
//     if ( ferror( stdin ) ) return 0;

//     size_t count = 0;
//     int c = fgetc( stdin ); // getchar();

//     while ( ( !feof( stdin ) ) && ( !ferror( stdin ) )
//             && ( c != EOF ) && ( c != '\0' ) && ( c != '\n' ) ) {
//         ++count;
//         c = fgetc( stdin ); // getchar();
//     }

//     return count;
// }
