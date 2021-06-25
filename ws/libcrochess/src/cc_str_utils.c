// Copyright (c) 2021 Mario Mlačak, mmlacak@gmail.com
// Licensed under 3-clause (modified) BSD license. See LICENSE for details.

#include <stddef.h>
#include <stdbool.h>
#include <stdlib.h>
#include <ctype.h>
#include <string.h>

#include "cc_defines.h"
#include "cc_str_utils.h"


bool cc_str_to_case( char * const restrict str, bool is_lower_or_upper )
{
    if ( !str ) return false;

    char * s = str;
    while ( *s )
    {
        if ( is_lower_or_upper )
            *s = tolower( *s );
        else
            *s = toupper( *s );

        ++s;
    }

    return true;
}

char * cc_str_to_case_new( char const * const restrict str, bool is_lower_or_upper )
{
    if ( !str ) return NULL;

    size_t len = strlen( str );
    char * lc = malloc( len + 1 );
    char * s = lc;

    char const * pos = str;
    while ( *pos )
    {
        if ( is_lower_or_upper )
            *s = tolower( *pos );
        else
            *s = toupper( *pos );

        ++s;
        ++pos;
    }

    lc[ len ] = '\0';

    return lc;
}


size_t cc_str_len_bound( char const * const restrict str, size_t max_len )
{
    if ( !str ) return 0;
    if ( max_len == 0 ) return 0;

    char const * s = str;
    size_t len = 0;

    while ( ( *s++ ) && ( ++len < max_len ) ) ;

    return len;
}

char * cc_str_duplicate_new( char const * const restrict str )
{
    if ( !str ) return NULL;

    size_t len = strlen( str );

    char * new = (char *)malloc( len + 1 );

    char const * s = str;
    char * n = new;
    while ( *s ) *n++ = *s++; // ( *s != '\0' )

    *n = '\0';

    return new;
}

char * cc_str_duplicate_len_new( char const * const restrict str, size_t max_len )
{
    if ( !str ) return NULL;

    size_t len = cc_str_len_bound( str, max_len );

    char * new = (char *)malloc( len + 1 );

    if ( len > 0 )
    {
        char const * s = str;
        char * n = new;

        for ( size_t i = 0; ( i < len ) && ( *s != '\0' ); ++i )
            *n++ = *s++;

        *n = '\0';
    }
    else *new = '\0';

    return new;
}


char * cc_str_concatenate_new(  char const * const restrict str_1,
                                char const * const restrict str_2 )
{
    if ( !str_1 ) return NULL;
    if ( !str_2 ) return NULL;

    size_t len = strlen( str_1 ) + strlen( str_2 );

    char * new = (char *)malloc( len + 1 );

    char const * s = str_1;
    char * n = new;
    while ( *s ) *n++ = *s++; // ( *s != '\0' )

    s = str_2;
    while ( *s ) *n++ = *s++; // ( *s != '\0' )

    *n = '\0';

    return new;
}

char * cc_str_concatenate_len_new(  char const * const restrict str_1,
                                    char const * const restrict str_2,
                                    size_t max_len )
{
    if ( !str_1 ) return NULL;
    if ( !str_2 ) return NULL;

    size_t len_1 = cc_str_len_bound( str_1, max_len );
    size_t len_2 = cc_str_len_bound( str_2, max_len );
    size_t len = CC_MIN( len_1 + len_2, max_len );

    char * new = (char *)malloc( len + 1 );

    if ( len > 0 )
    {
        char * n = new;

        char const * s = str_1;
        len_1 = CC_MIN( len_1, len );
        if ( len_1 > 0 )
            for ( size_t i = 0; ( i < len_1 ) && ( *s != '\0' ); ++i )
                *n++ = *s++;

        s = str_2;
        len_2 = len - len_1;
        if ( len_2 > 0 )
            for ( size_t i = 0; ( i < len_2 ) && ( *s != '\0' ); ++i )
                *n++ = *s++;

        *n = '\0';
    }
    else *new = '\0';

    return new;
}
