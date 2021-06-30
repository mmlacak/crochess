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


size_t cc_str_len( char const * const restrict str )
{
    return ( str ) ? strlen( str ) : 0;
}

size_t cc_str_len_max( char const * const restrict str, size_t max_len )
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

    size_t len = cc_str_len( str );
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

    size_t len = cc_str_len_max( str, max_len );
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
    size_t len_1 = cc_str_len( str_1 );
    size_t len_2 = cc_str_len( str_2 );
    size_t len = len_1 + len_2;

    char * new = (char *)malloc( len + 1 );

    char const * s = str_1;
    char * n = new;
    if ( s )
        while ( *s ) *n++ = *s++; // ( *s != '\0' )

    s = str_2;
    if ( s )
        while ( *s ) *n++ = *s++; // ( *s != '\0' )

    *n = '\0';

    return new;
}

char * cc_str_concatenate_len_new(  char const * const restrict str_1,
                                    char const * const restrict str_2,
                                    size_t max_len )
{
    size_t len_1 = cc_str_len_max( str_1, max_len );
    size_t len_2 = cc_str_len_max( str_2, max_len );
    size_t len = CC_MIN( len_1 + len_2, max_len );

    char * new = (char *)malloc( len + 1 );

    if ( len > 0 )
    {
        char * n = new;

        char const * s = str_1;
        len_1 = CC_MIN( len_1, len );
        if ( s && ( len_1 > 0 ) )
            for ( size_t i = 0; ( i < len_1 ) && ( *s != '\0' ); ++i )
                *n++ = *s++;

        s = str_2;
        len_2 = len - len_1;
        if ( s && ( len_2 > 0 ) )
            for ( size_t i = 0; ( i < len_2 ) && ( *s != '\0' ); ++i )
                *n++ = *s++;

        *n = '\0';
    }
    else *new = '\0';

    return new;
}

char * cc_str_concatenate_char_new( char const * const restrict str,
                                    char const chr )
{
    if ( !str )
    {
        char * new = (char *)malloc( 2 );
        if ( !new ) return NULL;

        *new = chr;
        *(new + 1) = '\0';
        return new;
    }

    size_t len = cc_str_len( str ) + 1;
    char * new = (char *)malloc( len + 1 );
    if ( !new ) return NULL;

    char const * s = str;
    char * n = new;
    while ( *s ) *n++ = *s++; // ( *s != '\0' )

    *n++ = chr;
    *n = '\0';

    return new;
}

bool cc_str_append_char( char ** const restrict alloc_str,
                         char const chr )
{
    if ( !alloc_str ) return false;

    if ( !*alloc_str )
    {
        char * new = (char *)malloc( 2 );
        if ( !new ) return false;

        *new = chr;
        *(new + 1) = '\0';

        *alloc_str = new;
        return true;
    }

    size_t len = cc_str_len( *alloc_str ) + 1;
    char * new = realloc( *alloc_str, len + 1 );
    if ( !new ) return false;

    *alloc_str = new; // alloc_str was free'd by realloc().

    char * n = new;
    while ( *n ) ++n;

    *n++ = chr;
    *n = '\0';

    return new;
}

char * cc_str_append_new( char ** restrict alloc_str_1,
                          char ** restrict alloc_str_2 )
{
    if ( ( !alloc_str_1 ) && ( !alloc_str_2 ) ) return NULL;

    if ( !alloc_str_1 )
    {
        char * new = cc_str_duplicate_new( *alloc_str_2 );
        if ( !new ) return NULL;

        free( *alloc_str_2 );
        alloc_str_2 = NULL;

        return new;
    }

    if ( !alloc_str_2 )
    {
        char * new = cc_str_duplicate_new( *alloc_str_1 );
        if ( !new ) return NULL;

        free( *alloc_str_1 );
        alloc_str_1 = NULL;

        return new;
    }

    char * new = cc_str_concatenate_new( *alloc_str_1, *alloc_str_2 );
    if ( !new ) return NULL;

    free( *alloc_str_1 );
    alloc_str_1 = NULL;

    free( *alloc_str_2 );
    alloc_str_2 = NULL;

    return new;
}

char * cc_str_append_len_new( char ** restrict alloc_str_1,
                              char ** restrict alloc_str_2,
                              size_t max_len )
{
    if ( ( !alloc_str_1 ) && ( !alloc_str_2 ) ) return NULL;

    if ( !alloc_str_1 )
    {
        char * new = cc_str_duplicate_len_new( *alloc_str_2, max_len );
        if ( !new ) return NULL;

        free( *alloc_str_2 );
        alloc_str_2 = NULL;

        return new;
    }

    if ( !alloc_str_2 )
    {
        char * new = cc_str_duplicate_len_new( *alloc_str_1, max_len );
        if ( !new ) return NULL;

        free( *alloc_str_1 );
        alloc_str_1 = NULL;

        return new;
    }

    char * new = cc_str_concatenate_len_new( *alloc_str_1, *alloc_str_2, max_len );
    if ( !new ) return NULL;

    free( *alloc_str_1 );
    alloc_str_1 = NULL;

    free( *alloc_str_2 );
    alloc_str_2 = NULL;

    return new;
}
