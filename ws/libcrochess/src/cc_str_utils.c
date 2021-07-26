// Copyright (c) 2021 Mario Mlačak, mmlacak@gmail.com
// Licensed under 3-clause (modified) BSD license. See LICENSE for details.

#include <stddef.h>
#include <stdbool.h>
#include <stdlib.h>
#include <ctype.h>
#include <string.h>
#include <stdarg.h>
#include <stdio.h>

#include "cc_defines.h"
#include "cc_str_utils.h"

/**
    @file cc_str_utils.c
    @brief String utility functions.
*/


bool cc_str_to_case( char * const restrict str_io, bool to_upper_or_lower )
{
    if ( !str_io ) return false;

    char * s = str_io;
    while ( *s )
    {
        if ( to_upper_or_lower )
            *s = toupper( *s );
        else
            *s = tolower( *s );

        ++s;
    }

    return true;
}

char * cc_str_to_case_new( char const * const restrict str, bool to_upper_or_lower )
{
    if ( !str ) return NULL;

    size_t len = strlen( str );
    char * lc = malloc( len + 1 );
    if ( !lc ) return NULL;

    char * s = lc;
    char const * pos = str;
    while ( *pos )
    {
        if ( to_upper_or_lower )
            *s = toupper( *pos );
        else
            *s = tolower( *pos );

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

size_t cc_str_len_min( char const * const restrict str, size_t max_len )
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
    if ( !new ) return NULL;

    char const * s = str;
    char * n = new;
    while ( *s ) *n++ = *s++; // ( *s != '\0' )

    *n = '\0';

    return new;
}

char * cc_str_duplicate_len_new( char const * const restrict str, size_t max_len )
{
    if ( !str ) return NULL;

    size_t len = cc_str_len_min( str, max_len );
    char * new = (char *)malloc( len + 1 );
    if ( !new ) return NULL;

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


char * cc_str_concatenate_new( char const * const restrict str_1,
                               char const * const restrict str_2 )
{
    size_t len_1 = cc_str_len( str_1 );
    size_t len_2 = cc_str_len( str_2 );
    size_t len = len_1 + len_2;

    char * new = (char *)malloc( len + 1 );
    if ( !new ) return NULL;

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

char * cc_str_concatenate_len_new( char const * const restrict str_1,
                                   char const * const restrict str_2,
                                   size_t max_len )
{
    size_t len_1 = cc_str_len_min( str_1, max_len );
    size_t len_2 = cc_str_len_min( str_2, max_len );
    size_t len = CC_MIN( len_1 + len_2, max_len );

    char * new = (char *)malloc( len + 1 );
    if ( !new ) return NULL;

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


bool cc_str_append_char( char ** const restrict str_io_r,
                         char const chr )
{
    if ( !str_io_r ) return false;

    if ( !*str_io_r )
    {
        char * new = (char *)malloc( 2 );
        if ( !new ) return false;

        *new = chr;
        *(new + 1) = '\0';

        *str_io_r = new;
        return true;
    }

    size_t len = cc_str_len( *str_io_r ) + 1;
    char * new = realloc( *str_io_r, len + 1 );
    if ( !new ) return false;

    *str_io_r = new; // *str_io_r was free'd by realloc().

    char * n = new;
    while ( *n ) ++n;

    *n++ = chr;
    *n = '\0';

    return (bool)( new );
}

char * cc_str_append_new( char ** restrict str_1_f,
                          char ** restrict str_2_f )
{
    if ( ( !str_1_f ) && ( !str_2_f ) ) return NULL;

    char * new = NULL;

    if ( str_1_f && str_2_f )
        new = cc_str_concatenate_new( *str_1_f, *str_2_f );
    else if ( str_1_f )
        new = cc_str_duplicate_new( *str_1_f );
    else if ( str_2_f )
        new = cc_str_duplicate_new( *str_2_f );

    if ( !new ) return NULL;

    if ( str_1_f )
    {
        free( *str_1_f );
        *str_1_f = NULL;
    }

    if ( str_2_f )
    {
        free( *str_2_f );
        *str_2_f = NULL;
    }

    return new;
}

char * cc_str_append_len_new( char ** restrict str_1_f,
                              char ** restrict str_2_f,
                              size_t max_len )
{
    if ( ( !str_1_f ) && ( !str_2_f ) ) return NULL;

    char * new = NULL;

    if ( str_1_f && str_2_f )
        new = cc_str_concatenate_len_new( *str_1_f, *str_2_f, max_len );
    else if ( str_1_f )
        new = cc_str_duplicate_len_new( *str_1_f, max_len );
    else if ( str_2_f )
        new = cc_str_duplicate_len_new( *str_2_f, max_len );

    if ( !new ) return NULL;

    if ( str_1_f )
    {
        free( *str_1_f );
        *str_1_f = NULL;
    }

    if ( str_2_f )
    {
        free( *str_2_f );
        *str_2_f = NULL;
    }

    return new;
}

char * cc_str_append_format_new( char ** restrict str_f,
                                 char const * const restrict fmt, ... )
{
    va_list args;
    va_start( args, fmt );

    va_list tmp;
    va_copy( tmp, args );

    int len = vsnprintf( NULL, 0, fmt, tmp ); // len does not include \0.
    if ( len < 0 )
    {
        va_end( tmp );
        va_end( args );
        return NULL;
    }

    va_end( tmp );

    size_t len_min = (size_t)len;
    char * new = (char *)malloc( len_min + 1 );
    if ( !new )
    {
        va_end( args );
        return NULL;
    }

    int len_2 = vsnprintf( new, len_min + 1, fmt, args );
    if ( len_2 < 0 )
    {
        va_end( args );
        return NULL;
    }

    va_end( args );

    size_t len_min_2 = (size_t)len_2;
    if ( len_min != len_min_2 )
    {
        free( new );
        return NULL;
    }

    // No need to free() str_f, new; cc_str_append_new() does that.
    return cc_str_append_new( str_f, &new );
}

char * cc_str_append_format_len_new( char ** restrict str_f,
                                     size_t max_len,
                                     char const * const restrict fmt, ... )
{
    va_list args;
    va_start( args, fmt );

    va_list tmp;
    va_copy( tmp, args );

    int len = vsnprintf( NULL, 0, fmt, tmp ); // len does not include \0.
    if ( len < 0 )
    {
        va_end( tmp );
        va_end( args );
        return NULL;
    }

    va_end( tmp );

    size_t len_min = ( (size_t)len < max_len ) ? (size_t)len : max_len;
    char * new = (char *)malloc( len_min + 1 );
    if ( !new )
    {
        va_end( args );
        return NULL;
    }

    int len_2 = vsnprintf( new, len_min + 1, fmt, args );
    if ( len_2 < 0 )
    {
        va_end( args );
        return NULL;
    }

    va_end( args );

    size_t len_min_2 = (size_t)len_2;
    if ( len_min != len_min_2 )
    {
        free( new );
        return NULL;
    }

    // No need to free() str_f, new; cc_str_append_len_new() does that.
    return cc_str_append_len_new( str_f, &new, max_len );
}
