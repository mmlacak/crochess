// Copyright (c) 2021 Mario Mlačak, mmlacak@gmail.com
// Licensed under GNU GPL v3+ license. See LICENSING, COPYING files for details.

#include <stddef.h>
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

bool cc_str_count_chars( char const * const restrict str,
                         cc_ctype_fp_ischar_t fp_is_char,
                         size_t * const restrict count_o )
{
    if ( !str ) return false;
    if ( !fp_is_char ) return false;
    if ( !count_o ) return false;

    char const * s = str;
    *count_o = 0;

    while ( *s != '\0' )
    {
        if ( fp_is_char( *s++ ) ) *count_o += 1;
    }

    return true;
}

char const * cc_str_traverse_chars( char const * const restrict str,
                                    cc_ctype_fp_ischar_t fp_is_char,
                                    bool const skip_or_stop_at )
{
    if ( !str ) return NULL;
    if ( !fp_is_char ) return NULL;

    char const * s = str;

    while ( ( *s != '\0' ) && ( skip_or_stop_at == fp_is_char( *s ) ) ) ++s;

    return s;
}


bool cc_str_to_case( char * const restrict str_io,
                     bool const to_upper_or_lower )
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

char * cc_str_to_case_new( char const * const restrict str,
                           bool const to_upper_or_lower )
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


size_t cc_str_len( char const * const restrict first,
                   char const * const restrict end_d,
                   size_t const max_len_d )
{
    if ( !first ) return 0;

    size_t len = 0;
    char const * s = first;

    if ( max_len_d == CC_MAX_LEN_IGNORE )
    {
        while ( *s != '\0' ) ++s;
        len = s - first;
    }
    else
        while ( ( *s != '\0' ) && ( ++len < max_len_d ) ) ++s;

    if ( end_d )
        // Effectively, checks if end_d belongs to a string,
        // i.e. first + 1 <= end_d <= first + strlen() + 2 (at most).
        //
        // At most +2 because +1 for '\0' might not be present,
        // another +1 because end of string is first char which
        // does not belong to that string.
        //
        // In case end_d < first, (size_t) cast pushes (end_d - first)
        // towards max ints, so MIN() will still return reasonable len.
        len = CC_MIN( len, (size_t)(end_d - first) );

    // Not needed, len was already capped in the while() loop above.
    // return CC_MIN( len, max_len_d );
    return len;
}

int cc_str_len_format( char const * const restrict fmt, ... )
{
    va_list args;
    va_start( args, fmt );

    int len = vsnprintf( NULL, 0, fmt, args ); // len does not include \0.
    va_end( args );

    return len;
}

char const * cc_str_end( char const * const restrict str,
                         size_t const max_len_d )
{
    if ( !str ) return NULL;

    char const * end = str;
    size_t index = 0;

    while ( *end++ != '\0' )
        if ( ( max_len_d != CC_MAX_LEN_IGNORE ) && ( index++ >= max_len_d ) )
            return end;

    return ++end;
}

bool cc_str_compare( char const * const restrict first_1,
                     char const * const restrict end_1_d,
                     char const * const restrict first_2,
                     char const * const restrict end_2_d,
                     size_t const max_len_d,
                     long long * const restrict index_o )
{
    if ( !first_1 ) return false;
    if ( !first_2 ) return false;

    char const * str_1 = first_1;
    char const * last_1 = ( end_1_d ) ? end_1_d - 1 : NULL;
    char const * str_2 = first_2;
    char const * last_2 = ( end_2_d ) ? end_2_d - 1 : NULL;
    size_t index = 1;

    while ( ( *str_1 != '\0' )
         && ( *str_2 != '\0' )
         && ( ( max_len_d == CC_MAX_LEN_IGNORE ) || ( index < max_len_d ) ) )
    {
        if ( last_1 && ( str_1 >= last_1 ) )
            break;

        if ( last_2 && ( str_2 >= last_2 ) )
            break;

        if ( *str_1 != *str_2 )
            break;

        ++index;
        ++str_1;
        ++str_2;
    }

    *index_o = ( *str_1 == *str_2 ) ? 0 :
               ( *str_1 < *str_2 ) ? (long long)-index : (long long)index;
    return true;
}

bool cc_str_is_equal( char const * const restrict first_1,
                      char const * const restrict end_1_d,
                      char const * const restrict first_2,
                      char const * const restrict end_2_d,
                      size_t const max_len_d )
{
    if ( !first_1 ) return false;
    if ( !first_2 ) return false;

    size_t len_1 = cc_str_len( first_1, end_1_d, max_len_d );
    size_t len_2 = cc_str_len( first_2, end_2_d, max_len_d );

    if ( len_1 != len_2 ) return false;

    long long index = 0;

    if ( cc_str_compare( first_1, end_1_d, first_2, end_2_d, max_len_d, &index ) )
        return ( index == 0 );

    return false;
}

bool cc_str_copy_new( char const * const restrict str,
                      size_t const length,
                      char ** const restrict str_o )
{
    if ( !str ) return false;
    if ( !str_o ) return false;
    if ( *str_o ) return false;

    char * tmp__t = malloc( length + 1 );
    if ( !tmp__t ) return false;

    if ( !strncpy( tmp__t, str, length ) )
        return false;
    tmp__t[ length ] = '\0';

    *str_o = tmp__t;
    return true;
}

bool cc_str_copy_until_end_new( char const * const restrict str,
                                char const * const restrict end,
                                char ** const restrict str_o )
{
    if ( !str ) return false;
    if ( !end ) return false;

    size_t length = end - str;

    return cc_str_copy_new( str, length, str_o );
}


char * cc_str_format_new( size_t const max_len_d,
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

    size_t len_min = ( max_len_d != CC_MAX_LEN_IGNORE ) ? CC_MIN( (size_t)len, max_len_d )
                                                        : (size_t)len;
    char * new__t = (char *)malloc( len_min + 1 );
    if ( !new__t )
    {
        va_end( args );
        return NULL;
    }

    int len_2 = vsnprintf( new__t, len_min + 1, fmt, args );
    if ( len_2 < 0 )
    {
        free( new__t );
        va_end( args );
        return NULL;
    }

    va_end( args );

    size_t len_min_2 = (size_t)len_2;
    if ( len_min != len_min_2 )
    {
        free( new__t );
        return NULL;
    }

    return new__t;
}

char * cc_str_duplicate_new( char const * const restrict str,
                             bool const do_reverse,
                             size_t const max_len_d )
{
    if ( !str ) return NULL;

    size_t len = cc_str_len( str, NULL, max_len_d );
    char * new = (char *)malloc( len + 1 );
    if ( !new ) return NULL;

    if ( len > 0 )
    {
        char const * s = str;
        char * n = do_reverse ? new + len : new;

        if ( do_reverse )
        {
            *n-- = '\0';

            for ( size_t i = 0; ( i < len ) && ( *s != '\0' ); ++i )
                *n-- = *s++;
        }
        else
        {
            for ( size_t i = 0; ( i < len ) && ( *s != '\0' ); ++i )
                *n++ = *s++;

            *n = '\0';
        }
    }
    else
        *new = '\0';

    return new;
}

char * cc_str_concatenate_new( char const * const restrict str_1_d,
                               char const * const restrict str_2_d,
                               size_t const max_len_d )
{
    size_t len_1 = cc_str_len( str_1_d, NULL, max_len_d );
    size_t len_2 = cc_str_len( str_2_d, NULL, max_len_d );
    size_t len = ( max_len_d != CC_MAX_LEN_IGNORE ) ? CC_MIN( len_1 + len_2, max_len_d )
                                                    : len_1 + len_2;

    char * new = (char *)malloc( len + 1 );
    if ( !new ) return NULL;

    if ( len > 0 )
    {
        char * n = new;

        char const * s = str_1_d;
        len_1 = CC_MIN( len_1, len );
        if ( s && ( len_1 > 0 ) )
            for ( size_t i = 0; ( i < len_1 ) && ( *s != '\0' ); ++i )
                *n++ = *s++;

        s = str_2_d;
        len_2 = len - len_1;
        if ( s && ( len_2 > 0 ) )
            for ( size_t i = 0; ( i < len_2 ) && ( *s != '\0' ); ++i )
                *n++ = *s++;

        *n = '\0';
    }
    else *new = '\0';

    return new;
}

bool cc_str_append_char( char ** const restrict str_io__r,
                         char const chr )
{
    if ( !str_io__r ) return false;

    if ( !*str_io__r )
    {
        char * new = (char *)malloc( 2 );
        if ( !new ) return false;

        *new = chr;
        *(new + 1) = '\0';

        *str_io__r = new;
        return true;
    }

    size_t len = cc_str_len( *str_io__r, NULL, CC_MAX_LEN_IGNORE ) + 1;
    char * new = realloc( *str_io__r, len + 1 );
    if ( !new ) return false;

    *str_io__r = new; // *str_io__r was free'd by realloc().

    char * n = new;
    while ( *n ) ++n;

    *n++ = chr;
    *n = '\0';

    return (bool)( new );
}

char * cc_str_append_new( char ** const restrict str_1__f,
                          char ** const restrict str_2__f,
                          size_t const max_len_d )
{
    if ( ( !str_1__f ) && ( !str_2__f ) ) return NULL;

    char * new = NULL;

    if ( str_1__f && str_2__f )
        new = cc_str_concatenate_new( *str_1__f, *str_2__f, max_len_d );
    else if ( str_1__f )
        new = cc_str_duplicate_new( *str_1__f, false, max_len_d );
    else if ( str_2__f )
        new = cc_str_duplicate_new( *str_2__f, false, max_len_d );

    if ( !new ) return NULL;

    if ( str_1__f )
    {
        free( *str_1__f );
        *str_1__f = NULL;
    }

    if ( str_2__f )
    {
        free( *str_2__f );
        *str_2__f = NULL;
    }

    return new;
}

char * cc_str_append_format_new( char ** const restrict str__f,
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
    char * new__t = (char *)malloc( len_min + 1 );
    if ( !new__t )
    {
        va_end( args );
        return NULL;
    }

    int len_2 = vsnprintf( new__t, len_min + 1, fmt, args );
    if ( len_2 < 0 )
    {
        free( new__t );
        va_end( args );
        return NULL;
    }

    va_end( args );

    size_t len_min_2 = (size_t)len_2;
    if ( len_min != len_min_2 )
    {
        free( new__t );
        return NULL;
    }

    // No need to free() str__f, new__t; cc_str_append_new() does that.
    return cc_str_append_new( str__f, &new__t, CC_MAX_LEN_IGNORE );
}

char * cc_str_append_format_min_new( char ** const restrict str__f,
                                     size_t const max_len,
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
    char * new__t = (char *)malloc( len_min + 1 );
    if ( !new__t )
    {
        va_end( args );
        return NULL;
    }

    int len_2 = vsnprintf( new__t, len_min + 1, fmt, args );
    if ( len_2 < 0 )
    {
        free( new__t );
        va_end( args );
        return NULL;
    }

    va_end( args );

    size_t len_min_2 = (size_t)len_2;
    if ( len_min != len_min_2 )
    {
        free( new__t );
        return NULL;
    }

    // No need to free() str__f, new__t; cc_str_append_new() does that.
    return cc_str_append_new( str__f, &new__t, max_len );
}
