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

bool cc_str_count_chars( char const * restrict str,
                         cc_ctype_fp_ischar_t fp_is_char,
                         size_t * restrict count__o )
{
    if ( !str ) return false;
    if ( !fp_is_char ) return false;
    if ( !count__o ) return false;

    char const * s = str;
    *count__o = 0;

    while ( *s != '\0' )
    {
        if ( fp_is_char( *s++ ) ) *count__o += 1;
    }

    return true;
}

char const * cc_str_traverse_chars( char const * restrict str,
                                    cc_ctype_fp_ischar_t fp_is_char,
                                    bool skip_or_stop_at )
{
    if ( !str ) return NULL;
    if ( !fp_is_char ) return NULL;

    char const * s = str;

    while ( ( *s != '\0' ) && ( skip_or_stop_at == fp_is_char( *s ) ) ) ++s;

    return s;
}


bool cc_str_to_case( char * restrict str__io,
                     bool to_upper_or_lower )
{
    if ( !str__io ) return false;

    char * s = str__io;
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

char * cc_str_to_case_new( char const * restrict str,
                           bool to_upper_or_lower )
{
    if ( !str ) return NULL;

    size_t len = strlen( str );
    char * lc__t = malloc( len + 1 );
    if ( !lc__t ) return NULL;

    char * s = lc__t;
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

    lc__t[ len ] = '\0';

    return lc__t;
}


size_t cc_str_len( char const * restrict first,
                   char const * restrict end__d,
                   size_t max_len__d )
{
    if ( !first ) return 0;

    size_t len = 0;
    char const * s = first;

    if ( max_len__d == CC_MAX_LEN_IGNORE )
    {
        while ( *s != '\0' ) ++s;
        len = s - first;
    }
    else
        while ( ( *s != '\0' ) && ( ++len < max_len__d ) ) ++s;

    if ( end__d )
        // Effectively, checks if end__d belongs to a string,
        // i.e. first + 1 <= end__d <= first + strlen() + 2 (at most).
        //
        // At most +2 because +1 for '\0' (which might not be present),
        // and another +1 because end of string is a first char which
        // does not belong to that string.
        //
        // In case end__d < first, (size_t) cast pushes (end__d - first)
        // towards max ints, so MIN() will still return reasonable len.
        len = CC_MIN( len, (size_t)(end__d - first) );

    // Not needed, len was already capped in the while() loop above.
    // return CC_MIN( len, max_len__d );
    return len;
}

int cc_str_len_format( char const * restrict fmt, ... )
{
    va_list args;
    va_start( args, fmt );

    int len = vsnprintf( NULL, 0, fmt, args ); // len does not include \0.
    va_end( args );

    return len;
}

char const * cc_str_end( char const * restrict str,
                         size_t max_len__d )
{
    if ( !str ) return NULL;

    char const * end = str;
    size_t index = 0;

    while ( *end++ != '\0' )
        if ( ( max_len__d != CC_MAX_LEN_IGNORE ) && ( index++ >= max_len__d ) )
            return end;

    return ++end;
}

bool cc_str_compare( char const * restrict first_1,
                     char const * restrict end_1__d,
                     char const * restrict first_2,
                     char const * restrict end_2__d,
                     size_t max_len__d,
                     long long * restrict index__o )
{
    if ( !first_1 ) return false;
    if ( !first_2 ) return false;

    char const * str_1 = first_1;
    char const * last_1 = ( end_1__d ) ? end_1__d - 1 : NULL;
    char const * str_2 = first_2;
    char const * last_2 = ( end_2__d ) ? end_2__d - 1 : NULL;
    size_t index = 1;

    while ( ( *str_1 != '\0' )
         && ( *str_2 != '\0' )
         && ( ( max_len__d == CC_MAX_LEN_IGNORE ) || ( index < max_len__d ) ) )
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

    *index__o =
        ( *str_1 == *str_2 ) ? 0
                             : ( *str_1 < *str_2 ) ? (long long)(-index)
                                                   : (long long)index;

    return true;
}

bool cc_str_is_equal( char const * restrict first_1,
                      char const * restrict end_1__d,
                      char const * restrict first_2,
                      char const * restrict end_2__d,
                      size_t max_len__d )
{
    if ( !first_1 ) return false;
    if ( !first_2 ) return false;

    size_t len_1 = cc_str_len( first_1, end_1__d, max_len__d );
    size_t len_2 = cc_str_len( first_2, end_2__d, max_len__d );
    if ( len_1 != len_2 ) return false;

    long long index = 0;
    if ( cc_str_compare( first_1, end_1__d, first_2, end_2__d, max_len__d, &index ) )
        return ( index == 0 );

    return false;
}

char * cc_str_copy_new( char const * restrict first,
                        char const * restrict end__d,
                        size_t max_len__d )
{
    if ( !first ) return NULL;

    size_t len = cc_str_len( first, end__d, max_len__d );
    char * tmp__t = malloc( len + 1 );
    if ( !tmp__t ) return NULL;

    if ( !strncpy( tmp__t, first, len ) )
        return NULL;
    tmp__t[ len ] = '\0';

    return tmp__t;
}

char * cc_str_format_new( size_t max_len__d,
                          char const * restrict fmt, ... )
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

    size_t len_min = ( max_len__d != CC_MAX_LEN_IGNORE ) ? CC_MIN( (size_t)len, max_len__d )
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
        CC_FREE( new__t );
        va_end( args );
        return NULL;
    }

    va_end( args );

    if ( len_min != (size_t const)len_2 )
    {
        CC_FREE( new__t );
        return NULL;
    }

    return new__t;
}

char * cc_str_duplicate_new( char const * restrict str,
                             bool do_reverse,
                             size_t max_len__d )
{
    if ( !str ) return NULL;

    size_t len = cc_str_len( str, NULL, max_len__d );
    char * new__t = (char *)malloc( len + 1 );
    if ( !new__t ) return NULL;

    if ( len > 0 )
    {
        char const * s = str;
        char * n = do_reverse ? new__t + len : new__t;

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
        *new__t = '\0';

    return new__t;
}

char * cc_str_concatenate_new( char const * restrict str_1__d,
                               char const * restrict str_2__d,
                               size_t max_len__d )
{
    size_t len_1 = cc_str_len( str_1__d, NULL, max_len__d );
    size_t len_2 = cc_str_len( str_2__d, NULL, max_len__d );
    size_t len = ( max_len__d != CC_MAX_LEN_IGNORE ) ? CC_MIN( len_1 + len_2, max_len__d )
                                                     : len_1 + len_2;

    char * new__t = (char *)malloc( len + 1 );
    if ( !new__t ) return NULL;

    if ( len > 0 )
    {
        char * n = new__t;

        char const * s = str_1__d;
        len_1 = CC_MIN( len_1, len );
        if ( s && ( len_1 > 0 ) )
            for ( size_t i = 0; ( i < len_1 ) && ( *s != '\0' ); ++i )
                *n++ = *s++;

        s = str_2__d;
        len_2 = len - len_1;
        if ( s && ( len_2 > 0 ) )
            for ( size_t i = 0; ( i < len_2 ) && ( *s != '\0' ); ++i )
                *n++ = *s++;

        *n = '\0';
    }
    else *new__t = '\0';

    return new__t;
}

char * cc_str_append_new( char ** restrict str_1__f,
                          char ** restrict str_2__f,
                          size_t max_len__d )
{
    if ( ( !str_1__f ) && ( !str_2__f ) ) return NULL;

    char * new = NULL;

    if ( str_1__f && str_2__f )
        new = cc_str_concatenate_new( *str_1__f, *str_2__f, max_len__d );
    else if ( str_1__f )
        new = cc_str_duplicate_new( *str_1__f, false, max_len__d );
    else if ( str_2__f )
        new = cc_str_duplicate_new( *str_2__f, false, max_len__d );

    if ( !new ) return NULL;

    if ( str_1__f )
        CC_FREE_NULL( str_1__f );

    if ( str_2__f )
        CC_FREE_NULL( str_2__f );

    return new;
}

char * cc_str_append_format_new( char ** restrict str__f,
                                 size_t max_len__d,
                                 char const * restrict fmt, ... )
{
    va_list args;
    va_start( args, fmt );

    va_list tmp;
    va_copy( tmp, args );

    int len = vsnprintf( NULL, 0, fmt, tmp ); // len does not include \0.
    if ( len < 0 ) // error?
    {
        va_end( tmp );
        va_end( args );
        return NULL;
    }

    va_end( tmp );

    size_t len_min = ( max_len__d != CC_MAX_LEN_IGNORE ) ? CC_MIN( (size_t)len, max_len__d )
                                                         : (size_t)len;
    char * new__t = (char *)malloc( len_min + 1 );
    if ( !new__t )
    {
        va_end( args );
        return NULL;
    }

    int len_2 = vsnprintf( new__t, len_min + 1, fmt, args );
    if ( len_2 < 0 ) // error?
    {
        CC_FREE( new__t );
        va_end( args );
        return NULL;
    }

    va_end( args );

    if ( len_min != (size_t const)len_2 )
    {
        CC_FREE( new__t );
        return NULL;
    }

    // No need to free() str__f, new__t; cc_str_append_new() does that.
    return cc_str_append_new( str__f,
                              &new__t,
                              max_len__d );
}
