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


CcString * cc_string__new( char const * restrict str )
{
    CcString * str__a = malloc( sizeof( CcString ) );
    if ( !str__a ) return NULL;

    str__a->str = cc_str_duplicate__new( str, false, BUFSIZ );
    str__a->next = NULL;

    return str__a;
}

CcString * cc_string_append( CcString * restrict strings__io,
                             char const * restrict str )
{
    if ( !strings__io ) return NULL;

    CcString * str__t = cc_string__new( str );
    if ( !str__t ) return NULL;

    CcString * pm = strings__io;
    while ( pm->next ) pm = pm->next; // rewind
    pm->next = str__t; // append // Ownersip transfer --> str__t is now weak pointer.

    return str__t;
}

CcString * cc_string_append_or_init( CcString ** restrict strings__io,
                                     char const * restrict str )
{
    if ( !strings__io ) return NULL;

    CcString * str__t = cc_string_append( *strings__io, str );

    if ( !*strings__io ) *strings__io = str__t; // Ownersip transfer --> str__t is now weak pointer.

    return str__t;
}

CcString * cc_string_append_or_init_format( CcString ** restrict strings__io,
                                            char const * restrict fmt, ... )
{

    va_list args;
    va_start( args, fmt );

    char * msg__a = cc_str_format__new( BUFSIZ, fmt, args );

    va_end( args );

    if ( !msg__a ) return NULL;

    CcString * pm__w = cc_string_append_or_init( strings__io, msg__a );

    CC_FREE( msg__a );

    return pm__w;
}

bool cc_string_free_all( CcString ** restrict strings__f )
{
    if ( !strings__f ) return false;
    if ( !*strings__f ) return true;

    CcString * pm = *strings__f;

    while ( pm )
    {
        CC_FREE( pm->str );

        CcString * tmp = pm->next;
        CC_FREE( pm );
        pm = tmp;
    }

    *strings__f = NULL;
    return true;
}


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

    char const * str__w = str;

    while ( ( *str__w != '\0' ) && ( skip_or_stop_at == fp_is_char( *str__w ) ) ) ++str__w;

    return str__w;
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

char * cc_str_to_case__new( char const * restrict str,
                            bool to_upper_or_lower )
{
    if ( !str ) return NULL;

    size_t len = strlen( str );
    char * lc__a = malloc( len + 1 );
    if ( !lc__a ) return NULL;

    char * s = lc__a;
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

    lc__a[ len ] = '\0';

    return lc__a;
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

    char const * end__w = str;
    size_t index = 0;

    while ( *end__w++ != '\0' )
        if ( ( max_len__d != CC_MAX_LEN_IGNORE ) && ( index++ >= max_len__d ) )
            return end__w;

    return ++end__w;
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

char * cc_str_copy__new( char const * restrict first,
                         char const * restrict end__d,
                         size_t max_len__d )
{
    if ( !first ) return NULL;

    size_t len = cc_str_len( first, end__d, max_len__d );
    char * str__a = malloc( len + 1 );
    if ( !str__a ) return NULL;

    if ( !strncpy( str__a, first, len ) )
        return NULL;
    str__a[ len ] = '\0';

    return str__a;
}

char * cc_str_format__new( size_t max_len__d,
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
    char * str__a = (char *)malloc( len_min + 1 );
    if ( !str__a )
    {
        va_end( args );
        return NULL;
    }

    int len_2 = vsnprintf( str__a, len_min + 1, fmt, args );
    if ( len_2 < 0 )
    {
        CC_FREE( str__a );
        va_end( args );
        return NULL;
    }

    va_end( args );

    if ( len_min != (size_t const)len_2 )
    {
        CC_FREE( str__a );
        return NULL;
    }

    return str__a;
}

char * cc_str_duplicate__new( char const * restrict str,
                              bool do_reverse,
                              size_t max_len__d )
{
    if ( !str ) return NULL;

    size_t len = cc_str_len( str, NULL, max_len__d );
    char * str__a = (char *)malloc( len + 1 );
    if ( !str__a ) return NULL;

    if ( len > 0 )
    {
        char const * s = str;
        char * n = do_reverse ? str__a + len : str__a;

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
        *str__a = '\0';

    return str__a;
}

char * cc_str_concatenate__new( char const * restrict str_1__d,
                                char const * restrict str_2__d,
                                size_t max_len__d )
{
    size_t len_1 = cc_str_len( str_1__d, NULL, max_len__d );
    size_t len_2 = cc_str_len( str_2__d, NULL, max_len__d );
    size_t len = ( max_len__d != CC_MAX_LEN_IGNORE ) ? CC_MIN( len_1 + len_2, max_len__d )
                                                     : len_1 + len_2;

    char * str__a = (char *)malloc( len + 1 );
    if ( !str__a ) return NULL;

    if ( len > 0 )
    {
        char * n = str__a;

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
    else *str__a = '\0';

    return str__a;
}

char * cc_str_extend__new( char ** restrict str_1__f,
                           char const * restrict str_2__d,
                           size_t max_len__d )
{
    if ( !str_1__f ) return NULL;

    char * str__a = cc_str_concatenate__new( *str_1__f, str_2__d, max_len__d );
    if ( !str__a ) return NULL;

    if ( str_1__f )
        CC_FREE_NULL( str_1__f );

    return str__a;
}

char * cc_str_append__new( char ** restrict str_1__f,
                           char ** restrict str_2__f,
                           size_t max_len__d )
{
    if ( ( !str_1__f ) && ( !str_2__f ) ) return NULL;

    char * str__a = NULL;

    if ( str_1__f && str_2__f )
        str__a = cc_str_concatenate__new( *str_1__f, *str_2__f, max_len__d );
    else if ( str_1__f )
        str__a = cc_str_duplicate__new( *str_1__f, false, max_len__d );
    else if ( str_2__f )
        str__a = cc_str_duplicate__new( *str_2__f, false, max_len__d );

    if ( !str__a ) return NULL;

    if ( str_1__f )
        CC_FREE_NULL( str_1__f );

    if ( str_2__f )
        CC_FREE_NULL( str_2__f );

    return str__a;
}

char * cc_str_append_format__new( char ** restrict str__f,
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
    char * str__t = (char *)malloc( len_min + 1 );
    if ( !str__t )
    {
        va_end( args );
        return NULL;
    }

    int len_2 = vsnprintf( str__t, len_min + 1, fmt, args );
    if ( len_2 < 0 ) // error?
    {
        CC_FREE( str__t );
        va_end( args );
        return NULL;
    }

    va_end( args );

    if ( len_min != (size_t const)len_2 )
    {
        CC_FREE( str__t );
        return NULL;
    }

    // No need to free() str__f, str__t; cc_str_append__new() does that.
    return cc_str_append__new( str__f, &str__t, max_len__d );
}
