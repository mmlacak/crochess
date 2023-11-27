// Copyright (c) 2021 Mario Mlačak, mmlacak@gmail.com
// Licensed under GNU GPL v3+ license. See LICENSING, COPYING files for details.

// #include <stddef.h>
#include <ctype.h>
#include <string.h>
#include <stdarg.h>
#include <stdio.h>

#include "cc_defines.h"
#include "cc_str_utils.h"

/**
    @file cc_str_utils.c
    @brief Strings, char arrays utility functions.
*/


bool cc_str_clear( char * restrict str__io_a,
                   size_t max_len__d ) {
    if ( !str__io_a ) return false;

    char * s = str__io_a;

    if ( max_len__d == CC_MAX_LEN_ZERO_TERMINATED ) {
        while ( *s ) *s++ = '\0';
    } else {
        size_t c = 0;

        while ( ( *s ) && ( c++ < max_len__d ) )
            *s++ = '\0';
    }

    return true;
}

bool cc_str_is_empty( char const * restrict str, bool ignore_spaces ) {
    if ( !str ) return true;

    char const * s = str;

    while ( *s != '\0' ) {
        if ( ignore_spaces && isspace( *s ) ) {
            ++s;
            continue;
        }

        if ( isprint( *s ) ) return false;
        ++s;
    }

    return true; // ( *s == '\0' );
}

bool cc_str_count_chars( char const * restrict str,
                         cc_ctype_fp_ischar_t fp_is_char,
                         size_t max_len__d,
                         size_t * restrict count__o ) {
    if ( !str ) return false;
    if ( !fp_is_char ) return false;
    if ( !count__o ) return false;

    char const * s = str;
    *count__o = 0;

    if ( max_len__d == CC_MAX_LEN_ZERO_TERMINATED ) {
        while ( *s != '\0' )
            if ( fp_is_char( *s++ ) ) *count__o += 1;
    } else {
        size_t c = 0;

        while ( ( *s ) && ( c++ < max_len__d ) )
            if ( fp_is_char( *s++ ) )
                *count__o += 1;
    }

    return true;
}

char const * cc_str_contains_char( char c,
                                   bool case_sensitive,
                                   char const * restrict start,
                                   char const * restrict end__d,
                                   size_t max_len__d ) {
    if ( !start ) return NULL;

    char const * s = start;
    size_t count = 0;

    while ( *s != '\0' ) {
        if ( case_sensitive ) {
            if ( *s == c ) return s;
        } else {
            if ( tolower( *s ) == tolower( c ) )
                return s;
        }

        if ( end__d && ( s >= end__d ) ) return NULL;

        if ( ( max_len__d != CC_MAX_LEN_ZERO_TERMINATED ) && ( count >= max_len__d ) )
            return NULL;

        ++count;
        ++s; }

    return NULL; }

char const * cc_str_traverse_chars( char const * restrict str,
                                    cc_ctype_fp_ischar_t fp_is_char,
                                    bool skip_or_stop_at,
                                    size_t max_len__d ) {
    if ( !str ) return NULL;
    if ( !fp_is_char ) return NULL;

    char const * str__w = str;

    if ( max_len__d == CC_MAX_LEN_ZERO_TERMINATED ) {
        while ( ( *str__w )
             && ( skip_or_stop_at == fp_is_char( *str__w ) ) )
                ++str__w;
    } else {
        size_t c = 0;

        while ( ( *str__w )
             && ( c++ < max_len__d )
             && ( skip_or_stop_at == fp_is_char( *str__w ) ) )
                ++str__w;
    }

    return str__w;
}


bool cc_str_to_case( char * restrict str__io_a,
                     bool to_upper_or_lower,
                     size_t max_len__d ) {
    if ( !str__io_a ) return false;

    char * s = str__io_a;

    if ( max_len__d == CC_MAX_LEN_ZERO_TERMINATED ) {
        while ( *s ) {
            *s = ( to_upper_or_lower ) ? toupper( *s )
                                       : tolower( *s );
            ++s;
        }
    } else {
        size_t c = 0;

        while ( ( *s ) && ( c++ < max_len__d ) ) {
            *s = ( to_upper_or_lower ) ? toupper( *s )
                                       : tolower( *s );
            ++s;
        }
    }

    return true;
}

char * cc_str_to_case__new( char const * restrict str,
                            bool to_upper_or_lower,
                            size_t max_len__d ) {
    if ( !str ) return NULL;

    size_t len = strlen( str );
    if ( max_len__d != CC_MAX_LEN_ZERO_TERMINATED )
        len = CC_MIN( len, max_len__d );

    char * lc__a = malloc( len + 1 );
    if ( !lc__a ) return NULL;

    char * s = lc__a;
    char const * pos = str;

    if ( max_len__d == CC_MAX_LEN_ZERO_TERMINATED ) {
        while ( *pos ) {
            *s++ = ( to_upper_or_lower ) ? toupper( *pos )
                                         : tolower( *pos );
            ++pos;
        }
    } else {
        size_t c = 0;

        while ( ( *pos ) && ( c++ < len ) ) {
            *s++ = ( to_upper_or_lower ) ? toupper( *pos )
                                         : tolower( *pos );
            ++pos;
        }
    }

    lc__a[ len ] = '\0';

    return lc__a;
}


char const * cc_str_end( char const * restrict start,
                         char const * restrict end__d,
                         size_t max_len__d ) {
    if ( !start ) return NULL;

    size_t len = 0;
    char const * s = start;

    if ( !end__d ) {
        if ( max_len__d == CC_MAX_LEN_ZERO_TERMINATED )
            while ( *s != '\0' ) ++s;
        else
            while ( ( *s != '\0' ) && ( len++ < max_len__d ) ) ++s;
    } else {
        char const * e = end__d;

        if ( max_len__d == CC_MAX_LEN_ZERO_TERMINATED )
            while ( ( *s != '\0' ) && ( s < e ) ) ++s;
        else
            while ( ( *s != '\0' ) && ( s < e ) && ( len++ < max_len__d ) ) ++s;
    }

    return s;
}

size_t cc_str_len( char const * restrict start,
                   char const * restrict end__d,
                   size_t max_len__d ) {
    char const * end = cc_str_end( start, end__d, max_len__d );
    if ( !end ) return 0;

    return (size_t)(end - start);
}

int cc_str_len_fmt_va( char const * restrict fmt, va_list args ) {
    va_list tmp;
    va_copy( tmp, args );

    int len = vsnprintf( NULL, 0, fmt, tmp ); // len does not include '\0'.
    va_end( tmp );

    return len;
}

int cc_str_len_format( char const * restrict fmt, ... ) {
    va_list args;
    va_start( args, fmt );

    int len = vsnprintf( NULL, 0, fmt, args ); // len does not include '\0'.
    //
    // Keeping implementation here, just to avoid another function call,
    // although VA call would provide single reference behaviour.
    //
    // int len = cc_str_len_fmt_va( fmt, args ); // len does not include '\0'.

    va_end( args );

    return len;
}

bool cc_str_is_equal( char const * restrict start_1,
                      char const * restrict end_1__d,
                      char const * restrict start_2,
                      char const * restrict end_2__d,
                      size_t max_len__d ) {
    if ( !start_1 ) return false;
    if ( !start_2 ) return false;

    size_t len_1 = cc_str_len( start_1, end_1__d, max_len__d );
    size_t len_2 = cc_str_len( start_2, end_2__d, max_len__d );
    if ( len_1 != len_2 ) return false;
    if ( len_1 == 0 ) return true; // Two empty strings are equal.

    int res = strncmp( start_1, start_2, len_1 );
    return ( res == 0 );
}


size_t cc_str_copy( char const * restrict start,
                    char const * restrict end__d,
                    size_t max_len__d,
                    char * restrict dest__o,
                    char const * restrict dest_end__d,
                    size_t size_dest__d ) {
    if ( !start ) return 0;
    if ( !dest__o ) return 0;

    size_t len = cc_str_len( start, end__d, max_len__d );
    if ( len < 1 ) return 0;

    if ( ( size_dest__d != CC_SIZE_IGNORE ) && ( size_dest__d < len ) )
        return 0;

    if ( dest_end__d && ( dest_end__d < dest__o + len ) )
        return 0;

    if ( !strncpy( dest__o, start, len ) )
        return 0;

    if ( ( size_dest__d == CC_SIZE_IGNORE ) || ( len < size_dest__d ) )
        dest__o[ len ] = '\0';

    return len;
}

char * cc_str_copy__new( char const * restrict start,
                         char const * restrict end__d,
                         size_t max_len__d ) {
    if ( !start ) return NULL;

    size_t len = cc_str_len( start, end__d, max_len__d );
    char * str__a = malloc( len + 1 );
    if ( !str__a ) return NULL;

    if ( !strncpy( str__a, start, len ) ) return NULL;
    str__a[ len ] = '\0';

    return str__a;
}

char * cc_str_fmt_va__new( size_t max_len__d,
                           char const * restrict fmt,
                           va_list args ) {
    if ( !fmt ) return NULL;

    va_list tmp;
    va_copy( tmp, args );

    int len = cc_str_len_fmt_va( fmt, tmp );
    if ( len < 0 ) { // error ?
        va_end( tmp );
        return NULL;
    }

    size_t len_min =
        ( max_len__d != CC_MAX_LEN_ZERO_TERMINATED ) ? CC_MIN( (size_t)len, max_len__d )
                                                     : (size_t)len;

    char * str__a = (char *)malloc( len_min + 1 );
    if ( !str__a ) {
        va_end( tmp );
        return NULL;
    }

    int len_2 = vsnprintf( str__a, len_min + 1, fmt, tmp );
    if ( len_2 < 0 ) { // error ?
        CC_FREE( str__a );
        va_end( tmp );
        return NULL;
    }

    va_end( tmp );

    if ( len_min != (size_t const)len_2 ) {
        CC_FREE( str__a );
        return NULL;
    }

    return str__a;
}

char * cc_str_fmt__new( size_t max_len__d,
                        char const * restrict fmt, ... ) {
    if ( !fmt ) return NULL;

    va_list args;
    va_start( args, fmt );

    char * str__a = cc_str_fmt_va__new( max_len__d, fmt, args );

    va_end( args );

    return str__a;
}

char * cc_str_duplicate__new( char const * restrict str,
                              bool do_reverse,
                              size_t max_len__d ) {
    if ( !str ) return NULL;

    size_t len = cc_str_len( str, NULL, max_len__d );
    char * str__a = (char *)malloc( len + 1 ); // +1 for '\0'
    if ( !str__a ) return NULL;

    if ( len > 0 ) {
        char const * s = str;
        char * n = do_reverse ? str__a + len : str__a;

        if ( do_reverse ) {
            *n-- = '\0';

            for ( size_t i = 0; ( i < len ) && ( *s != '\0' ); ++i ) *n-- = *s++;
        } else {
            for ( size_t i = 0; ( i < len ) && ( *s != '\0' ); ++i ) *n++ = *s++;

            *n = '\0';
        }
    } else
        *str__a = '\0';

    return str__a;
}

char * cc_str_append_into( char * restrict str__io_a,
                           size_t size_dest__d,
                           char const * restrict str,
                           size_t max_len__d ) {
    if ( !str__io_a ) return NULL;
    if ( !str ) return NULL;

    bool if_ignore_size = ( size_dest__d == CC_SIZE_IGNORE );
    bool if_zero_terminated = ( max_len__d == CC_MAX_LEN_ZERO_TERMINATED );

    size_t count = 0;
    char * io = str__io_a;

    while ( *io != '\0' ) {
        if ( if_ignore_size || ( count < size_dest__d ) ) {
            ++io;
            ++count;
        } else
            return NULL; // Early exit, I/O string is already full.
    }

    count += 1; // +1, to leave room for '\0'
    size_t appended = 0;
    char const * s = str;

    while ( *s != '\0' ) {
        if ( ( if_ignore_size || ( count < size_dest__d ) ) &&
                ( if_zero_terminated || ( appended < max_len__d ) ) ) {
            *io++ = *s++;

            ++appended;
            ++count;
        } else
            break;
    }

    *io = '\0';
    return io;
}

char * cc_str_append__new( char const * restrict str_1__d,
                           char const * restrict str_2__d,
                           size_t max_len__d ) {
    size_t len_1 = cc_str_len( str_1__d, NULL, max_len__d );
    size_t len_2 = cc_str_len( str_2__d, NULL, max_len__d );

    size_t len =
        ( max_len__d != CC_MAX_LEN_ZERO_TERMINATED ) ? CC_MIN( len_1 + len_2, max_len__d )
                                                     : len_1 + len_2;

    char * str__a = (char *)malloc( len + 1 );
    if ( !str__a ) return NULL;

    if ( len > 0 ) {
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
    } else
        *str__a = '\0';

    return str__a;
}

char * cc_str_append_free__new( char ** restrict str_1__f,
                                char ** restrict str_2__f,
                                size_t max_len__d ) {
    if ( ( !str_1__f ) && ( !str_2__f ) ) return NULL;

    char * str__a = NULL;

    if ( str_1__f && str_2__f )
        str__a = cc_str_append__new( *str_1__f, *str_2__f, max_len__d );
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

char * cc_str_append_fmt_va__new( char ** restrict str__f,
                                  size_t max_len__d,
                                  char const * restrict fmt,
                                  va_list args ) {
    va_list tmp;
    va_copy( tmp, args );

    int len = cc_str_len_fmt_va( fmt, tmp ); // len does not include '\0'.
    if ( len < 0 ) { // error?
        va_end( tmp );
        return NULL;
    }

    size_t len_min =
        ( max_len__d != CC_MAX_LEN_ZERO_TERMINATED ) ? CC_MIN( (size_t)len, max_len__d )
                                                     : (size_t)len;

    char * str__t = (char *)malloc( len_min + 1 );
    if ( !str__t ) {
        va_end( tmp );
        return NULL;
    }

    int len_2 = vsnprintf( str__t, len_min + 1, fmt, tmp );
    if ( len_2 < 0 ) { // error?
        CC_FREE( str__t );
        va_end( tmp );
        return NULL;
    }

    va_end( tmp );

    if ( len_min != (size_t const)len_2 ) {
        CC_FREE( str__t );
        return NULL;
    }

    // No need to free() str__f, str__t; cc_str_append_free__new() does that.
    return cc_str_append_free__new( str__f, &str__t, max_len__d );
}

char * cc_str_append_fmt__new( char ** restrict str__f,
                               size_t max_len__d,
                               char const * restrict fmt, ... ) {
    va_list args;
    va_start( args, fmt );

    char * str__a = cc_str_append_fmt_va__new( str__f, max_len__d, fmt, args );

    va_end( args );

    return str__a;
}

// TODO :: (?) move / return newly allocated string (?)
//
bool cc_str_print( char const * restrict start,
                   char const * restrict end__d,
                   size_t max_len__d,
                   char const * restrict fmt_str,
                   size_t fmt_len__d,
                   char const * restrict fmt__d, ... ) {

#ifdef __CC_STR_PRINT_INFO__

    if ( !start ) return false;
    if ( !fmt_str ) return false;

    char * str__a = cc_str_copy__new( start, end__d, max_len__d );
    if ( !str__a ) return false;

    int result = printf( fmt_str, str__a );

    CC_FREE( str__a );

    if ( result < 0 ) return false;

    bool has_fmt = ( fmt__d && ( *fmt__d != '\0' ) );
    if ( !has_fmt ) return true;

    va_list args;
    va_start( args, fmt__d );

    char * fmt__a = cc_str_fmt_va__new( fmt_len__d, fmt__d, args );
    if ( !fmt__a ) {
        va_end( args );
        return false;
    }

    result = printf( "%s", fmt__a );

    CC_FREE( fmt__a );
    va_end( args );

    return ( result >= 0 );

#else // __CC_STR_PRINT_INFO__

    return true;

#endif // __CC_STR_PRINT_INFO__

}
//
// TODO :: (?) move / return newly allocated string (?)
