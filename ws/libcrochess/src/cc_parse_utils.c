// Copyright (c) 2021 Mario Mlačak, mmlacak@gmail.com
// Licensed under 3-clause (modified) BSD license. See LICENSE for details.

#include "cc_parse_utils.h"


bool cc_parse_utils_char_is_ply_gather( char const c )
{
    return ( ( c == '[' ) || ( c == ']' ) );
}

char const * cc_parse_utils_go_ply_gathers( char const * const restrict move_str, bool skip_or_stop_at )
{
    if ( !move_str ) return NULL;

    char const * m = move_str;

    if ( skip_or_stop_at )
        while ( ( *m != '\0' ) && cc_parse_utils_char_is_ply_gather( *m ) ) ++m;
    else
        while ( ( *m != '\0' ) && !cc_parse_utils_char_is_ply_gather( *m ) ) ++m;

    return m;
}

size_t cc_parse_utils_ply_divider_len( char const * const restrict move_str )
{
    if ( !move_str ) return 0;

    char const * c = move_str;

    if ( *c == '~' ) return 1;

    if ( *c == '|' )
    {
        if ( *++c == '|' ) return 2;
        return 1;
    }

    if ( *c == '@' )
    {
        if ( *++c == '@' )
        {
            if ( *++c == '@' ) return 3;
            return 2;
        }
        return 1;
    }

    if ( *c == ':' )
    {
        if ( *++c == ':' ) return 2;
        return 0; // En passant is not ply divider.
    }

    return 0;
}

char const * cc_parse_utils_go_ply_dividers( char const * const restrict move_str, bool skip_or_stop_at )
{
    if ( !move_str ) return NULL;

    char const * m = move_str;

    if ( skip_or_stop_at )
        while ( *m != '\0' )
        {
            size_t len = cc_parse_utils_ply_divider_len( m );

            if ( len > 0 )
                m += len;
            else
                break;
        }
    else
        while ( ( *m != '\0' ) && ( !cc_parse_utils_ply_divider_len( m ) ) ) ++m;

    return m;
}

char * cc_parse_utils_next_ply_str_new( char const * const restrict move_str_s,
                                        CcParseMsg ** parse_msgs_io )
{
    if ( !parse_msgs_io ) return NULL;

    /* static char const * move_start = NULL; */
    static char const * ply_start = NULL;
    static char const * ply_end = NULL;

    bool parse_1st = (bool)move_str_s;

    if ( move_str_s )
    {
        /* move_start = */ ply_start = ply_end = move_str_s;
    }

    if ( !ply_end ) return NULL;

    if ( *ply_end == '\0' )
    {
        ply_end = NULL; // Invalidate future calls without initialization.
        return NULL;
    }

    if ( !parse_1st )
        ply_start = cc_parse_utils_go_ply_dividers( ply_end, false );

    ply_end = cc_parse_utils_go_ply_dividers( ply_start, true );
    ply_end = cc_parse_utils_go_ply_dividers( ply_end, false );

    if ( ply_end == ply_start ) return NULL;

    size_t len = ply_end - ply_start;
    char * ply_str = malloc( len + 1 );
    if ( !ply_str ) return NULL;

    char const * in = ply_start;
    char * out = ply_str;

    while ( in < ply_end )
    {
        if ( !cc_parse_utils_char_is_ply_gather( *in ) )
            *out++ = *in++;
        else
            ++in;
    }

    *out = '\0';

    return ply_str;
}
