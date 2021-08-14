// Copyright (c) 2021 Mario Mlačak, mmlacak@gmail.com
// Licensed under 3-clause (modified) BSD license. See LICENSE for details.

#include <ctype.h>

#include "cc_piece.h"

#include "cc_parse_utils.h"

/**
    @file cc_parse_utils.c
    @brief Helper functions to parse algebraic notation.
*/


bool cc_parse_utils_char_is_ply_gather( char const c )
{
    return ( ( c == '[' ) || ( c == ']' ) );
}

char const * cc_parse_utils_go_ply_gather( char const * const restrict move_str,
                                           bool const skip_or_stop_at )
{
    if ( !move_str ) return NULL;

    char const * m = move_str;

    if ( skip_or_stop_at )
        while ( ( *m != '\0' ) && cc_parse_utils_char_is_ply_gather( *m ) ) ++m;
    else
        while ( ( *m != '\0' ) && !cc_parse_utils_char_is_ply_gather( *m ) ) ++m;

    return m;
}

size_t cc_parse_utils_ply_link_len( char const * const restrict move_str )
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

char const * cc_parse_utils_go_ply_link( char const * const restrict move_str,
                                         bool const skip_or_stop_at )
{
    if ( !move_str ) return NULL;

    char const * m = move_str;

    if ( skip_or_stop_at )
        while ( *m != '\0' )
        {
            size_t len = cc_parse_utils_ply_link_len( m );

            if ( len > 0 )
                m += len;
            else
                break;
        }
    else
        while ( ( *m != '\0' ) && ( !cc_parse_utils_ply_link_len( m ) ) ) ++m;

    return m;
}

char * cc_parse_utils_next_ply_str_new( char const * const restrict move_str_s )
{
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
        ply_start = cc_parse_utils_go_ply_link( ply_end, false );

    ply_end = cc_parse_utils_go_ply_link( ply_start, true );
    ply_end = cc_parse_utils_go_ply_link( ply_end, false );

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

bool cc_parse_utils_get_ply_link( char const * const restrict ply_str,
                                  CcPlyLinkEnum * const restrict link_o )
{
    if ( !ply_str ) return false;
    if ( !link_o ) return false;

    size_t len = cc_parse_utils_ply_link_len( ply_str );

    if ( len == 0 )
    {
        *link_o = CC_PLE_Ply;
        return true;
    }
    else if ( len == 1 )
    {
        char const c_0 = ply_str[ 0 ];

        if ( c_0 == '~' )
        {
            *link_o = CC_PLE_Ply;
            return true;
        }
        else if ( c_0 == '|' )
        {
            *link_o = CC_PLE_Teleportation;
            return true;
        }
        else if ( c_0 == '@' )
        {
            *link_o = CC_PLE_TranceJourney;
            return true;
        }
    }
    else if ( len == 2 )
    {
        char const c_0 = ply_str[ 0 ];
        char const c_1 = ply_str[ 1 ];

        if ( ( c_0 == '|' ) && ( c_1 == '|' ) )
        {
            *link_o = CC_PLE_FailedTeleportation;
            return true;
        }
        else if ( ( c_0 == '@' ) && ( c_1 == '@' ) )
        {
            *link_o = CC_PLE_DualTranceJourney;
            return true;
        }
        else if ( ( c_0 == ':' ) && ( c_1 == ':' ) )
        {
            *link_o = CC_PLE_PawnSacrifice;
            return true;
        }
    }
    else if ( len == 3 )
    {
        char const c_0 = ply_str[ 0 ];
        char const c_1 = ply_str[ 1 ];
        char const c_2 = ply_str[ 2 ];

        if ( ( c_0 == '@' ) && ( c_1 == '@' ) && ( c_2 == '@' ) )
        {
            *link_o = CC_PLE_FailedTranceJourney;
            return true;
        }
    }

    return false;
}

char const * cc_parse_utils_get_steps_str( char const * const restrict ply_str )
{
    if ( !ply_str ) return NULL;

    size_t len = cc_parse_utils_ply_link_len( ply_str );

    char const * p = ply_str + len;

    if ( isupper( *p ) ) ++p;

    return p;
}

size_t cc_parse_utils_step_link_len( char const * const restrict ply_str )
{
    if ( !ply_str ) return 0;

    char const * c = ply_str;

    if ( *c == '-' ) return 1;

    if ( *c == '.' )
    {
        if ( *++c == '.' ) return 2;
        return 1;
    }

    return 0;
}

char const * cc_parse_utils_go_step_link( char const * const restrict ply_str,
                                          bool const skip_or_stop_at )
{
    if ( !ply_str ) return NULL;

    char const * p = ply_str;

    if ( skip_or_stop_at )
        while ( *p != '\0' )
        {
            size_t len = cc_parse_utils_step_link_len( p );

            if ( len > 0 )
                p += len;
            else
                break;
        }
    else
        while ( ( *p != '\0' ) && ( !cc_parse_utils_step_link_len( p ) ) ) ++p;

    return p;
}
