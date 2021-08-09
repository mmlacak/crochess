// Copyright (c) 2021 Mario Mlačak, mmlacak@gmail.com
// Licensed under 3-clause (modified) BSD license. See LICENSE for details.

#include <stdbool.h>
#include <ctype.h>
#include <stdlib.h>
#include <string.h>
#include <stdio.h>

#include "cc_chessboard.h"

#include "cc_step.h"
#include "cc_ply.h"
#include "cc_move.h"

#include "cc_parse_msg.h"
#include "cc_parse_move.h"


CcPly * cc_parse_ply( char const * const restrict ply_str,
                      CcChessboard const * const restrict cb,
                      CcParseMsg ** parse_msgs_io )
{
    if ( !ply_str ) return NULL;
    if ( !cb ) return NULL;
    if ( !parse_msgs_io ) return NULL;


// TODO
    return NULL;
}

CcMove * cc_parse_move( char const * const restrict move_str,
                        CcChessboard const * const restrict cb,
                        CcParseMsg ** parse_msgs_io )
{
    if ( !move_str ) return NULL;
    if ( !cb ) return NULL;
    if ( !parse_msgs_io ) return NULL;

    // char const * ply_str = cc_next_token_new( move_str, "~[]+#_" );
    // if ( !ply_str ) return NULL;

    // CcPly * ply = cc_parse_ply( ply_str, cb );

    // free( ply_str );
    // if ( !ply ) return NULL;

    // while ( ply_str = cc_next_token_new( NULL, NULL ) )
    // {
    //     if ( !ply_str ) break;

    //     CcPly * ply_next = cc_parse_ply( ply_str, cb );
    //     free( ply_str );

    //     if ( !ply_next ) break;

    //     ply->next = ply_next;
    // }

    // CcMoveStatusEnum mse = CC_MSE_None;
    // CcMove * move = cc_move_new( move_str, ply, mse );

    // return move;

// TODO
    return NULL;
}


bool cc_parse_char_is_ply_gather( char const c )
{
    return ( ( c == '[' ) || ( c == ']' ) );
}

char const * cc_parse_ply_gathers( char const * const restrict move_str, bool skip_or_stop_at )
{
    if ( !move_str ) return NULL;

    char const * m = move_str;

    if ( skip_or_stop_at )
        while ( ( *m != '\0' ) && cc_parse_char_is_ply_gather( *m ) ) ++m;
    else
        while ( ( *m != '\0' ) && !cc_parse_char_is_ply_gather( *m ) ) ++m;

    return m;
}

bool cc_parse_is_segment_divider( char const * const restrict move_str )
{
    if ( !move_str ) return false;

    char const c = *move_str;

    if ( ( c == '~' ) || ( c == '@' ) || ( c == '|' ) || ( c == ':' ) ) return true;

// TODO :: FIX
    // if ( *move_str == ':' )
    //     return ( *(move_str + 1) == ':' );

    return false;
}

char const * cc_parse_segment_divider( char const * const restrict move_str, bool skip_or_stop_at )
{
    if ( !move_str ) return NULL;

    char const * m = move_str;

    if ( skip_or_stop_at )
        while ( ( *m != '\0' ) && ( cc_parse_is_segment_divider( m ) ) ) ++m;
    else
        while ( ( *m != '\0' ) && ( !cc_parse_is_segment_divider( m ) ) ) ++m;

    return m;
}

char * cc_parse_next_segment_str_new( char const * const restrict move_str_s,
                                      CcParseMsg ** parse_msgs_io )
{
    if ( !parse_msgs_io ) return NULL;

    static char const * move_start = NULL;
    static char const * seg_start = NULL;
    static char const * seg_end = NULL;

    bool parse_1st = (bool)move_str_s;

    if ( move_str_s )
    {
        move_start = seg_start = seg_end = move_str_s;
    }

    if ( !seg_end ) return NULL;

    if ( *seg_end == '\0' )
    {
        seg_end = NULL; // Invalidate future calls without initialization.
        return NULL;
    }

    if ( !parse_1st )
        seg_start = cc_parse_segment_divider( seg_end, false );

    seg_end = cc_parse_segment_divider( seg_start, true );
    seg_end = cc_parse_segment_divider( seg_end, false );

    if ( seg_end == seg_start ) return NULL;

    size_t len = seg_end - seg_start;
    char * seg = malloc( len + 1 );
    if ( !seg ) return NULL;

    char const * pos = seg_start;
    char * res = seg;

    while ( pos < seg_end )
    {
        if ( !cc_parse_char_is_ply_gather( *pos ) )
            *res++ = *pos++;
        else
            ++pos;
    }

    *res = '\0';

    return seg;
}


CcParseMsg * cc_parse_msg_get_last( CcParseMsg const * const restrict parse_msgs )
{
    if ( !parse_msgs ) return NULL;

    CcParseMsg * pm = (CcParseMsg *)parse_msgs;

    while ( pm->next )
        pm = pm->next;

    return pm;
}
