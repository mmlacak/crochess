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
                      CcParseMsg ** parse_msgs )
{
    if ( !ply_str ) return NULL;
    if ( !cb ) return NULL;
    if ( !parse_msgs ) return NULL;


// TODO
    return NULL;
}

CcMove * cc_parse_move( char const * const restrict move_str,
                        CcChessboard const * const restrict cb,
                        CcParseMsg ** parse_msgs )
{
    if ( !move_str ) return NULL;
    if ( !cb ) return NULL;
    if ( !parse_msgs ) return NULL;

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


char * cc_parse_next_ply_str_new( char const * const restrict move_str_s,
                                  CcParseMsg ** parse_msgs )
{
    if ( !parse_msgs ) return NULL;

    static char const * move_start = NULL;
    static char const * ply_start = NULL;
    static char const * ply_end = NULL;

    bool parse_1st = (bool)move_str_s;
    bool skipped_opening_bracket = false;
    bool skipped_separators = false;
    bool skipped_teminators = false;

    if ( move_str_s )
    {
        move_start = ply_start = ply_end = move_str_s;

        if ( *ply_start == '[' )
        {
            skipped_opening_bracket = true;
            ply_end = ++ply_start;
        }
    }
    else
    {
        if ( !ply_end ) return NULL;

        // if ( *ply_end != '\0' )
        //     ply_start = ply_end + 1;
        // else
        //     ply_start = ply_end;
        // ply_start = ply_end + ( ( *ply_end != '\0' ) ? 1 : 0 );
        ply_start = ply_end + ( isalnum( *ply_end ) ? 1 : 0 );

        if ( *ply_start == ']' ) ++ply_start;

        if ( *ply_start == '~' )
        {
            ++ply_start;
            skipped_separators = true;
        }
        else if ( *ply_start == '|' )
        {
            ++ply_start;

            if ( *ply_start == '|' )
            {
                ++ply_start;
                skipped_teminators = true;
            }
            else skipped_separators = true;
        }
        else if ( *ply_start == '@' )
        {
            ++ply_start;

            if ( *ply_start == '@' )
            {
                ++ply_start;
                skipped_teminators = true;

                if ( *ply_start == '@' ) ++ply_start;
            }
            else skipped_separators = true;
        }
        else if ( *ply_start == ':' )
        {
            ++ply_start;
            skipped_separators = true;

            if ( *ply_start == ':' ) ++ply_start;
            else
            {
                cc_parse_msg_init_or_append_new( parse_msgs, CC_PME_Error, ply_start - move_start, "malformed ply separator" );
                return NULL;
            }
        }

        if ( *ply_start == '[' )
        {
            skipped_opening_bracket = true;
            ++ply_start;
        }
    }

    if ( ( !parse_1st ) && ( !skipped_separators ) && skipped_opening_bracket )
    {
        cc_parse_msg_init_or_append_new( parse_msgs, CC_PME_Error, ply_start - move_start, "ply started without separator" );
        return NULL;
    }

    if ( *ply_start == '+' )
    {
        ply_end = NULL; // Move end, invalidate another call on the same str.
        return "+";
    }
    else if ( *ply_start == '#' )
    {
        ply_end = NULL; // Move end, invalidate another call on the same str.
        return "#";
    }
    else if ( *ply_start == '\0' )
    {
        ply_end = NULL; // String end, invalidate another call on the same str.

        if ( skipped_separators || skipped_teminators || skipped_opening_bracket )
        {
            cc_parse_msg_init_or_append_new( parse_msgs, CC_PME_Error, ply_start - move_start, "premature end of ply" );
        }

        return NULL;
    }

    ply_end = ply_start + 1;
    while ( isalnum( *ply_end ) ) ++ply_end;

    if ( ( skipped_opening_bracket ) && ( *ply_end != ']' ) )
    {
        cc_parse_msg_init_or_append_new( parse_msgs, CC_PME_Error, ply_start - move_start, "no closing bracket" );
        return NULL;
    }

    if ( ( *ply_end == ']' ) && ( !skipped_opening_bracket ) )
    {
        cc_parse_msg_init_or_append_new( parse_msgs, CC_PME_Error, ply_start - move_start, "no opening bracket" );
        return NULL;
    }

    if ( ply_end == ply_start ) return NULL;

    size_t len = ply_end - ply_start;
    char * pos = malloc( len + 1 );
    if ( !pos ) return NULL;

    strncpy( pos, ply_start, len );
    pos[ len ] = '\0';

    return pos;
}


CcParseMsg * cc_parse_msg_get_last( CcParseMsg const * const restrict parse_msgs )
{
    if ( !parse_msgs ) return NULL;

    CcParseMsg * pm = (CcParseMsg *)parse_msgs;

    while ( pm->next )
        pm = pm->next;

    return pm;
}
