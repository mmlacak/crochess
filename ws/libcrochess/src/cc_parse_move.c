// Copyright (c) 2021 Mario Mlačak, mmlacak@gmail.com
// Licensed under 3-clause (modified) BSD license. See LICENSE for details.

// #include <stdbool.h>
// #include <ctype.h>
// #include <stdlib.h>
// #include <string.h>
// #include <stdio.h>

#include "cc_parse_msg.h"
#include "cc_parse_utils.h"
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
