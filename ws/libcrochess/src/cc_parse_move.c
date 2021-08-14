// Copyright (c) 2021 Mario Mlačak, mmlacak@gmail.com
// Licensed under 3-clause (modified) BSD license. See LICENSE for details.

// #include <stdbool.h>
#include <ctype.h>
// #include <stdlib.h>
// #include <string.h>
// #include <stdio.h>

#include "cc_parse_msg.h"
#include "cc_parse_utils.h"
#include "cc_parse_move.h"


bool cc_parse_ply_get_piece( char const * const restrict ply_str,
                             bool const is_light,
                             CcPieceEnum * const restrict piece_o )
{
    if ( !ply_str ) return false;

    char const * p = ply_str;

    p = cc_parse_utils_go_ply_link( p, true );
    if ( !p ) return false;

    if ( isupper( *p ) ) // <!> Useage of cc_piece_is_symbol() here is bug,
                         //     all other upper chars would end as Pawns.
        *piece_o = cc_piece_from_symbol( *p, is_light );
    else
        *piece_o = ( is_light ) ? CC_PE_LightPawn : CC_PE_DarkPawn;

    return cc_piece_is_valid( *piece_o );
}

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
