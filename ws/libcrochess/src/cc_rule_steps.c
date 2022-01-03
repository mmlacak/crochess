// Copyright (c) 2021 Mario Mlačak, mmlacak@gmail.com
// Licensed under GNU GPL v3+ license. See LICENSING, COPYING files for details.

#include <stddef.h>
#include <math.h>

#include "cc_defines.h"
#include "cc_pos.h"
#include "cc_gen_pos.h"
#include "cc_gen_steps.h"
#include "cc_rule_steps.h"


bool cc_rule_steps_find_piece_start_pos( CcGame * restrict game,
                                         CcPlyLinkEnum ple,
                                         CcPieceEnum piece,
                                         bool include_opposite,
                                         int disamb_i__d,
                                         int disamb_j__d,
                                         CcPos dest,
                                         CcPos * restrict start__o )
{
    if ( !game ) return false;
    if ( !game->chessboard ) return false;
    if ( !start__o ) return false;

    CcChessboard * cb = game->chessboard;

    bool is_disamb_i = ( cc_chessboard_is_coord_on_board( cb, disamb_i__d ) );
    bool is_disamb_j = ( cc_chessboard_is_coord_on_board( cb, disamb_j__d ) );

    if ( ( CC_COORD_IS_VALID( disamb_i__d ) ) && ( !is_disamb_i ) ) return false;
    if ( ( CC_COORD_IS_VALID( disamb_j__d ) ) && ( !is_disamb_j ) ) return false;

    if ( !cc_chessboard_is_pos_on_board( cb, dest.i, dest.j ) )
        return false;

    CcPosLink * pos_s__a = NULL;

    if ( is_disamb_i && is_disamb_j )
    {
        CcPieceEnum pe = cc_chessboard_get_piece( cb, disamb_i__d, disamb_j__d );

        if ( CC_PIECE_IS_THE_SAME( pe, piece ) )
        {
            if (  cc_rule_steps_check_movement( game, ple, pe,
                                                cc_pos( disamb_i__d, disamb_j__d ),
                                                dest,
                                                &pos_s__a ) )
            {
                *start__o = cc_pos( disamb_i__d, disamb_j__d );

                cc_pos_link_free_all( &pos_s__a );
                return true;
            }
        }

        return false;
    }

    CcPos start = cc_pos_invalid();

    while ( cc_gen_steps_piece_pos_iter( cb, piece, include_opposite, &start ) )
    {
        if ( is_disamb_i && ( disamb_i__d != start.i ) ) continue;
        if ( is_disamb_j && ( disamb_j__d != start.j ) ) continue;

        if ( cc_rule_steps_check_movement( game, ple, piece, start, dest, &pos_s__a ) )
        {
            *start__o = start;

            cc_pos_link_free_all( &pos_s__a );
            return true;
        }
    }

    return false;
}


// TODO :: REDESIGN
bool cc_rule_steps_is_ply_allowed( CcGame * restrict game,
                                   CcPieceEnum piece,
                                   CcPos start,
                                   CcPos dest )
{
    if ( !game ) return false;
    if ( !game->chessboard ) return false;

    CcChessboard * cb = game->chessboard;
    CcPieceEnum target = cc_chessboard_get_piece( cb, dest.i, dest.j );

    // An empty field, always targetable.
    if ( CC_PIECE_IS_NONE( target ) ) return true;

    // Kings can't be ever captured, activated.
    if ( CC_PIECE_IS_KING( target ) ) return false;

    bool is_same_owner = cc_piece_has_same_owner( piece, target );

    // Own Wave, Pyramid can be activated by any own piece.
    if ( ( CC_PIECE_IS_WAVE( target ) || CC_PIECE_IS_PYRAMID( target ) )
        && is_same_owner )
            return true;

    // Wave can activate other Wave, or any other own piece.
    if ( CC_PIECE_IS_WAVE( piece ) )
        if ( CC_PIECE_IS_WAVE( target ) || is_same_owner )
            return true;

    // Starchild can activate own Starchild, Wave; on its step-fields.
    if ( CC_PIECE_IS_STARCHILD( piece ) )
        if ( ( CC_PIECE_IS_STARCHILD( target ) || CC_PIECE_IS_WAVE( target ) )
            && is_same_owner )
                return true;

    // Special case, not handled:
    // Starchild can activate any own piece (except King), opponent’s
    // Starchild and any Star on its neighboring-fields.

    // Monolith, Star can only move to an empty field.
    if ( CC_PIECE_IS_MONOLITH( piece ) || CC_PIECE_IS_STAR( piece ) )
        return ( CC_PIECE_IS_NONE( target ) );

    // Any piece, own or opponent's, can teleport, except Kings.
    if ( !CC_PIECE_IS_KING( piece ) )
    {
        if ( CC_PIECE_IS_MONOLITH( target ) ) return true;
        if ( CC_PIECE_IS_STAR( target ) ) return true;
    }

    // Any piece can capture opponent's piece.
    if ( cc_piece_has_different_owner( piece, target ) ) return true;

    return false;
}
// TODO :: REDESIGN

bool cc_rule_steps_check_bishop( CcGame * restrict game,
                                 CcPlyLinkEnum ple,
                                 CcPieceEnum piece,
                                 CcPos start,
                                 CcPos dest,
                                 CcPosLink ** restrict pls__o )
{
    if ( !game ) return false;
    if ( !game->chessboard ) return false;
    if ( !pls__o ) return false;
    if ( *pls__o ) return false;

    if ( !CC_PIECE_IS_BISHOP( piece ) ) return false;

    CcPos offset = cc_pos_subtract( dest, start );
    if ( ( offset.i == 0 ) || ( offset.j == 0 ) ) return false;
    if ( abs( offset.i ) != abs( offset.j ) ) return false;

    CcPos step = cc_pos( CC_SIGN( offset.i ), CC_SIGN( offset.j ) );
    if ( !CC_GEN_POS_BISHOP_STEP_IS_VALID( step ) ) return false;

    CcChessboard * cb = game->chessboard;
    CcPosLink * pl__t = NULL;

    for ( CcPos p = cc_pos_add( start, step ); !cc_pos_is_equal( p, dest ); p = cc_pos_add( p, step ) )
    {
        CcPieceEnum pe = cc_chessboard_get_piece( cb, p.i, p.j );
        if ( !CC_PIECE_IS_NONE( pe ) ) return false;

        if ( !cc_pos_link_append_or_init( &pl__t, p ) )
        {
            cc_pos_link_free_all( &pl__t );
            return false;
        }
    }

    if ( cc_rule_steps_is_ply_allowed( game, piece, start, dest ) )
    {
        *pls__o = pl__t; // Ownership transfer --> pl__t is now weak pointer --> no free().
        return true;
    }

    cc_pos_link_free_all( &pl__t );
    return false;
}

bool cc_rule_steps_check_movement( CcGame * restrict game,
                                   CcPlyLinkEnum ple,
                                   CcPieceEnum piece,
                                   CcPos start,
                                   CcPos dest,
                                   CcPosLink ** restrict pls__o )
{
    // if ( !cb ) return false;
    // if ( !pls__o ) return false;
    // if ( *pls__o ) return false;

    if ( CC_PIECE_IS_BISHOP( piece ) )
        return cc_rule_steps_check_bishop( game, ple, piece, start, dest, pls__o );
    else
        return false;
}
