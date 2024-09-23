// Copyright (c) 2024 Mario Mlačak, mmlacak@gmail.com
// Licensed under GNU GPL v3+ license. See LICENSING, COPYING files for details.

#include "cc_checks.h"

#include "cc_pos_defs.h"
#include "cc_pos_utils.h"

#include "cc_path_single_step.h"


static bool cc_path_knight( CcChessboard * cb,
                            CcPieceType knight,
                            CcTagType tag,
                            CcPos from_pos,
                            cc_uint_t momentum,
                            CcPathLink ** path__io ) {
    bool is_starter = CC_TAG_IS_MOVE_STARTER_FLAG( tag );
    if ( is_starter && ( momentum > 0 ) ) return false;

    // Calculating momentum only for the very next step, since Knight is single-step piece.
    cc_uint_t mm = momentum;
    CcMaybeBoolEnum has_mm = cc_check_momentum_for_next_step( knight, tag, &mm );
    if ( !CC_MAYBE_IS_TRUE( has_mm ) ) return false;

    CcTypedStep const * ts = NULL;

    while ( CC_ITER_KNIGHT_STEPS( &ts, CC_STE_None ) ) {
        CcPos pos = cc_pos_add( from_pos, ts->step, 1 );
        if ( !cc_chessboard_is_pos_on_board( cb, pos.i, pos.j ) ) continue;

        CcMaybeBoolEnum capturing = cc_check_piece_can_capture_at( cb, knight, pos );
        if ( CC_MAYBE_IS_VOID( capturing ) ) return false;

        if ( CC_MAYBE_IS_FALSE( capturing ) ) {
            // TODO :: check if Knight can activate piece @ destination

            CcMaybeBoolEnum blocked = cc_check_piece_is_blocked_at( cb, knight, pos );
            if ( CC_MAYBE_IS_VOID( blocked ) ) return false;
            if ( CC_MAYBE_IS_TRUE( blocked ) ) continue; // Knight blocked, can't capture ...
        }

        CcPathLink * pl__t = cc_path_link__new( pos, mm );
        if ( !pl__t ) return false;

        CcPathLink * pl__w = cc_path_link_diverge( path__io, &pl__t );
        if ( !pl__w ) {
            cc_path_link_free_all( &pl__t );
            return false;
        }


        // TODO :: check if diverging piece @ destination
        //      --> tag no longer move starter
        //      --> do recursion
    }

    return false; // TODO
}

bool cc_path_single_step( CcChessboard * cb,
                          CcPieceType piece,
                          CcTagType tag,
                          CcPieceType activator,
                          CcPos from_pos,
                          cc_uint_t momentum,
                          CcPathLink ** path__o ) {
    if ( !cb ) return false;
    if ( !path__o ) return false;
    if ( *path__o ) return false;

    if ( !CC_PIECE_IS_VALID( piece ) ) return false;
    if ( !CC_TAG_IS_VALID( tag ) ) return false;
    if ( !cc_chessboard_is_pos_on_board( cb, from_pos.i, from_pos.j ) ) return false;

    CcPieceType from_piece = cc_chessboard_get_piece( cb, from_pos.i, from_pos.j );

    CcPathLink * path__t = cc_path_link__new( from_pos, momentum );
    if ( !path__t ) return false;

    if ( CC_PIECE_IS_TELEPORTER( from_piece ) ) {
        // TODO
    // } else if ( ... ) { // TODO :: trance-journeys
    // } else if ( ... ) { // TODO :: Pawn-sacrifice
    // } else if ( ... ) { // TODO :: sense-journeys
    } else {
        if ( CC_PIECE_IS_PAWN( piece ) ) {
            // if ( !cc_path_pawn( cb, piece, tag, from_pos, momentum, &path__t ) )
            //     return false;
            //
            // *path__o = path__t;
            // path__t = NULL;
            return false; // TODO
        } else if ( CC_PIECE_IS_KNIGHT( piece ) ) {
            if ( cc_path_knight( cb, piece, tag, from_pos, momentum, &path__t ) ) {
                *path__o = path__t;
                path__t = NULL;
                return true;
            }
        } else if ( CC_PIECE_IS_WAVE( piece ) ) {
            if ( !CC_PIECE_IS_VALID( activator ) ) return false;
            if ( !CC_PIECE_IS_ACTIVATOR( activator ) ) return false;

            // TODO
        } else
            return false;
    }

    if ( path__t ) {
        cc_path_link_free_all( &path__t );
    }

    return false;
}
