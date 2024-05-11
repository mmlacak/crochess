// Copyright (c) 2024 Mario Mlačak, mmlacak@gmail.com
// Licensed under GNU GPL v3+ license. See LICENSING, COPYING files for details.

#include "cc_path_single_step.h"

/**
    @file cc_path_single_step.c
    @brief Path generating, filtering for single-step pieces.
*/


static bool cc_path_pawn( CcChessboard * cb,
                          CcPieceEnum pawn,
                          CcPos pos,
                          CcPathLink ** path__e_a ) {
    // <i> Not needed, already checked in the only caller, i.e. cc_path_single_step().
    // if ( !cb ) return false;
    // if ( !path__e_a ) return false;
    // if ( *path__e_a ) return false;

    if ( !cc_chessboard_is_pos_on_board( cb, pos.i, pos.j ) ) return false;
    if ( !CC_PIECE_IS_PAWN( pawn ) ) return false;

    CcPieceEnum piece = cc_chessboard_get_piece( cb, pos.i, pos.j );
    bool is_pos_current = ( piece == pawn );

    bool is_pawn_light = cc_piece_is_light( pawn );
    CcTypedStep const * step = NULL;
    CcTypedStep const * guard = NULL;

    if ( cc_variant_has_sideways_pawns( cb->type ) ) {
        step = is_pawn_light ? CC_STEPS_LIGHT_SIDEWAYS_PAWN : CC_STEPS_DARK_SIDEWAYS_PAWN;
        guard = step + CC_STEPS_SIDEWAYS_PAWN_LEN;
    } else {
        step = is_pawn_light ? CC_STEPS_LIGHT_PAWN : CC_STEPS_DARK_PAWN;
        guard = step + CC_STEPS_PAWN_LEN;
    }

    // if ( !step || !guard ) return false; // <!> Not needed, as long as both vars are certainly initialized.

    CcTypedStep const * s = step;
    bool result = true;

    while ( s && ( s <= guard ) ) {
        if ( !CC_TYPED_STEP_IS_VALID( *s ) ) break;

        if ( s->type == CC_STE_Capture ) {
            CcPos destination = cc_pos_add( pos, s->step, 1 );
            // CcPieceEnum opponent = cc_chessboard_get_piece( cb, destination.i, destination.j );
            // if ( cc_piece_has_same_color( piece, opponent ) ) continue;
            if ( !CC_MAYBE_IS_TRUE( cc_chessboard_is_opponent_at( cb, destination.i, destination.j, pawn ) ) ) continue;

            CcPosPieceTag ppt = cc_convert_pos_to_ppt( cb, destination );
            CcPptLink * pptl__t = cc_ppt_link__new( ppt );
            if ( !( result = pptl__t ) ) break;

            if ( !( result = cc_path_link_append( path__e_a, &pptl__t ) ) ) {
                cc_ppt_link_free_all( &pptl__t );
                break;
            }
        } else if ( s->type == CC_STE_Movement ) {
        } else
            return false;

        s += 1;
    }


    if ( !result ) {
        cc_path_link_free_all( path__e_a );
        return false;
    } else
        return true;
}


bool cc_path_single_step( CcChessboard * cb,
                          CcPieceEnum piece,
                          CcPieceEnum activator,
                          CcPos pos,
                          CcPathLink ** path__e_a ) {
    if ( !cb ) return false;
    if ( !path__e_a ) return false;
    if ( *path__e_a ) return false;

    CcPathLink * path__t = NULL;

    if ( CC_PIECE_IS_PAWN( piece ) ) {
        if ( !cc_path_pawn( cb, piece, pos, &path__t ) ) return false;

        *path__e_a = path__t;
        path__t = NULL;
        return true;
    }


    return false;
}
