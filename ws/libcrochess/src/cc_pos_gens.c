// Copyright (c) 2024 Mario Mlačak, mmlacak@gmail.com
// Licensed under GNU GPL v3+ license. See LICENSING, COPYING files for details.

#include "cc_defines.h"
#include "cc_math.h"
#include "cc_pos_defs.h"
#include "cc_pos_gens.h"

/**
    @file cc_pos_gens.c
    @brief Position generators.
*/


bool cc_pawn_steps( CcChessboard * cb,
                    CcPieceEnum activator,
                    CcPieceEnum piece,
                    CcPos current_pos,
                    CcStepTypeEnum steps_type,
                    CcPosLink ** steps__od ) {
    if ( !steps__od ) return false;
    if ( *steps__od ) return false;

    if ( !( CC_PIECE_IS_PAWN( piece ) || CC_PIECE_IS_WAVE( piece ) ) )
        return false;

    if ( CC_PIECE_IS_WAVE( piece ) && ( !CC_PIECE_IS_PAWN( activator ) ) )
        return false;

    int i = current_pos.i;
    int j = current_pos.j;

    CcPieceEnum pe = cc_chessboard_get_piece( cb, i, j );
    if ( piece != pe ) return false;

    CcPosLink * pl__t = NULL;

    if ( CC_STEPS_HAS_MOVEMENT( steps_type ) ) {
    }

    // TODO :: DELETE !!
    //
    // CcPos const * steps = NULL;
    // size_t size = 0;
    // CcPos const * capture_steps = NULL;
    // size_t capture_size = 0;

    // bool has_sideways_variant = cc_variant_has_sideways_pawns( cb->type );
    // bool is_light = ( ( piece == CC_PE_LightPawn ) ||
    //                   ( CC_PIECE_IS_WAVE( piece ) && ( activator == CC_PE_LightPawn ) ) );

    // if ( CC_STEPS_HAS_MOVEMENT( steps_type ) ) {
    //     if ( has_sideways_variant ) {
    //         steps = is_light ? CC_STEPS_LIGHT_SIDEWAYS_PAWN : CC_STEPS_DARK_SIDEWAYS_PAWN;
    //         size = CC_STEPS_SIDEWAYS_PAWN_SIZE;
    //     } else {
    //         steps = is_light ? CC_STEPS_LIGHT_PAWN : CC_STEPS_DARK_PAWN;
    //         size = CC_STEPS_PAWN_SIZE;
    //     }
    // }

    // if ( !cc_convert_steps_to_pos_link( steps, size, &pl__t ) ) {
    //     cc_pos_link_free_all( &pl__t );
    //     return false;
    // }

    // if ( CC_STEPS_HAS_CAPTURE( steps_type ) ) {
    //     capture_steps = is_light ? CC_STEPS_CAPTURE_LIGHT_PAWN : CC_STEPS_CAPTURE_DARK_PAWN;
    //     capture_size = CC_STEPS_CAPTURE_PAWN_SIZE;
    // }

    // if ( !cc_convert_steps_to_pos_link( capture_steps, capture_size, &pl__t ) ) {
    //     cc_pos_link_free_all( &pl__t );
    //     return false;
    // }
    //
    // TODO :: DELETE !!

    // Ownership transfer.
    *steps__od = pl__t;
    // pl__t = NULL; // Not needed.

    return true;
}
