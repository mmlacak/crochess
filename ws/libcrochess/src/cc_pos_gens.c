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


bool cc_pawn_steps( CcVariantEnum type,
                    CcPieceEnum activator,
                    CcPieceEnum piece,
                    CcStepTypeEnum steps_type,
                    CcPosLink ** restrict steps__od ) {
    if ( !steps__od ) return false;
    if ( *steps__od ) return false;

    if ( !( CC_PIECE_IS_PAWN( piece ) || CC_PIECE_IS_WAVE( piece ) ) )
        return false;

    if ( CC_PIECE_IS_WAVE( piece ) && ( !CC_PIECE_IS_PAWN( activator ) ) )
        return false;

    CcPos const * steps = NULL;
    size_t size = 0;
    CcPos const * capture_steps = NULL;
    size_t capture_size = 0;
    CcPosLink * pl__t = NULL;

    bool has_sideways_variant = cc_variant_has_sideways_pawns( type );
    bool is_light = ( ( piece == CC_PE_LightPawn ) ||
                      ( CC_PIECE_IS_WAVE( piece ) && ( activator == CC_PE_LightPawn ) ) );

    if ( CC_STEPS_HAS_MOVEMENT( steps_type ) ) {
        if ( has_sideways_variant ) {
            steps = is_light ? CC_STEPS_LIGHT_SIDEWAYS_PAWN : CC_STEPS_DARK_SIDEWAYS_PAWN;
            size = CC_STEPS_SIDEWAYS_PAWN_SIZE;
        } else {
            steps = is_light ? CC_STEPS_LIGHT_PAWN : CC_STEPS_DARK_PAWN;
            size = CC_STEPS_PAWN_SIZE;
        }
    }

    if ( !cc_convert_steps_to_pos_link( steps, size, &pl__t ) ) {
        cc_pos_link_free_all( &pl__t );
        return false;
    }

    if ( CC_STEPS_HAS_CAPTURE( steps_type ) ) {
        capture_steps = is_light ? CC_STEPS_CAPTURE_LIGHT_PAWN : CC_STEPS_CAPTURE_DARK_PAWN;
        capture_size = CC_STEPS_CAPTURE_PAWN_SIZE;
    }

    if ( !cc_convert_steps_to_pos_link( capture_steps, capture_size, &pl__t ) ) {
        cc_pos_link_free_all( &pl__t );
        return false;
    }

    // Ownership transfer.
    *steps__od = pl__t;
    // pl__t = NULL; // Not needed.

    return true;
}
