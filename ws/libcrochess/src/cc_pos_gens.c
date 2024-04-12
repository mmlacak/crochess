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


bool cc_pawn_all_steps( CcVariantEnum type,
                        CcPieceEnum activator,
                        CcPieceEnum piece,
                        CcPosLink ** restrict all_steps__od ) {
    if ( !all_steps__od ) return false;
    if ( *all_steps__od ) return false;

    CcPosLink * pl__t = NULL;

    if ( ( piece == CC_PE_LightPawn ) ||
         ( CC_PIECE_IS_WAVE( piece ) && ( activator == CC_PE_LightPawn ) ) ) {
        CcPos const steps[ 4 ] = {
            { .i =  0, .j =  1 },
            { .i = -1, .j =  1 },
            { .i =  1, .j =  1 },

            CC_POS_INVALID,
        };

        if ( !cc_convert_steps_to_pos_link( steps, 3, &pl__t ) ) {
            cc_pos_link_free_all( &pl__t );
            return false;
        }
    } else if ( piece == CC_PE_DarkPawn ||
                ( CC_PIECE_IS_WAVE( piece ) && ( activator == CC_PE_DarkPawn ) ) ) {
        CcPos const steps[ 4 ] = {
            { .i =  0, .j = -1 },
            { .i = -1, .j = -1 },
            { .i =  1, .j = -1 },

            CC_POS_INVALID,
        };

        if ( !cc_convert_steps_to_pos_link( steps, 3, &pl__t ) ) {
            cc_pos_link_free_all( &pl__t );
            return false;
        }
    }

    if ( cc_variant_has_sideways_pawns( type ) ) {
        CcPos const steps[ 3 ] = {
            { .i = -1, .j =  0 },
            { .i =  1, .j =  0 },

            CC_POS_INVALID,
        };

        if ( !cc_convert_steps_to_pos_link( steps, 2, &pl__t ) ) {
            cc_pos_link_free_all( &pl__t );
            return false;
        }
    }

    // Ownership transfer.
    *all_steps__od = pl__t;
    // pl__t = NULL; // Not needed.

    return true;
}
