// Copyright (c) 2024 Mario Mlačak, mmlacak@gmail.com
// Licensed under GNU GPL v3+ license. See LICENSING, COPYING files for details.

#include "cc_defines.h"
#include "cc_math.h"
#include "cc_pos_gens.h"

/**
    @file cc_pos_gens.c
    @brief Position generators.
*/


static bool cc_is_step_found( CcPos step, CcPosLink * restrict steps ) {
    if ( !steps ) return false;

    if ( !CC_POS_IS_VALID( step ) ) return false;

    CcPosLink * pl = steps;

    while ( pl ) {
        if ( CC_POS_IS_EQUAL( pl->pos, step ) )
            return true;

        pl = pl->next;
    }

    return false;
}

bool cc_steps_gen( CcVariantEnum type,
                   CcPieceEnum activator,
                   CcPieceEnum piece,
                   CcPos * restrict previous_step__iod,
                   CcPos * restrict last_step__iod,
                   CcPosLink ** restrict previous_steps__iod_a_F,
                   CcPosLink ** restrict possible_steps__iod_a_F ) {
    if ( !possible_steps__iod_a_F ) return false;
    if ( !last_step__iod ) return false;
    if ( !previous_steps__iod_a_F ) return false;

    bool bail_out = false;

    if ( CC_POS_IS_VALID( *last_step__iod ) ) {
        if ( !*possible_steps__iod_a_F )
            bail_out = true;
        else
            if ( !cc_is_step_found( *last_step__iod, *possible_steps__iod_a_F ) )
                bail_out = true;
    } else {
        if ( *possible_steps__iod_a_F )
            bail_out = true;
    }

    if ( ( !bail_out ) &&
         ( CC_PIECE_IS_CENTAUR( piece ) ||
            ( CC_PIECE_IS_WAVE( piece ) &&
                ( CC_PIECE_IS_UNICORN( activator ) || CC_PIECE_IS_CENTAUR( activator ) ) ) ) ) {
        if ( !previous_step__iod ) return false;

        if ( CC_POS_IS_VALID( *previous_step__iod ) ) {
            if ( !*previous_steps__iod_a_F )
                bail_out = true;
            else {
                if ( !cc_is_step_found( *previous_step__iod, *previous_steps__iod_a_F ) )
                    bail_out = true;
            }
        } else {
            if ( *previous_steps__iod_a_F )
                bail_out = true;
        }
    }

    if ( bail_out ) {
        *previous_step__iod = *last_step__iod = CC_POS_CAST_INVALID;

        cc_pos_link_free_all( previous_steps__iod_a_F );
        cc_pos_link_free_all( possible_steps__iod_a_F );

        return false;
    }

    CcPosLink * pl__t = NULL;

    // TODO :: generate new steps into pl__t



    // Steps: last --> previous, NULL --> last
    *previous_step__iod = *last_step__iod;
    *last_step__iod = CC_POS_CAST_INVALID;

    cc_pos_link_free_all( previous_steps__iod_a_F );
    *previous_steps__iod_a_F = *possible_steps__iod_a_F;
    *possible_steps__iod_a_F = NULL;

    if ( pl__t ) {
        // Ownership transfer.
        *possible_steps__iod_a_F = pl__t;
        // pl__t = NULL; // Not needed.

        return true;
    } else
        return false;
}
