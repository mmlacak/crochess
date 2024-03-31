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

static bool cc_steps_gen_bail_out( CcPos * restrict previous_step__iod,
                                   CcPos * restrict last_step__iod,
                                   CcPosLink ** restrict previous_steps__iod_af,
                                   CcPosLink ** restrict possible_steps__iod_af ) {

    if ( previous_step__iod )
        *previous_step__iod = CC_POS_CAST_INVALID;

    if ( last_step__iod )
        *last_step__iod = CC_POS_CAST_INVALID;

    cc_pos_link_free_all( previous_steps__iod_af );

    cc_pos_link_free_all( possible_steps__iod_af );

    return false;
}

bool cc_steps_gen( CcVariantEnum type,
                   CcPieceEnum activator,
                   CcPieceEnum piece,
                   CcPos * restrict previous_step__iod,
                   CcPos * restrict last_step__iod,
                   CcPosLink ** restrict previous_steps__iod_af,
                   CcPosLink ** restrict possible_steps__iod_af ) {
    if ( !possible_steps__iod_af ) return false;
    if ( !last_step__iod ) return false;
    if ( !previous_steps__iod_af ) return false;

    bool bail_out = false;

    if ( CC_POS_IS_VALID( *last_step__iod ) ) {
        if ( *possible_steps__iod_af ) {
            if ( !cc_is_step_found( *last_step__iod, *possible_steps__iod_af ) )
                bail_out = true;
        } else
            bail_out = true;
    } else {
        if ( *possible_steps__iod_af )
            bail_out = true;
    }

    if ( ( !bail_out ) && CC_PIECE_IS_TWO_STEP( piece, activator ) ) {
        if ( !previous_step__iod ) return false;

        if ( CC_POS_IS_VALID( *previous_step__iod ) ) {
            if ( *previous_steps__iod_af ) {
                if ( !cc_is_step_found( *previous_step__iod, *previous_steps__iod_af ) )
                    bail_out = true;
            } else
                bail_out = true;
        } else {
            if ( *previous_steps__iod_af )
                bail_out = true;
        }
    }

    if ( bail_out ) {
        return cc_steps_gen_bail_out( previous_step__iod,
                                      last_step__iod,
                                      previous_steps__iod_af,
                                      possible_steps__iod_af );
    }

    CcPosLink * pl__t = NULL;

    if ( CC_PIECE_IS_TWO_STEP( piece, activator ) ) {
        if ( previous_step__iod ) {
            if ( !cc_pos_link_append( &pl__t, *previous_step__iod ) ) {
                return cc_steps_gen_bail_out( previous_step__iod,
                                              last_step__iod,
                                              previous_steps__iod_af,
                                              possible_steps__iod_af );
            }
        }
    }

    // TODO :: generate new steps into pl__t



    // Steps: last --> previous, NULL --> last
    *previous_step__iod = *last_step__iod;
    *last_step__iod = CC_POS_CAST_INVALID;

    cc_pos_link_free_all( previous_steps__iod_af );
    *previous_steps__iod_af = *possible_steps__iod_af;
    *possible_steps__iod_af = NULL;

    if ( pl__t ) {
        // Ownership transfer.
        *possible_steps__iod_af = pl__t;
        // pl__t = NULL; // Not needed.

        return true;
    } else
        return false;
}
