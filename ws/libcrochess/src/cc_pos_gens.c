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

static bool cc_steps_gen_bail_out( CcPos * restrict previous_step__d,
                                   CcPos * restrict last_step__d,
                                   CcPosLink ** restrict previous_steps__f,
                                   CcPosLink ** restrict possible_steps__f,
                                   CcPosLink ** restrict temp_steps__f ) {

    if ( previous_step__d )
        *previous_step__d = CC_POS_CAST_INVALID;

    if ( last_step__d )
        *last_step__d = CC_POS_CAST_INVALID;

    cc_pos_link_free_all( previous_steps__f );

    cc_pos_link_free_all( possible_steps__f );

    cc_pos_link_free_all( temp_steps__f );

    return false;
}

static bool cc_convert_steps_to_pos_link( CcPos const steps[],
                                          size_t steps_len,
                                          CcPosLink ** restrict steps__iod_a ) {
    if ( !steps__iod_a ) return false;

    CcPosLink * pl__t = NULL;

    for ( size_t k = 0; k < steps_len; ++k ) {
        CcPos p = steps[ k ];

        if ( !CC_POS_IS_VALID( p ) ) break;

        if ( !cc_pos_link_append( &pl__t, p ) ) {
            cc_pos_link_free_all( &pl__t );
            return false;
        }
    }

    // Ownership transfer.
    if ( !cc_pos_link_extend( steps__iod_a, &pl__t ) ) {
        cc_pos_link_free_all( &pl__t );
        cc_pos_link_free_all( steps__iod_a );
        return false;
    }

    return true;
}

static bool cc_pawn_steps( CcVariantEnum type,
                           CcPieceEnum activator,
                           CcPieceEnum piece,
                           CcPosLink ** restrict steps__od ) {
    if ( !steps__od ) return false;
    if ( *steps__od ) return false;

    CcPosLink * pl__t = NULL;
    bool bail_out = false;

    if ( ( piece == CC_PE_LightPawn ) ||
         ( CC_PIECE_IS_WAVE( piece ) && ( activator == CC_PE_LightPawn ) ) ) {
        CcPos const steps[ 4 ] = {
            { .i =  0, .j =  1 },
            { .i = -1, .j =  1 },
            { .i =  1, .j =  1 },

            CC_POS_INVALID,
        };

        if ( !cc_convert_steps_to_pos_link( steps, 3, &pl__t ) )
            bail_out = true;
    } else if ( piece == CC_PE_DarkPawn ||
                ( CC_PIECE_IS_WAVE( piece ) && ( activator == CC_PE_DarkPawn ) ) ) {
        CcPos const steps[ 4 ] = {
            { .i =  0, .j = -1 },
            { .i = -1, .j = -1 },
            { .i =  1, .j = -1 },

            CC_POS_INVALID,
        };

        if ( !cc_convert_steps_to_pos_link( steps, 3, &pl__t ) )
            bail_out = true;
    }

    if ( ( !bail_out ) && cc_variant_has_sideways_pawns( type ) ) {
        CcPos const steps[ 3 ] = {
            { .i = -1, .j =  0 },
            { .i =  1, .j =  0 },

            CC_POS_INVALID,
        };

        if ( !cc_convert_steps_to_pos_link( steps, 2, &pl__t ) )
            bail_out = true;
    }

    if ( bail_out ) {
        cc_pos_link_free_all( &pl__t );
        return false;
    }

    // Ownership transfer.
    *steps__od = pl__t;
    // pl__t = NULL; // Not needed.

    return true;
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
                                      possible_steps__iod_af,
                                      NULL );
    }

    CcPosLink * pl__t = NULL;

    if ( CC_PIECE_IS_TWO_STEP( piece, activator ) ) {
        if ( previous_step__iod && CC_POS_IS_VALID( *previous_step__iod ) ) {
            if ( !cc_pos_link_append( &pl__t, *previous_step__iod ) ) {
                return cc_steps_gen_bail_out( previous_step__iod,
                                              last_step__iod,
                                              previous_steps__iod_af,
                                              possible_steps__iod_af,
                                              &pl__t );
            }
        }
    } else if ( !CC_PIECE_HAS_NEW_STEP_AFTER_EACH( piece ) ) {
        if ( last_step__iod && CC_POS_IS_VALID( *last_step__iod ) ) {
            if ( !cc_pos_link_append( &pl__t, *last_step__iod ) ) {
                return cc_steps_gen_bail_out( previous_step__iod,
                                              last_step__iod,
                                              previous_steps__iod_af,
                                              possible_steps__iod_af,
                                              &pl__t );
            }
        }
    }

    if ( !pl__t ) {
        if ( CC_PIECE_IS_PAWN( piece ) ||
           ( CC_PIECE_IS_WAVE( piece ) && CC_PIECE_IS_PAWN( activator ) ) ) {
            if ( !cc_pawn_steps( type, activator, piece, &pl__t ) ) {
                return cc_steps_gen_bail_out( previous_step__iod,
                                              last_step__iod,
                                              previous_steps__iod_af,
                                              possible_steps__iod_af,
                                              &pl__t );
            }
        }

        // TODO :: generate new steps into pl__t
    }

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
