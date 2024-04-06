// Copyright (c) 2021, 2022, 2024 Mario Mlačak, mmlacak@gmail.com
// Licensed under GNU GPL v3+ license. See LICENSING, COPYING files for details.

#include "cc_pos_defs.h"

/**
    @file cc_pos_defs.c
    @brief Position, step definitions, checkers.
*/


CcPos const CC_STEPS_LIGHT_PAWN[ CC_STEPS_PAWN_SIZE ] = {
    { .i =  0, .j =  1 },

    CC_POS_INVALID,
};

CcPos const CC_STEPS_DARK_PAWN[ CC_STEPS_PAWN_SIZE ] = {
    { .i =  0, .j = -1 },

    CC_POS_INVALID,
};

CcPos const CC_STEPS_CAPTURE_LIGHT_PAWN[ CC_STEPS_CAPTURE_PAWN_SIZE ] = {
    { .i = -1, .j =  1 },
    { .i =  1, .j =  1 },

    CC_POS_INVALID,
};

CcPos const CC_STEPS_CAPTURE_DARK_PAWN[ CC_STEPS_CAPTURE_PAWN_SIZE ] = {
    { .i = -1, .j = -1 },
    { .i =  1, .j = -1 },

    CC_POS_INVALID,
};

CcPos const CC_STEPS_ALL_LIGHT_PAWN[ CC_STEPS_ALL_PAWN_SIZE ] = {
    // step
    { .i =  0, .j =  1 },

    // capture-steps
    { .i = -1, .j =  1 },
    { .i =  1, .j =  1 },

    CC_POS_INVALID,
};

CcPos const CC_STEPS_ALL_DARK_PAWN[ CC_STEPS_ALL_PAWN_SIZE ] = {
    // step
    { .i =  0, .j = -1 },

    // capture-steps
    { .i = -1, .j = -1 },
    { .i =  1, .j = -1 },

    CC_POS_INVALID,
};

CcPos const CC_STEPS_LIGHT_SIDEWAYS_PAWN[ CC_STEPS_SIDEWAYS_PAWN_SIZE ] = {
    { .i =  0, .j =  1 },
    { .i = -1, .j =  0 },
    { .i =  1, .j =  0 },

    CC_POS_INVALID,
};

CcPos const CC_STEPS_DARK_SIDEWAYS_PAWN[ CC_STEPS_SIDEWAYS_PAWN_SIZE ] = {
    { .i =  0, .j = -1 },
    { .i = -1, .j =  0 },
    { .i =  1, .j =  0 },

    CC_POS_INVALID,
};

CcPos const CC_STEPS_ALL_LIGHT_SIDEWAYS_PAWN[ CC_STEPS_ALL_SIDEWAYS_PAWN_SIZE ] = {
    // steps
    { .i =  0, .j =  1 },
    { .i = -1, .j =  0 },
    { .i =  1, .j =  0 },

    // capture-steps
    { .i = -1, .j =  1 },
    { .i =  1, .j =  1 },

    CC_POS_INVALID,
};

CcPos const CC_STEPS_ALL_DARK_SIDEWAYS_PAWN[ CC_STEPS_ALL_SIDEWAYS_PAWN_SIZE ] = {
    // steps
    { .i =  0, .j = -1 },
    { .i = -1, .j =  0 },
    { .i =  1, .j =  0 },

    // capture-steps
    { .i = -1, .j = -1 },
    { .i =  1, .j = -1 },

    CC_POS_INVALID,
};


CcPos const CC_STEPS_KNIGHT[ CC_STEPS_KNIGHT_SIZE ] = {
    { .i =  2, .j =  1 },
    { .i =  1, .j =  2 },

    { .i = -1, .j =  2 },
    { .i = -2, .j =  1 },

    { .i = -2, .j = -1 },
    { .i = -1, .j = -2 },

    { .i =  1, .j = -2 },
    { .i =  2, .j = -1 },

    CC_POS_INVALID,
};

CcPos const CC_STEPS_BISHOP[ CC_STEPS_BISHOP_SIZE ] = {
    { .i =  1, .j =  1 },
    { .i = -1, .j =  1 },
    { .i = -1, .j = -1 },
    { .i =  1, .j = -1 },

    CC_POS_INVALID,
};

CcPos const CC_STEPS_ROOK[ CC_STEPS_ROOK_SIZE ] = {
    { .i =  1, .j =  0 },
    { .i =  0, .j =  1 },
    { .i = -1, .j =  0 },
    { .i =  0, .j = -1 },

    CC_POS_INVALID,
};

CcPos const CC_STEPS_QUEEN[ CC_STEPS_QUEEN_SIZE ] = {
    { .i =  1, .j =  0 },
    { .i =  1, .j =  1 },
    { .i =  0, .j =  1 },
    { .i = -1, .j =  1 },
    { .i = -1, .j =  0 },
    { .i = -1, .j = -1 },
    { .i =  0, .j = -1 },
    { .i =  1, .j = -1 },

    CC_POS_INVALID,
};

CcPos const CC_STEPS_LONG_UNICORN[ CC_STEPS_LONG_UNICORN_SIZE ] = {
    { .i =  4, .j =  1 },
    { .i =  3, .j =  2 },
    { .i =  2, .j =  3 },
    { .i =  1, .j =  4 },

    { .i = -1, .j =  4 },
    { .i = -2, .j =  3 },
    { .i = -3, .j =  2 },
    { .i = -4, .j =  1 },

    { .i = -4, .j = -1 },
    { .i = -3, .j = -2 },
    { .i = -2, .j = -3 },
    { .i = -1, .j = -4 },

    { .i =  1, .j = -4 },
    { .i =  2, .j = -3 },
    { .i =  3, .j = -2 },
    { .i =  4, .j = -1 },

    CC_POS_INVALID,
};

CcPos const CC_STEPS_SERPENT_LEFT[ CC_STEPS_SERPENT_SIZE ] = {
    { .i = -1, .j =  1 },
    { .i =  1, .j = -1 },

    CC_POS_INVALID,
};

CcPos const CC_STEPS_SERPENT_RIGHT[ CC_STEPS_SERPENT_SIZE ] = {
    { .i = -1, .j = -1 },
    { .i =  1, .j =  1 },

    CC_POS_INVALID,
};

bool cc_is_step_valid( CcPos step, CcPos const steps[], size_t steps_size__d ) {
    if ( !CC_POS_IS_VALID( step ) ) return false;

    for ( size_t k = 0;
          (steps_size__d == CC_STEPS_SIZE_INVALID_POS_TERMINATED) || (k < steps_size__d);
          ++k ) {
        CcPos p = steps[ k ];

        if ( !CC_POS_IS_VALID( p ) ) return false; // break; // This is fine, as long as there is nothing after the loop.

        if ( cc_pos_is_equal( step, p ) ) return true;
    }

    return false;
}


bool cc_is_the_same_color( CcPieceEnum piece, CcPos pos ) {
    if ( cc_piece_is_light( piece ) && CC_IS_FIELD_LIGHT( pos.i, pos.j ) )
        return true;

    if ( cc_piece_is_dark( piece ) && CC_IS_FIELD_DARK( pos.i, pos.j ) )
        return true;

    return false;
}

bool cc_is_step_found( CcPos step, CcPosLink * restrict steps ) {
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

bool cc_convert_steps_to_pos_link( CcPos const steps[],
                                   size_t steps_size__d,
                                   CcPosLink ** restrict steps__iod_a ) {
    if ( !steps__iod_a ) return false;

    CcPosLink * pl__t = NULL;

    for ( size_t k = 0;
          (steps_size__d == CC_STEPS_SIZE_INVALID_POS_TERMINATED) || (k < steps_size__d);
          ++k ) {
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

CcPptLink * cc_join_ppt_links( CcPptLink ** restrict ppt_link__iod,
                               CcPptLink ** restrict ppt_link__n ) {
    if ( !ppt_link__iod ) return NULL;
    if ( !ppt_link__n ) return NULL;

    if ( !*ppt_link__n ) return *ppt_link__iod;

    if ( !*ppt_link__iod ) {
        // Ownership transfer.
        *ppt_link__iod = *ppt_link__n;
        *ppt_link__n = NULL;

        return *ppt_link__iod;
    }

    CcPptLink * last = *ppt_link__iod;
    CC_FASTFORWARD( last );

    CcPptLink * first = *ppt_link__n;

    if ( cc_pos_is_equal( last->ppt.pos, first->ppt.pos ) ) {
        if ( last->ppt.piece != first->ppt.piece )
            return NULL;

        if ( last->ppt.tag != first->ppt.tag )
            return NULL;

        // Position, piece, and tag are all the same,
        // so we drop extra location, not needed anymore.
        CcPptLink * to_free = first;
        first = first->next;

        CC_FREE( to_free ); // This is fine, as long as CcPptLink doesn't have pointer(s) to other structs/unions that need to be free()-ed.
    }

    // Ownership transfer.
    last->next = first;
    *ppt_link__n = NULL;

    return last->next;
}


bool cc_is_pawn_step( CcVariantEnum type, CcPieceEnum piece, CcPos step ) {
    if ( cc_variant_has_sideways_pawns( type ) ) {
        if ( piece == CC_PE_LightPawn )
            return CC_LIGHT_SIDEWAYS_PAWN_STEP_IS_VALID( step );
        else if ( piece == CC_PE_DarkPawn )
            return CC_DARK_SIDEWAYS_PAWN_STEP_IS_VALID( step );
        else
            return false;
    } else {
        if ( piece == CC_PE_LightPawn )
            return CC_LIGHT_PAWN_STEP_IS_VALID( step );
        else if ( piece == CC_PE_DarkPawn )
            return CC_DARK_PAWN_STEP_IS_VALID( step );
        else
            return false;
    }
}

bool cc_is_pawn_capture_step( CcPieceEnum piece, CcPos step ) {
    if ( piece == CC_PE_LightPawn )
        return CC_LIGHT_PAWN_CAPTURE_STEP_IS_VALID( step );
    else if ( piece == CC_PE_DarkPawn )
        return CC_DARK_PAWN_CAPTURE_STEP_IS_VALID( step );
    else
        return false;
}

bool cc_is_scout_capture_step( CcPieceEnum piece, CcPos step ) {
    if ( piece == CC_PE_LightScout )
        return CC_LIGHT_SCOUT_CAPTURE_STEP_IS_VALID( step );
    else if ( piece == CC_PE_DarkScout )
        return CC_DARK_SCOUT_CAPTURE_STEP_IS_VALID( step );
    else
        return false;
}

bool cc_is_shaman_capture_step( CcPieceEnum piece, CcPos step ) {
    if ( piece == CC_PE_LightShaman ) {
        return CC_LIGHT_SHAMAN_CAPTURE_STEP_IS_VALID( step );
    } else if ( piece == CC_PE_DarkShaman ) {
        return CC_DARK_SHAMAN_CAPTURE_STEP_IS_VALID( step );
    } else
        return false;
}

bool cc_is_capture_step( CcVariantEnum type,
                         CcPieceEnum activator,
                         CcPieceEnum piece,
                         CcPos step,
                         CcPos step_2 ) {
    if ( !CC_PIECE_IS_VALID( piece ) ) return false;

    if ( !cc_pos_is_valid( step ) ) return false;
    if ( cc_pos_is_static_step( step ) ) return false;

    if ( CC_PIECE_IS_PAWN( piece ) )
        return cc_is_pawn_capture_step( piece, step );
    else if ( CC_PIECE_IS_SHAMAN( piece ) )
        return cc_is_shaman_capture_step( piece, step );
    else if ( CC_PIECE_IS_WAVE( piece ) ) {
        if ( !CC_PIECE_CAN_ACTIVATE( activator ) )
            return false;

        return cc_is_capture_step( type, CC_PE_None, activator, step, step_2 );
    } else if ( CC_PIECE_IS_MONOLITH( piece ) )
        return false; // Monolith can't capture.
    else if ( CC_PIECE_IS_STAR( piece ) )
        return false; // Star can't capture.
    else if ( CC_PIECE_IS_STARCHILD( piece ) )
        return false; // Starchild can't capture.

    else if ( CC_PIECE_IS_KING( piece ) )
        return CC_KING_STEP_IS_VALID( step );
    else if ( CC_PIECE_IS_KNIGHT( piece ) )
        return CC_KNIGHT_STEP_IS_VALID( step );
    else if ( CC_PIECE_IS_BISHOP( piece ) )
        return CC_BISHOP_STEP_IS_VALID( step );
    else if ( CC_PIECE_IS_QUEEN( piece ) )
        return CC_QUEEN_STEP_IS_VALID( step );

    else if ( CC_PIECE_IS_PYRAMID( piece ) )
        return CC_PYRAMID_STEP_IS_VALID( step );
    else if ( CC_PIECE_IS_UNICORN( piece ) ) {
        return ( ( CC_UNICORN_SHORT_STEP_IS_VALID( step )
                   && CC_UNICORN_LONG_STEP_IS_VALID( step_2 ) )
               || ( CC_UNICORN_LONG_STEP_IS_VALID( step )
                    && CC_UNICORN_SHORT_STEP_IS_VALID( step_2 ) ) );
    } else if ( CC_PIECE_IS_STAR( piece ) ) {
        if ( !CC_PIECE_CAN_ACTIVATE_STAR( activator ) )
            return false;

        return CC_STAR_STEP_IS_VALID( step );
    } else if ( CC_PIECE_IS_CENTAUR( piece ) ) {
        return ( ( CC_CENTAUR_SHORT_STEP_IS_VALID( step )
                   && CC_CENTAUR_LONG_STEP_IS_VALID( step_2 ) )
               || ( CC_CENTAUR_LONG_STEP_IS_VALID( step )
                    && CC_CENTAUR_SHORT_STEP_IS_VALID( step_2 ) ) );
    } else if ( CC_PIECE_IS_SCOUT( piece ) )
        return cc_is_scout_capture_step( piece, step );
    else if ( CC_PIECE_IS_GRENADIER( piece ) ) {
        return CC_GRENADIER_CAPTURE_STEP_IS_VALID( step );
    } else if ( CC_PIECE_IS_SERPENT( piece ) )
        return CC_SERPENT_STEP_IS_VALID( step );

    return false;
}


bool cc_is_step_miracle( CcPieceEnum piece, CcPos step ) {
    if ( CC_PIECE_IS_STARCHILD( piece ) )
        return CC_STARCHILD_MIRACLE_STEP_IS_VALID( step );

    return false;
}
