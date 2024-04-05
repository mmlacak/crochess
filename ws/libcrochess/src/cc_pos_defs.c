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

CcPos const CC_STEPS_MONOLITH_LEFT[ CC_STEPS_MONOLITH_SIZE ] = {
    { .i =  2, .j =  1 },
    { .i = -1, .j =  2 },
    { .i = -2, .j = -1 },
    { .i =  1, .j = -2 },

    CC_POS_INVALID,
};

CcPos const CC_STEPS_MONOLITH_RIGHT[ CC_STEPS_MONOLITH_SIZE ] = {
    { .i =  1, .j =  2 },
    { .i = -2, .j =  1 },
    { .i = -1, .j = -2 },
    { .i =  2, .j = -1 },

    CC_POS_INVALID,
};

bool cc_is_step_valid( CcPos step, CcPos const array[  ], size_t array_len ) {
    if ( !CC_POS_IS_VALID( step ) ) return false;

    for ( size_t k = 0; k < array_len; ++k ) {
        CcPos p = array[ k ];

        if ( !CC_POS_IS_VALID( p ) )
            return false;

        if ( cc_pos_is_equal( step, p ) )
            return true;
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

bool cc_is_pawn_capture_step( CcVariantEnum type, CcPieceEnum piece, CcPos step ) {
    if ( piece == CC_PE_LightPawn )
        return CC_LIGHT_PAWN_CAPTURE_STEP_IS_VALID( step );
    else if ( piece == CC_PE_DarkPawn )
        return CC_DARK_PAWN_CAPTURE_STEP_IS_VALID( step );
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
        return cc_is_pawn_capture_step( type, piece, step );
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

    // TODO :: check validity of steps of all other pieces.

    return true;
}


bool cc_is_step_miracle( CcPieceEnum piece, CcPos step ) {
    if ( CC_PIECE_IS_STARCHILD( piece ) )
        return CC_STARCHILD_MIRACLE_STEP_IS_VALID( step );

    return false;
}
