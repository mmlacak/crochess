// Copyright (c) 2021, 2022 Mario Mlačak, mmlacak@gmail.com
// Licensed under GNU GPL v3+ license. See LICENSING, COPYING files for details.

#include "cc_path_defs.h"

/**
    @file cc_path_defs.c
    @brief Path checkers.
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

bool cc_is_step_valid( CcPos step,
                       CcPos const array[  ],
                       size_t array_len ) {
    for ( int k = 0; (size_t)k < array_len; ++k ) {
        CcPos p = array[ k ];

        if ( cc_pos_is_equal( step, p ) )
            return true;
    }

    return false;
}


bool cc_is_pawn_step( CcVariantEnum type, CcPieceEnum piece, CcPos step ) {
    if ( cc_variant_has_sideways_pawns( type ) ) {
        if ( cc_piece_is_light( piece ) )
            return CC_LIGHT_SIDEWAYS_PAWN_STEP_IS_VALID( step );
        else
            return CC_DARK_SIDEWAYS_PAWN_STEP_IS_VALID( step ); }
    else {
        if ( cc_piece_is_light( piece ) )
            return CC_LIGHT_PAWN_STEP_IS_VALID( step );
        else
            return CC_DARK_PAWN_STEP_IS_VALID( step );
    }
}

bool cc_is_pawn_capture_step( CcVariantEnum type, CcPieceEnum piece, CcPos step ) {
    if ( cc_piece_is_light( piece ) )
        return CC_LIGHT_PAWN_CAPTURE_STEP_IS_VALID( step );
    else
        return CC_DARK_PAWN_CAPTURE_STEP_IS_VALID( step );
}
