// Copyright (c) 2021, 2022, 2024 Mario Mlačak, mmlacak@gmail.com
// Licensed under GNU GPL v3+ license. See LICENSING, COPYING files for details.

#include "cc_pos_defs.h"


CcTypedStep const CC_STEPS_LIGHT_PAWN[ CC_STEPS_PAWN_SIZE ] = {
    // step
    { .step = { .i =  0, .j =  1 }, .type = CC_STE_MovementOnly },

    // capture-steps
    { .step = { .i = -1, .j =  1 }, .type = CC_STE_CaptureOnly },
    { .step = { .i =  1, .j =  1 }, .type = CC_STE_CaptureOnly },

    CC_TYPED_STEP_INVALID,
};

CcTypedStep const CC_STEPS_DARK_PAWN[ CC_STEPS_PAWN_SIZE ] = {
    // step
    { .step = { .i =  0, .j = -1 }, .type = CC_STE_MovementOnly },

    // capture-steps
    { .step = { .i = -1, .j = -1 }, .type = CC_STE_CaptureOnly },
    { .step = { .i =  1, .j = -1 }, .type = CC_STE_CaptureOnly },

    CC_TYPED_STEP_INVALID,
};

CcTypedStep const CC_STEPS_LIGHT_SIDEWAYS_PAWN[ CC_STEPS_SIDEWAYS_PAWN_SIZE ] = {
    // steps
    { .step = { .i =  0, .j =  1 }, .type = CC_STE_MovementOnly },
    { .step = { .i = -1, .j =  0 }, .type = CC_STE_MovementOnly },
    { .step = { .i =  1, .j =  0 }, .type = CC_STE_MovementOnly },

    // capture-steps
    { .step = { .i = -1, .j =  1 }, .type = CC_STE_CaptureOnly },
    { .step = { .i =  1, .j =  1 }, .type = CC_STE_CaptureOnly },

    CC_TYPED_STEP_INVALID,
};

CcTypedStep const CC_STEPS_DARK_SIDEWAYS_PAWN[ CC_STEPS_SIDEWAYS_PAWN_SIZE ] = {
    // steps
    { .step = { .i =  0, .j = -1 }, .type = CC_STE_MovementOnly },
    { .step = { .i = -1, .j =  0 }, .type = CC_STE_MovementOnly },
    { .step = { .i =  1, .j =  0 }, .type = CC_STE_MovementOnly },

    // capture-steps
    { .step = { .i = -1, .j = -1 }, .type = CC_STE_CaptureOnly },
    { .step = { .i =  1, .j = -1 }, .type = CC_STE_CaptureOnly },

    CC_TYPED_STEP_INVALID,
};


CcTypedStep const CC_STEPS_KNIGHT[ CC_STEPS_KNIGHT_SIZE ] = {
    { .step = { .i =  2, .j =  1 }, .type = CC_STE_CaptureOrMovement },
    { .step = { .i =  1, .j =  2 }, .type = CC_STE_CaptureOrMovement },

    { .step = { .i = -1, .j =  2 }, .type = CC_STE_CaptureOrMovement },
    { .step = { .i = -2, .j =  1 }, .type = CC_STE_CaptureOrMovement },

    { .step = { .i = -2, .j = -1 }, .type = CC_STE_CaptureOrMovement },
    { .step = { .i = -1, .j = -2 }, .type = CC_STE_CaptureOrMovement },

    { .step = { .i =  1, .j = -2 }, .type = CC_STE_CaptureOrMovement },
    { .step = { .i =  2, .j = -1 }, .type = CC_STE_CaptureOrMovement },

    CC_TYPED_STEP_INVALID,
};

CcTypedStep const CC_STEPS_BISHOP[ CC_STEPS_BISHOP_SIZE ] = {
    { .step = { .i =  1, .j =  1 }, .type = CC_STE_CaptureOrMovement },
    { .step = { .i = -1, .j =  1 }, .type = CC_STE_CaptureOrMovement },
    { .step = { .i = -1, .j = -1 }, .type = CC_STE_CaptureOrMovement },
    { .step = { .i =  1, .j = -1 }, .type = CC_STE_CaptureOrMovement },

    CC_TYPED_STEP_INVALID,
};

CcTypedStep const CC_STEPS_ROOK[ CC_STEPS_ROOK_SIZE ] = {
    { .step = { .i =  1, .j =  0 }, .type = CC_STE_CaptureOrMovement },
    { .step = { .i =  0, .j =  1 }, .type = CC_STE_CaptureOrMovement },
    { .step = { .i = -1, .j =  0 }, .type = CC_STE_CaptureOrMovement },
    { .step = { .i =  0, .j = -1 }, .type = CC_STE_CaptureOrMovement },

    CC_TYPED_STEP_INVALID,
};

CcTypedStep const CC_STEPS_QUEEN[ CC_STEPS_QUEEN_SIZE ] = {
    { .step = { .i =  1, .j =  0 }, .type = CC_STE_CaptureOrMovement },
    { .step = { .i =  1, .j =  1 }, .type = CC_STE_CaptureOrMovement },
    { .step = { .i =  0, .j =  1 }, .type = CC_STE_CaptureOrMovement },
    { .step = { .i = -1, .j =  1 }, .type = CC_STE_CaptureOrMovement },
    { .step = { .i = -1, .j =  0 }, .type = CC_STE_CaptureOrMovement },
    { .step = { .i = -1, .j = -1 }, .type = CC_STE_CaptureOrMovement },
    { .step = { .i =  0, .j = -1 }, .type = CC_STE_CaptureOrMovement },
    { .step = { .i =  1, .j = -1 }, .type = CC_STE_CaptureOrMovement },

    CC_TYPED_STEP_INVALID,
};

CcTypedStep const CC_STEPS_LONG_UNICORN[ CC_STEPS_LONG_UNICORN_SIZE ] = {
    { .step = { .i =  4, .j =  1 }, .type = CC_STE_CaptureOrMovement },
    { .step = { .i =  3, .j =  2 }, .type = CC_STE_CaptureOrMovement },
    { .step = { .i =  2, .j =  3 }, .type = CC_STE_CaptureOrMovement },
    { .step = { .i =  1, .j =  4 }, .type = CC_STE_CaptureOrMovement },

    { .step = { .i = -1, .j =  4 }, .type = CC_STE_CaptureOrMovement },
    { .step = { .i = -2, .j =  3 }, .type = CC_STE_CaptureOrMovement },
    { .step = { .i = -3, .j =  2 }, .type = CC_STE_CaptureOrMovement },
    { .step = { .i = -4, .j =  1 }, .type = CC_STE_CaptureOrMovement },

    { .step = { .i = -4, .j = -1 }, .type = CC_STE_CaptureOrMovement },
    { .step = { .i = -3, .j = -2 }, .type = CC_STE_CaptureOrMovement },
    { .step = { .i = -2, .j = -3 }, .type = CC_STE_CaptureOrMovement },
    { .step = { .i = -1, .j = -4 }, .type = CC_STE_CaptureOrMovement },

    { .step = { .i =  1, .j = -4 }, .type = CC_STE_CaptureOrMovement },
    { .step = { .i =  2, .j = -3 }, .type = CC_STE_CaptureOrMovement },
    { .step = { .i =  3, .j = -2 }, .type = CC_STE_CaptureOrMovement },
    { .step = { .i =  4, .j = -1 }, .type = CC_STE_CaptureOrMovement },

    CC_TYPED_STEP_INVALID,
};

CcTypedStep const CC_STEPS_SERPENT_LEFT[ CC_STEPS_SERPENT_SIZE ] = {
    { .step = { .i = -1, .j =  1 }, .type = CC_STE_CaptureOrMovement },
    { .step = { .i =  1, .j = -1 }, .type = CC_STE_CaptureOrMovement },

    CC_TYPED_STEP_INVALID,
};

CcTypedStep const CC_STEPS_SERPENT_RIGHT[ CC_STEPS_SERPENT_SIZE ] = {
    { .step = { .i = -1, .j = -1 }, .type = CC_STE_CaptureOrMovement },
    { .step = { .i =  1, .j =  1 }, .type = CC_STE_CaptureOrMovement },

    CC_TYPED_STEP_INVALID,
};

CcTypedStep const CC_STEPS_LIGHT_SHAMAN[ CC_STEPS_LIGHT_SHAMAN_SIZE ] = {
    //
    // Knight steps

    { .step = { .i =  2, .j =  1 }, .type = CC_STE_MovementOnly },
    { .step = { .i =  1, .j =  2 }, .type = CC_STE_MovementOnly },

    { .step = { .i = -1, .j =  2 }, .type = CC_STE_MovementOnly },
    { .step = { .i = -2, .j =  1 }, .type = CC_STE_MovementOnly },

    { .step = { .i = -2, .j = -1 }, .type = CC_STE_MovementOnly },
    { .step = { .i = -1, .j = -2 }, .type = CC_STE_MovementOnly },

    { .step = { .i =  1, .j = -2 }, .type = CC_STE_MovementOnly },
    { .step = { .i =  2, .j = -1 }, .type = CC_STE_MovementOnly },

    //
    // long Unicorn capture-steps

    { .step = { .i =  4, .j =  1 }, .type = CC_STE_CaptureOnly },
    { .step = { .i =  3, .j =  2 }, .type = CC_STE_CaptureOnly },
    { .step = { .i =  2, .j =  3 }, .type = CC_STE_CaptureOnly },
    { .step = { .i =  1, .j =  4 }, .type = CC_STE_CaptureOnly },

    { .step = { .i = -1, .j =  4 }, .type = CC_STE_CaptureOnly },
    { .step = { .i = -2, .j =  3 }, .type = CC_STE_CaptureOnly },
    { .step = { .i = -3, .j =  2 }, .type = CC_STE_CaptureOnly },
    { .step = { .i = -4, .j =  1 }, .type = CC_STE_CaptureOnly },

    { .step = { .i = -4, .j = -1 }, .type = CC_STE_CaptureOnly },
    { .step = { .i = -3, .j = -2 }, .type = CC_STE_CaptureOnly },
    { .step = { .i = -2, .j = -3 }, .type = CC_STE_CaptureOnly },
    { .step = { .i = -1, .j = -4 }, .type = CC_STE_CaptureOnly },

    { .step = { .i =  1, .j = -4 }, .type = CC_STE_CaptureOnly },
    { .step = { .i =  2, .j = -3 }, .type = CC_STE_CaptureOnly },
    { .step = { .i =  3, .j = -2 }, .type = CC_STE_CaptureOnly },
    { .step = { .i =  4, .j = -1 }, .type = CC_STE_CaptureOnly },

    CC_TYPED_STEP_INVALID,
};

CcTypedStep const CC_STEPS_DARK_SHAMAN[ CC_STEPS_DARK_SHAMAN_SIZE ] = {
    //
    // Knight capture-steps

    { .step = { .i =  2, .j =  1 }, .type = CC_STE_CaptureOnly },
    { .step = { .i =  1, .j =  2 }, .type = CC_STE_CaptureOnly },

    { .step = { .i = -1, .j =  2 }, .type = CC_STE_CaptureOnly },
    { .step = { .i = -2, .j =  1 }, .type = CC_STE_CaptureOnly },

    { .step = { .i = -2, .j = -1 }, .type = CC_STE_CaptureOnly },
    { .step = { .i = -1, .j = -2 }, .type = CC_STE_CaptureOnly },

    { .step = { .i =  1, .j = -2 }, .type = CC_STE_CaptureOnly },
    { .step = { .i =  2, .j = -1 }, .type = CC_STE_CaptureOnly },

    //
    // long Unicorn steps

    { .step = { .i =  4, .j =  1 }, .type = CC_STE_MovementOnly },
    { .step = { .i =  3, .j =  2 }, .type = CC_STE_MovementOnly },
    { .step = { .i =  2, .j =  3 }, .type = CC_STE_MovementOnly },
    { .step = { .i =  1, .j =  4 }, .type = CC_STE_MovementOnly },

    { .step = { .i = -1, .j =  4 }, .type = CC_STE_MovementOnly },
    { .step = { .i = -2, .j =  3 }, .type = CC_STE_MovementOnly },
    { .step = { .i = -3, .j =  2 }, .type = CC_STE_MovementOnly },
    { .step = { .i = -4, .j =  1 }, .type = CC_STE_MovementOnly },

    { .step = { .i = -4, .j = -1 }, .type = CC_STE_MovementOnly },
    { .step = { .i = -3, .j = -2 }, .type = CC_STE_MovementOnly },
    { .step = { .i = -2, .j = -3 }, .type = CC_STE_MovementOnly },
    { .step = { .i = -1, .j = -4 }, .type = CC_STE_MovementOnly },

    { .step = { .i =  1, .j = -4 }, .type = CC_STE_MovementOnly },
    { .step = { .i =  2, .j = -3 }, .type = CC_STE_MovementOnly },
    { .step = { .i =  3, .j = -2 }, .type = CC_STE_MovementOnly },
    { .step = { .i =  4, .j = -1 }, .type = CC_STE_MovementOnly },

    CC_TYPED_STEP_INVALID,
};

CcTypedStep const CC_STEPS_LIGHT_SCOUT[ CC_STEPS_SCOUT_SIZE ] = {
    // light Pawn steps
    { .step = { .i =  0, .j =  1 }, .type = CC_STE_MovementOnly },
    { .step = { .i = -1, .j =  0 }, .type = CC_STE_MovementOnly },
    { .step = { .i =  1, .j =  0 }, .type = CC_STE_MovementOnly },

    // dark Pawn capture-steps
    { .step = { .i = -1, .j = -1 }, .type = CC_STE_CaptureOnly },
    { .step = { .i =  1, .j = -1 }, .type = CC_STE_CaptureOnly },

    CC_TYPED_STEP_INVALID,
};

CcTypedStep const CC_STEPS_DARK_SCOUT[ CC_STEPS_SCOUT_SIZE ] = {
    // dark Pawn steps
    { .step = { .i =  0, .j = -1 }, .type = CC_STE_MovementOnly },
    { .step = { .i = -1, .j =  0 }, .type = CC_STE_MovementOnly },
    { .step = { .i =  1, .j =  0 }, .type = CC_STE_MovementOnly },

    // light Pawn capture-steps
    { .step = { .i = -1, .j =  1 }, .type = CC_STE_CaptureOnly },
    { .step = { .i =  1, .j =  1 }, .type = CC_STE_CaptureOnly },

    CC_TYPED_STEP_INVALID,
};

CcTypedStep const CC_STEPS_GRENADIER[ CC_STEPS_GRENADIER_SIZE ] = {
    // Rook steps
    { .step = { .i =  1, .j =  0 }, .type = CC_STE_MovementOnly },
    { .step = { .i =  0, .j =  1 }, .type = CC_STE_MovementOnly },
    { .step = { .i = -1, .j =  0 }, .type = CC_STE_MovementOnly },
    { .step = { .i =  0, .j = -1 }, .type = CC_STE_MovementOnly },

    // Bishop capture-steps
    { .step = { .i =  1, .j =  1 }, .type = CC_STE_CaptureOnly },
    { .step = { .i = -1, .j =  1 }, .type = CC_STE_CaptureOnly },
    { .step = { .i = -1, .j = -1 }, .type = CC_STE_CaptureOnly },
    { .step = { .i =  1, .j = -1 }, .type = CC_STE_CaptureOnly },

    CC_TYPED_STEP_INVALID,
};

CcTypedStep const CC_STEPS_MIRACLE_STARCHILD[ CC_STEPS_MIRACLE_STARCHILD_SIZE ] = {
    { .step = { .i =  1, .j =  0 }, .type = CC_STE_Alternative },
    { .step = { .i =  1, .j =  1 }, .type = CC_STE_Alternative },
    { .step = { .i =  0, .j =  1 }, .type = CC_STE_Alternative },
    { .step = { .i = -1, .j =  1 }, .type = CC_STE_Alternative },
    { .step = { .i = -1, .j =  0 }, .type = CC_STE_Alternative },
    { .step = { .i = -1, .j = -1 }, .type = CC_STE_Alternative },
    { .step = { .i =  0, .j = -1 }, .type = CC_STE_Alternative },
    { .step = { .i =  1, .j = -1 }, .type = CC_STE_Alternative },

    CC_TYPED_STEP_INVALID,
};

CcTypedStep const CC_STEPS_DISPLACEMENT_TRANCE_JOURNEY[ CC_STEPS_DISPLACEMENT_TRANCE_JOURNEY_SIZE ] = {
    // I. quadrant
    { .step = { .i =  3, .j =  1 }, .type = CC_STE_MovementOnly },
    { .step = { .i =  2, .j =  2 }, .type = CC_STE_MovementOnly },
    { .step = { .i =  1, .j =  3 }, .type = CC_STE_MovementOnly },

    { .step = { .i =  4, .j =  2 }, .type = CC_STE_MovementOnly },
    { .step = { .i =  3, .j =  3 }, .type = CC_STE_MovementOnly },
    { .step = { .i =  2, .j =  4 }, .type = CC_STE_MovementOnly },

    { .step = { .i =  6, .j =  2 }, .type = CC_STE_MovementOnly },
    { .step = { .i =  5, .j =  3 }, .type = CC_STE_MovementOnly },
    { .step = { .i =  4, .j =  4 }, .type = CC_STE_MovementOnly },
    { .step = { .i =  3, .j =  5 }, .type = CC_STE_MovementOnly },
    { .step = { .i =  2, .j =  6 }, .type = CC_STE_MovementOnly },

    // II. quadrant
    { .step = { .i = -3, .j =  1 }, .type = CC_STE_MovementOnly },
    { .step = { .i = -2, .j =  2 }, .type = CC_STE_MovementOnly },
    { .step = { .i = -1, .j =  3 }, .type = CC_STE_MovementOnly },

    { .step = { .i = -4, .j =  2 }, .type = CC_STE_MovementOnly },
    { .step = { .i = -3, .j =  3 }, .type = CC_STE_MovementOnly },
    { .step = { .i = -2, .j =  4 }, .type = CC_STE_MovementOnly },

    { .step = { .i = -6, .j =  2 }, .type = CC_STE_MovementOnly },
    { .step = { .i = -5, .j =  3 }, .type = CC_STE_MovementOnly },
    { .step = { .i = -4, .j =  4 }, .type = CC_STE_MovementOnly },
    { .step = { .i = -3, .j =  5 }, .type = CC_STE_MovementOnly },
    { .step = { .i = -2, .j =  6 }, .type = CC_STE_MovementOnly },

    // III. quadrant
    { .step = { .i = -3, .j = -1 }, .type = CC_STE_MovementOnly },
    { .step = { .i = -2, .j = -2 }, .type = CC_STE_MovementOnly },
    { .step = { .i = -1, .j = -3 }, .type = CC_STE_MovementOnly },

    { .step = { .i = -4, .j = -2 }, .type = CC_STE_MovementOnly },
    { .step = { .i = -3, .j = -3 }, .type = CC_STE_MovementOnly },
    { .step = { .i = -2, .j = -4 }, .type = CC_STE_MovementOnly },

    { .step = { .i = -6, .j = -2 }, .type = CC_STE_MovementOnly },
    { .step = { .i = -5, .j = -3 }, .type = CC_STE_MovementOnly },
    { .step = { .i = -4, .j = -4 }, .type = CC_STE_MovementOnly },
    { .step = { .i = -3, .j = -5 }, .type = CC_STE_MovementOnly },
    { .step = { .i = -2, .j = -6 }, .type = CC_STE_MovementOnly },

    // IV. quadrant
    { .step = { .i =  3, .j = -1 }, .type = CC_STE_MovementOnly },
    { .step = { .i =  2, .j = -2 }, .type = CC_STE_MovementOnly },
    { .step = { .i =  1, .j = -3 }, .type = CC_STE_MovementOnly },

    { .step = { .i =  4, .j = -2 }, .type = CC_STE_MovementOnly },
    { .step = { .i =  3, .j = -3 }, .type = CC_STE_MovementOnly },
    { .step = { .i =  2, .j = -4 }, .type = CC_STE_MovementOnly },

    { .step = { .i =  6, .j = -2 }, .type = CC_STE_MovementOnly },
    { .step = { .i =  5, .j = -3 }, .type = CC_STE_MovementOnly },
    { .step = { .i =  4, .j = -4 }, .type = CC_STE_MovementOnly },
    { .step = { .i =  3, .j = -5 }, .type = CC_STE_MovementOnly },
    { .step = { .i =  2, .j = -6 }, .type = CC_STE_MovementOnly },

    CC_TYPED_STEP_INVALID,
};


bool cc_is_typed_step_valid( CcTypedStep step, CcTypedStep const steps[], size_t steps_len__d ) {
    if ( !CC_TYPED_STEP_IS_VALID( step ) ) return false;

    for ( size_t k = 0;
          (steps_len__d == CC_STEPS_LEN_INVALID_DATA_TERMINATED) || (k < steps_len__d);
          ++k ) {
        CcTypedStep p = steps[ k ];

        if ( !CC_TYPED_STEP_IS_VALID( p ) ) break;

        if ( cc_typed_step_is_equal( step, p ) ) return true;
    }

    return false;
}

bool cc_is_same_color( CcPieceType piece, CcPos pos ) {
    if ( cc_piece_is_light( piece ) && CC_IS_FIELD_LIGHT( pos.i, pos.j ) )
        return true;

    if ( cc_piece_is_dark( piece ) && CC_IS_FIELD_DARK( pos.i, pos.j ) )
        return true;

    return false;
}

bool cc_convert_steps_to_pos_link( CcTypedStep const steps[],
                                   size_t steps_len__d,
                                   CcTypedStepLink ** steps__o ) {
    if ( !steps__o ) return false;
    if ( *steps__o ) return false;

    CcTypedStepLink * tsl__t = NULL;

    for ( size_t k = 0;
          (steps_len__d == CC_STEPS_LEN_INVALID_DATA_TERMINATED) || (k < steps_len__d);
          ++k ) {
        CcTypedStep ts = steps[ k ];

        if ( !CC_TYPED_STEP_IS_VALID( ts ) ) break;

        if ( !cc_typed_step_link_append( &tsl__t, ts ) ) {
            cc_typed_step_link_free_all( &tsl__t );
            return false;
        }
    }

    // Ownership transfer.
    *steps__o = tsl__t;

    return true;
}
