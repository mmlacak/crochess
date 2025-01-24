// Copyright (c) 2021, 2022, 2024, 2025 Mario Mlačak, mmlacak@gmail.com
// Licensed under GNU GPL v3+ license. See LICENSING, COPYING files for details.

#include <stdlib.h> // abs()

#include "cc_typed_step_defs.h"


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

CcTypedStep const CC_STEPS_SERPENT_LEFT[ CC_STEPS_DIAGONAL_SERPENT_SIZE ] = {
    { .step = { .i = -1, .j =  1 }, .type = CC_STE_CaptureOrMovement },
    { .step = { .i =  1, .j = -1 }, .type = CC_STE_CaptureOrMovement },

    CC_TYPED_STEP_INVALID,
};

CcTypedStep const CC_STEPS_SERPENT_RIGHT[ CC_STEPS_DIAGONAL_SERPENT_SIZE ] = {
    { .step = { .i = -1, .j = -1 }, .type = CC_STE_CaptureOrMovement },
    { .step = { .i =  1, .j =  1 }, .type = CC_STE_CaptureOrMovement },

    CC_TYPED_STEP_INVALID,
};

CcTypedStep const CC_STEPS_ALL_SERPENT[ CC_STEPS_ALL_SERPENT_SIZE ] = {
    // Left diagonal steps
    { .step = { .i = -1, .j =  1 }, .type = CC_STE_CaptureOrMovement },
    { .step = { .i =  1, .j = -1 }, .type = CC_STE_CaptureOrMovement },

    // Right diagonal steps
    { .step = { .i = -1, .j = -1 }, .type = CC_STE_CaptureOrMovement },
    { .step = { .i =  1, .j =  1 }, .type = CC_STE_CaptureOrMovement },

    // Color-changing steps
    { .step = { .i =  1, .j =  0 }, .type = CC_STE_ColorChange },
    { .step = { .i =  0, .j =  1 }, .type = CC_STE_ColorChange },
    { .step = { .i = -1, .j =  0 }, .type = CC_STE_ColorChange },
    { .step = { .i =  0, .j = -1 }, .type = CC_STE_ColorChange },

    // Pawn displacement steps
    { .step = { .i =  1, .j =  0 }, .type = CC_STE_Displacement },
    { .step = { .i =  0, .j =  1 }, .type = CC_STE_Displacement },
    { .step = { .i = -1, .j =  0 }, .type = CC_STE_Displacement },
    { .step = { .i =  0, .j = -1 }, .type = CC_STE_Displacement },

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
    { .step = { .i =  1, .j =  0 }, .type = CC_STE_Miracle },
    { .step = { .i =  1, .j =  1 }, .type = CC_STE_Miracle },
    { .step = { .i =  0, .j =  1 }, .type = CC_STE_Miracle },
    { .step = { .i = -1, .j =  1 }, .type = CC_STE_Miracle },
    { .step = { .i = -1, .j =  0 }, .type = CC_STE_Miracle },
    { .step = { .i = -1, .j = -1 }, .type = CC_STE_Miracle },
    { .step = { .i =  0, .j = -1 }, .type = CC_STE_Miracle },
    { .step = { .i =  1, .j = -1 }, .type = CC_STE_Miracle },

    CC_TYPED_STEP_INVALID,
};

CcTypedStep const CC_STEPS_DISPLACEMENT_TRANCE_JOURNEY[ CC_STEPS_DISPLACEMENT_TRANCE_JOURNEY_SIZE ] = {
    // I. quadrant
    { .step = { .i =  3, .j =  1 }, .type = CC_STE_Displacement },
    { .step = { .i =  2, .j =  2 }, .type = CC_STE_Displacement },
    { .step = { .i =  1, .j =  3 }, .type = CC_STE_Displacement },

    { .step = { .i =  4, .j =  2 }, .type = CC_STE_Displacement },
    { .step = { .i =  3, .j =  3 }, .type = CC_STE_Displacement },
    { .step = { .i =  2, .j =  4 }, .type = CC_STE_Displacement },

    { .step = { .i =  6, .j =  2 }, .type = CC_STE_Displacement },
    { .step = { .i =  5, .j =  3 }, .type = CC_STE_Displacement },
    { .step = { .i =  4, .j =  4 }, .type = CC_STE_Displacement },
    { .step = { .i =  3, .j =  5 }, .type = CC_STE_Displacement },
    { .step = { .i =  2, .j =  6 }, .type = CC_STE_Displacement },

    // II. quadrant
    { .step = { .i = -3, .j =  1 }, .type = CC_STE_Displacement },
    { .step = { .i = -2, .j =  2 }, .type = CC_STE_Displacement },
    { .step = { .i = -1, .j =  3 }, .type = CC_STE_Displacement },

    { .step = { .i = -4, .j =  2 }, .type = CC_STE_Displacement },
    { .step = { .i = -3, .j =  3 }, .type = CC_STE_Displacement },
    { .step = { .i = -2, .j =  4 }, .type = CC_STE_Displacement },

    { .step = { .i = -6, .j =  2 }, .type = CC_STE_Displacement },
    { .step = { .i = -5, .j =  3 }, .type = CC_STE_Displacement },
    { .step = { .i = -4, .j =  4 }, .type = CC_STE_Displacement },
    { .step = { .i = -3, .j =  5 }, .type = CC_STE_Displacement },
    { .step = { .i = -2, .j =  6 }, .type = CC_STE_Displacement },

    // III. quadrant
    { .step = { .i = -3, .j = -1 }, .type = CC_STE_Displacement },
    { .step = { .i = -2, .j = -2 }, .type = CC_STE_Displacement },
    { .step = { .i = -1, .j = -3 }, .type = CC_STE_Displacement },

    { .step = { .i = -4, .j = -2 }, .type = CC_STE_Displacement },
    { .step = { .i = -3, .j = -3 }, .type = CC_STE_Displacement },
    { .step = { .i = -2, .j = -4 }, .type = CC_STE_Displacement },

    { .step = { .i = -6, .j = -2 }, .type = CC_STE_Displacement },
    { .step = { .i = -5, .j = -3 }, .type = CC_STE_Displacement },
    { .step = { .i = -4, .j = -4 }, .type = CC_STE_Displacement },
    { .step = { .i = -3, .j = -5 }, .type = CC_STE_Displacement },
    { .step = { .i = -2, .j = -6 }, .type = CC_STE_Displacement },

    // IV. quadrant
    { .step = { .i =  3, .j = -1 }, .type = CC_STE_Displacement },
    { .step = { .i =  2, .j = -2 }, .type = CC_STE_Displacement },
    { .step = { .i =  1, .j = -3 }, .type = CC_STE_Displacement },

    { .step = { .i =  4, .j = -2 }, .type = CC_STE_Displacement },
    { .step = { .i =  3, .j = -3 }, .type = CC_STE_Displacement },
    { .step = { .i =  2, .j = -4 }, .type = CC_STE_Displacement },

    { .step = { .i =  6, .j = -2 }, .type = CC_STE_Displacement },
    { .step = { .i =  5, .j = -3 }, .type = CC_STE_Displacement },
    { .step = { .i =  4, .j = -4 }, .type = CC_STE_Displacement },
    { .step = { .i =  3, .j = -5 }, .type = CC_STE_Displacement },
    { .step = { .i =  2, .j = -6 }, .type = CC_STE_Displacement },

    CC_TYPED_STEP_INVALID,
};


CcStepTypeEnum cc_get_step_type( CcPos step,
                                 CcStepTypeEnum filter__d,
                                 CcTypedStep const steps[],
                                 size_t steps_len__d ) {
    if ( !steps ) return CC_STE_None;
    if ( !CC_STEP_TYPE_IS_ENUMERATOR( filter__d ) ) return false;

    bool no_filter = ( filter__d == CC_STE_None );

    for ( size_t k = 0;
          (steps_len__d == CC_STEPS_LEN_GUARD_DATA_TERMINATED) || (k < steps_len__d);
          ++k ) {
        CcTypedStep p = steps[ k ];

        if ( !CC_TYPED_STEP_IS_VALID( p ) ) break;

        if ( no_filter || ( filter__d == p.type ) ) {
            if ( CC_POS_IS_EQUAL( step, p.step ) )
                return p.type;
        }
    }

    return CC_STE_None;
}

bool cc_is_typed_step_valid( CcTypedStep step,
                             CcStepTypeEnum filter__d,
                             CcTypedStep const steps[],
                             size_t steps_len__d ) {
    CcStepTypeEnum type = cc_get_step_type( step.step, filter__d, steps, steps_len__d );
    return ( type == step.type );
}


bool cc_convert_typed_steps_to_links( CcTypedStep const steps[],
                                      size_t steps_len__d,
                                      CcTypedStepLink ** steps__o ) {
    if ( !steps ) return false;
    if ( !steps__o ) return false;
    if ( *steps__o ) return false;

    CcTypedStepLink * tsl__t = NULL;

    for ( size_t k = 0;
          (steps_len__d == CC_STEPS_LEN_GUARD_DATA_TERMINATED) || (k < steps_len__d);
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

bool cc_iter_typed_steps( CcTypedStep const steps[],
                          size_t steps_len__d,
                          CcStepTypeEnum filter__d,
                          CcTypedStep const ** step__iod ) {
    if ( !steps ) return false;
    if ( !CC_STEP_TYPE_IS_ENUMERATOR( filter__d ) ) return false;
    if ( !step__iod ) return false;
    if ( *step__iod && ( *step__iod < steps ) ) return false;

    bool check_len = ( steps_len__d != CC_STEPS_LEN_GUARD_DATA_TERMINATED );

    if ( !*step__iod ) {
        *step__iod = steps;
    } else {
        bool do_filter = ( filter__d != CC_STE_None );

        do {
            ++*step__iod;

            if ( !CC_TYPED_STEP_IS_VALID( **step__iod ) ) { // Needed to prevent infinite loop, if bogus filter given & no len.
                *step__iod = NULL;
                return false;
            }
        } while ( ( do_filter && ( filter__d != (*step__iod)->type ) )
                  && ( !check_len || ( *step__iod < steps + steps_len__d ) ) ); // <= not needed, if all steps[] are filtered-out, last loop will drive **step__iod into invalid value.
    }

    if ( check_len && ( steps + steps_len__d <= *step__iod ) ) { // If equality is satisfied, step is already invalid.
        *step__iod = NULL;
        return false;
    }

    if ( !CC_TYPED_STEP_IS_VALID( **step__iod ) ) {
        *step__iod = NULL;
        return false;
    } else
        return true;
}

bool cc_iter_monolith_steps( cc_uint_t step_index,
                             CcTypedStep * step__io ) {
    if ( step_index < 1 ) return false;
    if ( !step__io ) return false;
    if ( step_index > 24 ) return false; // .. won't fit onto the largest chessboard.

    cc_uint_t step_count = 2 * step_index; // ... in one quadrant.
    int coord = (int)step_count;
    int sum_coords = coord + 1;

    step__io->type = CC_STE_MovementOnly;

    int i = step__io->step.i;
    int j = step__io->step.j;

    if ( i == 0 || j == 0 ) return false;

    if ( !CC_TYPED_STEP_IS_VALID( *step__io ) ) { // init 1st q.
        step__io->step.i = coord;
        step__io->step.j = 1;
        return true;
    } else {
        int abs_i = abs( i );
        int abs_j = abs( j );

        if ( abs_i + abs_j != sum_coords ) return false;
    }

    if ( i > 0 && j > 0 ) { // @ 1st quadrant
        if ( j < coord ) {
            --i;
            ++j;
        } else { // --> 2nd q.
            i = -1;
            j = coord;
        }
    } else if ( i < 0 && j > 0 ) { // @ 2nd quadrant
        if ( i > -coord ) {
            --i;
            --j;
        } else { // --> 3rd q.
            i = -coord;
            j = -1;
        }
    } else if ( i < 0 && j < 0 ) { // @ 3rd quadrant
        if ( j > -coord ) {
            ++i;
            --j;
        } else { // --> 4th q.
            i = 1;
            j = -coord;
        }
    } else if ( i > 0 && j < 0 ) { // @ 4th quadrant
        if ( i < coord ) {
            ++i;
            ++j;
        } else {
            *step__io = CC_TYPED_STEP_CAST_INVALID;
            return false;
        }
    } else
        return false;

    step__io->step.i = i;
    step__io->step.j = j;

    return true;
}

bool cc_iter_piece_steps( CcPieceType piece,
                          bool sideways_pawns,
                          bool short_step,
                          CcSerpentDiagonalEnum serpent_diagonal,
                          CcStepTypeEnum filter__d,
                          CcTypedStep const ** step__iod ) {
    if ( !step__iod ) return false;

    switch ( piece ) {
        case CC_PE_DarkPawn :
            return sideways_pawns ? CC_ITER_DARK_SIDEWAYS_PAWN_STEPS( step__iod, filter__d )
                                  : CC_ITER_DARK_PAWN_STEPS( step__iod, filter__d );

        case CC_PE_LightPawn :
            return sideways_pawns ? CC_ITER_LIGHT_SIDEWAYS_PAWN_STEPS( step__iod, filter__d )
                                  : CC_ITER_LIGHT_PAWN_STEPS( step__iod, filter__d );

        case CC_PE_DarkKnight :
        case CC_PE_LightKnight : return CC_ITER_KNIGHT_STEPS( step__iod, filter__d );

        case CC_PE_DarkBishop :
        case CC_PE_LightBishop : return CC_ITER_BISHOP_STEPS( step__iod, filter__d );

        case CC_PE_DarkRook :
        case CC_PE_LightRook : return CC_ITER_ROOK_STEPS( step__iod, filter__d );

        case CC_PE_DarkQueen :
        case CC_PE_LightQueen : return CC_ITER_QUEEN_STEPS( step__iod, filter__d );

        case CC_PE_DarkKing :
        case CC_PE_LightKing : return CC_ITER_KING_STEPS( step__iod, filter__d );

        case CC_PE_DarkPegasus :
        case CC_PE_LightPegasus : return CC_ITER_PEGASUS_STEPS( step__iod, filter__d );

        case CC_PE_DarkPyramid :
        case CC_PE_LightPyramid : return CC_ITER_PYRAMID_STEPS( step__iod, filter__d );

        case CC_PE_DarkUnicorn :
        case CC_PE_LightUnicorn :
            return short_step ? CC_ITER_UNICORN_SHORT_STEPS( step__iod, filter__d )
                              : CC_ITER_UNICORN_LONG_STEPS( step__iod, filter__d );

        case CC_PE_DarkWave :
        case CC_PE_LightWave : return false; // todo :: activator (?), new function (?)

        case CC_PE_DarkCentaur :
        case CC_PE_LightCentaur :
            return short_step ? CC_ITER_CENTAUR_SHORT_STEPS( step__iod, filter__d )
                              : CC_ITER_CENTAUR_LONG_STEPS( step__iod, filter__d );

        case CC_PE_DarkScout : return CC_ITER_DARK_SCOUT_STEPS( step__iod, filter__d );

        case CC_PE_LightScout : return CC_ITER_LIGHT_SCOUT_STEPS( step__iod, filter__d );

        case CC_PE_DarkGrenadier :
        case CC_PE_LightGrenadier : return CC_ITER_GRENADIER_STEPS( step__iod, filter__d );

        case CC_PE_DarkSerpent :
        case CC_PE_LightSerpent :
            if ( serpent_diagonal == CC_SDE_RightDiagonal ) {
                return CC_ITER_SERPENT_RIGHT_STEPS( step__iod, filter__d );
            } else if ( serpent_diagonal == CC_SDE_LeftDiagonal ) {
                return CC_ITER_SERPENT_LEFT_STEPS( step__iod, filter__d );
            } else if ( serpent_diagonal == CC_SDE_BothDiagonals ) {
                return CC_ITER_SERPENT_STEPS( step__iod, filter__d );
            } else
                return false;

        case CC_PE_DarkShaman : return CC_ITER_DARK_SHAMAN_STEPS( step__iod, filter__d );

        case CC_PE_LightShaman : return CC_ITER_LIGHT_SHAMAN_STEPS( step__iod, filter__d );

        case CC_PE_DarkStarchild :
        case CC_PE_LightStarchild : return CC_ITER_STARCHILD_MIRACLE_STEPS( step__iod, filter__d );

        case CC_PE_DimStar :
        case CC_PE_BrightStar : return CC_ITER_STAR_STEPS( step__iod, filter__d );

        case CC_PE_Monolith : return false; // [.] Use cc_iter_monolith_steps() instead.

        case CC_PE_None :
        default : return false;
    }
}
