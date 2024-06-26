// Copyright (c) 2021, 2022, 2024 Mario Mlačak, mmlacak@gmail.com
// Licensed under GNU GPL v3+ license. See LICENSING, COPYING files for details.

#include "cc_pos_defs.h"

/**
    @file cc_pos_defs.c
    @brief Position, step definitions, checkers.
*/


CcTypedStep const CC_STEPS_LIGHT_PAWN[ CC_STEPS_PAWN_SIZE ] = {
    // step
    { .step = { .i =  0, .j =  1 }, .type = CC_STE_Movement },

    // capture-steps
    { .step = { .i = -1, .j =  1 }, .type = CC_STE_Capture },
    { .step = { .i =  1, .j =  1 }, .type = CC_STE_Capture },

    CC_TYPED_STEP_INVALID,
};

CcTypedStep const CC_STEPS_DARK_PAWN[ CC_STEPS_PAWN_SIZE ] = {
    // step
    { .step = { .i =  0, .j = -1 }, .type = CC_STE_Movement },

    // capture-steps
    { .step = { .i = -1, .j = -1 }, .type = CC_STE_Capture },
    { .step = { .i =  1, .j = -1 }, .type = CC_STE_Capture },

    CC_TYPED_STEP_INVALID,
};

CcTypedStep const CC_STEPS_LIGHT_SIDEWAYS_PAWN[ CC_STEPS_SIDEWAYS_PAWN_SIZE ] = {
    // steps
    { .step = { .i =  0, .j =  1 }, .type = CC_STE_Movement },
    { .step = { .i = -1, .j =  0 }, .type = CC_STE_Movement },
    { .step = { .i =  1, .j =  0 }, .type = CC_STE_Movement },

    // capture-steps
    { .step = { .i = -1, .j =  1 }, .type = CC_STE_Capture },
    { .step = { .i =  1, .j =  1 }, .type = CC_STE_Capture },

    CC_TYPED_STEP_INVALID,
};

CcTypedStep const CC_STEPS_DARK_SIDEWAYS_PAWN[ CC_STEPS_SIDEWAYS_PAWN_SIZE ] = {
    // steps
    { .step = { .i =  0, .j = -1 }, .type = CC_STE_Movement },
    { .step = { .i = -1, .j =  0 }, .type = CC_STE_Movement },
    { .step = { .i =  1, .j =  0 }, .type = CC_STE_Movement },

    // capture-steps
    { .step = { .i = -1, .j = -1 }, .type = CC_STE_Capture },
    { .step = { .i =  1, .j = -1 }, .type = CC_STE_Capture },

    CC_TYPED_STEP_INVALID,
};


CcTypedStep const CC_STEPS_KNIGHT[ CC_STEPS_KNIGHT_SIZE ] = {
    { .step = { .i =  2, .j =  1 }, .type = CC_STE_Capture },
    { .step = { .i =  1, .j =  2 }, .type = CC_STE_Capture },

    { .step = { .i = -1, .j =  2 }, .type = CC_STE_Capture },
    { .step = { .i = -2, .j =  1 }, .type = CC_STE_Capture },

    { .step = { .i = -2, .j = -1 }, .type = CC_STE_Capture },
    { .step = { .i = -1, .j = -2 }, .type = CC_STE_Capture },

    { .step = { .i =  1, .j = -2 }, .type = CC_STE_Capture },
    { .step = { .i =  2, .j = -1 }, .type = CC_STE_Capture },

    CC_TYPED_STEP_INVALID,
};

CcTypedStep const CC_STEPS_BISHOP[ CC_STEPS_BISHOP_SIZE ] = {
    { .step = { .i =  1, .j =  1 }, .type = CC_STE_Capture },
    { .step = { .i = -1, .j =  1 }, .type = CC_STE_Capture },
    { .step = { .i = -1, .j = -1 }, .type = CC_STE_Capture },
    { .step = { .i =  1, .j = -1 }, .type = CC_STE_Capture },

    CC_TYPED_STEP_INVALID,
};

CcTypedStep const CC_STEPS_ROOK[ CC_STEPS_ROOK_SIZE ] = {
    { .step = { .i =  1, .j =  0 }, .type = CC_STE_Capture },
    { .step = { .i =  0, .j =  1 }, .type = CC_STE_Capture },
    { .step = { .i = -1, .j =  0 }, .type = CC_STE_Capture },
    { .step = { .i =  0, .j = -1 }, .type = CC_STE_Capture },

    CC_TYPED_STEP_INVALID,
};

CcTypedStep const CC_STEPS_QUEEN[ CC_STEPS_QUEEN_SIZE ] = {
    { .step = { .i =  1, .j =  0 }, .type = CC_STE_Capture },
    { .step = { .i =  1, .j =  1 }, .type = CC_STE_Capture },
    { .step = { .i =  0, .j =  1 }, .type = CC_STE_Capture },
    { .step = { .i = -1, .j =  1 }, .type = CC_STE_Capture },
    { .step = { .i = -1, .j =  0 }, .type = CC_STE_Capture },
    { .step = { .i = -1, .j = -1 }, .type = CC_STE_Capture },
    { .step = { .i =  0, .j = -1 }, .type = CC_STE_Capture },
    { .step = { .i =  1, .j = -1 }, .type = CC_STE_Capture },

    CC_TYPED_STEP_INVALID,
};

CcTypedStep const CC_STEPS_LONG_UNICORN[ CC_STEPS_LONG_UNICORN_SIZE ] = {
    { .step = { .i =  4, .j =  1 }, .type = CC_STE_Capture },
    { .step = { .i =  3, .j =  2 }, .type = CC_STE_Capture },
    { .step = { .i =  2, .j =  3 }, .type = CC_STE_Capture },
    { .step = { .i =  1, .j =  4 }, .type = CC_STE_Capture },

    { .step = { .i = -1, .j =  4 }, .type = CC_STE_Capture },
    { .step = { .i = -2, .j =  3 }, .type = CC_STE_Capture },
    { .step = { .i = -3, .j =  2 }, .type = CC_STE_Capture },
    { .step = { .i = -4, .j =  1 }, .type = CC_STE_Capture },

    { .step = { .i = -4, .j = -1 }, .type = CC_STE_Capture },
    { .step = { .i = -3, .j = -2 }, .type = CC_STE_Capture },
    { .step = { .i = -2, .j = -3 }, .type = CC_STE_Capture },
    { .step = { .i = -1, .j = -4 }, .type = CC_STE_Capture },

    { .step = { .i =  1, .j = -4 }, .type = CC_STE_Capture },
    { .step = { .i =  2, .j = -3 }, .type = CC_STE_Capture },
    { .step = { .i =  3, .j = -2 }, .type = CC_STE_Capture },
    { .step = { .i =  4, .j = -1 }, .type = CC_STE_Capture },

    CC_TYPED_STEP_INVALID,
};

CcTypedStep const CC_STEPS_SERPENT_LEFT[ CC_STEPS_SERPENT_SIZE ] = {
    { .step = { .i = -1, .j =  1 }, .type = CC_STE_Capture },
    { .step = { .i =  1, .j = -1 }, .type = CC_STE_Capture },

    CC_TYPED_STEP_INVALID,
};

CcTypedStep const CC_STEPS_SERPENT_RIGHT[ CC_STEPS_SERPENT_SIZE ] = {
    { .step = { .i = -1, .j = -1 }, .type = CC_STE_Capture },
    { .step = { .i =  1, .j =  1 }, .type = CC_STE_Capture },

    CC_TYPED_STEP_INVALID,
};

CcTypedStep const CC_STEPS_LIGHT_SHAMAN[ CC_STEPS_LIGHT_SHAMAN_SIZE ] = {
    //
    // Knight steps

    { .step = { .i =  2, .j =  1 }, .type = CC_STE_Movement },
    { .step = { .i =  1, .j =  2 }, .type = CC_STE_Movement },

    { .step = { .i = -1, .j =  2 }, .type = CC_STE_Movement },
    { .step = { .i = -2, .j =  1 }, .type = CC_STE_Movement },

    { .step = { .i = -2, .j = -1 }, .type = CC_STE_Movement },
    { .step = { .i = -1, .j = -2 }, .type = CC_STE_Movement },

    { .step = { .i =  1, .j = -2 }, .type = CC_STE_Movement },
    { .step = { .i =  2, .j = -1 }, .type = CC_STE_Movement },

    //
    // long Unicorn capture-steps

    { .step = { .i =  4, .j =  1 }, .type = CC_STE_Capture },
    { .step = { .i =  3, .j =  2 }, .type = CC_STE_Capture },
    { .step = { .i =  2, .j =  3 }, .type = CC_STE_Capture },
    { .step = { .i =  1, .j =  4 }, .type = CC_STE_Capture },

    { .step = { .i = -1, .j =  4 }, .type = CC_STE_Capture },
    { .step = { .i = -2, .j =  3 }, .type = CC_STE_Capture },
    { .step = { .i = -3, .j =  2 }, .type = CC_STE_Capture },
    { .step = { .i = -4, .j =  1 }, .type = CC_STE_Capture },

    { .step = { .i = -4, .j = -1 }, .type = CC_STE_Capture },
    { .step = { .i = -3, .j = -2 }, .type = CC_STE_Capture },
    { .step = { .i = -2, .j = -3 }, .type = CC_STE_Capture },
    { .step = { .i = -1, .j = -4 }, .type = CC_STE_Capture },

    { .step = { .i =  1, .j = -4 }, .type = CC_STE_Capture },
    { .step = { .i =  2, .j = -3 }, .type = CC_STE_Capture },
    { .step = { .i =  3, .j = -2 }, .type = CC_STE_Capture },
    { .step = { .i =  4, .j = -1 }, .type = CC_STE_Capture },

    CC_TYPED_STEP_INVALID,
};

CcTypedStep const CC_STEPS_DARK_SHAMAN[ CC_STEPS_DARK_SHAMAN_SIZE ] = {
    //
    // Knight capture-steps

    { .step = { .i =  2, .j =  1 }, .type = CC_STE_Capture },
    { .step = { .i =  1, .j =  2 }, .type = CC_STE_Capture },

    { .step = { .i = -1, .j =  2 }, .type = CC_STE_Capture },
    { .step = { .i = -2, .j =  1 }, .type = CC_STE_Capture },

    { .step = { .i = -2, .j = -1 }, .type = CC_STE_Capture },
    { .step = { .i = -1, .j = -2 }, .type = CC_STE_Capture },

    { .step = { .i =  1, .j = -2 }, .type = CC_STE_Capture },
    { .step = { .i =  2, .j = -1 }, .type = CC_STE_Capture },

    //
    // long Unicorn steps

    { .step = { .i =  4, .j =  1 }, .type = CC_STE_Movement },
    { .step = { .i =  3, .j =  2 }, .type = CC_STE_Movement },
    { .step = { .i =  2, .j =  3 }, .type = CC_STE_Movement },
    { .step = { .i =  1, .j =  4 }, .type = CC_STE_Movement },

    { .step = { .i = -1, .j =  4 }, .type = CC_STE_Movement },
    { .step = { .i = -2, .j =  3 }, .type = CC_STE_Movement },
    { .step = { .i = -3, .j =  2 }, .type = CC_STE_Movement },
    { .step = { .i = -4, .j =  1 }, .type = CC_STE_Movement },

    { .step = { .i = -4, .j = -1 }, .type = CC_STE_Movement },
    { .step = { .i = -3, .j = -2 }, .type = CC_STE_Movement },
    { .step = { .i = -2, .j = -3 }, .type = CC_STE_Movement },
    { .step = { .i = -1, .j = -4 }, .type = CC_STE_Movement },

    { .step = { .i =  1, .j = -4 }, .type = CC_STE_Movement },
    { .step = { .i =  2, .j = -3 }, .type = CC_STE_Movement },
    { .step = { .i =  3, .j = -2 }, .type = CC_STE_Movement },
    { .step = { .i =  4, .j = -1 }, .type = CC_STE_Movement },

    CC_TYPED_STEP_INVALID,
};

CcTypedStep const CC_STEPS_LIGHT_SCOUT[ CC_STEPS_SCOUT_SIZE ] = {
    // light Pawn steps
    { .step = { .i =  0, .j =  1 }, .type = CC_STE_Movement },
    { .step = { .i = -1, .j =  0 }, .type = CC_STE_Movement },
    { .step = { .i =  1, .j =  0 }, .type = CC_STE_Movement },

    // dark Pawn capture-steps
    { .step = { .i = -1, .j = -1 }, .type = CC_STE_Capture },
    { .step = { .i =  1, .j = -1 }, .type = CC_STE_Capture },

    CC_TYPED_STEP_INVALID,
};

CcTypedStep const CC_STEPS_DARK_SCOUT[ CC_STEPS_SCOUT_SIZE ] = {
    // dark Pawn steps
    { .step = { .i =  0, .j = -1 }, .type = CC_STE_Movement },
    { .step = { .i = -1, .j =  0 }, .type = CC_STE_Movement },
    { .step = { .i =  1, .j =  0 }, .type = CC_STE_Movement },

    // light Pawn capture-steps
    { .step = { .i = -1, .j =  1 }, .type = CC_STE_Capture },
    { .step = { .i =  1, .j =  1 }, .type = CC_STE_Capture },

    CC_TYPED_STEP_INVALID,
};

CcTypedStep const CC_STEPS_GRENADIER[ CC_STEPS_GRENADIER_SIZE ] = {
    // Rook steps
    { .step = { .i =  1, .j =  0 }, .type = CC_STE_Movement },
    { .step = { .i =  0, .j =  1 }, .type = CC_STE_Movement },
    { .step = { .i = -1, .j =  0 }, .type = CC_STE_Movement },
    { .step = { .i =  0, .j = -1 }, .type = CC_STE_Movement },

    // Bishop capture-steps
    { .step = { .i =  1, .j =  1 }, .type = CC_STE_Capture },
    { .step = { .i = -1, .j =  1 }, .type = CC_STE_Capture },
    { .step = { .i = -1, .j = -1 }, .type = CC_STE_Capture },
    { .step = { .i =  1, .j = -1 }, .type = CC_STE_Capture },

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

bool cc_is_step_valid( CcTypedStep step, CcTypedStep const steps[], size_t steps_len__d ) {
    if ( !CC_TYPED_STEP_IS_VALID( step ) ) return false;

    for ( size_t k = 0;
          (steps_len__d == CC_STEPS_LEN_INVALID_DATA_TERMINATED) || (k < steps_len__d);
          ++k ) {
        CcTypedStep p = steps[ k ];

        if ( !CC_TYPED_STEP_IS_VALID( p ) ) return false; // break; // This is fine, as long as there is nothing after the loop.

        if ( cc_typed_step_is_equal( step, p ) ) return true;
    }

    return false;
}

bool cc_is_same_color( CcPieceEnum piece, CcPos pos ) {
    if ( cc_piece_is_light( piece ) && CC_IS_FIELD_LIGHT( pos.i, pos.j ) )
        return true;

    if ( cc_piece_is_dark( piece ) && CC_IS_FIELD_DARK( pos.i, pos.j ) )
        return true;

    return false;
}

bool cc_convert_steps_to_pos_link( CcTypedStep const steps[],
                                   size_t steps_len__d,
                                   CcTypedStepLink ** steps__iod_a ) {
    if ( !steps__iod_a ) return false;

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
    if ( !cc_typed_step_link_extend( steps__iod_a, &tsl__t ) ) {
        cc_typed_step_link_free_all( &tsl__t );
        cc_typed_step_link_free_all( steps__iod_a );
        return false;
    }

    return true;
}


// TODO :: FIX
//
// static bool cc_starting_steps_pawn( CcVariantEnum variant,
//                                     CcPieceEnum piece,
//                                     CcTypedStepLink ** starting_steps__o ) {
//     // if ( !starting_steps__o ) return false;
//     // if ( *starting_steps__o ) return false;

//     // if ( CC_PIECE_IS_PAWN( piece ) ) return false;

//     if ( cc_variant_has_sideways_pawns( variant ) ) {
//         if ( piece == CC_PE_LightPawn )
//             return cc_convert_steps_to_pos_link( CC_STEPS_ALL_LIGHT_SIDEWAYS_PAWN,
//                                                  CC_STEPS_ALL_SIDEWAYS_PAWN_LEN,
//                                                  starting_steps__o );

//         if ( piece == CC_PE_DarkPawn )
//             return cc_convert_steps_to_pos_link( CC_STEPS_ALL_DARK_SIDEWAYS_PAWN,
//                                                  CC_STEPS_ALL_SIDEWAYS_PAWN_LEN,
//                                                  starting_steps__o );
//     } else {
//         if ( piece == CC_PE_LightPawn )
//             return cc_convert_steps_to_pos_link( CC_STEPS_ALL_LIGHT_PAWN,
//                                                  CC_STEPS_ALL_PAWN_LEN,
//                                                  starting_steps__o );

//         if ( piece == CC_PE_DarkPawn )
//             return cc_convert_steps_to_pos_link( CC_STEPS_ALL_DARK_PAWN,
//                                                  CC_STEPS_ALL_PAWN_LEN,
//                                                  starting_steps__o );
//     }

//     return false;
// }

// static bool cc_starting_steps_unicorn( CcPieceEnum piece,
//                                        CcTypedStep pos,
//                                        CcTypedStepLink ** starting_steps__o ) {
//     // if ( !starting_steps__o ) return false;
//     // if ( *starting_steps__o ) return false;

//     // if ( CC_PIECE_IS_UNICORN( piece ) ) return false;

//     if ( cc_is_same_color( piece, pos ) ) {
//         return cc_convert_steps_to_pos_link( CC_STEPS_SHORT_UNICORN,
//                                              CC_STEPS_SHORT_UNICORN_LEN,
//                                              starting_steps__o );
//     } else {
//         return cc_convert_steps_to_pos_link( CC_STEPS_LONG_UNICORN,
//                                              CC_STEPS_LONG_UNICORN_LEN,
//                                              starting_steps__o );
//     }
// }

// static bool cc_starting_steps_centaur( CcPieceEnum piece,
//                                        CcTypedStep pos,
//                                        CcTypedStepLink ** starting_steps__o ) {
//     // if ( !starting_steps__o ) return false;
//     // if ( *starting_steps__o ) return false;

//     // if ( CC_PIECE_IS_CENTAUR( piece ) ) return false;

//     if ( cc_is_same_color( piece, pos ) ) {
//         return cc_convert_steps_to_pos_link( CC_STEPS_SHORT_CENTAUR,
//                                              CC_STEPS_SHORT_CENTAUR_LEN,
//                                              starting_steps__o );
//     } else {
//         return cc_convert_steps_to_pos_link( CC_STEPS_LONG_CENTAUR,
//                                              CC_STEPS_LONG_CENTAUR_LEN,
//                                              starting_steps__o );
//     }
// }

// static bool cc_starting_steps_scout( CcPieceEnum piece,
//                                      CcTypedStepLink ** starting_steps__o ) {
//     // if ( !starting_steps__o ) return false;
//     // if ( *starting_steps__o ) return false;

//     // if ( CC_PIECE_IS_SCOUT( piece ) ) return false;

//     if ( piece == CC_PE_LightScout ) {
//         return cc_convert_steps_to_pos_link( CC_STEPS_ALL_LIGHT_SCOUT,
//                                              CC_STEPS_ALL_SCOUT_SIZE,
//                                              starting_steps__o );
//     } else if ( piece == CC_PE_DarkScout ) {
//         return cc_convert_steps_to_pos_link( CC_STEPS_ALL_DARK_SCOUT,
//                                              CC_STEPS_ALL_SCOUT_SIZE,
//                                              starting_steps__o );
//     }

//     return false;
// }

// static bool cc_starting_steps_starchild( CcVariantEnum variant,
//                                          /* CcPieceEnum piece, */
//                                          CcTypedStep pos,
//                                          CcTypedStepLink ** starting_steps__o ) {
//     // if ( !starting_steps__o ) return false;
//     // if ( *starting_steps__o ) return false;

//     // if ( CC_PIECE_IS_STARCHILD( piece ) ) return false;

//     int field_color = // field_color is opposite color of currently occupied field.
//         CC_IS_FIELD_LIGHT( pos.i, pos.j ) ? CC_FIELD_COLOR_DARK
//                                           : CC_FIELD_COLOR_LIGHT;
//     size_t board_size = cc_variant_board_size( variant );

//     for ( int j = 0; (size_t)j < board_size; ++j ) {
//         for ( int i = 0; (size_t)i < board_size; ++i ) {
//             if ( CC_IS_FIELD_COLOR( i, j, field_color ) ) {
//                 if ( !cc_typed_step_link_append( starting_steps__o, cc_pos( i, j ) ) ) {
//                     cc_typed_step_link_free_all( starting_steps__o );
//                     return false;
//                 }
//             }
//         }
//     }

//     return true;
// }

// bool cc_starting_steps( CcVariantEnum variant,
//                         CcPieceEnum piece,
//                         CcPieceEnum activator,
//                         CcTypedStep pos,
//                         CcTypedStepLink ** starting_steps__o ) {
//     if ( !starting_steps__o ) return false;
//     if ( *starting_steps__o ) return false;

//     if ( CC_PIECE_IS_VALID( piece ) ) return false;

//     if ( CC_PIECE_IS_PAWN( piece ) ) {
//         return cc_starting_steps_pawn( variant, piece, starting_steps__o );
//     } else if ( CC_PIECE_IS_KNIGHT( piece ) ) {
//         return cc_convert_steps_to_pos_link( CC_STEPS_KNIGHT, CC_STEPS_KNIGHT_LEN, starting_steps__o );
//     } else if ( CC_PIECE_IS_BISHOP( piece ) ) {
//         return cc_convert_steps_to_pos_link( CC_STEPS_BISHOP, CC_STEPS_BISHOP_LEN, starting_steps__o );
//     } else if ( CC_PIECE_IS_ROOK( piece ) ) {
//         return cc_convert_steps_to_pos_link( CC_STEPS_ROOK, CC_STEPS_ROOK_LEN, starting_steps__o );
//     } else if ( CC_PIECE_IS_QUEEN( piece ) ) {
//         return cc_convert_steps_to_pos_link( CC_STEPS_QUEEN, CC_STEPS_QUEEN_LEN, starting_steps__o );
//     } else if ( CC_PIECE_IS_KING( piece ) ) {
//         return cc_convert_steps_to_pos_link( CC_STEPS_KING, CC_STEPS_KING_LEN, starting_steps__o );

//     } else if ( CC_PIECE_IS_PEGASUS( piece ) ) {
//         return cc_convert_steps_to_pos_link( CC_STEPS_PEGASUS, CC_STEPS_PEGASUS_LEN, starting_steps__o );
//     } else if ( CC_PIECE_IS_PYRAMID( piece ) ) {
//         return cc_convert_steps_to_pos_link( CC_STEPS_PYRAMID, CC_STEPS_PYRAMID_LEN, starting_steps__o );
//     } else if ( CC_PIECE_IS_UNICORN( piece ) ) {
//         return cc_starting_steps_unicorn( piece, pos, starting_steps__o );
//     } else if ( CC_PIECE_IS_WAVE( piece ) ) {
//         if ( !CC_PIECE_IS_ACTIVATOR( activator ) ) return false;
//         return cc_starting_steps( variant, activator, CC_PE_None, pos, starting_steps__o );
//     } else if ( CC_PIECE_IS_STAR( piece ) ) {
//         if ( !CC_PIECE_IS_STARCHILD( activator ) ) return false;
//         return cc_convert_steps_to_pos_link( CC_STEPS_STAR, CC_STEPS_STAR_LEN, starting_steps__o );

//     } else if ( CC_PIECE_IS_CENTAUR( piece ) ) {
//         return cc_starting_steps_centaur( piece, pos, starting_steps__o );
//     } else if ( CC_PIECE_IS_GRENADIER( piece ) ) {
//         return cc_convert_steps_to_pos_link( CC_STEPS_ALL_GRENADIER, CC_STEPS_ALL_GRENADIER_LEN, starting_steps__o );
//     } else if ( CC_PIECE_IS_SCOUT( piece ) ) {
//         return cc_starting_steps_scout( piece, starting_steps__o );

//     } else if ( CC_PIECE_IS_SERPENT( piece ) ) {
//         return cc_convert_steps_to_pos_link( CC_STEPS_ALL_SERPENT, CC_STEPS_ALL_SERPENT_LEN, starting_steps__o );
//     } else if ( CC_PIECE_IS_SHAMAN( piece ) ) {
//         return cc_convert_steps_to_pos_link( CC_STEPS_ALL_SHAMAN, CC_STEPS_ALL_SHAMAN_LEN, starting_steps__o );
//     } else if ( CC_PIECE_IS_MONOLITH( piece ) ) {
//         return cc_convert_steps_to_pos_link( CC_STEPS_STARTING_MONOLITH, CC_STEPS_STARTING_MONOLITH_LEN, starting_steps__o );
//     } else if ( CC_PIECE_IS_STARCHILD( piece ) ) {
//         return cc_starting_steps_starchild( variant, pos, starting_steps__o );
//     }

//     return false;
// }
//
// TODO :: FIX
