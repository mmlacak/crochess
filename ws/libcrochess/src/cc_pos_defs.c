// Copyright (c) 2021, 2022, 2024 Mario Mlačak, mmlacak@gmail.com
// Licensed under GNU GPL v3+ license. See LICENSING, COPYING files for details.

#include "cc_pos_defs.h"

/**
    @file cc_pos_defs.c
    @brief Position, step definitions, checkers.
*/


CcTypedStep const CC_STEPS_LIGHT_PAWN[ CC_STEPS_PAWN_SIZE ] = {
    { .step = { .i =  0, .j =  1 }, .type = CC_STE_Movement },

    CC_TYPED_STEP_INVALID,
};

CcTypedStep const CC_STEPS_DARK_PAWN[ CC_STEPS_PAWN_SIZE ] = {
    { .step = { .i =  0, .j = -1 }, .type = CC_STE_Movement },

    CC_TYPED_STEP_INVALID,
};

CcTypedStep const CC_STEPS_CAPTURE_LIGHT_PAWN[ CC_STEPS_CAPTURE_PAWN_SIZE ] = {
    { .step = { .i = -1, .j =  1 }, .type = CC_STE_Capture },
    { .step = { .i =  1, .j =  1 }, .type = CC_STE_Capture },

    CC_TYPED_STEP_INVALID,
};

CcTypedStep const CC_STEPS_CAPTURE_DARK_PAWN[ CC_STEPS_CAPTURE_PAWN_SIZE ] = {
    { .step = { .i = -1, .j = -1 }, .type = CC_STE_Capture },
    { .step = { .i =  1, .j = -1 }, .type = CC_STE_Capture },

    CC_TYPED_STEP_INVALID,
};

CcTypedStep const CC_STEPS_ALL_LIGHT_PAWN[ CC_STEPS_ALL_PAWN_SIZE ] = {
    // step
    { .step = { .i =  0, .j =  1 }, .type = CC_STE_Movement },

    // capture-steps
    { .step = { .i = -1, .j =  1 }, .type = CC_STE_Capture },
    { .step = { .i =  1, .j =  1 }, .type = CC_STE_Capture },

    CC_TYPED_STEP_INVALID,
};

CcTypedStep const CC_STEPS_ALL_DARK_PAWN[ CC_STEPS_ALL_PAWN_SIZE ] = {
    // step
    { .step = { .i =  0, .j = -1 }, .type = CC_STE_Movement },

    // capture-steps
    { .step = { .i = -1, .j = -1 }, .type = CC_STE_Capture },
    { .step = { .i =  1, .j = -1 }, .type = CC_STE_Capture },

    CC_TYPED_STEP_INVALID,
};

CcTypedStep const CC_STEPS_LIGHT_SIDEWAYS_PAWN[ CC_STEPS_SIDEWAYS_PAWN_SIZE ] = {
    { .step = { .i =  0, .j =  1 }, .type = CC_STE_Capture },
    { .step = { .i = -1, .j =  0 }, .type = CC_STE_Capture },
    { .step = { .i =  1, .j =  0 }, .type = CC_STE_Capture },

    CC_TYPED_STEP_INVALID,
};

CcTypedStep const CC_STEPS_DARK_SIDEWAYS_PAWN[ CC_STEPS_SIDEWAYS_PAWN_SIZE ] = {
    { .step = { .i =  0, .j = -1 }, .type = CC_STE_Capture },
    { .step = { .i = -1, .j =  0 }, .type = CC_STE_Capture },
    { .step = { .i =  1, .j =  0 }, .type = CC_STE_Capture },

    CC_TYPED_STEP_INVALID,
};

CcTypedStep const CC_STEPS_ALL_LIGHT_SIDEWAYS_PAWN[ CC_STEPS_ALL_SIDEWAYS_PAWN_SIZE ] = {
    // steps
    { .step = { .i =  0, .j =  1 }, .type = CC_STE_Movement },
    { .step = { .i = -1, .j =  0 }, .type = CC_STE_Movement },
    { .step = { .i =  1, .j =  0 }, .type = CC_STE_Movement },

    // capture-steps
    { .step = { .i = -1, .j =  1 }, .type = CC_STE_Capture },
    { .step = { .i =  1, .j =  1 }, .type = CC_STE_Capture },

    CC_TYPED_STEP_INVALID,
};

CcTypedStep const CC_STEPS_ALL_DARK_SIDEWAYS_PAWN[ CC_STEPS_ALL_SIDEWAYS_PAWN_SIZE ] = {
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

// TODO :: split light & dark Shaman --> split steps & capture-steps
//
// CcTypedStep const CC_STEPS_ALL_SHAMAN[ CC_STEPS_ALL_SHAMAN_SIZE ] = {
//     //
//     // Knight steps

//     { .step = { .i =  2, .j =  1 }, .type = CC_STE_Capture },
//     { .step = { .i =  1, .j =  2 }, .type = CC_STE_Capture },

//     { .step = { .i = -1, .j =  2 }, .type = CC_STE_Capture },
//     { .step = { .i = -2, .j =  1 }, .type = CC_STE_Capture },

//     { .step = { .i = -2, .j = -1 }, .type = CC_STE_Capture },
//     { .step = { .i = -1, .j = -2 }, .type = CC_STE_Capture },

//     { .step = { .i =  1, .j = -2 }, .type = CC_STE_Capture },
//     { .step = { .i =  2, .j = -1 }, .type = CC_STE_Capture },

//     //
//     // long Unicorn steps

//     { .step = { .i =  4, .j =  1 }, .type = CC_STE_Capture },
//     { .step = { .i =  3, .j =  2 }, .type = CC_STE_Capture },
//     { .step = { .i =  2, .j =  3 }, .type = CC_STE_Capture },
//     { .step = { .i =  1, .j =  4 }, .type = CC_STE_Capture },

//     { .step = { .i = -1, .j =  4 }, .type = CC_STE_Capture },
//     { .step = { .i = -2, .j =  3 }, .type = CC_STE_Capture },
//     { .step = { .i = -3, .j =  2 }, .type = CC_STE_Capture },
//     { .step = { .i = -4, .j =  1 }, .type = CC_STE_Capture },

//     { .step = { .i = -4, .j = -1 }, .type = CC_STE_Capture },
//     { .step = { .i = -3, .j = -2 }, .type = CC_STE_Capture },
//     { .step = { .i = -2, .j = -3 }, .type = CC_STE_Capture },
//     { .step = { .i = -1, .j = -4 }, .type = CC_STE_Capture },

//     { .step = { .i =  1, .j = -4 }, .type = CC_STE_Capture },
//     { .step = { .i =  2, .j = -3 }, .type = CC_STE_Capture },
//     { .step = { .i =  3, .j = -2 }, .type = CC_STE_Capture },
//     { .step = { .i =  4, .j = -1 }, .type = CC_STE_Capture },

//     CC_TYPED_STEP_INVALID,
// };

CcTypedStep const CC_STEPS_ALL_LIGHT_SCOUT[ CC_STEPS_ALL_SCOUT_SIZE ] = {
    // light Pawn steps
    { .step = { .i =  0, .j =  1 }, .type = CC_STE_Movement },
    { .step = { .i = -1, .j =  0 }, .type = CC_STE_Movement },
    { .step = { .i =  1, .j =  0 }, .type = CC_STE_Movement },

    // dark Pawn capture-steps
    { .step = { .i = -1, .j = -1 }, .type = CC_STE_Capture },
    { .step = { .i =  1, .j = -1 }, .type = CC_STE_Capture },

    CC_TYPED_STEP_INVALID,
};

CcTypedStep const CC_STEPS_ALL_DARK_SCOUT[ CC_STEPS_ALL_SCOUT_SIZE ] = {
    // dark Pawn steps
    { .step = { .i =  0, .j = -1 }, .type = CC_STE_Movement },
    { .step = { .i = -1, .j =  0 }, .type = CC_STE_Movement },
    { .step = { .i =  1, .j =  0 }, .type = CC_STE_Movement },

    // light Pawn capture-steps
    { .step = { .i = -1, .j =  1 }, .type = CC_STE_Capture },
    { .step = { .i =  1, .j =  1 }, .type = CC_STE_Capture },

    CC_TYPED_STEP_INVALID,
};

bool cc_is_step_valid( CcTypedStep ts, CcTypedStep const steps[], size_t steps_len__d ) {
    if ( !CC_TYPED_STEP_IS_VALID( ts ) ) return false;

    for ( size_t k = 0;
          (steps_len__d == CC_STEPS_LEN_INVALID_DATA_TERMINATED) || (k < steps_len__d);
          ++k ) {
        CcTypedStep p = steps[ k ];

        if ( !CC_TYPED_STEP_IS_VALID( p ) ) return false; // break; // This is fine, as long as there is nothing after the loop.

        if ( cc_typed_step_is_equal( ts, p ) ) return true;
    }

    return false;
}

// TODO :: FIX
//
// bool cc_is_same_color( CcPieceEnum piece, CcTypedStep pos ) {
//     if ( cc_piece_is_light( piece ) && CC_IS_FIELD_LIGHT( pos.i, pos.j ) )
//         return true;

//     if ( cc_piece_is_dark( piece ) && CC_IS_FIELD_DARK( pos.i, pos.j ) )
//         return true;

//     return false;
// }

// bool cc_is_step_found( CcTypedStep step, CcPosLink * steps ) {
//     if ( !steps ) return false;

//     if ( !CC_POS_IS_VALID( step ) ) return false;

//     CcPosLink * pl = steps;

//     while ( pl ) {
//         if ( CC_POS_IS_EQUAL( pl->pos, step ) )
//             return true;

//         pl = pl->next;
//     }

//     return false;
// }

// bool cc_convert_steps_to_pos_link( CcTypedStep const steps[],
//                                    size_t steps_len__d,
//                                    CcPosLink ** steps__iod_a ) {
//     if ( !steps__iod_a ) return false;

//     CcPosLink * pl__t = NULL;

//     for ( size_t k = 0;
//           (steps_len__d == CC_STEPS_LEN_INVALID_DATA_TERMINATED) || (k < steps_len__d);
//           ++k ) {
//         CcTypedStep p = steps[ k ];

//         if ( !CC_POS_IS_VALID( p ) ) break;

//         if ( !cc_pos_link_append( &pl__t, p ) ) {
//             cc_pos_link_free_all( &pl__t );
//             return false;
//         }
//     }

//     // Ownership transfer.
//     if ( !cc_pos_link_extend( steps__iod_a, &pl__t ) ) {
//         cc_pos_link_free_all( &pl__t );
//         cc_pos_link_free_all( steps__iod_a );
//         return false;
//     }

//     return true;
// }

// CcPptLink * cc_join_ppt_links( CcPptLink ** ppt_link__iod,
//                                CcPptLink ** ppt_link__n ) {
//     if ( !ppt_link__iod ) return NULL;
//     if ( !ppt_link__n ) return NULL;

//     if ( !*ppt_link__n ) return *ppt_link__iod;

//     if ( !*ppt_link__iod ) {
//         // Ownership transfer.
//         *ppt_link__iod = *ppt_link__n;
//         *ppt_link__n = NULL;

//         return *ppt_link__iod;
//     }

//     CcPptLink * last = *ppt_link__iod;
//     CC_FASTFORWARD( last );

//     CcPptLink * first = *ppt_link__n;

//     if ( cc_pos_is_equal( last->ppt.pos, first->ppt.pos ) ) {
//         if ( last->ppt.piece != first->ppt.piece )
//             return NULL;

//         if ( last->ppt.tag != first->ppt.tag )
//             return NULL;

//         // Position, piece, and tag are all the same,
//         // so we drop extra location, not needed anymore.
//         CcPptLink * to_free = first;
//         first = first->next;

//         CC_FREE( to_free ); // This is fine, as long as CcPptLink doesn't have pointer(s) to other structs/unions that need to be free()-ed.
//     }

//     // Ownership transfer.
//     last->next = first;
//     *ppt_link__n = NULL;

//     return last->next;
// }


// bool cc_is_pawn_step( CcVariantEnum variant, CcPieceEnum piece, CcTypedStep step ) {
//     if ( cc_variant_has_sideways_pawns( variant ) ) {
//         if ( piece == CC_PE_LightPawn )
//             return CC_LIGHT_SIDEWAYS_PAWN_STEP_IS_VALID( step );
//         else if ( piece == CC_PE_DarkPawn )
//             return CC_DARK_SIDEWAYS_PAWN_STEP_IS_VALID( step );
//     } else {
//         if ( piece == CC_PE_LightPawn )
//             return CC_LIGHT_PAWN_STEP_IS_VALID( step );
//         else if ( piece == CC_PE_DarkPawn )
//             return CC_DARK_PAWN_STEP_IS_VALID( step );
//     }

//     return false;
// }

// bool cc_is_pawn_capture_step( CcPieceEnum piece, CcTypedStep step ) {
//     if ( piece == CC_PE_LightPawn )
//         return CC_LIGHT_PAWN_CAPTURE_STEP_IS_VALID( step );
//     else if ( piece == CC_PE_DarkPawn )
//         return CC_DARK_PAWN_CAPTURE_STEP_IS_VALID( step );

//     return false;
// }

// bool cc_is_scout_capture_step( CcPieceEnum piece, CcTypedStep step ) {
//     if ( piece == CC_PE_LightScout )
//         return CC_LIGHT_SCOUT_CAPTURE_STEP_IS_VALID( step );
//     else if ( piece == CC_PE_DarkScout )
//         return CC_DARK_SCOUT_CAPTURE_STEP_IS_VALID( step );

//     return false;
// }

// bool cc_is_shaman_capture_step( CcPieceEnum piece, CcTypedStep step ) {
//     if ( piece == CC_PE_LightShaman )
//         return CC_LIGHT_SHAMAN_CAPTURE_STEP_IS_VALID( step );
//     else if ( piece == CC_PE_DarkShaman )
//         return CC_DARK_SHAMAN_CAPTURE_STEP_IS_VALID( step );

//     return false;
// }

// bool cc_is_capture_step( CcVariantEnum variant,
//                          CcPieceEnum activator,
//                          CcPieceEnum piece,
//                          CcTypedStep step,
//                          CcTypedStep step_2 ) {
//     if ( !CC_PIECE_IS_VALID( piece ) ) return false;

//     if ( !cc_pos_is_valid( step ) ) return false;
//     if ( cc_pos_is_static_step( step ) ) return false;

//     if ( CC_PIECE_IS_PAWN( piece ) )
//         return cc_is_pawn_capture_step( piece, step );
//     else if ( CC_PIECE_IS_SHAMAN( piece ) )
//         return cc_is_shaman_capture_step( piece, step );
//     else if ( CC_PIECE_IS_WAVE( piece ) ) {
//         if ( !CC_PIECE_IS_ACTIVATOR( activator ) ) return false;

//         return cc_is_capture_step( variant, CC_PE_None, activator, step, step_2 );
//     } else if ( CC_PIECE_IS_MONOLITH( piece ) )
//         return false; // Monolith can't capture.
//     else if ( CC_PIECE_IS_STAR( piece ) )
//         return false; // Star can't capture.
//     else if ( CC_PIECE_IS_STARCHILD( piece ) )
//         return false; // Starchild can't capture.

//     else if ( CC_PIECE_IS_KING( piece ) )
//         return CC_KING_STEP_IS_VALID( step );
//     else if ( CC_PIECE_IS_KNIGHT( piece ) )
//         return CC_KNIGHT_STEP_IS_VALID( step );
//     else if ( CC_PIECE_IS_BISHOP( piece ) )
//         return CC_BISHOP_STEP_IS_VALID( step );
//     else if ( CC_PIECE_IS_QUEEN( piece ) )
//         return CC_QUEEN_STEP_IS_VALID( step );

//     else if ( CC_PIECE_IS_PYRAMID( piece ) )
//         return CC_PYRAMID_STEP_IS_VALID( step );
//     else if ( CC_PIECE_IS_UNICORN( piece ) ) {
//         return ( ( CC_UNICORN_SHORT_STEP_IS_VALID( step )
//                    && CC_UNICORN_LONG_STEP_IS_VALID( step_2 ) )
//                || ( CC_UNICORN_LONG_STEP_IS_VALID( step )
//                     && CC_UNICORN_SHORT_STEP_IS_VALID( step_2 ) ) );
//     } else if ( CC_PIECE_IS_STAR( piece ) ) {
//         if ( !CC_PIECE_CAN_ACTIVATE_STAR( activator ) )
//             return false;

//         return CC_STAR_STEP_IS_VALID( step );
//     } else if ( CC_PIECE_IS_CENTAUR( piece ) ) {
//         return ( ( CC_CENTAUR_SHORT_STEP_IS_VALID( step )
//                    && CC_CENTAUR_LONG_STEP_IS_VALID( step_2 ) )
//                || ( CC_CENTAUR_LONG_STEP_IS_VALID( step )
//                     && CC_CENTAUR_SHORT_STEP_IS_VALID( step_2 ) ) );
//     } else if ( CC_PIECE_IS_SCOUT( piece ) )
//         return cc_is_scout_capture_step( piece, step );
//     else if ( CC_PIECE_IS_GRENADIER( piece ) ) {
//         return CC_GRENADIER_CAPTURE_STEP_IS_VALID( step );
//     } else if ( CC_PIECE_IS_SERPENT( piece ) )
//         return CC_SERPENT_STEP_IS_VALID( step );

//     return false;
// }


// bool cc_is_step_miracle( CcPieceEnum piece, CcTypedStep step ) {
//     if ( CC_PIECE_IS_STARCHILD( piece ) )
//         return CC_STARCHILD_MIRACLE_STEP_IS_VALID( step );

//     return false;
// }


// static bool cc_starting_steps_pawn( CcVariantEnum variant,
//                                     CcPieceEnum piece,
//                                     CcPosLink ** starting_steps__e_a ) {
//     // if ( !starting_steps__e_a ) return false;
//     // if ( *starting_steps__e_a ) return false;

//     // if ( CC_PIECE_IS_PAWN( piece ) ) return false;

//     if ( cc_variant_has_sideways_pawns( variant ) ) {
//         if ( piece == CC_PE_LightPawn )
//             return cc_convert_steps_to_pos_link( CC_STEPS_ALL_LIGHT_SIDEWAYS_PAWN,
//                                                  CC_STEPS_ALL_SIDEWAYS_PAWN_LEN,
//                                                  starting_steps__e_a );

//         if ( piece == CC_PE_DarkPawn )
//             return cc_convert_steps_to_pos_link( CC_STEPS_ALL_DARK_SIDEWAYS_PAWN,
//                                                  CC_STEPS_ALL_SIDEWAYS_PAWN_LEN,
//                                                  starting_steps__e_a );
//     } else {
//         if ( piece == CC_PE_LightPawn )
//             return cc_convert_steps_to_pos_link( CC_STEPS_ALL_LIGHT_PAWN,
//                                                  CC_STEPS_ALL_PAWN_LEN,
//                                                  starting_steps__e_a );

//         if ( piece == CC_PE_DarkPawn )
//             return cc_convert_steps_to_pos_link( CC_STEPS_ALL_DARK_PAWN,
//                                                  CC_STEPS_ALL_PAWN_LEN,
//                                                  starting_steps__e_a );
//     }

//     return false;
// }

// static bool cc_starting_steps_unicorn( CcPieceEnum piece,
//                                        CcTypedStep pos,
//                                        CcPosLink ** starting_steps__e_a ) {
//     // if ( !starting_steps__e_a ) return false;
//     // if ( *starting_steps__e_a ) return false;

//     // if ( CC_PIECE_IS_UNICORN( piece ) ) return false;

//     if ( cc_is_same_color( piece, pos ) ) {
//         return cc_convert_steps_to_pos_link( CC_STEPS_SHORT_UNICORN,
//                                              CC_STEPS_SHORT_UNICORN_LEN,
//                                              starting_steps__e_a );
//     } else {
//         return cc_convert_steps_to_pos_link( CC_STEPS_LONG_UNICORN,
//                                              CC_STEPS_LONG_UNICORN_LEN,
//                                              starting_steps__e_a );
//     }
// }

// static bool cc_starting_steps_centaur( CcPieceEnum piece,
//                                        CcTypedStep pos,
//                                        CcPosLink ** starting_steps__e_a ) {
//     // if ( !starting_steps__e_a ) return false;
//     // if ( *starting_steps__e_a ) return false;

//     // if ( CC_PIECE_IS_CENTAUR( piece ) ) return false;

//     if ( cc_is_same_color( piece, pos ) ) {
//         return cc_convert_steps_to_pos_link( CC_STEPS_SHORT_CENTAUR,
//                                              CC_STEPS_SHORT_CENTAUR_LEN,
//                                              starting_steps__e_a );
//     } else {
//         return cc_convert_steps_to_pos_link( CC_STEPS_LONG_CENTAUR,
//                                              CC_STEPS_LONG_CENTAUR_LEN,
//                                              starting_steps__e_a );
//     }
// }

// static bool cc_starting_steps_scout( CcPieceEnum piece,
//                                      CcPosLink ** starting_steps__e_a ) {
//     // if ( !starting_steps__e_a ) return false;
//     // if ( *starting_steps__e_a ) return false;

//     // if ( CC_PIECE_IS_SCOUT( piece ) ) return false;

//     if ( piece == CC_PE_LightScout ) {
//         return cc_convert_steps_to_pos_link( CC_STEPS_ALL_LIGHT_SCOUT,
//                                              CC_STEPS_ALL_SCOUT_SIZE,
//                                              starting_steps__e_a );
//     } else if ( piece == CC_PE_DarkScout ) {
//         return cc_convert_steps_to_pos_link( CC_STEPS_ALL_DARK_SCOUT,
//                                              CC_STEPS_ALL_SCOUT_SIZE,
//                                              starting_steps__e_a );
//     }

//     return false;
// }

// static bool cc_starting_steps_starchild( CcVariantEnum variant,
//                                          /* CcPieceEnum piece, */
//                                          CcTypedStep pos,
//                                          CcPosLink ** starting_steps__e_a ) {
//     // if ( !starting_steps__e_a ) return false;
//     // if ( *starting_steps__e_a ) return false;

//     // if ( CC_PIECE_IS_STARCHILD( piece ) ) return false;

//     int field_color = // field_color is opposite color of currently occupied field.
//         CC_IS_FIELD_LIGHT( pos.i, pos.j ) ? CC_FIELD_COLOR_DARK
//                                           : CC_FIELD_COLOR_LIGHT;
//     size_t board_size = cc_variant_board_size( variant );

//     for ( int j = 0; (size_t)j < board_size; ++j ) {
//         for ( int i = 0; (size_t)i < board_size; ++i ) {
//             if ( CC_IS_FIELD_COLOR( i, j, field_color ) ) {
//                 if ( !cc_pos_link_append( starting_steps__e_a, cc_pos( i, j ) ) ) {
//                     cc_pos_link_free_all( starting_steps__e_a );
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
//                         CcPosLink ** starting_steps__e_a ) {
//     if ( !starting_steps__e_a ) return false;
//     if ( *starting_steps__e_a ) return false;

//     if ( CC_PIECE_IS_VALID( piece ) ) return false;

//     if ( CC_PIECE_IS_PAWN( piece ) ) {
//         return cc_starting_steps_pawn( variant, piece, starting_steps__e_a );
//     } else if ( CC_PIECE_IS_KNIGHT( piece ) ) {
//         return cc_convert_steps_to_pos_link( CC_STEPS_KNIGHT, CC_STEPS_KNIGHT_LEN, starting_steps__e_a );
//     } else if ( CC_PIECE_IS_BISHOP( piece ) ) {
//         return cc_convert_steps_to_pos_link( CC_STEPS_BISHOP, CC_STEPS_BISHOP_LEN, starting_steps__e_a );
//     } else if ( CC_PIECE_IS_ROOK( piece ) ) {
//         return cc_convert_steps_to_pos_link( CC_STEPS_ROOK, CC_STEPS_ROOK_LEN, starting_steps__e_a );
//     } else if ( CC_PIECE_IS_QUEEN( piece ) ) {
//         return cc_convert_steps_to_pos_link( CC_STEPS_QUEEN, CC_STEPS_QUEEN_LEN, starting_steps__e_a );
//     } else if ( CC_PIECE_IS_KING( piece ) ) {
//         return cc_convert_steps_to_pos_link( CC_STEPS_KING, CC_STEPS_KING_LEN, starting_steps__e_a );

//     } else if ( CC_PIECE_IS_PEGASUS( piece ) ) {
//         return cc_convert_steps_to_pos_link( CC_STEPS_PEGASUS, CC_STEPS_PEGASUS_LEN, starting_steps__e_a );
//     } else if ( CC_PIECE_IS_PYRAMID( piece ) ) {
//         return cc_convert_steps_to_pos_link( CC_STEPS_PYRAMID, CC_STEPS_PYRAMID_LEN, starting_steps__e_a );
//     } else if ( CC_PIECE_IS_UNICORN( piece ) ) {
//         return cc_starting_steps_unicorn( piece, pos, starting_steps__e_a );
//     } else if ( CC_PIECE_IS_WAVE( piece ) ) {
//         if ( !CC_PIECE_IS_ACTIVATOR( activator ) ) return false;
//         return cc_starting_steps( variant, activator, CC_PE_None, pos, starting_steps__e_a );
//     } else if ( CC_PIECE_IS_STAR( piece ) ) {
//         if ( !CC_PIECE_IS_STARCHILD( activator ) ) return false;
//         return cc_convert_steps_to_pos_link( CC_STEPS_STAR, CC_STEPS_STAR_LEN, starting_steps__e_a );

//     } else if ( CC_PIECE_IS_CENTAUR( piece ) ) {
//         return cc_starting_steps_centaur( piece, pos, starting_steps__e_a );
//     } else if ( CC_PIECE_IS_GRENADIER( piece ) ) {
//         return cc_convert_steps_to_pos_link( CC_STEPS_ALL_GRENADIER, CC_STEPS_ALL_GRENADIER_LEN, starting_steps__e_a );
//     } else if ( CC_PIECE_IS_SCOUT( piece ) ) {
//         return cc_starting_steps_scout( piece, starting_steps__e_a );

//     } else if ( CC_PIECE_IS_SERPENT( piece ) ) {
//         return cc_convert_steps_to_pos_link( CC_STEPS_ALL_SERPENT, CC_STEPS_ALL_SERPENT_LEN, starting_steps__e_a );
//     } else if ( CC_PIECE_IS_SHAMAN( piece ) ) {
//         return cc_convert_steps_to_pos_link( CC_STEPS_ALL_SHAMAN, CC_STEPS_ALL_SHAMAN_LEN, starting_steps__e_a );
//     } else if ( CC_PIECE_IS_MONOLITH( piece ) ) {
//         return cc_convert_steps_to_pos_link( CC_STEPS_STARTING_MONOLITH, CC_STEPS_STARTING_MONOLITH_LEN, starting_steps__e_a );
//     } else if ( CC_PIECE_IS_STARCHILD( piece ) ) {
//         return cc_starting_steps_starchild( variant, pos, starting_steps__e_a );
//     }

//     return false;
// }
//
// TODO :: FIX
