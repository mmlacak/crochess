// Copyright (c) 2024, 2025 Mario Mlačak, mmlacak@gmail.com
// Licensed under GNU GPL v3+ license. See LICENSING, COPYING files for details.

// #include "cc_defines.h"
// #include "cc_math.h"

#include "cc_pos_utils.h"
#include "cc_path_utils.h"
#include "cc_checks.h"


// TODO :: DELETE
//
// CcMaybeBoolEnum cc_path_side_effect( CcChessboard * cb,
//                                      CcPosDesc moving,
//                                      CcPosDesc encounter,
//                                      CcSideEffect * side_effect__io ) {
//     if ( !side_effect__io ) return CC_MBE_Void;
//     if ( !CC_SIDE_EFFECT_TYPE_IS_ENUMERATOR( *side_effect__io ) ) return CC_MBE_Void;
//
//     if ( !CC_PIECE_IS_VALID( moving.piece ) ) return CC_MBE_Void;
//     if ( !CC_PIECE_IS_ENUMERATOR( encounter.piece ) ) return CC_MBE_Void;
//
//     if ( !CC_TAG_IS_ENUMERATOR( moving.tag ) ) return CC_MBE_Void;
//     if ( !CC_TAG_IS_ENUMERATOR( encounter.tag ) ) return CC_MBE_Void;
//
//     if ( !cc_chessboard_is_pos_on_board( cb, moving.pos.i, moving.pos.j ) ) return CC_MBE_Void;
//     if ( !cc_chessboard_is_pos_on_board( cb, encounter.pos.i, encounter.pos.j ) ) return CC_MBE_Void;
//
//     if ( *side_effect__io == CC_SETE_None ) {
//         *side_effect__io = CC_SETE_Capture;
//
//         if ( CC_PIECE_CAN_CAPTURE( moving ) &&
//              CC_PIECE_CAN_BE_CAPTURED( encounter ) ) return CC_MBE_True;
//     }
//
//     if ( *side_effect__io == CC_SETE_Capture ) {
//         *side_effect__io = CC_SETE_Displacement;
//
//         if ( CC_PIECE_CAN_DISPLACE( moving ) &&
//              CC_PIECE_CAN_BE_DISPLACED( encounter ) ) return CC_MBE_True;
//
//         // Trance-journey has to be handled separately.
//     }
//
//     if ( *side_effect__io == CC_SETE_Displacement ) {
//         *side_effect__io = CC_SETE_EnPassant;
//
//         if ( CC_PIECE_CAN_CAPTURE_EN_PASSANT( moving ) &&
//              CC_PIECE_CAN_BE_CAPTURED_EN_PASSANT( encounter ) ) return CC_MBE_True;
//     }
//
//     if ( *side_effect__io == CC_SETE_EnPassant ) {
//         *side_effect__io = CC_SETE_Castle;
//
//         if ( CC_PIECE_CAN_CASTLE( moving ) &&
//              CC_PIECE_CAN_CASTLE( encounter ) ) return CC_MBE_True;
//     }
//
//     if ( *side_effect__io == CC_SETE_Castle ) {
//         *side_effect__io = CC_SETE_Promotion;
//
//         if ( CC_PIECE_CAN_BE_PROMOTED( moving ) ) return CC_MBE_True;
//         else if ( CC_PIECE_CAN_PROMOTE( moving ) &&
//                   CC_PIECE_CAN_BE_PROMOTED( encounter ) ) return CC_MBE_True;
//     }
//
//
//     // TODO
//
//
//     return CC_MBE_Void; // TODO :: FIX
// }
//
// TODO :: DELETE

bool cc_path_side_effect( CcChessboard * cb,
                          CcPosDesc moving,
                          CcPosDesc encounter,
                          bool is_trance_journey,
                          CcPos displacement,
                          CcSideEffectLink ** side_effect_link__o_a ) {
    if ( !side_effect_link__o_a ) return false;
    if ( *side_effect_link__o_a ) return false;

    if ( !CC_PIECE_IS_VALID( moving.piece ) ) return false;
    if ( !CC_PIECE_IS_ENUMERATOR( encounter.piece ) ) return false;

    if ( !CC_TAG_IS_ENUMERATOR( moving.tag ) ) return false;
    if ( !CC_TAG_IS_ENUMERATOR( encounter.tag ) ) return false;

    CcLosingTagType ltt = cc_convert_tag_to_losing( encounter.tag );
    if ( !cc_check_piece_can_lose_tag( encounter.piece, ltt ) ) return false;

    if ( !cc_chessboard_is_pos_on_board( cb, moving.pos.i, moving.pos.j ) ) return false;
    if ( !cc_chessboard_is_pos_on_board( cb, encounter.pos.i, encounter.pos.j ) ) return false;

    if ( CC_PIECE_CAN_CAPTURE( moving.piece ) &&
            CC_PIECE_CAN_BE_CAPTURED( encounter.piece ) &&
            cc_piece_has_different_owner( moving.piece, encounter.piece ) ) {
        CcSideEffect se = cc_side_effect_capture( encounter.piece, ltt );
        CcSideEffectLink * se__w = cc_side_effect_link_append( side_effect_link__o_a, se );
        if ( !se__w ) {
            cc_side_effect_link_free_all( side_effect_link__o_a );
            return false;
        }
    }

    if ( !cc_chessboard_is_pos_on_board( cb, displacement.i, displacement.j ) ) return false;

    if ( is_trance_journey ) { // TODO :: enum --> none, displacement, capture, double trance-journey
        if ( !CC_PIECE_IS_SHAMAN( moving.piece ) ) {
            cc_side_effect_link_free_all( side_effect_link__o_a );
            return false;
        }

        if ( CC_PIECE_CAN_BE_DISPLACED_TRANCE_JOURNEY( encounter.piece ) ) {
            CcSideEffect se = cc_side_effect_displacement( encounter.piece, ltt, displacement );
            CcSideEffectLink * se__w = cc_side_effect_link_append( side_effect_link__o_a, se );
            if ( !se__w ) {
                cc_side_effect_link_free_all( side_effect_link__o_a );
                return false;
            }
        }
    } else if ( CC_PIECE_CAN_DISPLACE( moving.piece ) &&
                CC_PIECE_CAN_BE_DISPLACED( encounter.piece ) ) {
        CcSideEffect se = cc_side_effect_displacement( encounter.piece, ltt, displacement );
        CcSideEffectLink * se__w = cc_side_effect_link_append( side_effect_link__o_a, se );
        if ( !se__w ) {
            cc_side_effect_link_free_all( side_effect_link__o_a );
            return false;
        }
    }

    if ( CC_PIECE_CAN_CAPTURE_EN_PASSANT( moving.piece ) &&
            ( encounter.piece == CC_PE_None ) ) {
         // CC_PIECE_CAN_BE_CAPTURED_EN_PASSANT( encounter.piece ) &&
         // cc_piece_has_different_owner( moving.piece, encounter.piece ) ) {

        // TODO :: find distant private with en-passant tag

    }


    return false; // TODO :: FIX
}


static CcPathLink * _cc_path_segment_one_step__new( CcSideEffect side_effect,
                                                    CcGame * game,
                                                    CcPosDesc moving,
                                                    CcPos current_pos,
                                                    CcMomentum momentum,
                                                    CcTypedStep step ) {
    CcPos field = cc_pos_add( current_pos, step.step, 1 );
    CcStep * steps__t = NULL;

    CcPieceEnum piece = CC_PE_None;
    CcTagEnum tag = CC_TE_None;
    CcMomentum m = momentum;
    CcStep * step__w = NULL;

    while ( cc_chessboard_is_pos_on_board( game->chessboard, field.i, field.j ) ) {
        CcMaybeBoolEnum result = cc_momentum_calc_next( &m, 1 );

        if ( !CC_MAYBE_BOOL_IS_VALID( result ) ) { // Void --> error.
            cc_step_free_all( &steps__t );
            return NULL;
        }

        if ( result == CC_MBE_False ) break; // There is not enough momentum to move any further.

        step__w = cc_step_append_next_no_side_effect( &steps__t, field );
        if ( !step__w ) {
            cc_step_free_all( &steps__t );
            return NULL;
        }

        CcPieceEnum encounter = cc_chessboard_get_piece( game->chessboard, field.i, field.j );
        if ( encounter != CC_PE_None ) break; // Caller checks all possible interactions, including transparency; removes field of encounter if there are none.

        field = cc_pos_add( field, step.step, 1 );
    }

    if ( !steps__t ) return NULL;

    CcPathLink * pl__a = cc_path_link__new( side_effect, &steps__t, piece, tag, m );
    if ( !pl__a ) {
        cc_step_free_all( &steps__t );
        return NULL;
    }

    return pl__a;
}

static CcPathLink * _cc_path_one_step__new( CcSideEffect side_effect,
                                            CcGame * game,
                                            CcPosDesc moving,
                                            CcPos current_pos,
                                            CcMomentum momentum,
                                            CcTypedStep step ) {
    if ( CC_SIDE_EFFECT_TYPE_TERMINATES_PLY( side_effect.type ) ) {
        // Side-effect is terminal, no fields are visited after this point; so path node contains nothing valid, beside side-effect.
        CcPathLink * terminal__a = cc_path_link__new( side_effect, NULL, CC_PE_None, CC_TE_None, CC_MOMENTUM_CAST_SPENT );
        return terminal__a;
    } else if ( side_effect.type == CC_SETE_Capture ) {
        if ( !CC_PIECE_IS_SHAMAN( moving.piece ) ) {
            // Capturing for pieces other than Shaman is terminal, no fields are visited after this point; so path node contains nothing valid, beside side-effect.
            CcPathLink * capture__a = cc_path_link__new( side_effect, NULL, CC_PE_None, CC_TE_None, CC_MOMENTUM_CAST_SPENT );
            return capture__a;
        }
    }

    CcPathLink * pl__a = _cc_path_segment_one_step__new( side_effect, game, moving, current_pos, momentum, step );
    if ( !pl__a ) return NULL;
    if ( !pl__a->steps ) return pl__a; // Just a sanity check, should not happen.

    // TODO :: handle side-effects @ encountered piece

    CcStep * steps = pl__a->steps;

    CC_FASTFORWARD( steps );
    CcPos last = steps->field;

    CcPosDesc encounter = cc_convert_pos_to_pos_desc( game->chessboard, last );
    bool is_blocked = cc_check_piece_is_blocked_at( game->chessboard, moving.piece, last );
    CcMomentum m = pl__a->momentum;

    // TODO :: check all possible interactions, including transparency; remove field of encounter if there are none.

    // TODO :: if !is_blocked --> transparency

    // if ( ( encounter.piece == CC_PE_None ) ||
    //         ( !cc_check_piece_is_blocked_at( game->chessboard, moving.piece, last ) ) ) {
    //     step__w = cc_pos_link_append( &steps__t, last );
    //     if ( !step__w ) {
    //         cc_step_free_all( &steps__t );
    //         return NULL;
    //     }
    // } else
    //     break;

    if ( encounter.piece != CC_PE_None ) {
        // TODO :: check if pieces can interact

        if ( cc_check_piece_can_capture_at( game->chessboard, moving.piece, last ) ) {

        }

        if ( cc_check_piece_can_diverge_at( game->chessboard, moving.piece, m.momentum, CC_PE_None, last ) ) {

        }

    }

    return pl__a;
}

CcPathLink * cc_path_tree_one_step__new( CcGame * game,
                                         CcPosDesc moving ) {
    if ( !game ) return NULL;
    if ( !game->chessboard ) return NULL;

    if ( !CC_PIECE_IS_ONE_STEP( moving.piece ) ) return NULL; // TODO :: add Wave
    if ( !cc_chessboard_is_pos_on_board( game->chessboard, moving.pos.i, moving.pos.j ) ) return NULL;
    if ( !CC_TAG_IS_ENUMERATOR( moving.tag ) ) return NULL;

    // [!] Piece, and its tag, might not be at moving.pos position on chessboard,
    //     e.g. if already activated (transitioning problem); for everything
    //     else chessboard should be correct.

    CcPos field = moving.pos;

    CcStep * steps__t = cc_step_next_no_side_effect__new( field );
    if ( !steps__t ) return NULL;

    CcSideEffect se = cc_side_effect_none();
    CcMomentum m = CC_MOMENTUM_CAST_INITIAL;

    CcPathLink * pl__a = cc_path_link__new( se, &steps__t, moving.piece, moving.tag, m );
    if ( !pl__a ) {
        cc_step_free_all( &steps__t );
        return NULL;
    }

    bool sideways_pawns = CC_VARIANT_HAS_SIDEWAYS_PAWNS( game->chessboard->type );
    bool is_same_color = cc_pos_piece_are_same_color( field, moving.piece );
    CcTypedStep const * step = NULL;

    while ( cc_iter_piece_steps( moving.piece,
                                 sideways_pawns,
                                 is_same_color, // Only for Unicorn, Centaur is not single-step piece.
                                 CC_SDE_BothDiagonals, // Serpent is not single-step piece, although this is correct for initial step.
                                 CC_STE_None, // No filtering by step types.
                                 &step ) ) {

    }



    return pl__a;
}
