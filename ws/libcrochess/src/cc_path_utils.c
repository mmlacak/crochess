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

static CcPathLink * _cc_path_segment_one_step__new( CcGame * game,
                                                    CcPosDesc moving,
                                                    CcTypedStep step,
                                                    CcSideEffect side_effect,
                                                    CcMomentum momentum ) {
    CcPos field = cc_pos_add( moving.pos, step.step, 1 );
    CcPosLink * fields__t = NULL;

    CcPieceEnum piece = CC_PE_None;
    CcTagEnum tag = CC_TE_None;
    CcMomentum m = momentum;
    CcPosLink * field__w = NULL;

    while ( cc_chessboard_is_pos_on_board( game->chessboard, field.i, field.j ) ) {
        CcMaybeBoolEnum result = cc_momentum_calc_next( &m, 1 );

        if ( !CC_MAYBE_BOOL_IS_VALID( result ) ) { // Void --> error.
            cc_pos_link_free_all( &fields__t );
            return NULL;
        }

        if ( result == CC_MBE_False ) break; // There is not enough momentum to move any further.

        CcPosDesc encounter = cc_convert_pos_to_pos_desc( game->chessboard, field );
        bool is_blocked = cc_check_piece_is_blocked_at( game->chessboard, moving.piece, field );

        if ( ( encounter.piece == CC_PE_None ) || ( !is_blocked ) ) { // TODO :: check if other interactions are possible?
            field__w = cc_pos_link_append( &fields__t, field );
            if ( !field__w ) {
                cc_pos_link_free_all( &fields__t );
                return NULL;
            }

            if ( is_blocked ) break; // Interactions other than transparency are to be forked from encountered piece.
        } else
            break;

        field = cc_pos_add( field, step.step, 1 );
    }

    if ( !fields__t ) return NULL;

    CcPathLink * pl__a = cc_path_link__new( side_effect, fields__t, piece, tag, m );
    if ( !pl__a ) {
        cc_pos_link_free_all( &fields__t );
        return NULL;
    }

    return pl__a;
}

static CcPathLink * _cc_path_one_step__new( CcGame * game,
                                            CcPosDesc moving,
                                            CcTypedStep step,
                                            CcSideEffect side_effect,
                                            CcMomentum momentum ) {
    if ( side_effect.type == CC_SETE_Capture ) {
        // Capture is terminal, no fields are visited after this point; so path node contains nothing valid, beside side-effect.
        CcPathLink * capture__a = cc_path_link__new( side_effect, NULL, CC_PE_None, CC_TE_None, CC_MOMENTUM_CAST_SPENT );
        return capture__a;
    }

    CcPathLink * pl__a = _cc_path_segment_one_step__new( game, moving, step, side_effect, momentum );
    if ( !pl__a ) return NULL;

    // TODO :: handle side-effects @ encountered piece

    CcPosLink * fields = pl__a->fields;
    if ( !fields ) {
        cc_path_link_free_all( &pl__a );
        return NULL;
    }

    CC_FASTFORWARD( fields );

    CcPos field = fields->pos;

    CcPosDesc encounter = cc_convert_pos_to_pos_desc( game->chessboard, field );
    CcMomentum m = pl__a->momentum;

    // if ( ( encounter.piece == CC_PE_None ) ||
    //         ( !cc_check_piece_is_blocked_at( game->chessboard, moving.piece, field ) ) ) {
    //     field__w = cc_pos_link_append( &fields__t, field );
    //     if ( !field__w ) {
    //         cc_pos_link_free_all( &fields__t );
    //         return NULL;
    //     }
    // } else
    //     break;

    if ( encounter.piece != CC_PE_None ) {
        // TODO :: check if pieces can interact

        if ( cc_check_piece_can_capture_at( game->chessboard, moving.piece, field ) ) {

        }

        if ( cc_check_piece_can_diverge_at( game->chessboard, moving.piece, m.momentum, CC_PE_None, field ) ) {

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

    CcPosLink * fields__t = cc_pos_link__new( field );
    if ( !fields__t ) return NULL;

    CcSideEffect se = cc_side_effect_none();
    CcMomentum m = CC_MOMENTUM_CAST_INITIAL;

    CcPathLink * pl__a = cc_path_link__new( se, fields__t, moving.piece, moving.tag, m );
    if ( !pl__a ) {
        cc_pos_link_free_all( &fields__t );
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
