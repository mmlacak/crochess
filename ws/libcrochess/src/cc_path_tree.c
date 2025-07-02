// Copyright (c) 2024, 2025 Mario Mlačak, mmlacak@gmail.com
// Licensed under GNU GPL v3+ license. See LICENSING, COPYING files for details.

// #include "cc_defines.h"
// #include "cc_math.h"

#include "cc_pos_utils.h"
#include "cc_checks.h"
#include "cc_path_utils.h"
#include "cc_path_tree.h"


bool cc_path_side_effect( CcChessboard * cb,
                          CcPosDesc moving_from,
                          CcActivationDesc act_desc,
                          CcPosDesc encounter,
                          CcTranceJourneyTypeEnum trance_journey_type,
                          CcPos displacement,
                          CcSideEffectLink ** side_effect_link__o_a ) {
    if ( !cb ) return false;

    if ( !side_effect_link__o_a ) return false;
    if ( *side_effect_link__o_a ) return false;

    if ( !CC_PIECE_IS_VALID( moving_from.piece ) ) return false;
    if ( !CC_PIECE_IS_ENUMERATOR( encounter.piece ) ) return false;

    if ( !cc_chessboard_is_pos_on_board( cb, moving_from.pos.i, moving_from.pos.j ) ) return false;
    if ( !cc_chessboard_is_pos_on_board( cb, encounter.pos.i, encounter.pos.j ) ) return false;

    if ( CC_PIECE_CAN_CAPTURE( moving_from.piece ) &&
            CC_PIECE_CAN_BE_CAPTURED( encounter.piece ) &&
            ( cc_piece_has_different_owner( moving_from.piece, encounter.piece ) ||
              CC_TRANCE_JOURNEY_TYPE_IS_ANY_CAPTURE( trance_journey_type ) ) ) {
        CcSideEffect se = cc_side_effect_capture( encounter.piece );
        CcSideEffectLink * se__w = cc_side_effect_link_append( side_effect_link__o_a, se );
        if ( !se__w ) {
            cc_side_effect_link_free_all( side_effect_link__o_a );
            return false;
        }
    }

    if ( !cc_chessboard_is_pos_on_board( cb, displacement.i, displacement.j ) ) return false;

    if ( trance_journey_type == CC_TJTE_Displacement ) {
        if ( !CC_PIECE_IS_SHAMAN( moving_from.piece ) ) {
            cc_side_effect_link_free_all( side_effect_link__o_a );
            return false;
        }

        if ( CC_PIECE_CAN_BE_DISPLACED_TRANCE_JOURNEY( encounter.piece ) ) {
            CcSideEffect se = cc_side_effect_displacement( encounter.piece, displacement );
            CcSideEffectLink * se__w = cc_side_effect_link_append( side_effect_link__o_a, se );
            if ( !se__w ) {
                cc_side_effect_link_free_all( side_effect_link__o_a );
                return false;
            }
        }
    } else if ( trance_journey_type == CC_TJTE_None ) {
        if ( CC_PIECE_CAN_DISPLACE( moving_from.piece ) &&
                CC_PIECE_CAN_BE_DISPLACED( encounter.piece ) ) {
            CcSideEffect se = cc_side_effect_displacement( encounter.piece, displacement );
            CcSideEffectLink * se__w = cc_side_effect_link_append( side_effect_link__o_a, se );
            if ( !se__w ) {
                cc_side_effect_link_free_all( side_effect_link__o_a );
                return false;
            }
        }
    }

    if ( CC_PIECE_CAN_CAPTURE_EN_PASSANT( moving_from.piece ) &&
            ( encounter.piece == CC_PTE_None ) ) { // TODO :: or encountered piece can be activated
        CcPosDesc en_passant = CC_POS_DESC_CAST_INVALID;
        CcMaybeBoolEnum result = cc_find_en_passant_target( cb, moving_from.piece, act_desc, encounter.pos, &en_passant );
        if ( result == CC_MBE_True ) {
            CcSideEffect se = cc_side_effect_en_passant( en_passant.piece, en_passant.pos );
            CcSideEffectLink * se__w = cc_side_effect_link_append( side_effect_link__o_a, se );
            if ( !se__w ) {
                cc_side_effect_link_free_all( side_effect_link__o_a );
                return false;
            }
        }
    }


    return false; // TODO :: FIX
}


bool cc_path_segment_one_step( CcSideEffect side_effect,
                               CcPosDesc moving_from,
                               CcTypedStep step,
                               CcPathContext * path_ctx__io,
                               CcPathLink * pl__io ) {
    if ( !path_ctx__io ) return false;
    if ( !pl__io ) return false;

    if ( !CC_PIECE_IS_ONE_STEP( moving_from.piece ) ) return false;
    if ( !CC_TYPED_STEP_IS_VALID( step ) ) return false;
    if ( cc_path_context_is_legal( path_ctx__io, true, true ) != CC_MBE_True ) return false;

    cc_uint_t board_size = cc_variant_board_size( path_ctx__io->game__w->chessboard->type );
    if ( !CC_POS_IS_LEGAL( moving_from.pos, board_size ) ) return false;

    CcPos pos = moving_from.pos;
    if ( !cc_chessboard_is_pos_on_board( path_ctx__io->cb_current, pos.i, pos.j ) ) return false;

    CcStep * steps__t =
        path_ctx__io->ply_ctx.is_first_step ? cc_step_initial_no_side_effect__new( pos )
                                            : cc_step_next_no_side_effect__new( pos );
    if ( !steps__t ) return false;

    bool is_starting_pos = path_ctx__io->ply_ctx.is_first && path_ctx__io->ply_ctx.is_first_step;
    CcActivationDesc act_desc = path_ctx__io->ply_ctx.activation;
    CcPieceTagType encounter = CC_PTE_None;

    #define STEP_COUNT 1

    path_ctx__io->ply_ctx.is_first_step = false;

    do {
        pos = cc_pos_add_steps( pos, step.step, STEP_COUNT );

        if ( cc_chessboard_is_pos_on_board( path_ctx__io->cb_current, pos.i, pos.j ) ) {
            if ( cc_activation_desc_calc_next_momentum( &act_desc, STEP_COUNT ) != CC_MBE_True )
                break;

            encounter = cc_chessboard_get_piece( path_ctx__io->cb_current, pos.i, pos.j );

            CcStep * steps__w = cc_step_append_next_no_side_effect( &steps__t, pos );
            if ( !steps__w ) {
                cc_step_free_all( &steps__t );
                return false;
            }

            if ( encounter != CC_PTE_None ) break;
        } else
            break;
    } while ( cc_activation_desc_is_usable( act_desc, path_ctx__io->ply_ctx.is_first ) );

    CcPathLink * pl__w = cc_path_link_append( &pl__io, side_effect, &steps__t, encounter, act_desc );
    if ( !pl__w ) {
        cc_step_free_all( &steps__t );
        return false;
    }

    path_ctx__io->ply_ctx.activation = act_desc;

    // if ( !CC_PIECE_IS_NONE( encounter ) )
    //     break; // TODO :: side-effect --> fork | alt | sub

    return true;
}

bool cc_path_tree( CcSideEffect side_effect,
                   CcPosDesc moving_from,
                   CcPathContext * path_ctx__io,
                   CcPathLink * pl__io ) {
    if ( !path_ctx__io ) return false;
    if ( !pl__io ) return false;

    if ( cc_path_context_is_legal( path_ctx__io, true, false ) != CC_MBE_True ) return false;

    bool sideways_pawns = CC_VARIANT_HAS_SIDEWAYS_PAWNS( path_ctx__io->game__w->chessboard->type );
    bool is_same_color = cc_pos_piece_are_same_color( moving_from.pos, moving_from.piece );
    CcTypedStep const * step = NULL;

    if ( CC_PIECE_IS_ONE_STEP( moving_from.piece ) ) {
        while ( cc_iter_piece_steps( moving_from.piece,
                                     sideways_pawns,
                                     is_same_color, // Filler, Unicorn and Centaur are not one-step pieces.
                                     CC_SDE_BothDiagonals, // Filler, Serpent is not one-step piece.
                                     CC_STE_None, // No filtering by step types.
                                     &step ) ) {

        }
    } // TODO :: other (types of) pieces


    return false; // TODO :: FIX
}

bool cc_path_tree_init__new( CcGame * game,
                             CcPosDesc moving_from,
                             CcPathLink ** path_link__iod_a,
                             CcPathContext ** path_ctx__iod_a ) {
    if ( !game ) return false;
    if ( !game->chessboard ) return false;

    if ( !path_ctx__iod_a ) return false;
    if ( *path_ctx__iod_a ) return false;

    if ( !path_link__iod_a ) return false;
    if ( *path_link__iod_a ) return false;

    if ( !CC_PIECE_IS_ACTIVE( moving_from.piece ) ) return false;
    if ( !cc_chessboard_is_pos_on_board( game->chessboard, moving_from.pos.i, moving_from.pos.j ) ) return false;

    *path_ctx__iod_a = cc_path_context__new( game ); // Game ownership has not been transferred here.
    if ( !*path_ctx__iod_a ) return false;

    if ( !cc_path_context_init( *path_ctx__iod_a, moving_from, true ) ) {
        cc_path_context_free_all( path_ctx__iod_a );
        return false;
    }

    // <!> Piece, and its tag, might not be at moving_from.pos position on chessboard,
    //     e.g. if already activated (transitioning problem); for everything
    //     else chessboard should be correct.

    CcStep * steps__t = cc_step_next_no_side_effect__new( moving_from.pos );
    if ( !steps__t ) {
        cc_path_context_free_all( path_ctx__iod_a );
        return false;
    }

    CcSideEffect se = cc_side_effect_none();
    CcActivationDesc ad = CC_ACTIVATION_DESC_CAST_INITIAL; // Activation descriptor in path context is also initialized to the same.

    *path_link__iod_a = cc_path_link__new( se, &steps__t, moving_from.piece, ad );
    if ( !*path_link__iod_a ) {
        cc_path_context_free_all( path_ctx__iod_a );
        cc_step_free_all( &steps__t );
        return false;
    }

    return true;
}
