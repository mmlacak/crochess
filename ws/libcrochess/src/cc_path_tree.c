// Copyright (c) 2024, 2025 Mario Mlačak, mmlacak@gmail.com
// Licensed under GNU GPL v3+ license. See LICENSING, COPYING files for details.

// #include "cc_defines.h"
// #include "cc_math.h"

#include "cc_pos_utils.h"
#include "cc_checks.h"
#include "cc_path_utils.h"
#include "cc_path_tree.h"


bool cc_path_side_effect( CcPosDesc moving_from,
                          CcTypedStep last_step,
                          CcPosDesc encounter,
                          CcPathContext * path_ctx__io,
                          CcPathSideEffectLink ** side_effect_link__o_a ) {
    if ( !path_ctx__io ) return false;

    if ( !side_effect_link__o_a ) return false;
    if ( *side_effect_link__o_a ) return false;

    CcChessboard * cb = path_ctx__io->cb_current;
    if ( !cb ) return false;

    CcActivationDesc act_desc = path_ctx__io->ply_ctx.act_desc;
    CcMultiStagePlyTypeEnum ms = path_ctx__io->move_ctx.multi_stage;

    if ( !CC_PIECE_IS_VALID( moving_from.piece ) ) return false;
    if ( !CC_PIECE_IS_ENUMERATOR( encounter.piece ) ) return false;

    if ( !cc_activation_desc_is_valid( act_desc, moving_from.piece, path_ctx__io->ply_ctx.is_first ) ) return false;

    if ( !cc_chessboard_is_pos_on_board( cb, moving_from.pos.i, moving_from.pos.j ) ) return false;
    if ( !cc_chessboard_is_pos_on_board( cb, encounter.pos.i, encounter.pos.j ) ) return false;

    // TODO :: FIX
    // if ( CC_MULTI_STAGE_PLY_TYPE_IS_TRANCE_CAPTURE( ms ) ) {
    //     // TODO
    // } else if ( ms == CC_MSPTE_TJ_Displacing ) {
    //     if ( !CC_PIECE_IS_SHAMAN( moving_from.piece ) ) {
    //         cc_side_effect_link_free_all( side_effect_link__o_a );
    //         return false;
    //     }

    //     CcPos displacement; // TODO :: FIX

    //     if ( CC_PIECE_CAN_BE_DISPLACED_TRANCE_JOURNEY( encounter.piece ) ) {
    //         CcSideEffect se = cc_side_effect_displacement( encounter.piece, displacement );
    //         CcPathSideEffectLink * se__w = cc_side_effect_link_append( side_effect_link__o_a, CC_PLNLE_Sub, se );
    //         if ( !se__w ) {
    //             cc_side_effect_link_free_all( side_effect_link__o_a );
    //             return false;
    //         }
    //     }
    // } else if ( ms == CC_MSPTE_None ) {
    //     if ( CC_PIECE_CAN_DISPLACE( moving_from.piece ) &&
    //             CC_PIECE_CAN_BE_DISPLACED( encounter.piece ) ) {
    //         CcPos displacement; // TODO :: FIX

    //         CcSideEffect se = cc_side_effect_displacement( encounter.piece, displacement );
    //         CcPathSideEffectLink * se__w = cc_side_effect_link_append( side_effect_link__o_a, CC_PLNLE_Sub, se );
    //         if ( !se__w ) {
    //             cc_side_effect_link_free_all( side_effect_link__o_a );
    //             return false;
    //         }
    //     }
    // }
    // TODO :: FIX

    if ( cc_check_piece_can_capture( moving_from.piece, encounter.piece ) ) {
        CcSideEffect se = cc_side_effect_capture( encounter.piece );
        CcPathSideEffectLink * se__w = cc_side_effect_link_append( side_effect_link__o_a, CC_PLNLE_Fork, se ); // TODO :: FIX :: CC_PLNLE_Fork
        if ( !se__w ) {
            cc_side_effect_link_free_all( side_effect_link__o_a );
            return false;
        }
    }

    if ( CC_PIECE_CAN_CAPTURE_EN_PASSANT( moving_from.piece ) &&
            ( encounter.piece == CC_PTE_None ) ) { // TODO :: or encountered piece can be activated
        CcPosDesc en_passant = CC_POS_DESC_CAST_INVALID;
        if ( cc_find_en_passant_target( cb, moving_from.piece, act_desc, path_ctx__io->ply_ctx.is_first, encounter.pos, &en_passant ) ) {
            CcSideEffect se = cc_side_effect_en_passant( en_passant.piece, en_passant.pos );
            CcPathSideEffectLink * se__w = cc_side_effect_link_append( side_effect_link__o_a, CC_PLNLE_Fork, se ); // TODO :: FIX :: CC_PLNLE_Fork
            if ( !se__w ) {
                cc_side_effect_link_free_all( side_effect_link__o_a );
                return false;
            }
        }
    }


    return false; // TODO :: FIX
}


CcPathLink * cc_path_segment_one_step__new( CcSideEffect side_effect,
                                            CcPosDesc moving_from,
                                            CcTypedStep step,
                                            CcPathContext * path_ctx__io ) {
    if ( !path_ctx__io ) return NULL;
    if ( !path_ctx__io->game__w ) return NULL;
    if ( !path_ctx__io->game__w->chessboard ) return NULL;
    if ( !path_ctx__io->cb_current ) return NULL;

    if ( path_ctx__io->game__w->chessboard->type != path_ctx__io->cb_current->type ) return NULL;
    if ( !CC_VARIANT_IS_VALID( path_ctx__io->cb_current->type ) ) return NULL;

    cc_uint_t board_size = cc_variant_board_size( path_ctx__io->cb_current->type );
    if ( !CC_IS_BOARD_SIZE_VALID( board_size ) ) return NULL;

    if ( !CC_PIECE_IS_ONE_STEP( moving_from.piece ) ) return NULL;
    if ( !cc_chessboard_is_pos_on_board( path_ctx__io->cb_current, moving_from.pos.i, moving_from.pos.j ) ) return NULL;
    if ( !CC_TYPED_STEP_IS_VALID( step ) ) return NULL;
    if ( !cc_path_context_is_legal( path_ctx__io, true, true ) ) return NULL;
    if ( !cc_activation_desc_is_valid( path_ctx__io->ply_ctx.act_desc, moving_from.piece, path_ctx__io->ply_ctx.is_first ) ) return NULL;

    CcPos pos = moving_from.pos;
    CcStep * steps__t = cc_step_initial_no_side_effect__new( pos );
    if ( !steps__t ) return NULL;

    bool is_starting_pos = path_ctx__io->ply_ctx.is_first;
    CcActivationDesc act_desc = path_ctx__io->ply_ctx.act_desc;
    CcActivationDesc ad = act_desc;
    CcPieceTagType encounter = CC_PTE_None;

    #define STEP_COUNT 1

    do {
        pos = cc_pos_add_steps( pos, step.step, STEP_COUNT );

        if ( cc_chessboard_is_pos_on_board( path_ctx__io->cb_current, pos.i, pos.j ) ) {
            if ( cc_activation_desc_calc_momentum( &ad, STEP_COUNT ) != CC_MBE_True )
                break;

            encounter = cc_chessboard_get_piece( path_ctx__io->cb_current, pos.i, pos.j );

            if ( encounter == CC_PTE_None ) {
                CcStep * steps__w = cc_step_append_next_no_side_effect( &steps__t, pos );
                if ( !steps__w ) {
                    cc_step_free_all( &steps__t );
                    return NULL;
                }
            } else
                break; // TODO :: add side-effects here
        } else
            break;

        act_desc = ad;
    } while ( cc_activation_desc_is_usable( act_desc, moving_from.piece, path_ctx__io->ply_ctx.is_first ) );

    CcPathLink * pl__a = cc_path_link__new( side_effect, &steps__t, encounter, act_desc );
    if ( !pl__a ) {
        cc_step_free_all( &steps__t );
        return NULL;
    }

    path_ctx__io->ply_ctx.act_desc = act_desc;

    // if ( !CC_PIECE_IS_NONE( encounter ) )
    //     break; // TODO :: side-effect --> fork | alt | sub

    return pl__a;
}

bool cc_path_tree( CcSideEffect side_effect,
                   CcPosDesc moving_from,
                   CcPathContext * path_ctx__io,
                   CcPathLink * pl__io ) {
    if ( !path_ctx__io ) return false;
    if ( !pl__io ) return false;

    if ( !cc_path_context_is_legal( path_ctx__io, true, false ) ) return false;

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

    CcSideEffect se = cc_side_effect_none();
    CcActivationDesc ad = CC_ACTIVATION_DESC_CAST_INITIAL; // Activation descriptor in path context is also initialized to the same.

    *path_link__iod_a = cc_path_link__new( se, NULL, moving_from.piece, ad );
    if ( !*path_link__iod_a ) {
        cc_path_context_free_all( path_ctx__iod_a );
        return false;
    }

    return true;
}
