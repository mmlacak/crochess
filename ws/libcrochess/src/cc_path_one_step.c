// Copyright (c) 2025 Mario Mlačak, mmlacak@gmail.com
// Licensed under GNU GPL v3+ license. See LICENSING, COPYING files for details.

// #include "cc_checks.h"
//
// #include "cc_typed_step_defs.h"
// #include "cc_pos_utils.h"

#include "cc_path_one_step.h"


// TODO :: DELETE
// static CcPathNode * _cc_path_segment_one_step__new( CcSideEffect side_effect,
//                                                     CcPathContext * path_ctx,
//                                                     CcPosDesc moving,
//                                                     CcPos current_pos,
//                                                     CcActivationDesc act_desc,
//                                                     CcTypedStep step ) {
//     CcPos field = cc_pos_add_steps( current_pos, step.step, 1 );
//     CcStep * steps__t = NULL;
//
//     CcPieceTagType piece = CC_PTE_None;
//     CcActivationDesc ad = act_desc;
//     CcStep * step__w = NULL;
//
//     while ( cc_chessboard_is_pos_on_board( path_ctx->game__w->chessboard, field.i, field.j ) ) {
//         CcMaybeBoolEnum result = cc_activation_desc_calc_momentum( &ad, 1 );
//
//         if ( !CC_MAYBE_BOOL_IS_VALID( result ) ) { // Void --> error.
//             cc_step_free_all( &steps__t );
//             return NULL;
//         }
//
//         if ( result == CC_MBE_False ) break; // There is not enough momentum to move any further.
//
//         step__w = cc_step_append_next_no_side_effect( &steps__t, field );
//         if ( !step__w ) {
//             cc_step_free_all( &steps__t );
//             return NULL;
//         }
//
//         CcPieceTagType encounter = cc_chessboard_get_piece( path_ctx->game__w->chessboard, field.i, field.j );
//         if ( encounter != CC_PTE_None ) break; // Caller checks all possible interactions, including transparency; removes field of encounter if there are none.
//
//         field = cc_pos_add_steps( field, step.step, 1 );
//     }
//
//     if ( !steps__t ) return NULL;
//
//     CcPathNode * pl__a = cc_path_node__new( side_effect, &steps__t, piece, ad );
//     if ( !pl__a ) {
//         cc_step_free_all( &steps__t );
//         return NULL;
//     }
//
//     return pl__a;
// }
// TODO :: DELETE

// TODO :: DELETE
// static CcPathNode * _cc_path_one_step__new( CcSideEffect side_effect,
//                                             CcPathContext * path_ctx,
//                                             CcPosDesc moving,
//                                             CcPos current_pos,
//                                             CcActivationDesc act_desc,
//                                             CcTypedStep step ) {
//     if ( CC_SIDE_EFFECT_TYPE_TERMINATES_PLY( side_effect.type ) ) {
//         // Side-effect is terminal, no fields are visited after this point; so path node contains nothing valid, beside side-effect.
//         CcPathNode * terminal__a = cc_path_node__new( side_effect, NULL, CC_PTE_None, CC_TE_None, CC_ACTIVATION_DESC_CAST_SPENT );
//         return terminal__a;
//     } else if ( side_effect.type == CC_SETE_Capture ) {
//         if ( !CC_PIECE_IS_SHAMAN( moving.piece ) ) {
//             // Capturing for pieces other than Shaman is terminal, no fields are visited after this point; so path node contains nothing valid, beside side-effect.
//             CcPathNode * capture__a = cc_path_node__new( side_effect, NULL, CC_PTE_None, CC_TE_None, CC_ACTIVATION_DESC_CAST_SPENT );
//             return capture__a;
//         }
//     }
//
//     CcPathNode * pl__a = _cc_path_segment_one_step__new( side_effect, path_ctx, moving, current_pos, act_desc, step );
//     if ( !pl__a ) return NULL;
//     if ( !pl__a->steps ) return pl__a; // Just a sanity check, should not happen.
//
//     // TODO :: handle side-effects @ encountered piece
//
//     CcStep * steps = pl__a->steps;
//
//     CC_FASTFORWARD( steps );
//     CcPos last = steps->field;
//
//     CcPosDesc encounter = cc_convert_pos_to_pos_desc( path_ctx->game__w->chessboard, last );
//     bool is_blocked = cc_check_piece_is_blocked_at( path_ctx->game__w->chessboard, moving.piece, last );
//     CcActivationDesc ad = pl__a->act_desc;
//
//     // TODO :: check all possible interactions, including transparency; remove field of encounter if there are none.
//
//     // TODO :: if !is_blocked --> transparency
//
//     // if ( ( encounter.piece == CC_PTE_None ) ||
//     //         ( !cc_check_piece_is_blocked_at( path_ctx->game__w->chessboard, moving.piece, last ) ) ) {
//     //     step__w = cc_pos_link_append( &steps__t, last );
//     //     if ( !step__w ) {
//     //         cc_step_free_all( &steps__t );
//     //         return NULL;
//     //     }
//     // } else
//     //     break;
//
//     if ( encounter.piece != CC_PTE_None ) {
//         // TODO :: check if pieces can interact
//
//         if ( cc_check_piece_can_capture_at( path_ctx->game__w->chessboard, moving.piece, last ) ) {
//
//         }
//
//         if ( cc_check_piece_can_diverge_at( path_ctx->game__w->chessboard, moving.piece, ad.momentum, CC_PTE_None, last ) ) {
//
//         }
//
//     }
//
//     return pl__a;
// }
// TODO :: DELETE
