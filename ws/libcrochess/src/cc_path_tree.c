// Copyright (c) 2024, 2025 Mario Mlačak, mmlacak@gmail.com
// Licensed under GNU GPL v3+ license. See LICENSING, COPYING files for details.

// #include "cc_defines.h"
// #include "cc_math.h"

#include "cc_pos_utils.h"
#include "cc_checks.h"
#include "cc_path_utils.h"
#include "cc_path_tree.h"


// todo :: TEST :: DELETE ???
//
// bool cc_path_side_effects( CcPosDesc moving_from,
//                            CcTypedStep last_step,
//                            CcPosDesc encounter,
//                            CcPathContext * path_ctx__io,
//                            CcPathSideEffectLink ** side_effect_link__o_a ) {
//     if ( !path_ctx__io ) return false;

//     if ( !side_effect_link__o_a ) return false;
//     if ( *side_effect_link__o_a ) return false;

//     CcChessboard * cb = path_ctx__io->cb_current;
//     if ( !cb ) return false;

//     CcActivationDesc act_desc = path_ctx__io->ply_ctx.act_desc;
//     CcMultiStagePlyTypeEnum ms = path_ctx__io->move_ctx.multi_stage;

//     if ( !CC_PIECE_IS_VALID( moving_from.piece ) ) return false;
//     if ( !CC_PIECE_IS_ENUMERATOR( encounter.piece ) ) return false;

//     if ( !cc_activation_desc_is_legal( act_desc, moving_from.piece, path_ctx__io->ply_ctx.is_first ) ) return false;

//     if ( !cc_chessboard_is_pos_on_board( cb, moving_from.pos.i, moving_from.pos.j ) ) return false;
//     if ( !cc_chessboard_is_pos_on_board( cb, encounter.pos.i, encounter.pos.j ) ) return false;

//     CcPathSideEffectLink * sel__t = NULL;
//     CcPathNodeLinkageEnum plnle = CC_PNLE_NoLinkage; // CC_PNLE_Next;

//     //
//     // Terminal side-effects.

//     if ( cc_check_piece_can_capture( moving_from.piece, encounter.piece ) ) {
//         CcSideEffect se = cc_side_effect_capture( encounter.piece );
//         CcPathSideEffectLink * se__w = cc_path_side_effect_link_append( &sel__t, plnle, se );
//         if ( !se__w ) {
//             cc_path_side_effect_link_free_all( &sel__t );
//             return false;
//         }
//         plnle = CC_PNLE_Next;
//     }

//     // todo :: other terminating side-effects

//     //
//     // Non-terminal side-effects.

//     plnle = CC_PNLE_Next;

//     if ( cc_check_piece_can_step_over( moving_from.piece, encounter.piece, act_desc.momentum ) ) {
//         CcSideEffect se = cc_side_effect_transparency( encounter.piece );
//         CcPathSideEffectLink * se__w = cc_path_side_effect_link_append( &sel__t, plnle, se );
//         if ( !se__w ) {
//             cc_path_side_effect_link_free_all( &sel__t );
//             return false;
//         }
//         plnle = CC_PNLE_Fork;
//     }

//     if ( CC_PIECE_CAN_CAPTURE_EN_PASSANT( moving_from.piece ) &&
//             ( encounter.piece == CC_PTE_None ) ) { // TODO :: or encountered piece can be activated
//         CcPosDesc en_passant = CC_POS_DESC_CAST_INVALID;
//         if ( cc_find_en_passant_target( cb, moving_from.piece, act_desc, path_ctx__io->ply_ctx.is_first, encounter.pos, &en_passant ) ) {
//             CcSideEffect se = cc_side_effect_en_passant( en_passant.piece, en_passant.pos );
//             CcPathSideEffectLink * se__w = cc_path_side_effect_link_append( &sel__t, plnle, se );
//             if ( !se__w ) {
//                 cc_path_side_effect_link_free_all( &sel__t );
//                 return false;
//             }
//             plnle = CC_PNLE_Fork;
//         }
//     }


//     // todo :: FIX
//     // if ( CC_MULTI_STAGE_PLY_TYPE_IS_TRANCE_CAPTURE( ms ) ) {
//     //     // todo
//     // } else if ( ms == CC_MSPTE_TJ_Displacing ) {
//     //     if ( !CC_PIECE_IS_SHAMAN( moving_from.piece ) ) {
//     //         cc_path_side_effect_link_free_all( &sel__t );
//     //         return false;
//     //     }

//     //     CcPos displacement; // todo :: FIX

//     //     if ( CC_PIECE_CAN_BE_DISPLACED_TRANCE_JOURNEY( encounter.piece ) ) {
//     //         CcSideEffect se = cc_side_effect_displacement( encounter.piece, displacement );
//     //         CcPathSideEffectLink * se__w = cc_path_side_effect_link_append( &sel__t, CC_PNLE_Sub, se );
//     //         if ( !se__w ) {
//     //             cc_path_side_effect_link_free_all( &sel__t );
//     //             return false;
//     //         }
//     //     }
//     // } else if ( ms == CC_MSPTE_None ) {
//     //     if ( CC_PIECE_CAN_DISPLACE( moving_from.piece ) &&
//     //             CC_PIECE_CAN_BE_DISPLACED( encounter.piece ) ) {
//     //         CcPos displacement; // todo :: FIX

//     //         CcSideEffect se = cc_side_effect_displacement( encounter.piece, displacement );
//     //         CcPathSideEffectLink * se__w = cc_path_side_effect_link_append( &sel__t, CC_PNLE_Sub, se );
//     //         if ( !se__w ) {
//     //             cc_path_side_effect_link_free_all( &sel__t );
//     //             return false;
//     //         }
//     //     }
//     // }
//     // todo :: FIX


//     // todo :: other non-terminating side-effects



//     *side_effect_link__o_a = sel__t; // Ownership transfer, do not free( sel__t ).

//     return true;
// }

// bool cc_path_segment( CcSideEffect side_effect,
//                       CcPosDesc moving_from,
//                       CcTypedStep step_1,
//                       CcTypedStep step_2,
//                       CcPathContext * path_ctx__io,
//                       CcPathNode ** path_node__o_a,
//                       CcPathSideEffectLink ** side_effect_link__o_a ) {
//     if ( !path_ctx__io ) return false;
//     if ( !path_ctx__io->game__w ) return false;
//     if ( !path_ctx__io->game__w->chessboard ) return false;
//     if ( !path_ctx__io->cb_current ) return false;

//     if ( !path_node__o_a ) return false;
//     if ( *path_node__o_a ) return false;

//     if ( !side_effect_link__o_a ) return false;
//     if ( *side_effect_link__o_a ) return false;

//     if ( path_ctx__io->game__w->chessboard->type != path_ctx__io->cb_current->type ) return false;
//     if ( !CC_VARIANT_IS_VALID( path_ctx__io->cb_current->type ) ) return false;

//     cc_uint_t board_size = cc_variant_board_size( path_ctx__io->cb_current->type );
//     if ( !CC_IS_BOARD_SIZE_VALID( board_size ) ) return false;

//     if ( !CC_PIECE_IS_ONE_STEP( moving_from.piece ) ) return false;
//     if ( !cc_chessboard_is_pos_on_board( path_ctx__io->cb_current, moving_from.pos.i, moving_from.pos.j ) ) return false;
//     if ( !cc_path_context_is_legal( path_ctx__io, true, true ) ) return false;
//     if ( !cc_activation_desc_is_legal( path_ctx__io->ply_ctx.act_desc, moving_from.piece, path_ctx__io->ply_ctx.is_first ) ) return false;

//     CcPos pos = moving_from.pos;
//     CcPathSideEffectLink * sel__t = NULL;
//     CcStep * steps__t = cc_step_initial_no_side_effect__new( pos );
//     if ( !steps__t ) return false;

//     bool is_starting_pos = path_ctx__io->ply_ctx.is_first;
//     CcActivationDesc act_desc = path_ctx__io->ply_ctx.act_desc;
//     CcActivationDesc ad = act_desc;
//     CcPieceTagType encounter = CC_PTE_None;
//     CcTypedStep step = CC_TYPED_STEP_CAST_INVALID;

//     #define STEP_COUNT 1

//     do {
//         if ( !CC_TYPED_STEP_IS_VALID( step = cc_fetch_piece_step( moving_from.piece, pos, ad.activator, board_size, step_1, step_2 ) ) ) {
//             cc_step_free_all( &steps__t );
//             // cc_path_side_effect_link_free_all( &sel__t ); // Not needed, first time side-effect is allocated, the loop is exited at [1].
//             return false;
//         }

//         pos = cc_pos_add_steps( pos, step.step, STEP_COUNT );

//         if ( cc_chessboard_is_pos_on_board( path_ctx__io->cb_current, pos.i, pos.j ) ) {
//             if ( cc_activation_desc_calc_momentum( &ad, STEP_COUNT ) != CC_MBE_True )
//                 break;

//             encounter = cc_chessboard_get_piece( path_ctx__io->cb_current, pos.i, pos.j );

//             if ( encounter == CC_PTE_None ) {
//                 CcStep * steps__w = cc_step_append_next_no_side_effect( &steps__t, pos );
//                 if ( !steps__w ) {
//                     cc_step_free_all( &steps__t );
//                     // cc_path_side_effect_link_free_all( &sel__t ); // Not needed, first time side-effect is allocated, the loop is exited at [1].
//                     return false;
//                 }
//             } else {
//                 CcPosDesc encounter_pd = CC_POS_DESC_CAST( pos, encounter );

//                 if ( !cc_path_side_effects( moving_from, step, encounter_pd, path_ctx__io, &sel__t ) ) {
//                     cc_step_free_all( &steps__t );
//                     cc_path_side_effect_link_free_all( &sel__t );
//                     return false;
//                 }

//                 if ( sel__t ) {
//                     CcStep * steps__w = cc_step_append_next_no_side_effect( &steps__t, pos );
//                     if ( !steps__w ) {
//                         cc_step_free_all( &steps__t );
//                         cc_path_side_effect_link_free_all( &sel__t );
//                         return false;
//                     }
//                 }

//                 break; // [1]
//             }
//         } else
//             break;

//         act_desc = ad;
//     } while ( cc_activation_desc_is_usable( act_desc, moving_from.piece, path_ctx__io->ply_ctx.is_first ) );

//     CcPathNode * pn__t = cc_path_node__new( side_effect, &steps__t, encounter, act_desc );
//     if ( !pn__t ) {
//         cc_step_free_all( &steps__t );
//         cc_path_side_effect_link_free_all( &sel__t );
//         return false;
//     }

//     path_ctx__io->ply_ctx.act_desc = act_desc;

//     // if ( !CC_PIECE_IS_NONE( encounter ) )
//     //     break; // TODO :: side-effect --> fork | alt | sub

//     *path_node__o_a = pn__t; // Ownership transfer, do not free( pn__t ).
//     *side_effect_link__o_a = sel__t; // Ownership transfer, do not free( sel__t ).

//     return true;
// }
//
// todo :: TEST :: DELETE ???


bool cc_path_side_effects( CcPosDesc moving_from,
                           CcTypedStep step_1,
                           CcTypedStep step_2,
                           CcPosDesc encounter,
                           CcPathContext * path_ctx__io,
                           CcPathNode ** path_node__io_a ) {
    if ( !path_node__io_a ) return false;
    if ( !*path_node__io_a ) return false;
    if ( !( (*path_node__io_a)->steps ) ) return false;

    if ( !cc_path_context_is_legal( path_ctx__io, true, true ) ) return false;
    if ( cc_path_node_is_leaf( *path_node__io_a ) != CC_MBE_True ) return false;

    CcChessboard * cb = path_ctx__io->cb_current;
    if ( !cb ) return false;

    CcPathContext * path_ctx__a = cc_path_context_duplicate_all__new( path_ctx__io );
    if ( !path_ctx__a ) return false;

    CcActivationDesc * ad__w = &( path_ctx__a->ply_ctx.act_desc );
    CcMultiStagePlyTypeEnum ms = path_ctx__a->move_ctx.multi_stage;

    if ( !CC_PIECE_IS_VALID( moving_from.piece ) ) return false;
    if ( !CC_PIECE_IS_ENUMERATOR( encounter.piece ) ) return false; // Encountered piece == none, if en passant, for example.

    if ( !cc_activation_desc_is_legal( *ad__w, moving_from.piece, path_ctx__a->ply_ctx.is_first ) ) return false;

    if ( !cc_chessboard_is_pos_on_board( cb, moving_from.pos.i, moving_from.pos.j ) ) return false;
    if ( !cc_chessboard_is_pos_on_board( cb, encounter.pos.i, encounter.pos.j ) ) return false;

    CcStep * steps__t = NULL;
    bool is_encounter_step_appended = false;
    CcActivationDesc ad_encounter_step_appended = *ad__w;

    CcPathNode * pn_step_over__t = NULL;
    // todo :: add path nodes for other side-effects

    #define STEP_COUNT (1)

    //
    // Terminal side-effects.

    if ( cc_check_piece_can_capture( moving_from.piece, encounter.piece ) ) {
        if ( cc_activation_desc_calc_momentum( ad__w, STEP_COUNT ) != CC_MBE_True ) {
            cc_step_free_all( &steps__t );
            return false;
        }

        ad_encounter_step_appended = *ad__w;

        CcStep * step__w = cc_step_capture_append( &steps__t, CC_SLTE_Next, encounter.pos, encounter.piece );
        if ( !step__w ) {
            cc_step_free_all( &steps__t );
            return false;
        }

        is_encounter_step_appended = true;
    }

    // TODO :: other terminating side-effects

    //
    // Non-terminal side-effects.

    if ( cc_check_piece_can_step_over( moving_from.piece, encounter.piece, ad__w->momentum ) ) {
        CcSideEffect se = cc_side_effect_transparency( encounter.piece );
        CcPosDesc moving_from_transparency = CC_POS_DESC_CAST( encounter.pos, moving_from.piece );

        if ( !is_encounter_step_appended ) {
            if ( cc_activation_desc_calc_momentum( ad__w, STEP_COUNT ) != CC_MBE_True ) {
                cc_step_free_all( &steps__t );
                return false;
            }

            ad_encounter_step_appended = *ad__w;

            CcStep * step__w = cc_step_append_next_no_side_effect( &steps__t, encounter.pos );
            if ( !step__w ) {
                cc_step_free_all( &steps__t );
                return false;
            }

            is_encounter_step_appended = true;
        }

        if ( !cc_path_segment( se, moving_from_transparency, step_1, step_2, path_ctx__a, &pn_step_over__t ) ) {
            cc_path_node_free_all( &pn_step_over__t );
            return false;
        }
    }

    if ( CC_PIECE_CAN_CAPTURE_EN_PASSANT( moving_from.piece ) &&
            ( encounter.piece == CC_PTE_None ) ) { // TODO :: or encountered piece can be activated
        CcPosDesc en_passant = CC_POS_DESC_CAST_INVALID;
        if ( cc_find_en_passant_target( cb, moving_from.piece, *ad__w, path_ctx__a->ply_ctx.is_first, encounter.pos, &en_passant ) ) {
            CcSideEffect se = cc_side_effect_en_passant( en_passant.piece, en_passant.pos );

        }
    }


    // TODO :: FIX
    // if ( CC_MULTI_STAGE_PLY_TYPE_IS_TRANCE_CAPTURE( ms ) ) {
    //     // TODO
    // } else if ( ms == CC_MSPTE_TJ_Displacing ) {
    //     if ( !CC_PIECE_IS_SHAMAN( moving_from.piece ) ) {
    //         cc_path_side_effect_link_free_all( &sel__t );
    //         return false;
    //     }

    //     CcPos displacement; // TODO :: FIX

    //     if ( CC_PIECE_CAN_BE_DISPLACED_TRANCE_JOURNEY( encounter.piece ) ) {
    //         CcSideEffect se = cc_side_effect_displacement( encounter.piece, displacement );
    //         CcPathSideEffectLink * se__w = cc_path_side_effect_link_append( &sel__t, CC_PNLE_Sub, se );
    //         if ( !se__w ) {
    //             cc_path_side_effect_link_free_all( &sel__t );
    //             return false;
    //         }
    //     }
    // } else if ( ms == CC_MSPTE_None ) {
    //     if ( CC_PIECE_CAN_DISPLACE( moving_from.piece ) &&
    //             CC_PIECE_CAN_BE_DISPLACED( encounter.piece ) ) {
    //         CcPos displacement; // TODO :: FIX

    //         CcSideEffect se = cc_side_effect_displacement( encounter.piece, displacement );
    //         CcPathSideEffectLink * se__w = cc_path_side_effect_link_append( &sel__t, CC_PNLE_Sub, se );
    //         if ( !se__w ) {
    //             cc_path_side_effect_link_free_all( &sel__t );
    //             return false;
    //         }
    //     }
    // }
    // TODO :: FIX


    // TODO :: other non-terminating side-effects


    if ( steps__t ) {
        CcStep * step__w = cc_step_extend( &( (*path_node__io_a)->steps ), &steps__t );
        if ( !step__w ) {
            cc_step_free_all( &steps__t );
            cc_path_node_free_all( &pn_step_over__t );
            return false;
        }
    }

    if ( pn_step_over__t ) {
        pn_step_over__t->act_desc = *ad__w; // TODO :: RETHINK :: is ok ? in all situations ?

        if ( !cc_path_node_add_fork( path_node__io_a, &pn_step_over__t ) ) { // Ownership transfer, do not free( pn_step_over__t ) afterwards.
            cc_step_free_all( &steps__t );
            cc_path_node_free_all( &pn_step_over__t );
            return false;
        }
    }

    path_ctx__io->ply_ctx.act_desc = *ad__w; // path_ctx__a->ply_ctx.act_desc;

    if ( is_encounter_step_appended ) {
        ( *path_node__io_a )->act_desc = ad_encounter_step_appended;
    }

    cc_path_context_free_all( &path_ctx__a );

    return true;
}

bool cc_path_segment( CcSideEffect side_effect,
                      CcPosDesc moving_from,
                      CcTypedStep step_1,
                      CcTypedStep step_2,
                      CcPathContext * path_ctx__io,
                      CcPathNode ** path_node__o_a ) {
    if ( !path_node__o_a ) return false;
    if ( *path_node__o_a ) return false;

    if ( !CC_PIECE_IS_ONE_STEP( moving_from.piece ) ) return false;
    if ( !cc_path_context_is_legal( path_ctx__io, true, true ) ) return false;

    if ( !cc_chessboard_is_pos_on_board( path_ctx__io->cb_current, moving_from.pos.i, moving_from.pos.j ) ) return false;
    if ( !cc_activation_desc_is_legal( path_ctx__io->ply_ctx.act_desc, moving_from.piece, path_ctx__io->ply_ctx.is_first ) ) return false;

    cc_uint_t board_size = cc_variant_board_size( path_ctx__io->cb_current->type );
    CcPos pos = moving_from.pos;

    CcStep * steps__t = NULL;
    // If side-effect is valid --> this is movement from encounter, not initial piece movement -->
    // this step is already added to the parent path node steps, in cc_path_side_effects() --> skip it here.
    bool skip_first = CC_SIDE_EFFECT_TYPE_IS_VALID( side_effect.type );
    if ( !skip_first ) {
        steps__t = cc_step_initial_no_side_effect__new( pos );
        if ( !steps__t ) return false;
    }

    bool is_starting_pos = path_ctx__io->ply_ctx.is_first;
    CcActivationDesc act_desc = path_ctx__io->ply_ctx.act_desc;
    CcActivationDesc ad = act_desc;
    CcPieceTagType encounter = CC_PTE_None;
    CcTypedStep step = CC_TYPED_STEP_CAST_INVALID;

    #define STEP_COUNT (1)

    CcPathNode * pn__t = cc_path_node__new( side_effect, NULL, encounter, act_desc );
    if ( !pn__t ) {
        cc_step_free_all( &steps__t );
        return false;
    }

    do {
        if ( !CC_TYPED_STEP_IS_VALID( step = cc_fetch_piece_step( moving_from.piece, pos, ad.activator, board_size, step_1, step_2 ) ) ) {
            cc_path_node_free_all( &pn__t );
            cc_step_free_all( &steps__t );
            return false;
        }

        pos = cc_pos_add_steps( pos, step.step, STEP_COUNT );

        if ( cc_chessboard_is_pos_on_board( path_ctx__io->cb_current, pos.i, pos.j ) ) {
            if ( cc_activation_desc_calc_momentum( &ad, STEP_COUNT ) != CC_MBE_True )
                break;

            encounter = cc_chessboard_get_piece( path_ctx__io->cb_current, pos.i, pos.j );

            if ( encounter == CC_PTE_None ) {
                CcStep * steps__w = cc_step_append_next_no_side_effect( &steps__t, pos );
                if ( !steps__w ) {
                    cc_path_node_free_all( &pn__t );
                    cc_step_free_all( &steps__t );
                    return false;
                }
            } else
                break;
        } else
            break;

        act_desc = ad;
    } while ( cc_activation_desc_is_usable( act_desc, moving_from.piece, path_ctx__io->ply_ctx.is_first ) );

    pn__t->steps = steps__t; // Ownership transfer, do not free( steps__t ).
    // steps__t = NULL; // Not needed, not used anymore.

    pn__t->encounter = encounter;
    pn__t->act_desc = act_desc;

    path_ctx__io->ply_ctx.act_desc = act_desc;

    if ( !CC_PIECE_IS_NONE( encounter ) ) {
        CcPosDesc encounter_pd = CC_POS_DESC_CAST( pos, encounter );

        if ( !cc_path_side_effects( moving_from, step_1, step_2, encounter_pd, path_ctx__io, &pn__t ) ) {
            cc_path_node_free_all( &pn__t );
            // cc_step_free_all( &steps__t ); // Not needed, ownership already transferred.
            return false;
        }
    }

    *path_node__o_a = pn__t; // Ownership transfer, do not free( pn__t ).

    return true;
}

bool cc_path_tree( CcSideEffect side_effect,
                   CcPosDesc moving_from,
                   CcPathContext * path_ctx__io,
                   CcPathNode * pl__io ) {
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
                             CcPathNode ** path_link__iod_a,
                             CcPathContext ** path_ctx__iod_a ) {
    if ( !game ) return false;
    if ( !game->chessboard ) return false;

    cc_uint_t board_size = cc_chessboard_get_size( game->chessboard );
    if ( !CC_POS_DESC_IS_LEGAL( moving_from, board_size ) ) return false;

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

    *path_link__iod_a = cc_path_node__new( se, NULL, moving_from.piece, ad );
    if ( !*path_link__iod_a ) {
        cc_path_context_free_all( path_ctx__iod_a );
        return false;
    }

    return true;
}
