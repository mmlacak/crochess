// Copyright (c) 2024 Mario Mlačak, mmlacak@gmail.com
// Licensed under GNU GPL v3+ license. See LICENSING, COPYING files for details.

#include "cc_checks.h"

#include "cc_path_single_step.h"

/**
    @file cc_path_single_step.c
    @brief Path generating, filtering for single-step pieces.
*/


static bool cc_pos_desc_link_append_pos( CcChessboard * cb,
                                    CcPos destination,
                                    CcPosDescLink ** pptl__iod_a ) {
    // Not needed, static + known caller.
    // if ( !cb ) return false;
    // if ( !pptl__iod_a ) return false;

    CcPosDesc pd = cc_convert_pos_to_ppt( cb, destination );

    if ( !cc_pos_desc_link_append( pptl__iod_a, pd ) ) return false;

    return true;
}


static bool cc_path_pawn( CcChessboard * cb,
                          CcPosDesc pawn,
                          CcPos from_pos,
                          CcPosDescLink * already_traversed__d,
                          CcPathLink ** path__e_a ) {
    // Not needed, already checked in the only caller, i.e. cc_path_single_step().
    // if ( !cb ) return false;
    // if ( !cc_pos_desc_is_valid( pawn ) ) return false;
    // if ( !path__e_a ) return false;
    // if ( *path__e_a ) return false;
    // if ( !cc_chessboard_is_pos_on_board( cb, from_pos.i, from_pos.j ) ) return false;

    if ( !CC_PIECE_IS_PAWN( pawn.piece ) ) return false;

    bool is_pawn_light = cc_piece_is_light( pawn.piece );

    CcPosDescLink * pptl__t = NULL;
    bool result = true;

    CcTypedStep const * step = NULL;
    CcTypedStep const * guard = NULL;

    if ( cc_variant_has_sideways_pawns( cb->type ) ) {
        step = is_pawn_light ? CC_STEPS_LIGHT_SIDEWAYS_PAWN : CC_STEPS_DARK_SIDEWAYS_PAWN;
        guard = step + CC_STEPS_SIDEWAYS_PAWN_LEN;
    } else {
        step = is_pawn_light ? CC_STEPS_LIGHT_PAWN : CC_STEPS_DARK_PAWN;
        guard = step + CC_STEPS_PAWN_LEN;
    }

    // if ( !step || !guard ) return false; // <!> Not needed, as long as both vars are certainly initialized above.

    CcTypedStep const * s = step;
    bool do_append = false;

    // TODO :: static promotion

    while ( s && ( s <= guard ) ) {
        if ( !CC_TYPED_STEP_IS_VALID( *s ) ) break;

        if ( pptl__t ) { // Should be just initialized to NULL (1st run), or already transferred to path__e_a (all other runs).
            result = false;
            break;
        }

        if ( already_traversed__d ) {
            pptl__t = cc_pos_desc_link_duplicate_all__new( already_traversed__d );

            if ( !pptl__t ) {
                result = false;
                break;
            }
        }

        CcPos destination = cc_pos_add( from_pos, s->step, 1 );
        CcPieceEnum target = cc_chessboard_get_piece( cb, destination.i, destination.j );
        bool is_target_divergent = CC_PIECE_IS_DIVERGENT( target );

        do_append = false;

        if ( s->type == CC_STE_Capture ) {
            if ( CC_MAYBE_IS_TRUE( cc_check_piece_can_capture_at( cb, pawn.piece, destination ) )
                    || is_target_divergent ) {
                if ( !( result = cc_pos_desc_link_append_pos( cb, destination, &pptl__t ) && result ) ) break;
                do_append = true;
            }
        } else if ( s->type == CC_STE_Movement ) {
            if ( CC_MAYBE_IS_FALSE( cc_check_piece_is_blocked_at( cb, pawn.piece, pawn.momentum, destination ) )
                    || is_target_divergent ) {
                if ( s->step.i == 0 ) { // Vertical movement only --> rush(?)
                    bool is_rush = false;

                    if ( cc_pos_is_equal( pawn.pos, from_pos ) ) { // is_equal() --> likely not divergence, should find original Pawn.
                        CcPieceEnum pe = cc_chessboard_get_piece( cb, from_pos.i, from_pos.j );
                        if ( pe != pawn.piece ) { // Pawn not found on expected position --> hard fail.
                            result = false;
                            break;
                        }

                        CcTagEnum te = cc_chessboard_get_tag( cb, from_pos.i, from_pos.j );
                        is_rush = CC_TAG_CAN_RUSH( te );
                    }

                    do {
                        if ( !CC_MAYBE_IS_FALSE( cc_check_piece_is_blocked_at( cb, pawn.piece, pawn.momentum, destination ) ) ) break;
                        if ( !( result = cc_pos_desc_link_append_pos( cb, destination, &pptl__t ) && result ) ) break;
                        do_append = true;

                        // TODO :: is_target_divergent

                        destination = cc_pos_add( destination, s->step, 1 );
                    } while ( is_rush && cc_variant_is_rank_in_rush_limits( cb->type, is_pawn_light, destination.j ) );
                } else {
                    if ( !( result = cc_pos_desc_link_append_pos( cb, destination, &pptl__t ) && result ) ) break;
                    do_append = true;
                }
            }
        } else { // Neither a capture, nor a movement --> error.
            result = false;
            break;
        }

        // TODO :: recursive call to cc_path_pawn( ,,, pptl__t, ) if blocked by Shaman / Starchild then free pptl__t,
        //         otherwise just do standard cc_path_link_append()
        if ( is_target_divergent ) {
            if ( !( result = cc_path_pawn( cb, pawn, from_pos, pptl__t, path__e_a ) && result ) ) break;
        }

        if ( do_append ) {
            if ( !( result = cc_path_link_append( path__e_a, &pptl__t ) && result ) ) break;
        }

        s += 1;
    }

    if ( pptl__t ) {
        // Ppt-link produced, but not transferred to paths.
        cc_pos_desc_link_free_all( &pptl__t );
        result = false;
    }

    if ( !result ) {
        cc_path_link_free_all( path__e_a );
        return false;
    } else
        return true;
}


bool cc_path_single_step( CcChessboard * cb,
                          CcPosDesc piece,
                          CcPieceEnum activator,
                          CcPos from_pos,
                          CcPathLink ** path__e_a ) {
    if ( !cb ) return false;
    if ( !cc_pos_desc_is_valid( piece ) ) return false;
    if ( !path__e_a ) return false;
    if ( *path__e_a ) return false;

    if ( !cc_chessboard_is_pos_on_board( cb, from_pos.i, from_pos.j ) ) return false;

    CcPieceEnum from_piece = cc_chessboard_get_piece( cb, from_pos.i, from_pos.j );
    CcPathLink * path__t = NULL;

    if ( CC_PIECE_IS_TELEPORTER( from_piece ) ) {
        // TODO
    } else {
        if ( CC_PIECE_IS_PAWN( piece.piece ) ) {
            if ( !cc_path_pawn( cb, piece, from_pos, NULL, &path__t ) ) return false;

            *path__e_a = path__t;
            path__t = NULL;
            return true;
        } else if ( CC_PIECE_IS_WAVE( piece.piece ) ) {
            if ( !CC_PIECE_IS_ACTIVATOR( activator ) ) return false;

            // TODO
        } else
            return false;
    }

    // TODO

    return false;
}
