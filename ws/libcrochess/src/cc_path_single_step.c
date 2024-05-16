// Copyright (c) 2024 Mario Mlačak, mmlacak@gmail.com
// Licensed under GNU GPL v3+ license. See LICENSING, COPYING files for details.

#include "cc_checks.h"

#include "cc_path_single_step.h"

/**
    @file cc_path_single_step.c
    @brief Path generating, filtering for single-step pieces.
*/


static bool cc_ppt_link_append_pos( CcChessboard * cb,
                                    CcPos destination,
                                    CcPptLink ** pptl__iod_a ) {
    // <i> Not needed, static + known caller.
    // if ( !cb ) return false;
    // if ( !pptl__iod_a ) return false;

    CcPosPieceTag ppt = cc_convert_pos_to_ppt( cb, destination );

    if ( !cc_ppt_link_append( pptl__iod_a, ppt ) ) return false;

    return true;
}


static bool cc_path_pawn( CcChessboard * cb,
                          CcPosPieceTag pawn,
                          CcPos pos,
                          CcPathLink ** path__e_a ) {
    // <i> Not needed, already checked in the only caller, i.e. cc_path_single_step().
    // if ( !cb ) return false;
    // if ( !cc_pos_piece_tag_is_valid( pawn ) ) return false;
    // if ( !path__e_a ) return false;
    // if ( *path__e_a ) return false;

    if ( !cc_chessboard_is_pos_on_board( cb, pos.i, pos.j ) ) return false;
    if ( !CC_PIECE_IS_PAWN( pawn.piece ) ) return false;

    bool is_pawn_light = cc_piece_is_light( pawn.piece );
    CcTypedStep const * step = NULL;
    CcTypedStep const * guard = NULL;

    if ( cc_variant_has_sideways_pawns( cb->type ) ) {
        step = is_pawn_light ? CC_STEPS_LIGHT_SIDEWAYS_PAWN : CC_STEPS_DARK_SIDEWAYS_PAWN;
        guard = step + CC_STEPS_SIDEWAYS_PAWN_LEN;
    } else {
        step = is_pawn_light ? CC_STEPS_LIGHT_PAWN : CC_STEPS_DARK_PAWN;
        guard = step + CC_STEPS_PAWN_LEN;
    }

    // if ( !step || !guard ) return false; // <!> Not needed, as long as both vars are certainly initialized.

    CcTypedStep const * s = step;
    CcPptLink * pptl__t = NULL;
    bool result = true;

    while ( s && ( s <= guard ) ) {
        if ( CC_TYPED_STEP_IS_VALID( *s ) ) {
            CcPos destination = cc_pos_add( pos, s->step, 1 );

            if ( s->type == CC_STE_Capture ) {
                if ( CC_MAYBE_IS_TRUE( cc_check_piece_can_capture_at( cb, pawn.piece, destination ) ) ) {
                    if ( !( result = cc_ppt_link_append_pos( cb, destination, &pptl__t ) && result ) ) break;
                    if ( !( result = cc_path_link_append( path__e_a, &pptl__t ) && result ) ) break;
                }
            } else if ( s->type == CC_STE_Movement ) {
                if ( CC_MAYBE_IS_FALSE( cc_check_piece_is_blocked_at( cb, pawn.piece, destination ) ) ) {
                    if ( s->step.i == 0 ) { // Vertical movement only --> rush(?)
                        bool is_rush = false;

                        if ( cc_pos_is_equal( pawn.pos, pos ) ) {
                            CcPieceEnum pe = cc_chessboard_get_piece( cb, pos.i, pos.j );
                            if ( pe != pawn.piece ) return false;

                            CcTagEnum te = cc_chessboard_get_tag( cb, pos.i, pos.j );
                            is_rush = CC_TAG_CAN_RUSH( te );
                        }

                        do {
                            if ( !CC_MAYBE_IS_FALSE( cc_check_piece_is_blocked_at( cb, pawn.piece, destination ) ) ) break;
                            if ( !( result = cc_ppt_link_append_pos( cb, destination, &pptl__t ) && result ) ) break;

                            destination = cc_pos_add( destination, s->step, 1 );
                        } while ( is_rush && cc_variant_is_rank_in_rush_limits( cb->type, is_pawn_light, destination.j ) );

                        if ( result ) {
                            if ( !( result = cc_path_link_append( path__e_a, &pptl__t ) && result ) ) break;
                        } else
                            break;
                    } else {
                        if ( !( result = cc_ppt_link_append_pos( cb, destination, &pptl__t ) && result ) ) break;
                        if ( !( result = cc_path_link_append( path__e_a, &pptl__t ) && result ) ) break;
                    }
                }
            }
        }

        s += 1;
    }

    if ( pptl__t ) {
        // <i> Ppt-link produced, but not transferred to paths.
        cc_ppt_link_free_all( &pptl__t );
        result = false;
    }

    if ( !result ) {
        cc_path_link_free_all( path__e_a );
        return false;
    } else
        return true;
}


bool cc_path_single_step( CcChessboard * cb,
                          CcPosPieceTag piece,
                          CcPieceEnum activator,
                          CcPos pos,
                          CcPathLink ** path__e_a ) {
    if ( !cb ) return false;
    if ( !cc_pos_piece_tag_is_valid( piece ) ) return false;
    if ( !path__e_a ) return false;
    if ( *path__e_a ) return false;

    CcPathLink * path__t = NULL;

    if ( CC_PIECE_IS_PAWN( piece.piece ) ) {
        if ( !cc_path_pawn( cb, piece, pos, &path__t ) ) return false;

        *path__e_a = path__t;
        path__t = NULL;
        return true;
    } else if ( CC_PIECE_IS_WAVE( piece.piece ) ) {
        if ( !CC_PIECE_IS_ACTIVATOR( activator ) ) return false;

        // TODO
    } else
        return false;

    // TODO

    return false;
}
