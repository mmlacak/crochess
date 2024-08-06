// Copyright (c) 2024 Mario Mlačak, mmlacak@gmail.com
// Licensed under GNU GPL v3+ license. See LICENSING, COPYING files for details.

#include "cc_checks.h"

#include "cc_path_single_step.h"


static bool cc_path_pawn( CcChessboard * cb,
                          CcPieceType pawn,
                          CcTagType tag,
                          CcPos from_pos,
                          cc_uint_t momentum,
                          bool is_accumulating_momentum,
                          CcPosDescLink * already_traversed__d,
                          CcPathLink ** path__o ) {
    // Not needed, already checked in the only caller, i.e. cc_path_single_step().
    // if ( !cb ) return false;
    // if ( !path__o ) return false;
    // if ( *path__o ) return false;
    // if ( !cc_chessboard_is_pos_on_board( cb, from_pos.i, from_pos.j ) ) return false;

    if ( !CC_PIECE_IS_PAWN( pawn ) ) return false;

    bool is_pawn_light = cc_piece_is_light( pawn );

    CcPosDescLink * pptl__t = NULL;
    cc_uint_t mm = momentum;
    bool accumulating = is_accumulating_momentum;
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

    // TODO :: static promotion

    while ( s && ( s <= guard ) ) {
        if ( !CC_TYPED_STEP_IS_VALID( *s ) ) break;

        if ( pptl__t ) { // Should be just initialized to NULL (1st run), or already transferred to path__o (all other runs).
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

        // TODO :: cc_pos_add() + cc_calc_checked_momentum() --> new func w/ checks
        CcPos destination = cc_pos_add( from_pos, s->step, 1 );

        // CcPieceType target = cc_chessboard_get_piece( cb, destination.i, destination.j );
        bool can_diverge_at = CC_MAYBE_IS_TRUE( cc_check_piece_can_diverge_at( cb, pawn, mm, CC_PE_None, destination ) );
        bool do_append = false;

        if ( s->type == CC_STE_Capture ) {
            if ( CC_MAYBE_IS_TRUE( cc_check_piece_can_capture_at( cb, pawn, mm, destination ) )
                    || can_diverge_at ) {
                if ( !( result = cc_calc_checked_momentum( &mm, accumulating ) && result ) ) break;
                if ( !( result = cc_append_pos_to_pos_desc_link( cb, destination, mm, &pptl__t ) && result ) ) break;
                do_append = true;
            }
        } else if ( s->type == CC_STE_Movement ) {
            if ( CC_MAYBE_IS_FALSE( cc_check_piece_is_blocked_at( cb, pawn, mm, destination ) )
                    || can_diverge_at ) {
                if ( s->step.i == 0 ) { // Vertical movement only --> maybe rush?
                    bool is_rush = CC_TAG_CAN_RUSH( tag );

                    do {
                        if ( !CC_MAYBE_IS_FALSE( cc_check_piece_is_blocked_at( cb, pawn, mm, destination ) ) ) break;
                        if ( !( result = cc_calc_checked_momentum( &mm, accumulating ) && result ) ) break;
                        if ( !( result = cc_append_pos_to_pos_desc_link( cb, destination, mm, &pptl__t ) && result ) ) break;
                        do_append = true;

                        // TODO :: can_diverge_at

                        // TODO :: transparency

                        // TODO :: if target == Starchild, Pawn can either diverge, or use transparency

                        destination = cc_pos_add( destination, s->step, 1 );
                        can_diverge_at = CC_MAYBE_IS_TRUE( cc_check_piece_can_diverge_at( cb, pawn, mm, CC_PE_None, destination ) );
                    } while ( is_rush && cc_variant_is_rank_in_rush_limits( cb->type, is_pawn_light, destination.j ) );
                } else {
                    if ( !( result = cc_calc_checked_momentum( &mm, accumulating ) && result ) ) break;
                    if ( !( result = cc_append_pos_to_pos_desc_link( cb, destination, mm, &pptl__t ) && result ) ) break;
                    do_append = true;
                }
            }
        } else { // Neither a capture, nor a movement --> error.
            result = false;
            break;
        }

        // TODO :: recursive call to cc_path_pawn( ,,, pptl__t, ) if blocked by Shaman / Starchild then free pptl__t,
        //         otherwise just do standard cc_path_link_append()
        if ( can_diverge_at ) {
            if ( !( result = cc_path_pawn( cb, pawn, CC_TE_None, from_pos, mm, false, pptl__t, path__o ) && result ) ) break;
            do_append = false;
        }

        if ( do_append ) {
            if ( !( result = cc_path_link_append( path__o, &pptl__t ) && result ) ) break;
        }

        s += 1;
    }

    if ( pptl__t ) {
        // Ppt-link produced, but not transferred to paths.
        cc_pos_desc_link_free_all( &pptl__t );
        result = false;
    }

    if ( !result ) {
        cc_path_link_free_all( path__o );
        return false;
    } else
        return true;
}


bool cc_path_single_step( CcChessboard * cb,
                          CcPieceType piece,
                          CcTagType tag,
                          CcPieceType activator,
                          CcPos from_pos,
                          cc_uint_t momentum,
                          bool is_accumulating_momentum,
                          CcPathLink ** path__o ) {
    if ( !cb ) return false;
    if ( !path__o ) return false;
    if ( *path__o ) return false;

    if ( !cc_chessboard_is_pos_on_board( cb, from_pos.i, from_pos.j ) ) return false;

    CcPieceType from_piece = cc_chessboard_get_piece( cb, from_pos.i, from_pos.j );
    CcPathLink * path__t = NULL;

    if ( CC_PIECE_IS_TELEPORTER( from_piece ) ) {
        // TODO
    } else {
        if ( CC_PIECE_IS_PAWN( piece ) ) {
            if ( !cc_path_pawn( cb, piece, tag, from_pos, momentum, is_accumulating_momentum, NULL, &path__t ) ) return false;

            *path__o = path__t;
            path__t = NULL;
            return true;
        } else if ( CC_PIECE_IS_WAVE( piece ) ) {
            if ( !CC_PIECE_IS_ACTIVATOR( activator ) ) return false;

            // TODO
        } else
            return false;
    }

    // TODO

    return false;
}
