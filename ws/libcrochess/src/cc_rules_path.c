// Copyright (c) 2021, 2022 Mario Mlačak, mmlacak@gmail.com
// Licensed under GNU GPL v3+ license. See LICENSING, COPYING files for details.

#include "cc_rules_path.h"

#include "cc_path_defs.h"


bool cc_is_pawn_capture_valid( CcChessboard * restrict cb,
                               CcPieceEnum pawn,
                               CcPos start,
                               CcPos destination ) {
    if ( !cb ) return false;
    if ( !CC_PIECE_IS_PAWN( pawn ) ) return false;
    if ( !CC_IS_COORD_2_ON_BOARD( cb->size, start.i, start.j ) ) return false;
    if ( !CC_IS_COORD_2_ON_BOARD( cb->size, destination.i, destination.j ) ) return false;

    CcPieceEnum pe = cc_chessboard_get_piece( cb, destination.i, destination.j );

    if ( CC_PIECE_IS_NONE( pe ) ) return false;
    if ( cc_piece_has_same_color( pawn, pe ) ) return false;
    if ( !CC_PIECE_CAN_BE_CAPTURED( pe ) ) return false;

    CcPos step = cc_pos_difference( destination, start );

    return cc_is_pawn_capture_step( cb->type, pawn, step ); }

bool cc_is_pawn_step_valid( CcChessboard * restrict cb,
                            CcPieceEnum pawn,
                            CcPos start,
                            CcPos destination ) {
    if ( !cb ) return false;
    if ( !CC_PIECE_IS_PAWN( pawn ) ) return false;
    if ( !CC_IS_COORD_2_ON_BOARD( cb->size, start.i, start.j ) ) return false;
    if ( !CC_IS_COORD_2_ON_BOARD( cb->size, destination.i, destination.j ) ) return false;

    CcPieceEnum pe = cc_chessboard_get_piece( cb, destination.i, destination.j );

    if ( !CC_PIECE_IS_NONE( pe ) ) {
        if ( !cc_piece_has_same_color( pawn, pe ) ) return false;
        if ( !CC_PIECE_CAN_BE_ACTIVATED( pe ) ) return false; }

    CcPos step = CC_POS_CAST_INVALID;

    CcTagEnum te = cc_chessboard_get_tag( cb, destination.i, destination.j );
    if ( CC_TAG_CAN_RUSH( te ) ) {
        step = cc_pos_step( start, destination );

        // Can rush only forward.
        if ( cc_piece_is_light( pawn ) ) {
            if ( CC_LIGHT_PAWN_STEP_IS_VALID( step ) ) return true; }
        else
            if ( CC_DARK_PAWN_STEP_IS_VALID( step ) ) return true; }

    step = cc_pos_difference( destination, start );

    return cc_is_pawn_step( cb->type, pawn, step ); }
