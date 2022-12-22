// Copyright (c) 2021, 2022 Mario Mlačak, mmlacak@gmail.com
// Licensed under GNU GPL v3+ license. See LICENSING, COPYING files for details.

#include "cc_rules_path.h"

#include "cc_path_defs.h"


bool cc_is_pawn_capture_valid( CcChessboard * restrict cb,
                               CcPieceEnum piece,
                               CcPos start,
                               CcPos destination )
{
    if ( !cb ) return false;
    if ( !CC_PIECE_IS_PAWN( piece ) ) return false;
    if ( !CC_IS_COORD_2_ON_BOARD( cb->size, start.i, start.j ) ) return false;
    if ( !CC_IS_COORD_2_ON_BOARD( cb->size, destination.i, destination.j ) ) return false;

    CcPieceEnum pe = cc_chessboard_get_piece( cb, destination.i, destination.j );

    if ( CC_PIECE_IS_NONE( pe ) ) return false;
    if ( cc_piece_has_same_color( piece, pe ) ) return false;
    if ( !CC_PIECE_CAN_BE_CAPTURED( pe ) ) return false;

    CcPos step = cc_pos_step( start, destination );

    if ( cc_piece_is_light( piece ) )
        return CC_LIGHT_PAWN_CAPTURE_STEP_IS_VALID( step );
    else
        return CC_DARK_PAWN_CAPTURE_STEP_IS_VALID( step );
}

bool cc_is_pawn_step_valid( CcChessboard * restrict cb,
                            CcPieceEnum piece,
                            CcPos start,
                            CcPos destination )
{
    if ( !cb ) return false;
    if ( !CC_PIECE_IS_PAWN( piece ) ) return false;
    if ( !CC_IS_COORD_2_ON_BOARD( cb->size, start.i, start.j ) ) return false;
    if ( !CC_IS_COORD_2_ON_BOARD( cb->size, destination.i, destination.j ) ) return false;

    CcPieceEnum pe = cc_chessboard_get_piece( cb, destination.i, destination.j );

    if ( !CC_PIECE_IS_NONE( pe ) )
    {
        if ( !cc_piece_has_same_color( piece, pe ) ) return false;
        if ( !CC_PIECE_CAN_BE_ACTIVATED( pe ) ) return false;
    }

    CcPos step = cc_pos_step( start, destination );

    if ( cc_piece_is_light( piece ) )
    {
        if ( cc_variant_has_sideways_pawns( cb->type ) )
            return CC_LIGHT_SIDEWAYS_PAWN_STEP_IS_VALID( step );
        else
            return CC_LIGHT_PAWN_STEP_IS_VALID( step );
    }
    else
    {
        if ( cc_variant_has_sideways_pawns( cb->type ) )
            return CC_DARK_SIDEWAYS_PAWN_STEP_IS_VALID( step );
        else
            return CC_DARK_PAWN_STEP_IS_VALID( step );
    }
}
