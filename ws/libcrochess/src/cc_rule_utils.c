// Copyright (c) 2021 Mario Mlačak, mmlacak@gmail.com
// Licensed under 3-clause (modified) BSD license. See LICENSE for details.

#include "cc_rule_utils.h"


bool cc_rule_utils_find_en_passant_target( CcChessboard const * const restrict cb,
                                           CcPieceEnum const pawn_en_passant,
                                           int const step_i,
                                           int const step_j,
                                           CcPieceEnum * const restrict pawn_o,
                                           int * const restrict dist_j_o )
{
    if ( !cb ) return false;
    if ( !CC_PIECE_IS_PAWN( pawn_en_passant ) ) return false;
    if ( !cc_chessboard_is_pos_on_board( cb, step_i, step_j ) ) return false;
    if ( !CC_VARIANT_BOARD_SIZE_IS_VALID( cb->size ) ) return false;

    CcPieceEnum empty = cc_chessboard_get_piece( cb, step_i, step_j );
    if ( !CC_PIECE_IS_NONE( empty ) ) return false;

    int half = (int)cb->size / 2;
    if ( half <= 0 )
        return false;

    int diff_j = ( step_j < half ) ? 1 : -1;

    //
    // Check that position from step, 1 field towards initial positions of opponent Pawns
    // is also empty. This checks that opponent's Pawn rushed from, or over that position,
    // i.e. that step position was definitely rushed over by opponent's Pawn.
    int prev_start_j = step_j - diff_j;
    if ( !cc_chessboard_is_coord_on_board( cb, prev_start_j ) ) return false;
    CcPieceEnum prev_pawn = cc_chessboard_get_piece( cb, step_i, prev_start_j );
    if ( !CC_PIECE_IS_NONE( prev_pawn ) ) return false;

    // <.> Rank 0 --> piece row, rank 1 --> Pawn's row ==> start checking at rank 2.
    //     Similarly for dark side ==> end checking at size - 2.
    int lower_j = ( step_j < half ) ? 2 : half;
    int upper_j = ( step_j < half ) ? half : ( (int)cb->size - 2 );

    for ( int j = step_j + diff_j; ( lower_j <= j ) && ( j < upper_j ); j += diff_j )
    {
        CcPieceEnum pe = cc_chessboard_get_piece( cb, step_i, j );

        if ( CC_PIECE_IS_VALID( pe ) )
        {
            if ( cc_piece_is_opposite( pe, pawn_en_passant, true ) )
            {
                *pawn_o = pe;
                *dist_j_o = j;
                return true;
            }
            else
                return false;
        }
    }

    return false;
}
