// Copyright (c) 2021 Mario Mlačak, mmlacak@gmail.com
// Licensed under 3-clause (modified) BSD license. See LICENSE for details.

#include "cc_rule_utils.h"


bool cc_rule_utils_find_en_passant_target( CcChessboard const * const restrict cb,
                                           CcPieceEnum pawn_en_passant,
                                           int const step_i,
                                           int const step_j,
                                           CcPieceEnum * const restrict pawn_o,
                                           int * const restrict dist_j_o )
{
    if ( !cb ) return false;
    if ( !cc_piece_is_pawn( pawn_en_passant ) ) return false;
    if ( !cc_chessboard_is_pos_on_board( cb, step_i, step_j ) ) return false;

    int half = (int)cb->size / 2;
    if ( half <= 0 )
        return false;

    int diff_j = ( step_j < half ) ? 1 : -1;
    int lower_j = ( step_j < half ) ? 0 : half;
    int upper_j = ( step_j < half ) ? half : (int)cb->size;

    for ( int j = step_j + diff_j; ( lower_j <= j ) && ( j < upper_j ); j += diff_j )
    {
        CcPieceEnum pe = cc_chessboard_get_piece( cb, step_i, j );

        if ( cc_piece_is_valid( pe ) )
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
