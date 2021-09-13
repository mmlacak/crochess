// Copyright (c) 2021 Mario Mlačak, mmlacak@gmail.com
// Licensed under 3-clause (modified) BSD license. See LICENSE for details.

#include "cc_defines.h"
#include "cc_setup_board.h"
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

// TODO :: FIX :: check opponent's Pawn tag == CC_TE_EnPassant !!!

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


bool cc_rule_utils_find_castling_rook( CcChessboard const * const restrict cb,
                                       CcPieceEnum const king_castling,
                                       int const step_i_K,
                                       int const step_j_K,
                                       int * const restrict dest_i_R_io,
                                       CcPieceEnum * const restrict rook_o,
                                       int * const restrict start_i_R_o )
{
    if ( !cb ) return false;
    if ( !dest_i_R_io ) return false;
    if ( !rook_o ) return false;
    if ( !start_i_R_o ) return false;

    if ( !CC_PIECE_IS_KING( king_castling ) ) return false;
    if ( !cc_chessboard_is_pos_on_board( cb, step_i_K, step_j_K ) ) return false;
    if ( !CC_VARIANT_BOARD_SIZE_IS_VALID( cb->size ) ) return false;

    //
    // King checks

    int start_i_K = cc_setup_board_get_figure_row_initial_file( cb->type, king_castling, false );
    if ( !cc_chessboard_is_coord_on_board( cb, start_i_K ) )
        return false;

    CcPieceEnum king = cc_chessboard_get_piece( cb, start_i_K, step_j_K );
    if ( king_castling != king )
        return false;

    CcTagEnum tag_K = cc_chessboard_get_tag( cb, start_i_K, step_j_K );
    if ( !CC_TAG_CAN_CASTLE( tag_K ) )
        return false;

    //
    // Rook to find

    CcPieceEnum rook = cc_piece_is_light( king_castling, false ) ? CC_PE_LightRook
                                                                 : CC_PE_DarkRook;
    bool is_left = ( step_i_K < start_i_K );
    int dest_i_R = is_left ? step_i_K + 1 : step_i_K - 1;

    if ( cc_chessboard_is_coord_on_board( cb, *dest_i_R_io )
        && ( dest_i_R != *dest_i_R_io ) )
            return false;

    int start_i_R = cc_setup_board_get_figure_row_initial_file( cb->type, rook, is_left );
    if ( !cc_chessboard_is_coord_on_board( cb, start_i_R ) )
        return false;

    //
    // Rook checks

    rook = cc_chessboard_get_piece( cb, start_i_R, step_j_K );
    if ( ( !CC_PIECE_IS_ROOK( rook ) ) &&
         ( !cc_piece_is_the_same_color( king_castling, rook, false ) ) )
        return false;

    CcTagEnum tag_R = cc_chessboard_get_tag( cb, start_i_R, step_j_K );
    if ( !CC_TAG_CAN_CASTLE( tag_R ) )
        return false;

    //
    // Field are empty?

    int lower = is_left ? start_i_R + 1 : start_i_K + 1;
    int upper = is_left ? start_i_K - 1 : start_i_R - 1;

    for ( int i = lower; i <= upper; ++i )
    {
        CcPieceEnum empty = cc_chessboard_get_piece( cb, i, step_j_K );
        if ( !CC_PIECE_IS_NONE( empty ) )
            return false;
    }

// TODO :: FIX :: Everything between King and its destination must not be under attack!

    *dest_i_R_io = dest_i_R;
    *rook_o = rook;
    *start_i_R_o = start_i_R;

    return true;
}
