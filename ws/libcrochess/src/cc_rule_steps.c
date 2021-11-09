// Copyright (c) 2021 Mario Mlačak, mmlacak@gmail.com
// Licensed under GNU GPL v3+ license. See LICENSING, COPYING files for details.

#include "cc_rule_steps.h"

#include "cc_defines.h"


bool cc_rule_steps_piece_pos_iter( CcGame const * const restrict game,
                                   char const piece_symbol,
                                   CcPieceEnum * const restrict piece_o,
                                   int * const restrict pos_i_o,
                                   int * const restrict pos_j_o,
                                   bool const initialize_iter )
{
    if ( !game ) return false;
    if ( !game->chessboard ) return false;
    if ( !piece_o ) return false;
    if ( !pos_i_o ) return false;
    if ( !pos_j_o ) return false;

    if ( !cc_piece_is_symbol( piece_symbol ) ) return false;

    static int pos_i = 0;
    static int pos_j = 0;
    int size = (int)game->chessboard->size;

    if ( initialize_iter )
    {
        pos_i = 0;
        pos_j = 0;
    }

    for ( int i = pos_i; i < size; ++i )
    {
        for ( int j = pos_j; j < size; ++j )
        {
            CcPieceEnum pe = cc_chessboard_get_piece( game->chessboard, i, j );

            if ( cc_piece_symbol( pe ) == piece_symbol )
            {
                // Next position to check.
                if ( j < size - 1 )
                {
                    pos_j = j + 1;
                    pos_i = i;
                }
                else
                {
                    pos_j = 0;
                    pos_i = i + 1;
                }

                *piece_o = pe;
                *pos_i_o = i;
                *pos_j_o = j;

                return true;
            }
        }
    }

    return false;
}

bool cc_rule_steps_find_piece_start_pos( CcGame const * const restrict game,
                                         CcPlyLinkEnum const ple,
                                         char const piece_symbol,
                                         int const * const restrict disamb_i_d,
                                         int const * const restrict disamb_j_d,
                                         int const dest_i,
                                         int const dest_j,
                                         CcPieceEnum * const restrict piece_o,
                                         int * const restrict pos_i_o,
                                         int * const restrict pos_j_o )
{
    if ( !game ) return false;
    if ( !piece_o ) return false;
    if ( !pos_i_o ) return false;
    if ( !pos_j_o ) return false;

    if ( !cc_piece_is_symbol( piece_symbol ) ) return false;

    if ( disamb_i_d )
        if ( !cc_chessboard_is_coord_on_board( game->chessboard, *disamb_i_d ) )
            return false;

    if ( disamb_j_d )
        if ( !cc_chessboard_is_coord_on_board( game->chessboard, *disamb_j_d ) )
            return false;

    if ( !cc_chessboard_is_pos_on_board( game->chessboard, dest_i, dest_j ) )
        return false;

    int step_1_i = CC_OFF_BOARD_COORD;
    int step_1_j = CC_OFF_BOARD_COORD;
    int step_2_i = CC_OFF_BOARD_COORD;
    int step_2_j = CC_OFF_BOARD_COORD;

    if ( disamb_i_d && disamb_j_d )
    {
        CcPieceEnum pe = cc_chessboard_get_piece( game->chessboard, *disamb_i_d, *disamb_j_d );

        if ( cc_piece_symbol( pe ) == piece_symbol )
        {
            if (  cc_rule_steps_check_movement( game, ple, pe,
                                                *disamb_i_d, *disamb_j_d,
                                                dest_i, dest_j,
                                                &step_1_i, &step_1_j,
                                                &step_2_i, & step_2_j ) )
            {
                *piece_o = pe;
                *pos_i_o = *disamb_i_d;
                *pos_j_o = *disamb_j_d;
                return true;
            }
        }

        return false;
    }

    CcPieceEnum piece;
    int pos_i;
    int pos_j;
    bool initialize_iter = true;

    while ( cc_rule_steps_piece_pos_iter( game, piece_symbol, &piece, &pos_i, &pos_j, initialize_iter ) )
    {
        initialize_iter = false;

        if ( ( disamb_i_d ) && ( *disamb_i_d != pos_i ) ) continue;
        if ( ( disamb_j_d ) && ( *disamb_j_d != pos_j ) ) continue;

        if ( !cc_rule_steps_check_movement( game, ple, piece,
                                            pos_i, pos_j,
                                            dest_i, dest_j,
                                            &step_1_i, &step_1_j,
                                            &step_2_i, & step_2_j ) )
            continue;

        *piece_o = piece;
        *pos_i_o = pos_i;
        *pos_j_o = pos_j;
        return true;
    }

    return false;
}

bool cc_rule_steps_check_movement( CcGame const * const restrict game,
                                   CcPlyLinkEnum const ple,
                                   CcPieceEnum const piece,
                                   int const start_i,
                                   int const start_j,
                                   int const dest_i,
                                   int const dest_j,
                                   int * const restrict step_1_i_o,
                                   int * const restrict step_1_j_o,
                                   int * const restrict step_2_i_o,
                                   int * const restrict step_2_j_o )
{
    if ( !game ) return false;


    return false;
}
