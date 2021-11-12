// Copyright (c) 2021 Mario Mlačak, mmlacak@gmail.com
// Licensed under GNU GPL v3+ license. See LICENSING, COPYING files for details.

#include "cc_defines.h"
// #include "cc_pos.h"
#include "cc_rule_steps.h"


bool cc_rule_steps_piece_pos_iter( CcGame const * const restrict game,
                                   char const piece_symbol,
                                   CcPieceEnum * const restrict piece_o,
                                   CcPos * const restrict start_o,
                                   bool const initialize_iter )
{
    if ( !game ) return false;
    if ( !game->chessboard ) return false;
    if ( !piece_o ) return false;
    if ( !start_o ) return false;

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
                *start_o = cc_pos( i, j );
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
                                         CcPos * const restrict start_o )
{
    if ( !game ) return false;
    if ( !piece_o ) return false;
    if ( !start_o ) return false;

    if ( !cc_piece_is_symbol( piece_symbol ) ) return false;

    if ( disamb_i_d )
        if ( !cc_chessboard_is_coord_on_board( game->chessboard, *disamb_i_d ) )
            return false;

    if ( disamb_j_d )
        if ( !cc_chessboard_is_coord_on_board( game->chessboard, *disamb_j_d ) )
            return false;

    if ( !cc_chessboard_is_pos_on_board( game->chessboard, dest_i, dest_j ) )
        return false;

    CcPos dest = cc_pos( dest_i, dest_j );
    CcPos step_1 = cc_pos_empty();
    CcPos step_2 = cc_pos_empty();

    if ( disamb_i_d && disamb_j_d )
    {
        CcPieceEnum pe = cc_chessboard_get_piece( game->chessboard, *disamb_i_d, *disamb_j_d );

        if ( cc_piece_symbol( pe ) == piece_symbol )
        {
            if (  cc_rule_steps_check_movement( game, ple, pe,
                                                cc_pos( *disamb_i_d, *disamb_j_d ),
                                                dest,
                                                &step_1,
                                                &step_2 ) )
            {
                *piece_o = pe;
                *start_o = cc_pos( *disamb_i_d, *disamb_j_d );
                return true;
            }
        }

        return false;
    }

    CcPieceEnum piece = CC_PE_None;
    CcPos start = cc_pos_empty();
    bool initialize_iter = true;

    while ( cc_rule_steps_piece_pos_iter( game, piece_symbol, &piece, &start, initialize_iter ) )
    {
        initialize_iter = false;

        if ( ( disamb_i_d ) && ( *disamb_i_d != start.i ) ) continue;
        if ( ( disamb_j_d ) && ( *disamb_j_d != start.j ) ) continue;

        if ( !cc_rule_steps_check_movement( game, ple, piece,
                                            start,
                                            dest,
                                            &step_1,
                                            &step_2 ) )
            continue;

        *piece_o = piece;
        *start_o = start;
        return true;
    }

    return false;
}

bool cc_rule_steps_check_movement( CcGame const * const restrict game,
                                   CcPlyLinkEnum const ple,
                                   CcPieceEnum const piece,
                                   CcPos const start,
                                   CcPos const dest,
                                   CcPos * const restrict step_1_o,
                                   CcPos * const restrict step_2_o )
{
    if ( !game ) return false;


    return false;
}
