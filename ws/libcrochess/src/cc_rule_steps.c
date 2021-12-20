// Copyright (c) 2021 Mario Mlačak, mmlacak@gmail.com
// Licensed under GNU GPL v3+ license. See LICENSING, COPYING files for details.

#include <stddef.h>
#include <math.h>

#include "cc_defines.h"
// #include "cc_pos.h"
#include "cc_gen_steps.h"
#include "cc_rule_steps.h"


bool cc_rule_steps_piece_pos_iter( CcChessboard * restrict cb,
                                   char piece_symbol,
                                   CcPieceEnum * restrict piece_o,
                                   CcPos * restrict start_o,
                                   bool initialize_iter )
{
    if ( !cb ) return false;
    if ( !piece_o ) return false;
    if ( !start_o ) return false;

    if ( !cc_piece_is_symbol( piece_symbol ) ) return false;

    static int pos_i = 0;
    static int pos_j = 0;

    if ( initialize_iter )
    {
        pos_i = 0;
        pos_j = 0;
    }

    int size = (int)cb->size;

    for ( int i = pos_i; i < size; ++i )
    {
        for ( int j = pos_j; j < size; ++j )
        {
            CcPieceEnum pe = cc_chessboard_get_piece( cb, i, j );

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

bool cc_rule_steps_find_piece_start_pos( CcChessboard * restrict cb,
                                         CcPlyLinkEnum ple,
                                         char piece_symbol,
                                         int * restrict disamb_i_d,
                                         int * restrict disamb_j_d,
                                         int dest_i,
                                         int dest_j,
                                         CcPieceEnum * restrict piece_o,
                                         CcPos * restrict start_o )
{
    if ( !cb ) return false;
    if ( !piece_o ) return false;
    if ( !start_o ) return false;

    if ( !cc_piece_is_symbol( piece_symbol ) ) return false;

    if ( disamb_i_d )
        if ( !cc_chessboard_is_coord_on_board( cb, *disamb_i_d ) )
            return false;

    if ( disamb_j_d )
        if ( !cc_chessboard_is_coord_on_board( cb, *disamb_j_d ) )
            return false;

    if ( !cc_chessboard_is_pos_on_board( cb, dest_i, dest_j ) )
        return false;

    CcPos dest = cc_pos( dest_i, dest_j );
    CcPosLink * steps = NULL;

    if ( disamb_i_d && disamb_j_d )
    {
        CcPieceEnum pe = cc_chessboard_get_piece( cb, *disamb_i_d, *disamb_j_d );

        if ( cc_piece_symbol( pe ) == piece_symbol )
        {
            if (  cc_rule_steps_check_movement( cb, ple, pe,
                                                cc_pos( *disamb_i_d, *disamb_j_d ),
                                                dest,
                                                &steps ) )
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
    bool init = true;

    while ( cc_rule_steps_piece_pos_iter( cb, piece_symbol, &piece, &start, init ) )
    {
        init = false;

        if ( ( disamb_i_d ) && ( *disamb_i_d != start.i ) ) continue;
        if ( ( disamb_j_d ) && ( *disamb_j_d != start.j ) ) continue;

        if ( cc_rule_steps_check_movement( cb, ple, piece, start, dest, &steps ) )
        {
            *piece_o = piece;
            *start_o = start;
            return true;
        }
    }

    return false;
}


bool cc_rule_steps_check_bishop( CcChessboard * restrict cb,
                                 CcPlyLinkEnum ple,
                                 CcPieceEnum piece,
                                 CcPos start,
                                 CcPos dest,
                                 CcPosLink ** restrict steps_o )
{
    if ( !cb ) return false;
    if ( !steps_o ) return false;
    if ( *steps_o ) return false;

    if ( !CC_PIECE_IS_BISHOP( piece ) ) return false;

    CcPos offset = cc_pos_subtract( dest, start );
    if ( ( offset.i == 0 ) || ( offset.j == 0 ) ) return false;
    if ( abs( offset.i ) != abs( offset.j ) ) return false;

    CcPos step = cc_pos( CC_SIGN( offset.i ), CC_SIGN( offset.j ) );
    if ( !CC_GEN_STEPS_BISHOP_IS_VALID( step.i, step.j ) ) return false;

    CcPosLink * pl__t = NULL;
    for ( CcPos p = cc_pos_add( start, step ); !cc_pos_is_equal( p, dest ); p = cc_pos_add( p, step ) )
    {
        CcPieceEnum pe = cc_chessboard_get_piece( cb, p.i, p.j );
        if ( !CC_PIECE_IS_NONE( pe ) ) return false;

        if ( !cc_pos_link_append_pos_or_init( &pl__t, p ) )
        {
            free( pl__t );
            // pl__t = NULL; // Not neccessary.
            return false;
        }
    }

    CcPieceEnum pe_dest = cc_chessboard_get_piece( cb, dest.i, dest.j );

    if ( cc_piece_is_targetable( piece, pe_dest ) )
    {
        *steps_o = pl__t; // Ownership transfer --> pl__t is now weak pointer.
        return true;
    }
    else
    {
        free( pl__t );
        // pl__t = NULL; // Not neccessary.
        return false;
    }
}

bool cc_rule_steps_check_movement( CcChessboard * restrict cb,
                                   CcPlyLinkEnum ple,
                                   CcPieceEnum piece,
                                   CcPos start,
                                   CcPos dest,
                                   CcPosLink ** restrict steps_o )
{
    // if ( !cb ) return false;
    // if ( !steps_o ) return false;
    // if ( *steps_o ) return false;

    if ( CC_PIECE_IS_BISHOP( piece ) )
        return cc_rule_steps_check_bishop( cb, ple, piece, start, dest, steps_o );
    else
        return false;
}
