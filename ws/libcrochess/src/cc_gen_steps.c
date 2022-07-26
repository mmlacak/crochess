// Copyright (c) 2021, 2022 Mario Mlačak, mmlacak@gmail.com
// Licensed under GNU GPL v3+ license. See LICENSING, COPYING files for details.

#include "cc_gen_steps.h"


bool cc_piece_pos_iter( CcChessboard * restrict cb_before_activation,
                        CcPieceEnum piece,
                        bool include_opponent,
                        CcPos * restrict pos__io )
{
    if ( !cb_before_activation ) return false;
    if ( !pos__io ) return false;

    int size = (int)cb_before_activation->size;
    CcPos pos = *pos__io;

    // Next position to check.
    if ( !cc_chessboard_is_pos_on_board( cb_before_activation, pos.i, pos.j ) )
        pos = cc_pos( 0, 0 );
    else if ( pos.j < size - 1 )
        pos = cc_pos( pos.i, pos.j + 1 );
    else
        pos = cc_pos( pos.i + 1, 0 );

    for ( int i = pos.i; i < size; ++i )
    {
        for ( int j = pos.j; j < size; ++j )
        {
            CcPieceEnum pe = cc_chessboard_get_piece( cb_before_activation, i, j );

// TODO :: teleporting
            if ( CC_PIECE_IS_THE_SAME( pe, piece ) ||
                 ( include_opponent && cc_piece_is_opposite( pe, piece ) ) )
            {
                *pos__io = cc_pos( i, j );
                return true;
            }
// TODO :: teleporting
        }

        pos.j = 0;
    }

    *pos__io = cc_pos_invalid();
    return false;
}
