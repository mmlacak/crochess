// Copyright (c) 2021 Mario Mlačak, mmlacak@gmail.com
// Licensed under GNU GPL v3+ license. See LICENSING, COPYING files for details.

// #include <stddef.h>
// #include <math.h>

#include "cc_defines.h"
#include "cc_gen_steps.h"


bool cc_gen_steps_piece_pos_iter( CcChessboard * restrict cb,
                                  CcPieceEnum piece,
                                  bool include_opposite,
                                  CcPos * restrict pos__io )
{
    if ( !cb ) return false;
    if ( !pos__io ) return false;

    int size = (int)cb->size;
    CcPos pos = *pos__io;

    // Next position to check.
    if ( !CC_POS_IS_ON_BOARD( cb->size, pos.i, pos.j ) )
        pos = cc_pos( 0, 0 );
    else if ( pos.j < size - 1 )
        pos = cc_pos( pos.i, pos.j + 1 );
    else
        pos = cc_pos( pos.i + 1, 0 );

    for ( int i = pos.i; i < size; ++i )
    {
        for ( int j = pos.j; j < size; ++j )
        {
            CcPieceEnum pe = cc_chessboard_get_piece( cb, i, j );

// TODO :: teleporting
            if ( CC_PIECE_IS_THE_SAME( pe, piece ) ||
                 ( include_opposite && cc_piece_is_opposite( pe, piece ) ) )
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
