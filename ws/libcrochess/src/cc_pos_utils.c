// Copyright (c) 2024 Mario Mlačak, mmlacak@gmail.com
// Licensed under GNU GPL v3+ license. See LICENSING, COPYING files for details.

#include "cc_pos_utils.h"


CcPosDesc cc_convert_pos_to_pos_desc( CcChessboard * cb, CcPos pos ) {
    CcPosDesc pd = { .pos = pos, .piece = CC_PE_None, .tag = CC_TE_None };

    if ( cb ) {
        pd.piece = cc_chessboard_get_piece( cb, pos.i, pos.j );
        pd.tag = cc_chessboard_get_tag( cb, pos.i, pos.j );
    }

    return pd;
}


bool cc_iter_piece_pos( CcChessboard * cb,
                        CcPos expected__d,
                        CcPieceType piece,
                        bool include_opponent,
                        CcPos * pos__io ) {
    if ( !cb ) return false;
    if ( !pos__io ) return false;

    cc_uint_t size = cc_variant_board_size( cb->type );
    if ( !CC_IS_BOARD_SIZE_VALID( size ) ) return false;

    CcPos pos = *pos__io;

    // Next position to check.
    if ( !cc_chessboard_is_pos_on_board( cb, pos.i, pos.j ) )
        pos = CC_POS_CAST_ORIGIN_FIELD;
    else if ( pos.j < (int)( size - 1 ) )
        pos = CC_POS_CAST( pos.i, pos.j + 1 );
    else
        pos = CC_POS_CAST( pos.i + 1, 0 );

    bool is_comparable = CC_POS_IS_VALID( expected__d ) ||
                         CC_POS_IS_DISAMBIGUATION( expected__d );

    for ( int i = pos.i; i < (int)size; ++i ) {
        for ( int j = pos.j; j < (int)size; ++j ) {
            CcPieceType pe = cc_chessboard_get_piece( cb, i, j );

            if ( ( pe == piece ) ||
                    ( include_opponent && cc_piece_is_opposite( pe, piece ) ) ) {
                CcPos current = CC_POS_CAST( i, j );

                if ( ( !is_comparable ) ||
                       cc_pos_is_congruent( expected__d, current ) ) {
                    *pos__io = current;
                    return true;
                }
            }
        }

        pos.j = 0;
    }

    *pos__io = CC_POS_CAST_INVALID;
    return false;
}
