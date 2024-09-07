// Copyright (c) 2024 Mario Mlačak, mmlacak@gmail.com
// Licensed under GNU GPL v3+ license. See LICENSING, COPYING files for details.

#include "cc_pos_utils.h"


CcPosDesc cc_convert_pos_to_pos_desc( CcChessboard * cb, CcPos pos, cc_uint_t momentum ) {
    CcPosDesc pd = { .pos = pos, .piece = CC_PE_None, .tag = CC_TE_None, .momentum = momentum };

    if ( cb ) {
        pd.piece = cc_chessboard_get_piece( cb, pos.i, pos.j );
        pd.tag = cc_chessboard_get_tag( cb, pos.i, pos.j );
    }

    return pd;
}

bool cc_calc_checked_momentum( cc_uint_t * momentum__io, CcMaybeBoolEnum accumulating ) {
    if ( !momentum__io ) return false;

    cc_uint_t m = *momentum__io;

    if ( CC_MAYBE_IS_TRUE( accumulating ) ) {
        if ( m == UINT_MAX ) return false;
        *momentum__io = m + 1;
    } else if ( CC_MAYBE_IS_VOID( accumulating ) ) {
        if ( m == CC_UNSIGNED_MIN ) return false;
        *momentum__io = m - 1;
    } // If accumulating is CC_MBE_False, momentum stays the same, e.g. for Wave.

    return true;
}

// TODO :: REDO
CcPathLink * cc_build_path_segment__new( CcChessboard * cb,
                                         CcPos pos,
                                         cc_uint_t momentum,
                                         CcMaybeBoolEnum accumulating,
                                         CcTypedStepLink * steps ) {
    if ( !cb ) return NULL;
    if ( !steps ) return NULL;

    if ( !cc_pos_is_valid( pos ) ) return NULL;

    CcPathLink * pl__a = NULL;
    CcPos p = pos;
    CcPos last = CC_POS_INVALID;
    CcTypedStepLink * step = steps;

    bool result = true;
    bool is_on_board = true;
    cc_uint_t m = momentum;

    while ( result && step ) {
        last = p = cc_pos_add( p, step->step.step, 1 );

        if ( !( result = cc_calc_checked_momentum( &m, accumulating ) ) ) break;

        if ( !( result = cc_path_link_append( &pl__a, p, m ) ) ) break;

        step = step->next; // TODO :: redo
    }

    // Pieces can step outside chessboard ... e.g. Wave activated by Centaur.
    // Last position of any piece must be on-board.
    is_on_board = cc_chessboard_is_pos_on_board( cb, last.i, last.j );

    if ( !result || !is_on_board ) {
        cc_path_link_free_all( &pl__a );
        return NULL;
    }

    return pl__a;
}


bool cc_iter_piece_pos( CcChessboard * cb,
                        CcPos expected__d,
                        CcPieceType piece,
                        bool include_opponent,
                        CcPos * pos__io ) {
    if ( !cb ) return false;
    if ( !pos__io ) return false;

    cc_uint_t size = cc_variant_board_size( cb->type );
    CcPos pos = *pos__io;

    // Next position to check.
    if ( !cc_chessboard_is_pos_on_board( cb, pos.i, pos.j ) )
        pos = CC_POS_CAST_ORIGIN_FIELD;
    else if ( pos.j < (int)( size - 1 ) )
        pos = CC_POS_CAST( pos.i, pos.j + 1 );
    else
        pos = CC_POS_CAST( pos.i + 1, 0 );

    bool is_comparable = cc_pos_is_valid( expected__d ) ||
                         cc_pos_is_disambiguation( expected__d );

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
