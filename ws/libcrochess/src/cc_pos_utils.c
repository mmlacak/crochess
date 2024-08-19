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

bool cc_calc_checked_momentum( cc_uint_t * momentum__io, bool accumulating ) {
    if ( !momentum__io ) return false;

    cc_uint_t m = *momentum__io;

    if ( accumulating ) {
        if ( m == UINT_MAX ) return false;
        *momentum__io = m + 1;
    } else {
        if ( m == CC_UNSIGNED_MIN ) return false;
        *momentum__io = m - 1;
    }

    return true;
}

CcPosDescLink * cc_apply_steps_to_position__new( CcChessboard * cb,
                                                 CcPos pos,
                                                 cc_uint_t momentum,
                                                 bool accumulating,
                                                 CcTypedStepLink * steps ) {
    if ( !cb ) return NULL;
    if ( !steps ) return NULL;

    if ( !cc_pos_is_valid( pos ) ) return NULL;

    CcPosDescLink * pd_link__a = NULL;
    CcPos p = pos;
    CcPos last = CC_POS_INVALID;
    CcTypedStepLink * step = steps;

    bool result = true;
    bool is_on_board = true;
    cc_uint_t m = momentum;

    while ( result && step ) {
        last = p = cc_pos_add( p, step->step.step, 1 );

        if ( !( result = cc_calc_checked_momentum( &m, accumulating ) ) ) break;

        CcPosDesc pd = cc_convert_pos_to_pos_desc( cb, p, m );
        if ( !( result = cc_pos_desc_link_append( &pd_link__a, pd ) ) ) break;

        step = step->next;
    }

    // Pieces can step outside chessboard ... e.g. Wave activated by Centaur.
    // Last position of any piece must be on-board.
    is_on_board = cc_chessboard_is_pos_on_board( cb, last.i, last.j );

    if ( !result || !is_on_board ) {
        cc_pos_desc_link_free_all( &pd_link__a );
        return NULL;
    }

    return pd_link__a;
}


bool cc_append_pos_to_pos_desc_link( CcChessboard * cb,
                                     CcPos pos,
                                     cc_uint_t momentum,
                                     CcPosDescLink ** pdl__iod_a ) {
    if ( !cb ) return false;
    if ( !pdl__iod_a ) return false;

    CcPosDesc pd = cc_convert_pos_to_pos_desc( cb, pos, momentum );

    if ( !cc_pos_desc_link_append( pdl__iod_a, pd ) ) return false;

    return true;
}

bool cc_validate_pos_desc_link( CcChessboard * cb, CcPosDescLink * pd_link ) {
    if ( !cb ) return false;
    if ( !pd_link ) return false;

    CcPosDescLink * pdl = pd_link;

    // TODO :: check momentum forms a strictly increasing/decreasing sequance
    // cc_uint_t momentum = pdl->pd.momentum;

    while ( pdl ) {
        CcPosDesc pd = pdl->pd;
        CcPos pos = pd.pos;

        CcPieceType piece = cc_chessboard_get_piece( cb, pos.i, pos.j );
        if ( piece != pd.piece ) return false;

        CcTagType tag = cc_chessboard_get_tag( cb, pos.i, pos.j );
        if ( tag != pd.tag ) return false;

        if ( pdl != pd_link ) { // If not a 1st pos desc == starting pos --> momentum = 0.
            if ( pd.momentum == CC_UNSIGNED_MIN ) {
                if ( !CC_PIECE_IS_WEIGHTLESS( pd.piece ) )
                    return ( !pdl->next ); // No steps should follow if momentum is 0, and piece has weight (i.e. all, but Wave, Starchild).
            }
        }

        pdl = pdl->next;
    }

    return true;
}

bool cc_update_pos_desc_link( CcChessboard * cb, CcPosDescLink * pd_link__io ) {
    if ( !cb ) return false;
    if ( !pd_link__io ) return false;

    CcPosDescLink * p = pd_link__io;

    while ( p ) {
        CcPos pos = p->pd.pos;

        CcPieceType piece = cc_chessboard_get_piece( cb, pos.i, pos.j );
        p->pd.piece = piece;

        CcTagType tag = cc_chessboard_get_tag( cb, pos.i, pos.j );
        p->pd.tag = tag;

        p = p->next;
    }

    return true;
}

bool cc_apply_pos_desc_link( CcChessboard ** cb__io_r, CcPosDescLink * pd_link ) {
    if ( !cb__io_r ) return false;
    if ( !*cb__io_r ) return false;
    if ( !pd_link ) return false;

    CcPosDescLink * pdl = pd_link;
    CcChessboard * cb__t = cc_chessboard_duplicate__new( *cb__io_r );

    while ( pdl ) {
        CcPosDesc pd = pdl->pd;
        CcPos p = pd.pos;

        if ( !cc_chessboard_set_piece_tag( cb__t, p.i, p.j, pd.piece, pd.tag ) ) {
            cc_chessboard_free_all( &cb__t );
            return false;
        }

        pdl = pdl->next;
    }

    // free() + ownership transfer ~= realloc().
    cc_chessboard_free_all( cb__io_r );
    *cb__io_r = cb__t;

    return true;
}

bool cc_iter_piece_pos( CcChessboard * cb,
                        CcPos expected__d,
                        CcPieceType piece,
                        bool include_opponent,
                        CcPos * pos__io ) {
    if ( !cb ) return false;
    if ( !pos__io ) return false;

    int size = (int)cb->size;
    CcPos pos = *pos__io;

    // Next position to check.
    if ( !cc_chessboard_is_pos_on_board( cb, pos.i, pos.j ) )
        pos = CC_POS_CAST_ORIGIN_FIELD;
    else if ( pos.j < size - 1 )
        pos = CC_POS_CAST( pos.i, pos.j + 1 );
    else
        pos = CC_POS_CAST( pos.i + 1, 0 );

    bool is_comparable = cc_pos_is_valid( expected__d ) ||
                         cc_pos_is_disambiguation( expected__d );

    for ( int i = pos.i; i < size; ++i ) {
        for ( int j = pos.j; j < size; ++j ) {
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
