// Copyright (c) 2024 Mario Mlačak, mmlacak@gmail.com
// Licensed under GNU GPL v3+ license. See LICENSING, COPYING files for details.

#include "cc_pos_utils.h"

/**
    @file cc_pos_utils.c
    @brief Pos utils.
*/


CcPosDesc cc_convert_pos_to_ppt( CcChessboard * cb, CcPos pos ) {
    CcPosDesc pd = { .pos = pos, .piece = CC_PE_None, .tag = CC_TE_None };

    if ( cb ) {
        pd.piece = cc_chessboard_get_piece( cb, pos.i, pos.j );
        pd.tag = cc_chessboard_get_tag( cb, pos.i, pos.j );
    }

    return pd;
}

CcPosDescLink * cc_convert_steps_to_positions__new( CcChessboard * cb,
                                                CcPos current_pos,
                                                CcTypedStepLink * steps ) {
    if ( !cb ) return NULL;
    if ( !steps ) return NULL;

    if ( !cc_pos_is_valid( current_pos ) ) return NULL;

    CcPosDescLink * pd_link__a = NULL;
    CcPos pos = current_pos;
    CcTypedStepLink * step = steps;
    bool result = true;

    while ( result && step ) {
        pos = cc_pos_add( pos, step->step.step, 1 );
        // <i> Pieces can step outside chessboard ... e.g. Wave activated by Centaur.
        // if ( !( result = cc_chessboard_is_pos_on_board( cb, pos.i, pos.j ) ) ) break;

        CcPosDesc pd = cc_convert_pos_to_ppt( cb, pos );
        if ( !( result = cc_pos_desc_link_append( &pd_link__a, pd ) ) ) break;

        step = step->next;
    }

    if ( !result ) {
        cc_pos_desc_link_free_all( &pd_link__a );
        return NULL;
    }

    return pd_link__a;
}


bool cc_validate_pd_link( CcChessboard * cb, CcPosDescLink * pd_link ) {
    if ( !cb ) return false;
    if ( !pd_link ) return false;

    CcPosDescLink * pl = pd_link;

    while ( pl ) {
        CcPos pos = pl->pd.pos;

        CcPieceEnum piece = cc_chessboard_get_piece( cb, pos.i, pos.j );
        if ( piece != pl->pd.piece ) return false;

        CcTagEnum tag = cc_chessboard_get_tag( cb, pos.i, pos.j );
        if ( tag != pl->pd.tag ) return false;

        pl = pl->next;
    }

    return true;
}

bool cc_update_pd_link( CcChessboard * cb, CcPosDescLink * pd_link__io ) {
    if ( !cb ) return false;
    if ( !pd_link__io ) return false;

    CcPosDescLink * p = pd_link__io;

    while ( p ) {
        p->pd = cc_convert_pos_to_ppt( cb, p->pd.pos );

        p = p->next;
    }

    return true;
}

bool cc_apply_pd_link( CcChessboard ** cb__io_a, CcPosDescLink * pd_link ) {
    if ( !cb__io_a ) return false;
    if ( !*cb__io_a ) return false;
    if ( !pd_link ) return false;

    CcPosDescLink * pl = pd_link;
    CcChessboard * cb__t = cc_chessboard_duplicate__new( *cb__io_a );

    while ( pl ) {
        CcPosDesc pd = pl->pd;
        CcPos p = pd.pos;

        if ( !cc_chessboard_set_piece_tag( cb__t, p.i, p.j, pd.piece, pd.tag ) ) {
            cc_chessboard_free_all( &cb__t );
            return false;
        }

        pl = pl->next;
    }

    cc_chessboard_free_all( cb__io_a );
    *cb__io_a = cb__t; // Ownership transfer.

    return true;
}

bool cc_iter_piece_pos( CcChessboard * cb,
                        CcPos expected,
                        CcPieceEnum piece,
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

    bool is_comparable = cc_pos_is_valid( expected ) ||
                         cc_pos_is_disambiguation( expected );

    for ( int i = pos.i; i < size; ++i ) {
        for ( int j = pos.j; j < size; ++j ) {
            CcPieceEnum pe = cc_chessboard_get_piece( cb, i, j );

            if ( ( pe == piece ) ||
                    ( include_opponent && cc_piece_is_opposite( pe, piece ) ) ) {
                CcPos current = CC_POS_CAST( i, j );

                if ( ( !is_comparable ) ||
                       cc_pos_is_congruent( expected, current ) ) {
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
