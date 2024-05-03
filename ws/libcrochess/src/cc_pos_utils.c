// Copyright (c) 2024 Mario Mlačak, mmlacak@gmail.com
// Licensed under GNU GPL v3+ license. See LICENSING, COPYING files for details.

// #include "cc_path_defs.h"
#include "cc_pos_utils.h"

/**
    @file cc_pos_utils.c
    @brief Pos utils.
*/


CcPosPieceTag cc_convert_pos_to_ppt( CcChessboard * cb,
                                     CcPos pos ) {
    CcPosPieceTag ppt = { .pos = pos, .piece = CC_PE_None, .tag = CC_TE_None };

    if ( cb ) {
        ppt.piece = cc_chessboard_get_piece( cb, pos.i, pos.j );
        ppt.tag = cc_chessboard_get_tag( cb, pos.i, pos.j );
    }

    return ppt;
}

CcPptLink * cc_convert_pos_steps_to_ppt_link__new( CcChessboard * cb,
                                                   CcPos current_pos,
                                                   CcPosLink * steps ) {
    if ( !cb ) return NULL;
    if ( !steps ) return NULL;

    if ( !cc_pos_is_valid( current_pos ) ) return NULL;

    CcPptLink * ppt_link__a = NULL;
    CcPosLink * step = steps;
    bool is_failed = false;

    while ( !is_failed && step ) {
        CcPos p = cc_pos_add( current_pos, step->pos, 1 );
        if ( ( is_failed = cc_chessboard_is_pos_on_board( cb, p.i, p.j ) ) ) break;

        CcPosPieceTag ppt = cc_convert_pos_to_ppt( cb, p );
        if ( ( is_failed = cc_ppt_link_append( &ppt_link__a, ppt ) ) ) break;

        step = step->next;
    }

    if ( is_failed ) {
        cc_ppt_link_free_all( &ppt_link__a );
        return NULL;
    }

    return ppt_link__a;
}

CcPptLink * cc_convert_pos_link_to_ppt_link__new( CcChessboard * cb,
                                                  CcPosLink * pos_link ) {
    if ( !cb ) return NULL;
    if ( !pos_link ) return NULL;

    CcPptLink * ppt_link__a = NULL;
    CcPosLink * p = pos_link;

    while ( p ) {
        CcPosPieceTag ppt = cc_convert_pos_to_ppt( cb, p->pos );

        if ( !cc_ppt_link_append( &ppt_link__a, ppt ) ) {
            cc_ppt_link_free_all( &ppt_link__a );
            return NULL;
        }

        p = p->next;
    }

    return ppt_link__a;
}


bool cc_validate_ppt_link( CcChessboard * cb,
                           CcPptLink * ppt_link ) {
    if ( !cb ) return false;
    if ( !ppt_link ) return false;

    CcPptLink * pl = ppt_link;

    while ( pl ) {
        CcPos pos = pl->ppt.pos;

        CcPieceEnum piece = cc_chessboard_get_piece( cb, pos.i, pos.j );
        if ( piece != pl->ppt.piece ) return false;

        CcTagEnum tag = cc_chessboard_get_tag( cb, pos.i, pos.j );
        if ( tag != pl->ppt.tag ) return false;

        pl = pl->next;
    }

    return true;
}

bool cc_update_ppt_link( CcChessboard * cb,
                         CcPptLink * ppt_link__io ) {
    if ( !cb ) return false;
    if ( !ppt_link__io ) return false;

    CcPptLink * p = ppt_link__io;

    while ( p ) {
        p->ppt = cc_convert_pos_to_ppt( cb, p->ppt.pos );

        p = p->next;
    }

    return true;
}

bool cc_apply_ppt_link( CcChessboard ** cb__io_a,
                        CcPptLink * ppt_link ) {
    if ( !cb__io_a ) return false;
    if ( !*cb__io_a ) return false;
    if ( !ppt_link ) return false;

    CcPptLink * pl = ppt_link;
    CcChessboard * cb__t = cc_chessboard_duplicate__new( *cb__io_a );

    while ( pl ) {
        CcPosPieceTag ppt = pl->ppt;
        CcPos p = ppt.pos;

        if ( !cc_chessboard_set_piece_tag( cb__t, p.i, p.j, ppt.piece, ppt.tag ) ) {
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
