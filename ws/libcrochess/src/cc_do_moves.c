// Copyright (c) 2021 Mario Mlačak, mmlacak@gmail.com
// Licensed under GNU GPL v3+ license. See LICENSING, COPYING files for details.

#include <stdlib.h>

#include "cc_defines.h"
#include "cc_piece.h"
#include "cc_tag.h"
#include "cc_step.h"
#include "cc_ply.h"

#include "cc_do_moves.h"

/**
    @file cc_do_moves.c
    @brief Applying moves, plies, steps to chessboard.
*/


CcPlyLinkEnum * cc_get_next_ply_link( CcPly * restrict ply )
{
    if ( !ply ) return NULL;
    if ( !ply->next ) return NULL;
    return &( ply->next->link );
}

bool cc_is_teleporting_next( CcPly * restrict ply )
{
    CcPlyLinkEnum * pl = cc_get_next_ply_link( ply );
    if ( !pl ) return false;

    bool result = ( ( *pl == CC_PLE_Teleportation )
                 || ( *pl == CC_PLE_FailedTeleportation ) );

    return result;
}

bool cc_remove_all_temporarily_tags( CcChessboard * restrict cb__io )
{
    if ( !cb__io ) return false;

    CcChessboard * cb__a = cc_chessboard_duplicate_new( cb__io );
    if ( !cb__a ) return false;

    bool result = true;

    for ( int i = 0; (size_t)i < cb__io->size; ++i )
    {
        for ( int j = 0; (size_t)j < cb__io->size; ++j )
        {
            CcTagEnum te = cc_chessboard_get_tag( cb__a, i, j );

            if ( CC_TAG_IS_TEMPORARILY( te ) )
                result = result && cc_chessboard_set_tag( cb__a, i, j, CC_TE_None );
        }
    }

    if ( result ) cc_chessboard_copy( cb__io, cb__a );

    cc_chessboard_free_all( &cb__a );

    return result;
}


bool cc_do_step( CcChessboard * restrict cb__io,
                 CcMove * restrict move,
                 CcPly * restrict ply,
                 CcStep * restrict step )
{
    if ( !cb__io ) return false;
    if ( !move ) return false;
    if ( !move->plies ) return false;
    if ( !ply ) return false;
    if ( !step ) return false;

    CcPieceEnum pe = ply->piece;
    CcStep * steps = ply->steps;
    if ( !steps ) return false;

    CcStep * last = steps;
    while ( last->next ) { last = last->next; }

    bool result = true;
    bool is_first_ply = ( move->plies == ply );
    bool is_first_step = ( steps == step );
    bool is_last_step = ( last == step );

    if ( is_first_ply && is_first_step )
        result = result && cc_chessboard_set_piece_tag( cb__io, step->i, step->j, CC_PE_None, CC_TE_None );
    else
    {
        CcSideEffect * se = &( step->side_effect );

        switch ( se->type )
        {
            case CC_SEE_None :
            {
                if ( is_last_step && ( !cc_is_teleporting_next( ply ) ) )
                    result = result && cc_chessboard_set_piece_tag( cb__io, step->i, step->j, pe, CC_TE_None );
                break;
            }

            case CC_SEE_Capture :
            {
                if ( is_last_step )
                    result = result && cc_chessboard_set_piece_tag( cb__io, step->i, step->j, pe, CC_TE_None );
                else
                    result = result && cc_chessboard_set_piece_tag( cb__io, step->i, step->j, CC_PE_None, CC_TE_None );
                break;
            }

            case CC_SEE_Displacement :
            {
                if ( is_last_step )
                    result = result && cc_chessboard_set_piece_tag( cb__io, step->i, step->j, pe, CC_TE_None );
                else
                    result = result && cc_chessboard_set_piece_tag( cb__io, step->i, step->j, CC_PE_None, CC_TE_None );

                result = result && cc_chessboard_set_piece_tag( cb__io, se->displacement.dest_i, se->displacement.dest_j, se->displacement.piece, CC_TE_None );
                break;
            }

            case CC_SEE_EnPassant :
            {
                result = result && cc_chessboard_set_piece_tag( cb__io, step->i, step->j, pe, CC_TE_None );

                int i = se->en_passant.dest_i;
                int j = se->en_passant.dest_j;
                result = result && cc_chessboard_set_piece_tag( cb__io, i, j, CC_PE_None, CC_TE_None );

                break;
            }

            case CC_SEE_Castle :
            {
                result = result && cc_chessboard_set_piece_tag( cb__io, step->i, step->j, pe, CC_TE_None );

                CcPieceEnum rook = se->castle.rook;
                int start_i = se->castle.start_i;
                int start_j = se->castle.start_j;
                int dest_i = se->castle.dest_i;
                int dest_j = se->castle.dest_j;

                result = result && cc_chessboard_set_piece_tag( cb__io, start_i, start_j, CC_PE_None, CC_TE_None );
                result = result && cc_chessboard_set_piece_tag( cb__io, dest_i, dest_j, rook, CC_TE_None );

                break;
            }

            case CC_SEE_Promotion :
            {
                CcPieceEnum new = se->promote.piece;
                result = result && cc_chessboard_set_piece_tag( cb__io, step->i, step->j, new, CC_TE_None );
                break;
            }

            case CC_SEE_TagForPromotion :
            {
                cc_chessboard_set_tag( cb__io, step->i, step->j, CC_TE_DelayedPromotion );
                break;
            }

            case CC_SEE_Conversion :
            {
                CcPieceEnum new = se->convert.piece;
                result = result && cc_chessboard_set_piece_tag( cb__io, step->i, step->j, new, CC_TE_None );
                break;
            }

            case CC_SEE_FailedConversion : break; // Pyramid oblationed as usual, Starchild retains its original color; nothing to do here.

            case CC_SEE_Demotion :
            {
                result = result && cc_chessboard_set_piece_tag( cb__io, step->i, step->j, pe, CC_TE_None );

                CcPieceEnum pawn = cc_piece_demoting_to( se->demote.piece );
                int i = se->demote.dest_i;
                int j = se->demote.dest_j;
                result = result && cc_chessboard_set_piece_tag( cb__io, i, j, pawn, CC_TE_None );

                break;
            }

            case CC_SEE_Resurrection :
            {
                result = result && cc_chessboard_set_piece_tag( cb__io, step->i, step->j, pe, CC_TE_None );

                CcPieceEnum pe = se->resurrect.piece;
                int i = se->resurrect.dest_i;
                int j = se->resurrect.dest_j;
                result = result && cc_chessboard_set_piece_tag( cb__io, i, j, pe, CC_TE_None );

                break;
            }

            case CC_SEE_FailedResurrection :
            {
                result = result && cc_chessboard_set_piece_tag( cb__io, step->i, step->j, pe, CC_TE_None );
                break; // Resurrection blocked, or no captured pieces, nothing to do here.
            }
        }
    }

    return result;
}

bool cc_do_ply( CcChessboard * restrict cb__io,
                CcMove * restrict move,
                CcPly * restrict ply )
{
    // if ( !cb__io ) return false;

    // if ( !move ) return false;
    // if ( !move->plies ) return false;

    if ( !ply ) return false;

    bool result = true;
    CcStep * s = ply->steps;

    while ( s && result )
    {
        result = result && cc_do_step( cb__io, move, ply, s );
        s = s->next;
    }

    return result;
}

bool cc_do_moves( CcChessboard * restrict cb__io,
                  CcMove * restrict moves,
                  CcDoMoveEnum do_spec )
{
    if ( !cb__io ) return false;
    if ( !moves ) return false;

    CcChessboard * cb__a = cc_chessboard_duplicate_new( cb__io );
    if ( !cb__a ) return false;

    bool result = true;
    CcMove * mv = moves;

    if ( do_spec == CC_DME_DoOnlyLastMove )
        while ( mv->next ) mv = mv->next; // moves != NULL --> mv != NULL

    while ( mv && result )
    {
        if ( !mv->plies )
        {
            cc_chessboard_free_all( &cb__a );
            return false;
        }

        CcPly * p = mv->plies;
        while ( p && result )
        {
            result = result && cc_do_ply( cb__a, mv, p );
            p = p->next;
        }

        if ( do_spec != CC_DME_DoAllMoves ) break;

        mv = mv->next;
    }

    result = result && cc_remove_all_temporarily_tags( cb__a );

    if ( result ) cc_chessboard_copy( cb__io, cb__a );

    cc_chessboard_free_all( &cb__a );

    return result;
}
