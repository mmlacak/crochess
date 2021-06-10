// Copyright (c) 2021 Mario Mlačak, mmlacak@gmail.com
// Licensed under 3-clause (modified) BSD license. See LICENSE for details.

#include <stdlib.h>

#include "cc_defines.h"
#include "cc_piece.h"
#include "cc_step.h"
#include "cc_ply.h"

#include "cc_do_moves.h"


CcPlyLinkEnum * cc_get_next_ply_link( CcPly const * const restrict ply )
{
    if ( !ply ) return NULL;
    if ( !ply->next ) return NULL;
    return &( ply->next->link );
}

bool cc_is_teleporting_next( CcPly const * const restrict ply, bool including_wave )
{
    CcPlyLinkEnum * pl = cc_get_next_ply_link( ply );
    if ( !pl ) return false;

    bool result = ( ( *pl == CC_PLE_Teleportation )
                 || ( *pl == CC_PLE_FailedTeleportation )
                 || ( *pl == CC_PLE_FailedTeleportationOblation ) );

    if ( including_wave )
        result = result || ( *pl == CC_PLE_TeleportationWave );

    return result;
}


bool cc_do_all_plies( CcChessboard * const restrict cb, CcMove const * const restrict move, CcPly const * const restrict ply )
{
    if ( !cb ) return false;

    if ( !move ) return false;
    if ( !move->plies ) return false;

    if ( !ply ) return false;

    bool is_first_ply = ( move->plies == ply );

    CcPieceEnum pe = ply->piece;

    switch ( ply->link )
    {
        case CC_PLE_Ply :
        {
            CcStep * s = ply->ply.steps;

            while ( s )
            {
                switch ( s->link )
                {
                    case CC_SLE_Start :
                    {
                        if ( is_first_ply ) cc_chessboard_set_piece( cb, s->i, s->j, CC_PE_None );
                        break;
                    }

                    case CC_SLE_Next :
                    case CC_SLE_Distant :
                        break;

                    case CC_SLE_Destination :
                    {
                        CcSideEffect const * const se = &( s->side_effect );

                        switch ( se->type )
                        {
                            case CC_SEE_None :
                            {
                                if ( !cc_is_teleporting_next( ply, true ) )
                                {
                                    cc_chessboard_set_piece( cb, s->i, s->j, pe );
                                }

                                break;
                            }

                            case CC_SEE_Capture :
                            {
                                cc_chessboard_set_piece( cb, s->i, s->j, pe );
                                break;
                            }

                            case CC_SEE_Displacement : break; // shouldn't be here

                            case CC_SEE_EnPassant :
                            {
                                cc_chessboard_set_piece( cb, s->i, s->j, pe );

                                int i = se->en_passant.dest_i;
                                int j = se->en_passant.dest_j;
                                cc_chessboard_set_piece( cb, i, j, CC_PE_None );

                                break;
                            }

                            case CC_SEE_Castle :
                            {
                                cc_chessboard_set_piece( cb, s->i, s->j, pe );

                                CcPieceEnum rook = se->castle.rook;
                                int start_i = se->castle.start_i;
                                int start_j = se->castle.start_j;
                                int dest_i = se->castle.dest_i;
                                int dest_j = se->castle.dest_j;

                                cc_chessboard_set_piece( cb, start_i, start_j, CC_PE_None );
                                cc_chessboard_set_piece( cb, dest_i, dest_j, rook );

                                break;
                            }

                            case CC_SEE_Promotion :
                            {
                                CcPieceEnum new = se->promote.piece;
                                cc_chessboard_set_piece( cb, s->i, s->j, new );
                                break;
                            }

                            case CC_SEE_TagForPromotion :
                            {
                                cc_chessboard_set_tag( cb, s->i, s->j, CC_TE_DelayedPromotion );
                                break;
                            }

                            case CC_SEE_Conversion :
                            {
                                CcPieceEnum new = se->convert.piece;
                                cc_chessboard_set_piece( cb, s->i, s->j, new );
                                break;
                            }

                            case CC_SEE_FailedConversion : break; // Pyramid oblationed as usual, Starchild retains its original color; nothing to do here.

                            case CC_SEE_Demotion :
                            {
                                cc_chessboard_set_piece( cb, s->i, s->j, pe );

                                CcPieceEnum pe = se->demote.piece;
                                int i = se->demote.dest_i;
                                int j = se->demote.dest_j;
                                cc_chessboard_set_piece( cb, i, j, pe );

                                break;
                            }

                            case CC_SEE_Resurrection :
                            {
                                cc_chessboard_set_piece( cb, s->i, s->j, pe );

                                CcPieceEnum pe = se->resurrect.piece;
                                int i = se->resurrect.dest_i;
                                int j = se->resurrect.dest_j;
                                cc_chessboard_set_piece( cb, i, j, pe );

                                break;
                            }

                            case CC_SEE_FailedResurrection :
                            {
                                cc_chessboard_set_piece( cb, s->i, s->j, pe );
                                break; // Resurrection blocked, or no captured pieces, nothing to do here.
                            }
                        }

                        break;
                    }
                }

                s = s->next;
            }

            break;
        }

        case CC_PLE_Teleportation :
        case CC_PLE_FailedTeleportation :
        {
            int i = ply->teleport.i;
            int j = ply->teleport.j;

            cc_chessboard_set_piece( cb, i, j, pe );
            break;
        }

        case CC_PLE_TeleportationWave :
        {
            CcStep * s = ply->teleport_wave.steps;

            while ( s )
            {
                switch ( s->link )
                {
                    case CC_SLE_Start :
                    case CC_SLE_Next :
                    case CC_SLE_Distant :
                        break;

                    case CC_SLE_Destination :
                    {
                        cc_chessboard_set_piece( cb, s->i, s->j, pe );
                        break;
                    }
                }

                s = s->next;
            }

            break;
        }

        case CC_PLE_FailedTeleportationOblation : break; // Oblationed piece removed in previous ply, nothing to do here.

        case CC_PLE_TranceJourney :
        {
            CcStep * s = ply->trance_journey.steps;

            while ( s )
            {
                CcSideEffect se = s->side_effect;

                switch ( se.type )
                {
                    case CC_SEE_None : break;

                    case CC_SEE_Capture :
                    {
                        cc_chessboard_set_piece( cb, s->i, s->j, CC_PE_None );
                        break;
                    }

                    case CC_SEE_Displacement :
                    {
                        cc_chessboard_set_piece( cb, s->i, s->j, CC_PE_None );
                        cc_chessboard_set_piece( cb, se.displacement.dest_i, se.displacement.dest_j, se.displacement.piece );
                        break;
                    }

                    default : break; // Just to keep compilers happy; shouldn't be here.
                }

                switch ( s->link )
                {
                    case CC_SLE_Start : // In previous ply piece was activated, i.e. removed from board; so, nothing to do here.
                    case CC_SLE_Next :
                    case CC_SLE_Distant :
                        break;

                    case CC_SLE_Destination :
                    {
                        cc_chessboard_set_piece( cb, s->i, s->j, pe );
                        break;
                    }
                }

                s = s->next;
            }

            break;
        }

        case CC_PLE_DualTranceJourney :
        {
            CcPieceField * pf = ply->dual_trance_journey.captured;

            while ( pf )
            {
                cc_chessboard_set_piece( cb, pf->i, pf->j, CC_PE_None );
                pf = pf->next;
            }

            break;
        }

        case CC_PLE_FailedTranceJourney : break; // Current piece already removed from chessboard, nothing to do here.

        case CC_PLE_PawnSacrifice :
        {
            CcStep * s = ply->pawn_sacrifice.steps;

            while ( s )
            {
                CcSideEffect se = s->side_effect;

                switch ( se.type )
                {
                    case CC_SEE_None : break;

                    case CC_SEE_Capture :
                    {
                        cc_chessboard_set_piece( cb, s->i, s->j, CC_PE_None );
                        break;
                    }

                    default : break; // Just to keep compilers happy; shouldn't be here.
                }

                switch ( s->link )
                {
                    case CC_SLE_Start :
                    {
                        cc_chessboard_set_piece( cb, s->i, s->j, CC_PE_None );
                        break;
                    }

                    case CC_SLE_Next :
                    case CC_SLE_Distant :
                        break;

                    case CC_SLE_Destination :
                    {
                        if ( !cc_is_teleporting_next( ply, true ) )
                        {
                            cc_chessboard_set_piece( cb, s->i, s->j, pe );
                        }

                        break;
                    }
                }

                s = s->next;
            }

            break;
        }
    }

    return true;
}

bool cc_do_moves( CcChessboard * const restrict cb, CcMove const * const restrict move, bool do_only_last_move, bool do_all_moves )
{
    if ( !cb ) return false;
    if ( !move ) return false;

    bool result = true;
    CcMove const * mv = move;

    if ( do_only_last_move ) while ( mv->next ) mv = mv->next; // move != NULL --> mv != NULL

    while ( mv )
    {
        if ( !mv->plies ) return false;

        CcPly * p = mv->plies;
        while ( p )
        {
            result = result && cc_do_all_plies( cb, mv, p );
            p = p->next;
        }

        if ( !do_all_moves ) break;

        mv = mv->next;
    }

    return result;
}
