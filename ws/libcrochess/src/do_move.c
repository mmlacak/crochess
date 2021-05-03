// Copyright (c) 2021 Mario Mlačak, mmlacak@gmail.com
// Licensed under 3-clause (modified) BSD license. See LICENSE for details.

#include <stdlib.h>

#include "defines.h"
#include "piece_type.h"
#include "step.h"
#include "ply.h"

#include "do_move.h"


PlyLink * next_ply_link( Ply const * const restrict ply )
{
    if ( !ply ) return NULL;
    if ( !ply->next ) return NULL;
    return &( ply->next->link );
}

bool is_teleporting( Ply const * const restrict ply )
{
    PlyLink * pl = next_ply_link( ply );
    if ( !pl ) return false;

    return ( ( *pl == PL_Teleportation )
          || ( *pl == PL_TeleportationWave )
          || ( *pl == PL_FailedTeleportation )
          || ( *pl == PL_FailedTeleportationOblation ) );
}


bool do_ply( Chessboard * const restrict cb, Move const * const restrict move, Ply const * const restrict ply )
{
    if ( !cb ) return false;

    if ( !move ) return false;
    if ( !move->plies ) return false;

    if ( !ply ) return false;

    bool is_first_ply = ( move->plies == ply );

    PieceType pt = ply->piece;

    switch ( ply->link )
    {
        case PL_Ply :
        {
            Step * s = ply->ply.steps;

            while ( s )
            {
                switch ( s->link )
                {
                    case SL_Start :
                    {
                        if ( is_first_ply ) cb_set_piece( cb, s->i, s->j, PT_None );
                        break;
                    }

                    case SL_Next :
                    case SL_Distant :
                        break;

                    case SL_Destination :
                    {
                        PlySideEffect const * const pse = &( ply->side_effect );

                        switch ( pse->type )
                        {
                            case PSET_None :
                            {
                                if ( !is_teleporting( ply ) )
                                {
                                    cb_set_piece( cb, s->i, s->j, pt );
                                }

                                break;
                            }

                            case PSET_Capture :
                            {
                                cb_set_piece( cb, s->i, s->j, pt );
                                break;
                            }

                            case PSET_EnPassant :
                            {
                                cb_set_piece( cb, s->i, s->j, pt );

                                int i = pse->en_passant.dest_i;
                                int j = pse->en_passant.dest_j;
                                cb_set_piece( cb, i, j, PT_None );

                                break;
                            }

                            case PSET_Castle :
                            {
                                cb_set_piece( cb, s->i, s->j, pt );

                                PieceType rook = pse->castle.rook;
                                int start_i = pse->castle.start_i;
                                int start_j = pse->castle.start_j;
                                int dest_i = pse->castle.dest_i;
                                int dest_j = pse->castle.dest_j;

                                cb_set_piece( cb, start_i, start_j, PT_None );
                                cb_set_piece( cb, dest_i, dest_j, rook );

                                break;
                            }

                            case PSET_Promotion :
                            {
                                PieceType new = pse->promote.piece;
                                cb_set_piece( cb, s->i, s->j, new );
                                break;
                            }

                            case PSET_TagForPromotion :
                            {
                                cb_set_tag( cb, s->i, s->j, TT_DelayedPromotion );
                                break;
                            }

                            case PSET_Conversion :
                            {
                                PieceType new = pse->convert.piece;
                                cb_set_piece( cb, s->i, s->j, new );
                                break;
                            }

                            case PSET_FailedConversion : break; // Pyramid oblationed as usual, Starchild retains its original color; nothing to do here.

                            case PSET_Demotion :
                            {
                                cb_set_piece( cb, s->i, s->j, pt );

                                PieceType pt = pse->demote.piece;
                                int i = pse->demote.dest_i;
                                int j = pse->demote.dest_j;
                                cb_set_piece( cb, i, j, pt );

                                break;
                            }

                            case PSET_Resurrection :
                            {
                                cb_set_piece( cb, s->i, s->j, pt );

                                PieceType pt = pse->resurrect.piece;
                                int i = pse->resurrect.dest_i;
                                int j = pse->resurrect.dest_j;
                                cb_set_piece( cb, i, j, pt );

                                break;
                            }

                            case PSET_FailedResurrection :
                            {
                                cb_set_piece( cb, s->i, s->j, pt );
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

        case PL_Teleportation :
        case PL_FailedTeleportation :
        {
            int i = ply->teleport.i;
            int j = ply->teleport.j;

            cb_set_piece( cb, i, j, pt );
            break;
        }

        case PL_TeleportationWave :
        {
            Step * s = ply->teleport_wave.steps;

            while ( s )
            {
                switch ( s->link )
                {
                    case SL_Start :
                    case SL_Next :
                    case SL_Distant :
                        break;

                    case SL_Destination :
                    {
                        cb_set_piece( cb, s->i, s->j, pt );
                        break;
                    }
                }

                s = s->next;
            }

            break;
        }

        case PL_FailedTeleportationOblation : break; // Oblationed piece removed in previous ply, nothing to do here.

        case PL_TranceJourney :
        {
            TranceJourneyStep * s = ply->trance_journey.steps;

            while ( s )
            {
                StepSideEffect sse = s->side_effect;

                switch ( sse.type )
                {
                    case SSET_None : break;

                    case SSET_Capture :
                    {
                        cb_set_piece( cb, s->i, s->j, PT_None );
                        break;
                    }

                    case SSET_Displacement :
                    {
                        cb_set_piece( cb, s->i, s->j, PT_None );
                        cb_set_piece( cb, sse.displacement.i, sse.displacement.j, sse.displacement.piece );
                        break;
                    }
                }

                switch ( s->link )
                {
                    case SL_Start : // In previous ply piece was activated, i.e. removed from board; so, nothing to do here.
                    case SL_Next :
                    case SL_Distant :
                        break;

                    case SL_Destination :
                    {
                        cb_set_piece( cb, s->i, s->j, pt );
                        break;
                    }
                }

                s = s->next;
            }

            break;
        }

        case PL_DualTranceJourney :
        {
            PieceField * pf = ply->dual_trance_journey.captured;

            while ( pf )
            {
                cb_set_piece( cb, pf->i, pf->j, PT_None );
                pf = pf->next;
            }

            break;
        }

        case PL_FailedTranceJourney : break; // Current piece already removed from chessboard, nothing to do here.

        case PL_PawnSacrifice :
        {
            PawnSacrificeCaptureStep * s = ply->pawn_sacrifice.steps;

            while ( s )
            {
                StepSideEffect sse = s->side_effect;

                switch ( sse.type )
                {
                    case SSET_None : break;

                    case SSET_Capture :
                    {
                        cb_set_piece( cb, s->i, s->j, PT_None );
                        break;
                    }

                    case SSET_Displacement : break; // Just to keep compilers happy; shouldn't be here.
                }

                switch ( s->link )
                {
                    case SL_Start :
                    {
                        cb_set_piece( cb, s->i, s->j, PT_None );
                        break;
                    }

                    case SL_Next :
                    case SL_Distant :
                        break;

                    case SL_Destination :
                    {
                        if ( !is_teleporting( ply ) )
                        {
                            cb_set_piece( cb, s->i, s->j, pt );
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

bool do_move( Chessboard * const restrict cb, Move const * const restrict move )
{
    if ( !cb ) return false;

    if ( !move ) return false;
    if ( !move->plies ) return false;

    Ply * p = move->plies;

    while ( p )
    {
        do_ply( cb, move, p );
        p = p->next;
    }

    return true;
}
