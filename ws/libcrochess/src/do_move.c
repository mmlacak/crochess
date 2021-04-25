// Copyright (c) 2021 Mario Mlačak, mmlacak@gmail.com
// Licensed under 3-clause (modified) BSD license. See LICENSE for details.


#include "piece_type.h"
#include "step.h"
#include "ply.h"

#include "do_move.h"


bool do_ply( Chessboard * const restrict cb, Move const * const restrict move, Ply const * const restrict ply )
{
    if ( !cb ) return false;

    if ( !move ) return false;
    if ( !move->plies ) return false;

    if ( !ply ) return false;

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
                        cb_set_piece( cb, s->i, s->j, PT_None );
                        break;
                    };

                    case SL_Next :
                    case SL_Distant :
                        break;

                    case SL_Destination :
                    {
                        cb_set_piece( cb, s->i, s->j, pt );
                        break;
                    };
                }

                s = s->next;
            };

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
                    };
                }

                s = s->next;
            };

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
                };

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
                    };
                }

                s = s->next;
            };

            break;
        }

        case PL_DualTranceJourney :
        {
            PieceField * pf = ply->dual_trance_journey.captured;

            while ( pf )
            {
                cb_set_piece( cb, pf->i, pf->j, PT_None );
                pf = pf->next;
            };

            break;
        }

        case PL_FailedTranceJourney : break; // Current piece already removed from chessboard, nothing to do here.

        case PL_PawnSacrifice :
        {
            PawnSacrificeCaptureStep * s = ply->pawn_sacrifice.steps;

            while ( s )
            {
                switch ( s->link )
                {
                    case SL_Start :
                    case SL_Next :
                    case SL_Distant :
                    {
                        cb_set_piece( cb, s->i, s->j, PT_None );
                        break;
                    };

                    case SL_Destination :
                    {
                        cb_set_piece( cb, s->i, s->j, pt );
// TODO :: teleportation
                        break;
                    };
                }

                s = s->next;
            };

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
    };

    return true;
}
