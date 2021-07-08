// Copyright (c) 2021 Mario Mlačak, mmlacak@gmail.com
// Licensed under 3-clause (modified) BSD license. See LICENSE for details.

#include <stdlib.h>

#include "cc_defines.h"
#include "cc_ply.h"


CcPieceField * cc_ply_piece_field_new( CcPieceEnum piece, int i, int j )
{
    CcPieceField * pf = malloc( sizeof( CcPieceField ) );
    if ( !pf ) return NULL;

    pf->piece = piece;
    pf->i = i;
    pf->j = j;
    pf->next = NULL;

    return pf;
}

CcPieceField * cc_ply_append_piece_field_new( CcPieceField * const restrict piece_fields, CcPieceEnum piece, int i, int j )
{
    CcPieceField * new = cc_ply_piece_field_new( piece, i, j );
    if ( !new ) return NULL;
    if ( !piece_fields ) return new;

    CcPieceField * pf = piece_fields;
    while ( pf->next ) pf = pf->next; // rewind
    pf->next = new; // append

    return new;
}

bool cc_ply_free_all_piece_fields( CcPieceField ** const piece_fields )
{
    if ( !piece_fields ) return true;
    if ( !*piece_fields ) return false;

    CcPieceField * pf = *piece_fields;

    while ( pf )
    {
        CcPieceField * tmp = pf->next;
        free( pf );
        pf = tmp;
    }

    *piece_fields = NULL;
    return true;
}


CcPly * cc_ply_new( CcPlyLinkEnum link, CcPieceEnum piece,
                    CcStep ** restrict steps, int i, int j,
                    CcPieceField ** restrict captured )
{
    if ( ( !steps ) && ( ( link == CC_PLE_Ply )
                      || ( link == CC_PLE_TeleportationWave )
                      || ( link == CC_PLE_TranceJourney )
                      || ( link == CC_PLE_PawnSacrifice ) ) ) return NULL;

    if ( ( !captured ) && ( link == CC_PLE_DualTranceJourney ) ) return NULL;

    CcPly * ply = calloc( 1, sizeof( CcPly ) );
    if ( !ply ) return NULL;

    ply->link = link;
    ply->piece = piece;

    if ( ply->link == CC_PLE_Ply )
    {
        ply->ply.steps = *steps;
        *steps = NULL;
    }
    else if ( ply->link == CC_PLE_Teleportation )
    {
        ply->teleport.i = i;
        ply->teleport.j = j;
    }
    else if ( ply->link == CC_PLE_TeleportationWave )
    {
        ply->teleport_wave.steps = *steps;
        *steps = NULL;
    }
    // Nothing additional to do if link == CC_PLE_FailedTeleportationOblation.
    else if ( ply->link == CC_PLE_FailedTeleportation )
    {
        ply->failed_teleport.i = i;
        ply->failed_teleport.j = j;
    }
    else if ( ply->link == CC_PLE_TranceJourney )
    {
        ply->trance_journey.i = i;
        ply->trance_journey.j = j;
        ply->trance_journey.steps = *steps;
        *steps = NULL;
    }
    else if ( ply->link == CC_PLE_DualTranceJourney )
    {
        ply->dual_trance_journey.captured = *captured;
        *captured = NULL;
    }
    // Nothing additional to do if link == CC_PLE_FailedTranceJourney.
    else if ( ply->link == CC_PLE_PawnSacrifice )
    {
        ply->pawn_sacrifice.steps = *steps;
        *steps = NULL;
    }

    ply->next = NULL;

    return ply;
}

CcPly * cc_ply_append_new(  CcPly * const restrict plies,
                            CcPlyLinkEnum link, CcPieceEnum piece,
                            CcStep ** restrict steps, int i, int j,
                            CcPieceField ** restrict captured )
{
    CcPly * new = cc_ply_new( link, piece, steps, i, j, captured );
    if ( !new ) return NULL;
    if ( !plies ) return new;

    CcPly * p = plies;
    while ( p->next ) p = p->next; // rewind
    p->next = new; // append

    return new;
}

bool cc_ply_free_all_plies( CcPly ** const plies )
{
    if ( !plies ) return true;
    if ( !*plies ) return false;

    bool result = true;
    CcPly * ply = *plies;

    while ( ply )
    {
        switch ( ply->link )
        {
            case CC_PLE_Ply :
            {
                CcStep ** steps = &( ply->ply.steps );
                result = result && cc_step_free_all_steps( steps );
                break;
            }

            case CC_PLE_TeleportationWave :
            {
                CcStep ** steps = &( ply->teleport_wave.steps );
                result = result && cc_step_free_all_steps( steps );
                break;
            }

            case CC_PLE_TranceJourney :
            {
                CcStep ** steps = &( ply->trance_journey.steps );
                result = result && cc_step_free_all_steps( steps );
                break;
            }

            case CC_PLE_DualTranceJourney :
            {
                CcPieceField ** captured = &( ply->dual_trance_journey.captured );
                result = result && cc_ply_free_all_piece_fields( captured );
                break;
            }

            case CC_PLE_PawnSacrifice :
            {
                CcStep ** steps = &( ply->pawn_sacrifice.steps );
                result = result && cc_step_free_all_steps( steps );
                break;
            }

            case CC_PLE_Teleportation :
            case CC_PLE_FailedTeleportationOblation :
            case CC_PLE_FailedTeleportation :
            case CC_PLE_FailedTranceJourney :
                break;
        }

        CcPly * tmp = ply->next;
        free( ply );
        ply = tmp;
    }

    *plies = NULL;
    return result;
}


CcPly * cc_ply_cascade_new( CcPieceEnum piece, CcStep ** restrict steps )
{
    return cc_ply_new( CC_PLE_Ply, piece, steps, CC_OFF_BOARD_COORD, CC_OFF_BOARD_COORD, NULL );
}

CcPly * cc_ply_teleport_new( CcPieceEnum piece, int i, int j )
{
    return cc_ply_new( CC_PLE_Teleportation, piece, NULL, i, j, NULL );
}

CcPly * cc_ply_teleport_wave_new( CcPieceEnum piece, CcStep ** restrict steps )
{
    return cc_ply_new( CC_PLE_TeleportationWave, piece, steps, CC_OFF_BOARD_COORD, CC_OFF_BOARD_COORD, NULL );
}

CcPly * cc_ply_failed_teleport_oblation_new( CcPieceEnum piece )
{
    return cc_ply_new( CC_PLE_FailedTeleportationOblation, piece, NULL, CC_OFF_BOARD_COORD, CC_OFF_BOARD_COORD, NULL );
}

CcPly * cc_ply_failed_teleport_new( CcPieceEnum piece, int i, int j )
{
    return cc_ply_new( CC_PLE_FailedTeleportation, piece, NULL, i, j, NULL );
}

CcPly * cc_ply_trance_journey_new( CcPieceEnum piece, CcStep ** restrict steps, int i, int j )
{
    return cc_ply_new( CC_PLE_TranceJourney, piece, steps, i, j, NULL );
}

CcPly * cc_ply_dual_trance_journey_new( CcPieceField ** restrict captured )
{
    return cc_ply_new( CC_PLE_DualTranceJourney, CC_PE_None, NULL, CC_OFF_BOARD_COORD, CC_OFF_BOARD_COORD, captured );
}

CcPly * cc_ply_failed_trance_journey_new( CcPieceEnum piece )
{
    return cc_ply_new( CC_PLE_FailedTranceJourney, piece, NULL, CC_OFF_BOARD_COORD, CC_OFF_BOARD_COORD, NULL );
}

CcPly * cc_ply_pawn_sacrifice_new( CcPieceEnum piece, CcStep ** restrict steps )
{
    return cc_ply_new( CC_PLE_PawnSacrifice, piece, steps, CC_OFF_BOARD_COORD, CC_OFF_BOARD_COORD, NULL );
}


CcPly * cc_ply_cascade_append_new( CcPly * const restrict plies, CcPieceEnum piece, CcStep ** restrict steps )
{
    return cc_ply_append_new( plies, CC_PLE_Ply, piece, steps, CC_OFF_BOARD_COORD, CC_OFF_BOARD_COORD, NULL );
}

CcPly * cc_ply_teleport_append_new( CcPly * const restrict plies, CcPieceEnum piece, int i, int j )
{
    return cc_ply_append_new( plies, CC_PLE_Teleportation, piece, NULL, i, j, NULL );
}

CcPly * cc_ply_teleport_wave_append_new( CcPly * const restrict plies, CcPieceEnum piece, CcStep ** restrict steps )
{
    return cc_ply_append_new( plies, CC_PLE_TeleportationWave, piece, steps, CC_OFF_BOARD_COORD, CC_OFF_BOARD_COORD, NULL );
}

CcPly * cc_ply_failed_teleport_oblation_append_new( CcPly * const restrict plies, CcPieceEnum piece )
{
    return cc_ply_append_new( plies, CC_PLE_FailedTeleportationOblation, piece, NULL, CC_OFF_BOARD_COORD, CC_OFF_BOARD_COORD, NULL );
}

CcPly * cc_ply_failed_teleport_append_new( CcPly * const restrict plies, CcPieceEnum piece, int i, int j )
{
    return cc_ply_append_new( plies, CC_PLE_FailedTeleportation, piece, NULL, i, j, NULL );
}

CcPly * cc_ply_trance_journey_append_new( CcPly * const restrict plies, CcPieceEnum piece, CcStep ** restrict steps, int i, int j )
{
    return cc_ply_append_new( plies, CC_PLE_TranceJourney, piece, steps, i, j, NULL );
}

CcPly * cc_ply_dual_trance_journey_append_new( CcPly * const restrict plies, CcPieceField ** restrict captured )
{
    return cc_ply_append_new( plies, CC_PLE_DualTranceJourney, CC_PE_None, NULL, CC_OFF_BOARD_COORD, CC_OFF_BOARD_COORD, captured );
}

CcPly * cc_ply_failed_trance_journey_append_new( CcPly * const restrict plies, CcPieceEnum piece )
{
    return cc_ply_append_new( plies, CC_PLE_FailedTranceJourney, piece, NULL, CC_OFF_BOARD_COORD, CC_OFF_BOARD_COORD, NULL );
}

CcPly * cc_ply_pawn_sacrifice_append_new( CcPly * const restrict plies, CcPieceEnum piece, CcStep ** restrict steps )
{
    return cc_ply_append_new( plies, CC_PLE_PawnSacrifice, piece, steps, CC_OFF_BOARD_COORD, CC_OFF_BOARD_COORD, NULL );
}


CcStep * cc_ply_get_steps( CcPly const * const restrict ply )
{
    CcStep * s = NULL;

    switch ( ply->link )
    {
        case CC_PLE_Ply : s = ply->ply.steps; break;
        case CC_PLE_TeleportationWave : s = ply->teleport_wave.steps; break;
        case CC_PLE_TranceJourney : s = ply->trance_journey.steps; break;
        case CC_PLE_PawnSacrifice : s = ply->pawn_sacrifice.steps; break;
        default : /* s = NULL; */ break;
    }

    return s;
}

bool cc_ply_contains_side_effects( CcPly const * const restrict ply )
{
    if ( !ply ) return false;

    CcStep const * steps = cc_ply_get_steps( ply );
    if ( !steps ) return false;

    CcStep const * s = steps;
    while ( s->next )
    {
        if ( s->side_effect.type != CC_SEE_None ) return true;
        s = s->next;
    }

    return false;
}

size_t cc_ply_step_count( CcPly const * const restrict ply, bool include_starting_pos )
{
    if ( !ply ) return 0;

    CcStep const * steps = cc_ply_get_steps( ply );
    if ( !steps ) return 0;

    size_t count = 1;
    CcStep const * s = steps;

    while ( s->next )
    {
        if ( s->link == CC_SLE_Start )
        {
            if ( include_starting_pos ) ++count;
        }
        else
            ++count;

        s = s->next;
    }

    return count;
}
