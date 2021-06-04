// Copyright (c) 2021 Mario Mlačak, mmlacak@gmail.com
// Licensed under 3-clause (modified) BSD license. See LICENSE for details.

#include <stdlib.h>

#include "cc_defines.h"
#include "ply.h"


PlySideEffect ply_side_effect( PlySideEffectType type, PieceType piece, bool is_promo_tag_lost, int start_i, int start_j, int dest_i, int dest_j )
{
    PlySideEffect pse = { .type = type, };

    // Nothing more to do if type == PSET_None.
    if ( pse.type == PSET_Capture )
    {
        pse.capture.piece = piece;
        pse.capture.is_promo_tag_lost = is_promo_tag_lost;
    }
    else if ( pse.type == PSET_EnPassant )
    {
        pse.en_passant.dest_i = dest_i;
        pse.en_passant.dest_j = dest_j;
    }
    else if ( pse.type == PSET_Castle )
    {
        pse.castle.rook = piece;
        pse.castle.start_i = start_i;
        pse.castle.start_j = start_j;
        pse.castle.dest_i = dest_i;
        pse.castle.dest_j = dest_j;
    }
    else if ( pse.type == PSET_Promotion )
    {
        pse.promote.piece = piece;
    }
    // Nothing more to do if type == PSET_TagForPromotion.
    else if ( pse.type == PSET_Conversion )
    {
        pse.convert.piece = piece;
        pse.convert.is_promo_tag_lost = is_promo_tag_lost;
    }
    // Nothing more to do if type == PSET_FailedConversion.
    else if ( pse.type == PSET_Demotion )
    {
        pse.demote.piece = piece;
        pse.demote.dest_i = dest_i;
        pse.demote.dest_j = dest_j;
    }
    else if ( pse.type == PSET_Resurrection )
    {
        pse.resurrect.piece = piece;
        pse.resurrect.dest_i = dest_i;
        pse.resurrect.dest_j = dest_j;
    }
    // Nothing more to do if type == PSET_FailedResurrection.

    return pse;
}

PlySideEffect ply_side_effect_none()
{
    return ply_side_effect( PSET_None, PT_None, false, CC_OFF_BOARD_COORD, CC_OFF_BOARD_COORD, CC_OFF_BOARD_COORD, CC_OFF_BOARD_COORD );
}

PlySideEffect ply_side_effect_capture( PieceType piece, bool is_promo_tag_lost )
{
    return ply_side_effect( PSET_Capture, piece, is_promo_tag_lost, CC_OFF_BOARD_COORD, CC_OFF_BOARD_COORD, CC_OFF_BOARD_COORD, CC_OFF_BOARD_COORD );
}

PlySideEffect ply_side_effect_en_passant( int dest_i, int dest_j )
{
    return ply_side_effect( PSET_EnPassant, PT_None, false, CC_OFF_BOARD_COORD, CC_OFF_BOARD_COORD, dest_i, dest_j );
}

PlySideEffect ply_side_effect_castle( PieceType rook, int start_i, int start_j, int dest_i, int dest_j )
{
    return ply_side_effect( PSET_Castle, rook, false, start_i, start_j, dest_i, dest_j );
}

PlySideEffect ply_side_effect_promote( PieceType piece )
{
    return ply_side_effect( PSET_Promotion, piece, false, CC_OFF_BOARD_COORD, CC_OFF_BOARD_COORD, CC_OFF_BOARD_COORD, CC_OFF_BOARD_COORD );
}

PlySideEffect ply_side_effect_tag_for_promotion()
{
    return ply_side_effect( PSET_TagForPromotion, PT_None, false, CC_OFF_BOARD_COORD, CC_OFF_BOARD_COORD, CC_OFF_BOARD_COORD, CC_OFF_BOARD_COORD );
}

PlySideEffect ply_side_effect_convert( PieceType piece, bool is_promo_tag_lost )
{
    return ply_side_effect( PSET_Conversion, piece, is_promo_tag_lost, CC_OFF_BOARD_COORD, CC_OFF_BOARD_COORD, CC_OFF_BOARD_COORD, CC_OFF_BOARD_COORD );
}

PlySideEffect ply_side_effect_failed_conversion()
{
    return ply_side_effect( PSET_FailedConversion, PT_None, false, CC_OFF_BOARD_COORD, CC_OFF_BOARD_COORD, CC_OFF_BOARD_COORD, CC_OFF_BOARD_COORD );
}

PlySideEffect ply_side_effect_demote( PieceType piece, int dest_i, int dest_j )
{
    return ply_side_effect( PSET_Demotion, piece, false, CC_OFF_BOARD_COORD, CC_OFF_BOARD_COORD, dest_i, dest_j );
}

PlySideEffect ply_side_effect_resurrect( PieceType piece, int dest_i, int dest_j )
{
    return ply_side_effect( PSET_Resurrection, piece, false, CC_OFF_BOARD_COORD, CC_OFF_BOARD_COORD, dest_i, dest_j );
}

PlySideEffect ply_side_effect_failed_resurrection()
{
    return ply_side_effect( PSET_FailedResurrection, PT_None, false, CC_OFF_BOARD_COORD, CC_OFF_BOARD_COORD, CC_OFF_BOARD_COORD, CC_OFF_BOARD_COORD );
}


PieceField * ply_new_piece_field_alx( PieceType piece, int i, int j )
{
    PieceField * pf = malloc( sizeof( PieceField ) );
    if ( !pf ) return NULL;

    pf->piece = piece;
    pf->i = i;
    pf->j = j;
    pf->next = NULL;

    return pf;
}

PieceField * ply_append_piece_field_alx( PieceField * const restrict piece_fields, PieceType piece, int i, int j )
{
    PieceField * new = ply_new_piece_field_alx( piece, i, j );
    if ( !new ) return NULL;
    if ( !piece_fields ) return new;

    PieceField * pf = piece_fields;
    while ( pf->next ) pf = pf->next; // rewind
    pf->next = new; // append

    return new;
}

bool ply_free_all_piece_fields( PieceField ** const piece_fields )
{
    if ( !piece_fields ) return true;
    if ( !*piece_fields ) return false;

    PieceField * pf = *piece_fields;

    while ( pf )
    {
        PieceField * tmp = pf->next;
        free( pf );
        pf = tmp;
    }

    *piece_fields = NULL;
    return true;
}


Ply * ply_new_alx(  PlyLink link, PieceType piece,
                    Step * const restrict steps, int i, int j,
                    SideEffectStep * const restrict side_effect_steps,
                    PieceField * const restrict captured,
                    PlySideEffect side_effect )
{
    Ply * ply = calloc( 1, sizeof( Ply ) );
    if ( !ply ) return NULL;

    ply->link = link;
    ply->piece = piece;

    if ( ply->link == PL_Ply )
    {
        ply->ply.steps = steps;
    }
    else if ( ply->link == PL_Teleportation )
    {
        ply->teleport.i = i;
        ply->teleport.j = j;
    }
    else if ( ply->link == PL_TeleportationWave )
    {
        ply->teleport_wave.steps = steps;
    }
    // Nothing additional to do if link == PL_FailedTeleportationOblation.
    else if ( ply->link == PL_FailedTeleportation )
    {
        ply->failed_teleport.i = i;
        ply->failed_teleport.j = j;
    }
    else if ( ply->link == PL_TranceJourney )
    {
        ply->trance_journey.i = i;
        ply->trance_journey.j = j;
        ply->trance_journey.steps = side_effect_steps;
    }
    else if ( ply->link == PL_DualTranceJourney )
    {
        ply->dual_trance_journey.captured = captured;
    }
    // Nothing additional to do if link == PL_FailedTranceJourney.
    else if ( ply->link == PL_PawnSacrifice )
    {
        ply->pawn_sacrifice.steps = side_effect_steps;
    }

    ply->side_effect = side_effect;
    ply->next = NULL;

    return ply;
}

Ply * ply_append_alx(   Ply * const restrict plies,
                        PlyLink link, PieceType piece,
                        Step * const restrict steps, int i, int j,
                        SideEffectStep * const restrict side_effect_steps,
                        PieceField * const restrict captured,
                        PlySideEffect side_effect )
{
    Ply * new = ply_new_alx( link, piece, steps, i, j, side_effect_steps, captured, side_effect );
    if ( !new ) return NULL;
    if ( !plies ) return new;

    Ply * p = plies;
    while ( p->next ) p = p->next; // rewind
    p->next = new; // append

    return new;
}

bool ply_free_all_plies( Ply ** const plies )
{
    if ( !plies ) return true;
    if ( !*plies ) return false;

    bool result = true;
    Ply * ply = *plies;

    while ( ply )
    {
        switch ( ply->link )
        {
            case PL_Ply :
            {
                Step ** steps = &( ply->ply.steps );
                result = result && step_free_all_steps( steps );
                break;
            }

            case PL_TeleportationWave :
            {
                Step ** steps = &( ply->teleport_wave.steps );
                result = result && step_free_all_steps( steps );
                break;
            }

            case PL_TranceJourney :
            {
                SideEffectStep ** steps = &( ply->trance_journey.steps );
                result = result && step_free_all_side_effect_steps( steps );
                break;
            }

            case PL_DualTranceJourney :
            {
                PieceField ** captured = &( ply->dual_trance_journey.captured );
                result = result && ply_free_all_piece_fields( captured );
                break;
            }

            case PL_PawnSacrifice :
            {
                SideEffectStep ** steps = &( ply->pawn_sacrifice.steps );
                result = result && step_free_all_side_effect_steps( steps );
                break;
            }

            case PL_Teleportation :
            case PL_FailedTeleportationOblation :
            case PL_FailedTeleportation :
            case PL_FailedTranceJourney :
                break;
        }

        Ply * tmp = ply->next;
        free( ply );
        ply = tmp;
    }

    *plies = NULL;
    return result;
}


Ply * ply_new_ply_alx( PieceType piece, Step * const restrict steps, PlySideEffect side_effect )
{
    return ply_new_alx( PL_Ply, piece, steps, CC_OFF_BOARD_COORD, CC_OFF_BOARD_COORD, NULL, NULL, side_effect );
}

Ply * ply_new_teleport_alx( PieceType piece, int i, int j, PlySideEffect side_effect )
{
    return ply_new_alx( PL_Teleportation, piece, NULL, i, j, NULL, NULL, side_effect );
}

Ply * ply_new_teleport_wave_alx( PieceType piece, Step * const restrict steps, PlySideEffect side_effect )
{
    return ply_new_alx( PL_TeleportationWave, piece, steps, CC_OFF_BOARD_COORD, CC_OFF_BOARD_COORD, NULL, NULL, side_effect );
}

Ply * ply_new_failed_teleport_oblation_alx( PieceType piece, PlySideEffect side_effect )
{
    return ply_new_alx( PL_FailedTeleportationOblation, piece, NULL, CC_OFF_BOARD_COORD, CC_OFF_BOARD_COORD, NULL, NULL, side_effect );
}

Ply * ply_new_failed_teleport_alx( PieceType piece, int i, int j, PlySideEffect side_effect )
{
    return ply_new_alx( PL_FailedTeleportation, piece, NULL, i, j, NULL, NULL, side_effect );
}

Ply * ply_new_trance_journey_alx( PieceType piece, int i, int j, SideEffectStep * const restrict steps, PlySideEffect side_effect )
{
    return ply_new_alx( PL_TranceJourney, piece, NULL, i, j, steps, NULL, side_effect );
}

Ply * ply_new_dual_trance_journey_alx( PieceField * const restrict captured, PlySideEffect side_effect )
{
    return ply_new_alx( PL_DualTranceJourney, PT_None, NULL, CC_OFF_BOARD_COORD, CC_OFF_BOARD_COORD, NULL, captured, side_effect );
}

Ply * ply_new_failed_trance_journey_alx( PieceType piece, PlySideEffect side_effect )
{
    return ply_new_alx( PL_FailedTranceJourney, piece, NULL, CC_OFF_BOARD_COORD, CC_OFF_BOARD_COORD, NULL, NULL, side_effect );
}

Ply * ply_new_pawn_sacrifice_alx( PieceType piece, SideEffectStep * const restrict steps, PlySideEffect side_effect )
{
    return ply_new_alx( PL_PawnSacrifice, piece, NULL, CC_OFF_BOARD_COORD, CC_OFF_BOARD_COORD, steps, NULL, side_effect );
}


Ply * ply_append_ply_alx( Ply * const restrict plies, PieceType piece, Step * const restrict steps, PlySideEffect side_effect )
{
    return ply_append_alx( plies, PL_Ply, piece, steps, CC_OFF_BOARD_COORD, CC_OFF_BOARD_COORD, NULL, NULL, side_effect );
}

Ply * ply_append_teleport_alx( Ply * const restrict plies, PieceType piece, int i, int j, PlySideEffect side_effect )
{
    return ply_append_alx( plies, PL_Teleportation, piece, NULL, i, j, NULL, NULL, side_effect );
}

Ply * ply_append_teleport_wave_alx( Ply * const restrict plies, PieceType piece, Step * const restrict steps, PlySideEffect side_effect )
{
    return ply_append_alx( plies, PL_TeleportationWave, piece, steps, CC_OFF_BOARD_COORD, CC_OFF_BOARD_COORD, NULL, NULL, side_effect );
}

Ply * ply_append_failed_teleport_oblation_alx( Ply * const restrict plies, PieceType piece, PlySideEffect side_effect )
{
    return ply_append_alx( plies, PL_FailedTeleportationOblation, piece, NULL, CC_OFF_BOARD_COORD, CC_OFF_BOARD_COORD, NULL, NULL, side_effect );
}

Ply * ply_append_failed_teleport_alx( Ply * const restrict plies, PieceType piece, int i, int j, PlySideEffect side_effect )
{
    return ply_append_alx( plies, PL_FailedTeleportation, piece, NULL, i, j, NULL, NULL, side_effect );
}

Ply * ply_append_trance_journey_alx( Ply * const restrict plies, PieceType piece, int i, int j, SideEffectStep * const restrict steps, PlySideEffect side_effect )
{
    return ply_append_alx( plies, PL_TranceJourney, piece, NULL, i, j, steps, NULL, side_effect );
}

Ply * ply_append_dual_trance_journey_alx( Ply * const restrict plies, PieceField * const restrict captured, PlySideEffect side_effect )
{
    return ply_append_alx( plies, PL_DualTranceJourney, PT_None, NULL, CC_OFF_BOARD_COORD, CC_OFF_BOARD_COORD, NULL, captured, side_effect );
}

Ply * ply_append_failed_trance_journey_alx( Ply * const restrict plies, PieceType piece, PlySideEffect side_effect )
{
    return ply_append_alx( plies, PL_FailedTranceJourney, piece, NULL, CC_OFF_BOARD_COORD, CC_OFF_BOARD_COORD, NULL, NULL, side_effect );
}

Ply * ply_append_pawn_sacrifice_alx( Ply * const restrict plies, PieceType piece, SideEffectStep * const restrict steps, PlySideEffect side_effect )
{
    return ply_append_alx( plies, PL_PawnSacrifice, piece, NULL, CC_OFF_BOARD_COORD, CC_OFF_BOARD_COORD, steps, NULL, side_effect );
}
