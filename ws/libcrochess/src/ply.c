// Copyright (c) 2021 Mario Mlačak, mmlacak@gmail.com
// Licensed under 3-clause (modified) BSD license. See LICENSE for details.

#include <stdlib.h>

#include "defines.h"
#include "ply.h"


PlySideEffect * ply_new_side_effect_alx( PlySideEffectType type, PieceType piece, bool is_promo_tag_lost, int i, int j )
{
    PlySideEffect * pse = calloc( 1, sizeof( PlySideEffect ) );
    if ( !pse ) return NULL;

    pse->type = type;

    // Nothing more to do if type == PSET_None.
    if ( pse->type == PSET_Capture )
    {
        pse->capture.piece = piece;
        pse->capture.is_promo_tag_lost = is_promo_tag_lost;
    }
    else if ( pse->type == PSET_EnPassant )
    {
        pse->en_passant.i = i;
        pse->en_passant.j = j;
    }
    else if ( pse->type == PSET_Castle )
    {
        pse->castle.i = i;
        pse->castle.j = j;
    }
    else if ( pse->type == PSET_Promotion )
    {
        pse->promote.piece = piece;
    }
    // Nothing more to do if type == PSET_TagForPromotion.
    else if ( pse->type == PSET_Conversion )
    {
        pse->convert.piece = piece;
        pse->convert.is_promo_tag_lost = is_promo_tag_lost;
    }
    // Nothing more to do if type == PSET_FailedConversion.
    else if ( pse->type == PSET_Demotion )
    {
        pse->demote.piece = piece;
        pse->demote.i = i;
        pse->demote.j = j;
    }
    else if ( pse->type == PSET_Ressurecion )
    {
        pse->resurrect.piece = piece;
        pse->resurrect.i = i;
        pse->resurrect.j = j;
    }
    // Nothing more to do if type == PSET_FailedRessurecion.

    return pse;
}

PlySideEffect * ply_new_side_effect_none_alx()
{
    return ply_new_side_effect_alx( PSET_None, PT_None, false, OFF_BOARD_COORD, OFF_BOARD_COORD );
}

PlySideEffect * ply_new_side_effect_capture_alx( PieceType piece, bool is_promo_tag_lost )
{
    return ply_new_side_effect_alx( PSET_Capture, piece, is_promo_tag_lost, OFF_BOARD_COORD, OFF_BOARD_COORD );
}

PlySideEffect * ply_new_side_effect_en_passant_alx( int i, int j )
{
    return ply_new_side_effect_alx( PSET_EnPassant, PT_None, false, i, j );
}

PlySideEffect * ply_new_side_effect_castle_alx( int i, int j )
{
    return ply_new_side_effect_alx( PSET_Castle, PT_None, false, i, j );
}

PlySideEffect * ply_new_side_effect_promote_alx( PieceType piece )
{
    return ply_new_side_effect_alx( PSET_Promotion, piece, false, OFF_BOARD_COORD, OFF_BOARD_COORD );
}

PlySideEffect * ply_new_side_effect_tag_for_promotion_alx()
{
    return ply_new_side_effect_alx( PSET_TagForPromotion, PT_None, false, OFF_BOARD_COORD, OFF_BOARD_COORD );
}

PlySideEffect * ply_new_side_effect_convert_alx( PieceType piece, bool is_promo_tag_lost )
{
    return ply_new_side_effect_alx( PSET_Conversion, piece, is_promo_tag_lost, OFF_BOARD_COORD, OFF_BOARD_COORD );
}

PlySideEffect * ply_new_side_effect_failed_conversion_alx()
{
    return ply_new_side_effect_alx( PSET_FailedConversion, PT_None, false, OFF_BOARD_COORD, OFF_BOARD_COORD );
}

PlySideEffect * ply_new_side_effect_demote_alx( PieceType piece, int i, int j )
{
    return ply_new_side_effect_alx( PSET_Demotion, piece, false, i, j );
}

PlySideEffect * ply_new_side_effect_resurrect_alx( PieceType piece, int i, int j )
{
    return ply_new_side_effect_alx( PSET_Ressurecion, piece, false, i, j );
}

PlySideEffect * ply_new_side_effect_failed_resurrection_alx()
{
    return ply_new_side_effect_alx( PSET_FailedRessurecion, PT_None, false, OFF_BOARD_COORD, OFF_BOARD_COORD );
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


Ply * ply_new_alx( PlyLink link,
                   PieceType piece, Step * steps, int i, int j, TranceJourneyStep * trance_journey_steps, PieceField * captured,
                   unsigned int momentum, PlySideEffect side_effect )
{
    Ply * ply = calloc( 1, sizeof( Ply ) );
    if ( !ply ) return NULL;

    ply->link = link;

    if ( ply->link == PL_Ply )
    {
        ply->ply.piece = piece;
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
    else if ( ply->link == PL_FailedTeleportationOblation )
    {
        ply->failed_teleport_oblation.piece = piece;
    }
    else if ( ply->link == PL_FailedTeleportation )
    {
        ply->failed_teleport.piece = piece;
        ply->failed_teleport.i = i;
        ply->failed_teleport.j = j;
    }
    else if ( ply->link == PL_TranceJourney )
    {
        ply->trance_journey.piece = piece;
        ply->trance_journey.i = i;
        ply->trance_journey.j = j;
        ply->trance_journey.steps = trance_journey_steps;
    }
    else if ( ply->link == PL_DualTranceJourney )
    {
        ply->dual_trance_journey.captured = captured;
    }
    else if ( ply->link == PL_FailedTranceJourney )
    {
        ply->failed_trance_journey.piece = piece;
    }
    // Nothing additional to do if link == PL_PawnSacrifice.

    ply->momentum = momentum;
    ply->side_effect = side_effect;
    ply->next = NULL;

    return ply;
}

Ply * ply_new_ply_alx( PieceType piece, Step * steps, unsigned int momentum, PlySideEffect side_effect )
{
    return ply_new_alx( PL_Ply, piece, steps, OFF_BOARD_COORD, OFF_BOARD_COORD, NULL, NULL, momentum, side_effect );
}

Ply * ply_new_teleport_alx( int i, int j, unsigned int momentum, PlySideEffect side_effect )
{
    return ply_new_alx( PL_Teleportation, PT_None, NULL, i, j, NULL, NULL, momentum, side_effect );
}

Ply * ply_new_teleport_wave_alx( Step * steps, unsigned int momentum, PlySideEffect side_effect )
{
    return ply_new_alx( PL_TeleportationWave, PT_None, steps, OFF_BOARD_COORD, OFF_BOARD_COORD, NULL, NULL, momentum, side_effect );
}

Ply * ply_new_failed_teleport_oblation_alx( PieceType piece, unsigned int momentum, PlySideEffect side_effect )
{
    return ply_new_alx( PL_FailedTeleportationOblation, piece, NULL, OFF_BOARD_COORD, OFF_BOARD_COORD, NULL, NULL, momentum, side_effect );
}

Ply * ply_new_failed_teleport_alx( PieceType piece, int i, int j, unsigned int momentum, PlySideEffect side_effect )
{
    return ply_new_alx( PL_FailedTeleportation, piece, NULL, i, j, NULL, NULL, momentum, side_effect );
}

Ply * ply_new_trance_journey_alx( PieceType piece, int i, int j, TranceJourneyStep * steps, unsigned int momentum, PlySideEffect side_effect )
{
    return ply_new_alx( PL_TranceJourney, piece, NULL, i, j, steps, NULL, momentum, side_effect );
}

Ply * ply_new_dual_trance_journey_alx( PieceField * captured, unsigned int momentum, PlySideEffect side_effect )
{
    return ply_new_alx( PL_DualTranceJourney, PT_None, NULL, OFF_BOARD_COORD, OFF_BOARD_COORD, NULL, captured, momentum, side_effect );
}

Ply * ply_new_failed_trance_journey_alx( PieceType piece, unsigned int momentum, PlySideEffect side_effect )
{
    return ply_new_alx( PL_FailedTranceJourney, piece, NULL, OFF_BOARD_COORD, OFF_BOARD_COORD, NULL, NULL, momentum, side_effect );
}
