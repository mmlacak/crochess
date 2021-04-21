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



// typedef enum PlyLink
// {
//     PL_Ply, // Just one ply, starting or continuing cascade.
//     PL_Teleportation,
//     PL_TeleportationWave,
//     PL_FailedTeleportationOblation,
//     PL_FailedTeleportation,
//     PL_TranceJourney,
//     PL_DualTranceJourney,
//     PL_FailedTranceJourney,
//     PL_PawnSacrifice,
// } PlyLink;

// typedef struct Ply
// {
//     PlyLink link;

//     union
//     {
//         struct { PieceType piece; Step * steps; } ply;
//         struct { int i; int j; } teleport;
//         struct { Step * steps; } teleport_wave;
//         struct { PieceType piece; int i; int j; } failed_teleport;
//         struct { PieceType piece; } failed_teleport_oblation;
//         struct { PieceType piece; int i; int j; TranceJourneyStep * steps; } trance_journey;
//         struct { PieceField * captured; } dual_trance_journey;
//         struct { PieceType piece; } failed_trance_journey;
//     };

//     unsigned int momentum;
//     PlySideEffect side_effect;
//     struct Ply * next;
// } Ply;
