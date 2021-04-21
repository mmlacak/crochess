// Copyright (c) 2021 Mario Mlačak, mmlacak@gmail.com
// Licensed under 3-clause (modified) BSD license. See LICENSE for details.

#ifndef __PLY_H__
#define __PLY_H__


#include <stdbool.h>

#include "piece_type.h"
#include "step.h"


typedef enum PlyLink
{
    PL_Ply, // Just one ply, starting or continuing cascade.
    PL_Teleportation,
    PL_TeleportationWave,
    PL_FailedTeleportationOblation,
    PL_FailedTeleportation,
    PL_TranceJourney,
    PL_DualTranceJourney,
    PL_FailedTranceJourney,
    PL_PawnSacrifice,
} PlyLink;

typedef enum PlySideEffectType
{
    PSET_None,
    PSET_Capture,
    PSET_EnPassant,
    PSET_Castle,
    PSET_Promotion,
    PSET_TagForPromotion,
    PSET_Conversion,
    PSET_FailedConversion,
    PSET_Demotion,
    PSET_Ressurecion,
    PSET_FailedRessurecion,
} PlySideEffectType;

typedef struct PlySideEffect
{
    PlySideEffectType type;

    union
    {
        struct { PieceType piece; bool is_promo_tag_lost; } capture;
        struct { int i; int j; } en_passant;
        struct { int i; int j; } castle;
        struct { PieceType piece; } promote;
        struct { PieceType piece; bool is_promo_tag_lost; } convert;
        struct { PieceType piece; int i; int j; } demote;
        struct { PieceType piece; int i; int j; } resurrect;
    };
} PlySideEffect;

typedef struct PieceField
{
    PieceType piece;
    int i;
    int j;
    struct PieceField * next;
} PieceField;

typedef struct Ply
{
    PlyLink link;

    union
    {
        struct { PieceType piece; Step * steps; } ply;
        struct { int i; int j; } teleport;
        struct { Step * steps; } teleport_wave;
        struct { PieceType piece; int i; int j; } failed_teleport;
        struct { PieceType piece; } failed_teleport_oblation;
        struct { PieceType piece; int i; int j; TranceJourneyStep * steps; } trance_journey;
        struct { PieceField * captured; } dual_trance_journey;
        struct { PieceType piece; } failed_trance_journey;
    };

    unsigned int momentum;
    PlySideEffect side_effect;
    struct Ply * next;
} Ply;


#endif /* __PLY_H__ */
