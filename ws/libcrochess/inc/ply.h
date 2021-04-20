// Copyright (c) 2021 Mario Mlačak, mmlacak@gmail.com
// Licensed under 3-clause (modified) BSD license. See LICENSE for details.

#ifndef __PLY_H__
#define __PLY_H__

#include <stdbool.h>

#include "piece_type.h"
#include "step.h"


typedef enum PlyLink
{
    PL_Start,
    PL_Cascade,
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


typedef struct Ply
{
    PlyLink link;

    union
    {
        struct { PieceType piece; Step * steps; } start;
        struct { PieceType piece; Step * steps; } cascade;
        struct { int i; int j; } teleportation;
    };

// Step * steps;
    PlySideEffect side_effect;
    struct Ply * next;
} Ply;


#endif /* __PLY_H__ */
