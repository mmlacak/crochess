// Copyright (c) 2021 Mario Mlačak, mmlacak@gmail.com
// Licensed under 3-clause (modified) BSD license. See LICENSE for details.

#ifndef __PLY_H__
#define __PLY_H__


#include <stdbool.h>

#include "cc_piece.h"
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
    PSET_Resurrection,
    PSET_FailedResurrection,
} PlySideEffectType;

typedef struct PlySideEffect
{
    PlySideEffectType type;

    union
    {
        struct { CcPieceEnum piece; bool is_promo_tag_lost; } capture;
        struct { int dest_i; int dest_j; } en_passant;
        struct { CcPieceEnum rook; int start_i; int start_j; int dest_i; int dest_j; } castle;
        struct { CcPieceEnum piece; } promote;
        struct { CcPieceEnum piece; bool is_promo_tag_lost; } convert;
        struct { CcPieceEnum piece; int dest_i; int dest_j; } demote;
        struct { CcPieceEnum piece; int dest_i; int dest_j; } resurrect;
    };
} PlySideEffect;

PlySideEffect ply_side_effect( PlySideEffectType type, CcPieceEnum piece, bool is_promo_tag_lost, int start_i, int start_j, int dest_i, int dest_j );

PlySideEffect ply_side_effect_none();
PlySideEffect ply_side_effect_capture( CcPieceEnum piece, bool is_promo_tag_lost );
PlySideEffect ply_side_effect_en_passant( int dest_i, int dest_j );
PlySideEffect ply_side_effect_castle( CcPieceEnum rook, int start_i, int start_j, int dest_i, int dest_j );
PlySideEffect ply_side_effect_promote( CcPieceEnum piece );
PlySideEffect ply_side_effect_tag_for_promotion();
PlySideEffect ply_side_effect_convert( CcPieceEnum piece, bool is_promo_tag_lost );
PlySideEffect ply_side_effect_failed_conversion();
PlySideEffect ply_side_effect_demote( CcPieceEnum piece, int dest_i, int dest_j );
PlySideEffect ply_side_effect_resurrect( CcPieceEnum piece, int dest_i, int dest_j );
PlySideEffect ply_side_effect_failed_resurrection();


typedef struct PieceField
{
    CcPieceEnum piece;
    int i;
    int j;
    struct PieceField * next;
} PieceField;

PieceField * ply_new_piece_field_alx( CcPieceEnum piece, int i, int j );
PieceField * ply_append_piece_field_alx( PieceField * const restrict piece_fields, CcPieceEnum piece, int i, int j );
bool ply_free_all_piece_fields( PieceField ** const piece_fields );


typedef struct Ply
{
    PlyLink link;
    CcPieceEnum piece;

    union
    {
        struct { Step * steps; } ply;
        struct { int i; int j; } teleport;
        struct { Step * steps; } teleport_wave;
        struct { int i; int j; } failed_teleport;
        struct { int i; int j; SideEffectStep * steps; } trance_journey;
        struct { PieceField * captured; } dual_trance_journey;
        struct { SideEffectStep * steps; } pawn_sacrifice;
    };

    PlySideEffect side_effect;
    struct Ply * next;
} Ply;

Ply * ply_new_alx(  PlyLink link, CcPieceEnum piece,
                    Step * const restrict steps, int i, int j,
                    SideEffectStep * const restrict side_effect_steps,
                    PieceField * const restrict captured,
                    PlySideEffect side_effect );
Ply * ply_append_alx(   Ply * const restrict plies,
                        PlyLink link, CcPieceEnum piece,
                        Step * const restrict steps, int i, int j,
                        SideEffectStep * const restrict side_effect_steps,
                        PieceField * const restrict captured,
                        PlySideEffect side_effect );
bool ply_free_all_plies( Ply ** const plies );

Ply * ply_new_ply_alx( CcPieceEnum piece, Step * const restrict steps, PlySideEffect side_effect );
Ply * ply_new_teleport_alx( CcPieceEnum piece, int i, int j, PlySideEffect side_effect );
Ply * ply_new_teleport_wave_alx( CcPieceEnum piece, Step * const restrict steps, PlySideEffect side_effect );
Ply * ply_new_failed_teleport_oblation_alx( CcPieceEnum piece, PlySideEffect side_effect );
Ply * ply_new_failed_teleport_alx( CcPieceEnum piece, int i, int j, PlySideEffect side_effect );
Ply * ply_new_trance_journey_alx( CcPieceEnum piece, int i, int j, SideEffectStep * const restrict steps, PlySideEffect side_effect );
Ply * ply_new_dual_trance_journey_alx( PieceField * const restrict captured, PlySideEffect side_effect );
Ply * ply_new_failed_trance_journey_alx( CcPieceEnum piece, PlySideEffect side_effect );
Ply * ply_new_pawn_sacrifice_alx( CcPieceEnum piece, SideEffectStep * const restrict steps, PlySideEffect side_effect );

Ply * ply_append_ply_alx( Ply * const restrict plies, CcPieceEnum piece, Step * const restrict steps, PlySideEffect side_effect );
Ply * ply_append_teleport_alx( Ply * const restrict plies, CcPieceEnum piece, int i, int j, PlySideEffect side_effect );
Ply * ply_append_teleport_wave_alx( Ply * const restrict plies, CcPieceEnum piece, Step * const restrict steps, PlySideEffect side_effect );
Ply * ply_append_failed_teleport_oblation_alx( Ply * const restrict plies, CcPieceEnum piece, PlySideEffect side_effect );
Ply * ply_append_failed_teleport_alx( Ply * const restrict plies, CcPieceEnum piece, int i, int j, PlySideEffect side_effect );
Ply * ply_append_trance_journey_alx( Ply * const restrict plies, CcPieceEnum piece, int i, int j, SideEffectStep * const restrict steps, PlySideEffect side_effect );
Ply * ply_append_dual_trance_journey_alx( Ply * const restrict plies, PieceField * const restrict captured, PlySideEffect side_effect );
Ply * ply_append_failed_trance_journey_alx( Ply * const restrict plies, CcPieceEnum piece, PlySideEffect side_effect );
Ply * ply_append_pawn_sacrifice_alx( Ply * const restrict plies, CcPieceEnum piece, SideEffectStep * const restrict steps, PlySideEffect side_effect );


#endif /* __PLY_H__ */
