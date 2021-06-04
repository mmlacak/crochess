// Copyright (c) 2021 Mario Mlačak, mmlacak@gmail.com
// Licensed under 3-clause (modified) BSD license. See LICENSE for details.

#ifndef __CC_PLY_H__
#define __CC_PLY_H__


#include <stdbool.h>

#include "cc_piece.h"
#include "cc_step.h"


typedef enum CcPlyLinkEnum
{
    CC_PLE_Ply, // Just one ply, starting or continuing cascade.
    CC_PLE_Teleportation,
    CC_PLE_TeleportationWave,
    CC_PLE_FailedTeleportationOblation,
    CC_PLE_FailedTeleportation,
    CC_PLE_TranceJourney,
    CC_PLE_DualTranceJourney,
    CC_PLE_FailedTranceJourney,
    CC_PLE_PawnSacrifice,
} CcPlyLinkEnum;

typedef enum CcPlySideEffectEnum
{
    CC_PSEE_None,
    CC_PSEE_Capture,
    CC_PSEE_EnPassant,
    CC_PSEE_Castle,
    CC_PSEE_Promotion,
    CC_PSEE_TagForPromotion,
    CC_PSEE_Conversion,
    CC_PSEE_FailedConversion,
    CC_PSEE_Demotion,
    CC_PSEE_Resurrection,
    CC_PSEE_FailedResurrection,
} CcPlySideEffectEnum;

typedef struct CcPlySideEffect
{
    CcPlySideEffectEnum type;

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
} CcPlySideEffect;

CcPlySideEffect ply_side_effect( CcPlySideEffectEnum type, CcPieceEnum piece, bool is_promo_tag_lost, int start_i, int start_j, int dest_i, int dest_j );

CcPlySideEffect cc_ply_side_effect_none();
CcPlySideEffect cc_ply_side_effect_capture( CcPieceEnum piece, bool is_promo_tag_lost );
CcPlySideEffect cc_ply_side_effect_en_passant( int dest_i, int dest_j );
CcPlySideEffect cc_ply_side_effect_castle( CcPieceEnum rook, int start_i, int start_j, int dest_i, int dest_j );
CcPlySideEffect cc_ply_side_effect_promote( CcPieceEnum piece );
CcPlySideEffect cc_ply_side_effect_tag_for_promotion();
CcPlySideEffect cc_ply_side_effect_convert( CcPieceEnum piece, bool is_promo_tag_lost );
CcPlySideEffect cc_ply_side_effect_failed_conversion();
CcPlySideEffect cc_ply_side_effect_demote( CcPieceEnum piece, int dest_i, int dest_j );
CcPlySideEffect cc_ply_side_effect_resurrect( CcPieceEnum piece, int dest_i, int dest_j );
CcPlySideEffect cc_ply_side_effect_failed_resurrection();


typedef struct CcPieceField
{
    CcPieceEnum piece;
    int i;
    int j;
    struct CcPieceField * next;
} CcPieceField;

CcPieceField * cc_ply_piece_field_new( CcPieceEnum piece, int i, int j );
CcPieceField * cc_ply_append_piece_field_new( CcPieceField * const restrict piece_fields, CcPieceEnum piece, int i, int j );
bool cc_ply_free_all_piece_fields( CcPieceField ** const piece_fields );


typedef struct CcPly
{
    CcPlyLinkEnum link;
    CcPieceEnum piece;

    union
    {
        struct { CcStep * steps; } ply;
        struct { int i; int j; } teleport;
        struct { CcStep * steps; } teleport_wave;
        struct { int i; int j; } failed_teleport;
        struct { int i; int j; CcSideEffectStep * steps; } trance_journey;
        struct { CcPieceField * captured; } dual_trance_journey;
        struct { CcSideEffectStep * steps; } pawn_sacrifice;
    };

    CcPlySideEffect side_effect;
    struct CcPly * next;
} CcPly;

CcPly * cc_ply_new( CcPlyLinkEnum link, CcPieceEnum piece,
                    CcStep * const restrict steps, int i, int j,
                    CcSideEffectStep * const restrict side_effect_steps,
                    CcPieceField * const restrict captured,
                    CcPlySideEffect side_effect );
CcPly * cc_ply_append_new(  CcPly * const restrict plies,
                            CcPlyLinkEnum link, CcPieceEnum piece,
                            CcStep * const restrict steps, int i, int j,
                            CcSideEffectStep * const restrict side_effect_steps,
                            CcPieceField * const restrict captured,
                            CcPlySideEffect side_effect );
bool cc_ply_free_all_plies( CcPly ** const plies );

CcPly * cc_ply_new_ply_new( CcPieceEnum piece, CcStep * const restrict steps, CcPlySideEffect side_effect );
CcPly * cc_ply_new_teleport_new( CcPieceEnum piece, int i, int j, CcPlySideEffect side_effect );
CcPly * cc_ply_new_teleport_wave_new( CcPieceEnum piece, CcStep * const restrict steps, CcPlySideEffect side_effect );
CcPly * cc_ply_new_failed_teleport_oblation_new( CcPieceEnum piece, CcPlySideEffect side_effect );
CcPly * cc_ply_new_failed_teleport_new( CcPieceEnum piece, int i, int j, CcPlySideEffect side_effect );
CcPly * cc_ply_new_trance_journey_new( CcPieceEnum piece, int i, int j, CcSideEffectStep * const restrict steps, CcPlySideEffect side_effect );
CcPly * cc_ply_new_dual_trance_journey_new( CcPieceField * const restrict captured, CcPlySideEffect side_effect );
CcPly * cc_ply_new_failed_trance_journey_new( CcPieceEnum piece, CcPlySideEffect side_effect );
CcPly * cc_ply_new_pawn_sacrifice_new( CcPieceEnum piece, CcSideEffectStep * const restrict steps, CcPlySideEffect side_effect );

CcPly * cc_ply_append_ply_new( CcPly * const restrict plies, CcPieceEnum piece, CcStep * const restrict steps, CcPlySideEffect side_effect );
CcPly * cc_ply_append_teleport_new( CcPly * const restrict plies, CcPieceEnum piece, int i, int j, CcPlySideEffect side_effect );
CcPly * cc_ply_append_teleport_wave_new( CcPly * const restrict plies, CcPieceEnum piece, CcStep * const restrict steps, CcPlySideEffect side_effect );
CcPly * cc_ply_append_failed_teleport_oblation_new( CcPly * const restrict plies, CcPieceEnum piece, CcPlySideEffect side_effect );
CcPly * cc_ply_append_failed_teleport_new( CcPly * const restrict plies, CcPieceEnum piece, int i, int j, CcPlySideEffect side_effect );
CcPly * cc_ply_append_trance_journey_new( CcPly * const restrict plies, CcPieceEnum piece, int i, int j, CcSideEffectStep * const restrict steps, CcPlySideEffect side_effect );
CcPly * cc_ply_append_dual_trance_journey_new( CcPly * const restrict plies, CcPieceField * const restrict captured, CcPlySideEffect side_effect );
CcPly * cc_ply_append_failed_trance_journey_new( CcPly * const restrict plies, CcPieceEnum piece, CcPlySideEffect side_effect );
CcPly * cc_ply_append_pawn_sacrifice_new( CcPly * const restrict plies, CcPieceEnum piece, CcSideEffectStep * const restrict steps, CcPlySideEffect side_effect );


#endif /* __CC_PLY_H__ */
