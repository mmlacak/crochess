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


typedef struct CcPieceField
{
    CcPieceEnum piece;
    int i;
    int j;
    struct CcPieceField * next;
} CcPieceField;

CcPieceField * cc_ply_piece_field_new( CcPieceEnum piece, int i, int j );
CcPieceField * cc_ply_append_piece_field_new( CcPieceField * const restrict piece_fields,
                                              CcPieceEnum piece,
                                              int i,
                                              int j );
bool cc_ply_free_all_piece_fields( CcPieceField ** const piece_fields_f );


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
        struct { int i; int j; CcStep * steps; } trance_journey;
        struct { CcPieceField * captured; } dual_trance_journey;
        struct { CcStep * steps; } pawn_sacrifice;
    };

    struct CcPly * next;
} CcPly;

CcPly * cc_ply_new( CcPlyLinkEnum link, CcPieceEnum piece,
                    CcStep ** restrict steps_n, int i, int j,
                    CcPieceField ** restrict captured );

CcPly * cc_ply_append_new( CcPly * const restrict plies,
                           CcPlyLinkEnum link, CcPieceEnum piece,
                           CcStep ** restrict steps_n, int i, int j,
                           CcPieceField ** restrict captured );

bool cc_ply_free_all_plies( CcPly ** const plies_f );

CcPly * cc_ply_cascade_new( CcPieceEnum piece, CcStep ** restrict steps_n );
CcPly * cc_ply_teleport_new( CcPieceEnum piece, int i, int j );
CcPly * cc_ply_teleport_wave_new( CcPieceEnum piece, CcStep ** restrict steps_n );
CcPly * cc_ply_failed_teleport_oblation_new( CcPieceEnum piece );
CcPly * cc_ply_failed_teleport_new( CcPieceEnum piece, int i, int j );
CcPly * cc_ply_trance_journey_new( CcPieceEnum piece, CcStep ** restrict steps_n, int i, int j );
CcPly * cc_ply_dual_trance_journey_new( CcPieceField ** restrict captured );
CcPly * cc_ply_failed_trance_journey_new( CcPieceEnum piece );
CcPly * cc_ply_pawn_sacrifice_new( CcPieceEnum piece, CcStep ** restrict steps_n );

CcPly * cc_ply_cascade_append_new( CcPly * const restrict plies, CcPieceEnum piece, CcStep ** restrict steps_n );
CcPly * cc_ply_teleport_append_new( CcPly * const restrict plies, CcPieceEnum piece, int i, int j );
CcPly * cc_ply_teleport_wave_append_new( CcPly * const restrict plies, CcPieceEnum piece, CcStep ** restrict steps_n );
CcPly * cc_ply_failed_teleport_oblation_append_new( CcPly * const restrict plies, CcPieceEnum piece );
CcPly * cc_ply_failed_teleport_append_new( CcPly * const restrict plies, CcPieceEnum piece, int i, int j );
CcPly * cc_ply_trance_journey_append_new( CcPly * const restrict plies, CcPieceEnum piece, CcStep ** restrict steps_n, int i, int j );
CcPly * cc_ply_dual_trance_journey_append_new( CcPly * const restrict plies, CcPieceField ** restrict captured );
CcPly * cc_ply_failed_trance_journey_append_new( CcPly * const restrict plies, CcPieceEnum piece );
CcPly * cc_ply_pawn_sacrifice_append_new( CcPly * const restrict plies, CcPieceEnum piece, CcStep ** restrict steps_n );

CcStep * cc_ply_get_steps( CcPly const * const restrict ply );
bool cc_ply_contains_side_effects( CcPly const * const restrict ply );
size_t cc_ply_step_count( CcPly const * const restrict ply, bool include_starting_pos );

#endif /* __CC_PLY_H__ */
