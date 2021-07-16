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

CcPieceField * cc_ply_piece_field__new( CcPieceEnum piece, int i, int j );
CcPieceField * cc_ply_append_piece_field__new( CcPieceField * const restrict piece_fields,
                                               CcPieceEnum piece,
                                               int i,
                                               int j );
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
        struct { int i; int j; CcStep * steps; } trance_journey;
        struct { CcPieceField * captured; } dual_trance_journey;
        struct { CcStep * steps; } pawn_sacrifice;
    };

    struct CcPly * next;
} CcPly;

CcPly * cc_ply__new( CcPlyLinkEnum link, CcPieceEnum piece,
                     CcStep ** restrict steps, int i, int j,
                     CcPieceField ** restrict captured );

CcPly * cc_ply_append__new( CcPly * const restrict plies,
                            CcPlyLinkEnum link, CcPieceEnum piece,
                            CcStep ** restrict steps, int i, int j,
                            CcPieceField ** restrict captured );

bool cc_ply_free_all_plies( CcPly ** const plies );

CcPly * cc_ply_cascade__new( CcPieceEnum piece, CcStep ** restrict steps );
CcPly * cc_ply_teleport__new( CcPieceEnum piece, int i, int j );
CcPly * cc_ply_teleport_wave__new( CcPieceEnum piece, CcStep ** restrict steps );
CcPly * cc_ply_failed_teleport_oblation__new( CcPieceEnum piece );
CcPly * cc_ply_failed_teleport__new( CcPieceEnum piece, int i, int j );
CcPly * cc_ply_trance_journey__new( CcPieceEnum piece, CcStep ** restrict steps, int i, int j );
CcPly * cc_ply_dual_trance_journey__new( CcPieceField ** restrict captured );
CcPly * cc_ply_failed_trance_journey__new( CcPieceEnum piece );
CcPly * cc_ply_pawn_sacrifice__new( CcPieceEnum piece, CcStep ** restrict steps );

CcPly * cc_ply_cascade_append__new( CcPly * const restrict plies, CcPieceEnum piece, CcStep ** restrict steps );
CcPly * cc_ply_teleport_append__new( CcPly * const restrict plies, CcPieceEnum piece, int i, int j );
CcPly * cc_ply_teleport_wave_append__new( CcPly * const restrict plies, CcPieceEnum piece, CcStep ** restrict steps );
CcPly * cc_ply_failed_teleport_oblation_append__new( CcPly * const restrict plies, CcPieceEnum piece );
CcPly * cc_ply_failed_teleport_append__new( CcPly * const restrict plies, CcPieceEnum piece, int i, int j );
CcPly * cc_ply_trance_journey_append__new( CcPly * const restrict plies, CcPieceEnum piece, CcStep ** restrict steps, int i, int j );
CcPly * cc_ply_dual_trance_journey_append__new( CcPly * const restrict plies, CcPieceField ** restrict captured );
CcPly * cc_ply_failed_trance_journey_append__new( CcPly * const restrict plies, CcPieceEnum piece );
CcPly * cc_ply_pawn_sacrifice_append__new( CcPly * const restrict plies, CcPieceEnum piece, CcStep ** restrict steps );

CcStep * cc_ply_get_steps( CcPly const * const restrict ply );
bool cc_ply_contains_side_effects( CcPly const * const restrict ply );
size_t cc_ply_step_count( CcPly const * const restrict ply, bool include_starting_pos );

#endif /* __CC_PLY_H__ */
