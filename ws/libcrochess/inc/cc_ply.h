// Copyright (c) 2021, 2022, 2024 Mario Mlačak, mmlacak@gmail.com
// Licensed under GNU GPL v3+ license. See LICENSING, COPYING files for details.

#ifndef __CC_PLY_H__
#define __CC_PLY_H__

#include <stdbool.h>

#include "cc_piece.h"
#include "cc_step.h"


typedef enum CcPlyLinkTypeEnum {
    CC_PLTE_None,
    CC_PLTE_StartingPly,
    CC_PLTE_CascadingPly,
    CC_PLTE_Teleportation,
    CC_PLTE_TeleportationReemergence,
    CC_PLTE_TeleportationOblation,
    CC_PLTE_TranceJourney,
    CC_PLTE_DualTranceJourney,
    CC_PLTE_FailedTranceJourney,
    CC_PLTE_PawnSacrifice,
    CC_PLTE_SenseJourney,
    CC_PLTE_FailedSenseJourney,
} CcPlyLinkTypeEnum;


#define CC_PLY_LINK_TYPE_IS_ENUMERATOR(plte) ( ( CC_PLTE_None <= (plte) ) && ( (plte) <= CC_PLTE_FailedSenseJourney ) )

#define CC_PLY_LINK_TYPE_IS_VALID(plte) ( ( CC_PLTE_None < (plte) ) && ( (plte) <= CC_PLTE_FailedSenseJourney ) )


#define CC_PLY_LINK_TYPE_IS_ANY_TELEPORTATION(plte) ( ( (plte) == CC_PLTE_Teleportation )               \
                                                   || ( (plte) == CC_PLTE_TeleportationReemergence )    \
                                                   || ( (plte) == CC_PLTE_TeleportationOblation )

#define CC_PLY_LINK_TYPE_IS_ANY_TRANCE_JOURNEY(plte) ( ( (plte) == CC_PLTE_TranceJourney )          \
                                                    || ( (plte) == CC_PLTE_DualTranceJourney )      \
                                                    || ( (plte) == CC_PLTE_FailedTranceJourney ) )

#define CC_PLY_LINK_TYPE_IS_ANY_SENSE_JOURNEY(plte) ( ( (plte) == CC_PLTE_SenseJourney )         \
                                                   || ( (plte) == CC_PLTE_FailedSenseJourney ) )

#define CC_PLY_LINK_TYPE_IS_ACTIVATING_PIECE(plte) ( ( (plte) == CC_PLTE_CascadingPly )          \
                                                  || ( (plte) == CC_PLTE_TranceJourney )         \
                                                  || ( (plte) == CC_PLTE_DualTranceJourney )     \
                                                  || ( (plte) == CC_PLTE_FailedTranceJourney )   \
                                                  || ( (plte) == CC_PLTE_SenseJourney )          \
                                                  || ( (plte) == CC_PLTE_FailedSenseJourney ) )


char const * cc_ply_link_type_symbol( CcPlyLinkTypeEnum plte );

#define CC_MAX_LEN_PLY_LINK_TYPE_SYMBOL (3)


typedef struct CcPly {
    CcPlyLinkTypeEnum link;
    CcPieceType piece;
    CcLosingTagType lost_tag;
    CcStep * steps;

    struct CcPly * next;
} CcPly;

CcPly * cc_ply__new( CcPlyLinkTypeEnum link,
                     CcPieceType piece,
                     CcLosingTagType lost_tag,
                     CcStep ** steps__d_n );

CcPly * cc_ply_append( CcPly ** plies__iod_a,
                       CcPlyLinkTypeEnum link,
                       CcPieceType piece,
                       CcLosingTagType lost_tag,
                       CcStep ** steps__d_n );

CcPly * cc_ply_duplicate_all__new( CcPly * plies );

CcPly * cc_ply_extend( CcPly ** plies__iod_a,
                       CcPly ** plies__d_n );

bool cc_ply_free_all( CcPly ** plies__f );

size_t cc_ply_steps_count( CcPly * ply );

bool cc_ply_contains_side_effects( CcPly * ply );

CcPieceType cc_ply_find_activator( CcPly * plies,
                                   CcPly * ply__d );

char * cc_ply_all_to_string__new( CcPly * plies );


#endif /* __CC_PLY_H__ */
