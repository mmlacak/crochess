// Copyright (c) 2021, 2022, 2024 Mario Mlačak, mmlacak@gmail.com
// Licensed under GNU GPL v3+ license. See LICENSING, COPYING files for details.

#ifndef __CC_PLY_H__
#define __CC_PLY_H__

#include <stdbool.h>

#include "cc_piece.h"
#include "cc_parsed_step.h"


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


#define CC_PLY_LINK_TYPE_IS_ENUMERATOR(ple) ( ( CC_PLTE_None <= (ple) ) && ( (ple) <= CC_PLTE_FailedSenseJourney ) )

#define CC_PLY_LINK_TYPE_IS_VALID(ple) ( ( CC_PLTE_None < (ple) ) && ( (ple) <= CC_PLTE_FailedSenseJourney ) )


#define CC_PLY_LINK_TYPE_IS_ANY_TELEPORTATION(ple) ( ( (ple) == CC_PLTE_Teleportation )               \
                                                  || ( (ple) == CC_PLTE_TeleportationReemergence )    \
                                                  || ( (ple) == CC_PLTE_TeleportationOblation )

#define CC_PLY_LINK_TYPE_IS_ANY_TRANCE_JOURNEY(ple) ( ( (ple) == CC_PLTE_TranceJourney )          \
                                                   || ( (ple) == CC_PLTE_DualTranceJourney )      \
                                                   || ( (ple) == CC_PLTE_FailedTranceJourney ) )

#define CC_PLY_LINK_TYPE_IS_ANY_SENSE_JOURNEY(ple) ( ( (ple) == CC_PLTE_SenseJourney )        \
                                                  || ( (ple) == CC_PLTE_FailedSenseJourney )


char const * cc_ply_link_type_symbol( CcPlyLinkTypeEnum ple );

#define CC_MAX_LEN_PLY_LINK_TYPE_SYMBOL (3)


typedef struct CcPly {
    CcPlyLinkTypeEnum link;
    CcPieceType piece;
    CcLosingTagEnum lost_tag;
    CcParsedStep * steps;

    struct CcPly * next;
} CcPly;

CcPly * cc_ply__new( CcPlyLinkTypeEnum link,
                     CcPieceType piece,
                     CcLosingTagEnum lost_tag,
                     CcParsedStep ** steps__n );

CcPly * cc_ply_append( CcPly ** plies__iod_a,
                       CcPlyLinkTypeEnum link,
                       CcPieceType piece,
                       CcLosingTagEnum lost_tag,
                       CcParsedStep ** steps__n );

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
