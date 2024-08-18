// Copyright (c) 2021, 2022, 2024 Mario Mlačak, mmlacak@gmail.com
// Licensed under GNU GPL v3+ license. See LICENSING, COPYING files for details.

#ifndef __CC_PARSED_PLY_H__
#define __CC_PARSED_PLY_H__

#include <stdbool.h>

#include "cc_piece.h"
#include "cc_parsed_step.h"


typedef enum CcParsedPlyLinkEnum {
    CC_PPLE_None,
    CC_PPLE_StartingPly,
    CC_PPLE_CascadingPly,
    CC_PPLE_Teleportation,
    CC_PPLE_TeleportationReemergence,
    CC_PPLE_TeleportationOblation,
    CC_PPLE_TranceJourney,
    CC_PPLE_DualTranceJourney,
    CC_PPLE_FailedTranceJourney,
    CC_PPLE_PawnSacrifice,
    CC_PPLE_SenseJourney,
    CC_PPLE_FailedSenseJourney,
} CcParsedPlyLinkEnum;


#define CC_PARSED_PLY_LINK_IS_NONE(ple) ( (ple) == CC_PPLE_None )
#define CC_PARSED_PLY_LINK_IS_STARTING(ple) ( (ple) == CC_PPLE_StartingPly )
// TODO :: DOCS
#define CC_PARSED_PLY_LINK_IS_CASCADING(ple) ( (ple) == CC_PPLE_CascadingPly )

// TODO :: DOCS
#define CC_PARSED_PLY_LINK_IS_TELEPORTATION(ple) ( (ple) == CC_PPLE_Teleportation )
// TODO :: DOCS
#define CC_PARSED_PLY_LINK_IS_TELEPORTATION_REEMERGENCE(ple) ( (ple) == CC_PPLE_TeleportationReemergence )
// TODO :: DOCS
#define CC_PARSED_PLY_LINK_IS_TELEPORTATION_OBLATION(ple) ( (ple) == CC_PPLE_TeleportationOblation )

// TODO :: DOCS
#define CC_PARSED_PLY_LINK_IS_TRANCE_JOURNEY(ple) ( (ple) == CC_PPLE_TranceJourney )
// TODO :: DOCS
#define CC_PARSED_PLY_LINK_IS_DUAL_TRANCE_JOURNEY(ple) ( (ple) == CC_PPLE_DualTranceJourney )
// TODO :: DOCS
#define CC_PARSED_PLY_LINK_IS_FAILED_TRANCE_JOURNEY(ple) ( (ple) == CC_PPLE_FailedTranceJourney )

// TODO :: DOCS
#define CC_PARSED_PLY_LINK_IS_PAWN_SACRIFICE(ple) ( (ple) == CC_PPLE_PawnSacrifice )

// TODO :: DOCS
#define CC_PARSED_PLY_LINK_IS_SENSE_JOURNEY(ple) ( (ple) == CC_PPLE_SenseJourney )
// TODO :: DOCS
#define CC_PARSED_PLY_LINK_IS_FAILED_SENSE_JOURNEY(ple) ( (ple) == CC_PPLE_FailedSenseJourney )


// TODO :: DOCS
#define CC_PARSED_PLY_LINK_IS_ENUMERATOR(ple) ( ( CC_PPLE_None <= (ple) ) && ( (ple) <= CC_PPLE_FailedSenseJourney ) )

// TODO :: DOCS
#define CC_PARSED_PLY_LINK_IS_VALID(ple) ( ( CC_PPLE_None < (ple) ) && ( (ple) <= CC_PPLE_FailedSenseJourney ) )


// TODO :: DOCS
#define CC_PARSED_PLY_LINK_IS_ANY_TELEPORTATION(ple) ( ( (ple) == CC_PPLE_Teleportation )               \
                                                    || ( (ple) == CC_PPLE_TeleportationReemergence )    \
                                                    || ( (ple) == CC_PPLE_TeleportationOblation )

// TODO :: DOCS
#define CC_PARSED_PLY_LINK_IS_ANY_TRANCE_JOURNEY(ple) ( ( (ple) == CC_PPLE_TranceJourney )          \
                                                     || ( (ple) == CC_PPLE_DualTranceJourney )      \
                                                     || ( (ple) == CC_PPLE_FailedTranceJourney ) )

// TODO :: DOCS
#define CC_PARSED_PLY_LINK_IS_ANY_SENSE_JOURNEY(ple) ( ( (ple) == CC_PPLE_SenseJourney )        \
                                                    || ( (ple) == CC_PPLE_FailedSenseJourney )


char const * cc_parsed_ply_link_symbol( CcParsedPlyLinkEnum ple );

// TODO :: DOCS
#define CC_MAX_LEN_PARSED_PLY_LINK_SYMBOL (3)


typedef struct CcParsedPly {
    CcParsedPlyLinkEnum link;
    CcPieceType piece;
    CcLosingTagEnum lost_tag;
    CcParsedStep * steps;

    struct CcParsedPly * next;
} CcParsedPly;

// TODO :: DOCS
CcParsedPly * cc_parsed_ply__new( CcParsedPlyLinkEnum link,
                                  CcPieceType piece,
                                  CcLosingTagEnum lost_tag,
                                  CcParsedStep ** steps__n );

// TODO :: DOCS
CcParsedPly * cc_parsed_ply_append( CcParsedPly ** plies__iod_a,
                                    CcParsedPlyLinkEnum link,
                                    CcPieceType piece,
                                    CcLosingTagEnum lost_tag,
                                    CcParsedStep ** steps__n );

CcParsedPly * cc_parsed_ply_duplicate_all__new( CcParsedPly * plies );

CcParsedPly * cc_parsed_ply_extend( CcParsedPly ** plies__iod_a,
                                    CcParsedPly ** plies__d_n );

bool cc_parsed_ply_free_all( CcParsedPly ** plies__f );

size_t cc_parsed_ply_steps_count( CcParsedPly * ply );

bool cc_parsed_ply_contains_side_effects( CcParsedPly * ply );

CcPieceType cc_parsed_ply_find_activator( CcParsedPly * plies,
                                          CcParsedPly * ply__d );

char * cc_parsed_ply_all_to_short_string__new( CcParsedPly * plies );


#endif /* __CC_PARSED_PLY_H__ */
