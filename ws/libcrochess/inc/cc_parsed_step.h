// Copyright (c) 2021, 2022, 2024 Mario Mlačak, mmlacak@gmail.com
// Licensed under GNU GPL v3+ license. See LICENSING, COPYING files for details.

#ifndef __CC_PARSED_STEP_H__
#define __CC_PARSED_STEP_H__

#include <stddef.h>

#include "cc_pos.h"
#include "cc_parsed_side_effect.h"


// TODO :: revise VALID vs. IN_DOMAIN, for all macros

#define CC_IS_STEP_LINK_ENUMERATOR(sle) ( ( CC_PSLE_None <= (sle) ) && ( (sle) <= CC_PSLE_JustDestination ) )

#define CC_IS_STEP_LINK_VALID(sle) ( ( CC_PSLE_None < (sle) ) && ( (sle) <= CC_PSLE_JustDestination ) )

#define CC_IS_STEP_LINK_DESTINATION(sle) ( ( (sle) == CC_PSLE_Destination ) || ( (sle) == CC_PSLE_JustDestination ) )

typedef enum CcParsedStepLinkEnum {
    CC_PSLE_None, /* Step link not found, uninitialized, not parsed yet, or error happened. */
    CC_PSLE_Start, /* Position from which a piece started moving. */
    CC_PSLE_Reposition, /* In trance-journey, dark Shaman's distant starting field; separated by , (comma). */
    CC_PSLE_Next, /* Step immediately following previous, separated by . (dot). */
    CC_PSLE_Distant, /* Step not immediately following previous, separated by .. (double-dot). */
    CC_PSLE_Destination, /* Step to destination field, separated by - (hyphen). */
    CC_PSLE_JustDestination, /* Just destination field, no separators, no other steps. */
} CcParsedStepLinkEnum;

char const * cc_parsed_step_link_symbol( CcParsedStepLinkEnum sle );

#define CC_MAX_LEN_PARSED_STEP_LINK_SYMBOL (2)


typedef struct CcParsedStep {
    CcParsedStepLinkEnum link; /* Type of a link to previous step. */
    CcPos field; /* Field of a step. */
    CcParsedSideEffect side_effect; /* Side-effect structure. */

    struct CcParsedStep * next; /* Next step in a linked list. */
} CcParsedStep;

CcParsedStep * cc_parsed_step__new( CcParsedStepLinkEnum link,
                                    CcPos field,
                                    CcParsedSideEffect side_effect );

CcParsedStep * cc_parsed_step_append( CcParsedStep ** steps__iod_a,
                                      CcParsedStepLinkEnum link,
                                      CcPos field,
                                      CcParsedSideEffect side_effect );

CcParsedStep * cc_parsed_step_duplicate_all__new( CcParsedStep * steps );

CcParsedStep * cc_parsed_step_extend( CcParsedStep ** steps__iod_a,
                                      CcParsedStep ** steps__d_n );

size_t cc_parsed_step_count( CcParsedStep * steps );

CcParsedStep * cc_parsed_step_find_start( CcParsedStep * steps );

CcParsedStep * cc_parsed_step_find_destination( CcParsedStep * steps );

bool cc_parsed_step_free_all( CcParsedStep ** steps__f );

char * cc_parsed_step_all_to_short_string__new( CcParsedStep * steps );


CcParsedStep * cc_parsed_step_none__new( CcParsedStepLinkEnum link, CcPos field );

CcParsedStep * cc_parsed_step_capture__new( CcParsedStepLinkEnum link, CcPos field,
                                            CcPieceEnum piece,
                                            CcLosingTagEnum lost_tag );

CcParsedStep * cc_parsed_step_displacement__new( CcParsedStepLinkEnum link, CcPos field,
                                                 CcPieceEnum piece,
                                                 CcLosingTagEnum lost_tag,
                                                 CcPos destination );

CcParsedStep * cc_parsed_step_en_passant__new( CcParsedStepLinkEnum link, CcPos field,
                                               CcPieceEnum pawn,
                                               CcPos distant );

CcParsedStep * cc_parsed_step_castle__new( CcParsedStepLinkEnum link, CcPos field,
                                           CcPieceEnum rook,
                                           CcPos start,
                                           CcPos destination );

CcParsedStep * cc_parsed_step_promote__new( CcParsedStepLinkEnum link, CcPos field,
                                            CcPieceEnum captured,
                                            CcLosingTagEnum lost_tag,
                                            CcPieceEnum promoted_to );

CcParsedStep * cc_parsed_step_tag_for_promotion__new( CcParsedStepLinkEnum link, CcPos field,
                                                      CcPieceEnum captured,
                                                      CcLosingTagEnum lost_tag );

CcParsedStep * cc_parsed_step_convert__new( CcParsedStepLinkEnum link, CcPos field,
                                            CcPieceEnum piece,
                                            CcLosingTagEnum lost_tag );

CcParsedStep * cc_parsed_step_failed_conversion__new( CcParsedStepLinkEnum link, CcPos field );

CcParsedStep * cc_parsed_step_demote__new( CcParsedStepLinkEnum link, CcPos field,
                                           CcPieceEnum piece,
                                           CcLosingTagEnum lost_tag,
                                           CcPos distant );

CcParsedStep * cc_parsed_step_resurrect__new( CcParsedStepLinkEnum link, CcPos field,
                                              CcPieceEnum piece,
                                              CcPos destination );

CcParsedStep * cc_parsed_step_failed_resurrection__new( CcParsedStepLinkEnum link, CcPos field );


CcParsedStep * cc_parsed_step_none_append( CcParsedStep ** steps__iod_a,
                                           CcParsedStepLinkEnum link,
                                           CcPos field );

CcParsedStep * cc_parsed_step_capture_append( CcParsedStep ** steps__iod_a,
                                              CcParsedStepLinkEnum link,
                                              CcPos field,
                                              CcPieceEnum piece,
                                              CcLosingTagEnum lost_tag );

CcParsedStep * cc_parsed_step_displacement_append( CcParsedStep ** steps__iod_a,
                                                   CcParsedStepLinkEnum link,
                                                   CcPos field,
                                                   CcPieceEnum piece,
                                                   CcLosingTagEnum lost_tag,
                                                   CcPos destination );

CcParsedStep * cc_parsed_step_en_passant_append( CcParsedStep ** steps__iod_a,
                                                 CcParsedStepLinkEnum link,
                                                 CcPos field,
                                                 CcPieceEnum pawn,
                                                 CcPos distant );

CcParsedStep * cc_parsed_step_castle_append( CcParsedStep ** steps__iod_a,
                                             CcParsedStepLinkEnum link,
                                             CcPos field,
                                             CcPieceEnum rook,
                                             CcPos start,
                                             CcPos destination );

CcParsedStep * cc_parsed_step_promote_append( CcParsedStep ** steps__iod_a,
                                              CcParsedStepLinkEnum link,
                                              CcPos field,
                                              CcPieceEnum captured,
                                              CcLosingTagEnum lost_tag,
                                              CcPieceEnum promoted_to );

CcParsedStep * cc_parsed_step_tag_for_promotion_append( CcParsedStep ** steps__iod_a,
                                                        CcParsedStepLinkEnum link,
                                                        CcPos field,
                                                        CcPieceEnum captured,
                                                        CcLosingTagEnum lost_tag );

CcParsedStep * cc_parsed_step_convert_append( CcParsedStep ** steps__iod_a,
                                              CcParsedStepLinkEnum link,
                                              CcPos field,
                                              CcPieceEnum piece,
                                              CcLosingTagEnum lost_tag );

CcParsedStep * cc_parsed_step_failed_conversion_append( CcParsedStep ** steps__iod_a,
                                                        CcParsedStepLinkEnum link,
                                                        CcPos field );

CcParsedStep * cc_parsed_step_demote_append( CcParsedStep ** steps__iod_a,
                                             CcParsedStepLinkEnum link,
                                             CcPos field,
                                             CcPieceEnum piece,
                                             CcLosingTagEnum lost_tag,
                                             CcPos distant );

CcParsedStep * cc_parsed_step_resurrect_append( CcParsedStep ** steps__iod_a,
                                                CcParsedStepLinkEnum link,
                                                CcPos field,
                                                CcPieceEnum piece,
                                                CcPos destination );

CcParsedStep * cc_parsed_step_failed_resurrection_append( CcParsedStep ** steps__iod_a,
                                                          CcParsedStepLinkEnum link,
                                                          CcPos field );


#endif /* __CC_PARSED_STEP_H__ */
