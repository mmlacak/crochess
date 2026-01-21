// Copyright (c) 2021, 2022, 2024 Mario Mlačak, mmlacak@gmail.com
// Licensed under GNU GPL v3+ license. See LICENSING, COPYING files for details.

#ifndef __CC_STEP_H__
#define __CC_STEP_H__

#include <stddef.h>

#include "cc_pos.h"
#include "cc_side_effect.h"


#define CC_STEP_LINK_TYPE_IS_ENUMERATOR(sle) ( ( CC_SLTE_None <= (sle) ) && ( (sle) <= CC_SLTE_JustDestination ) )

#define CC_STEP_LINK_TYPE_IS_VALID(sle) ( ( CC_SLTE_None < (sle) ) && ( (sle) <= CC_SLTE_JustDestination ) )

#define CC_STEP_LINK_TYPE_IS_DESTINATION(sle) ( ( (sle) == CC_SLTE_Destination ) || ( (sle) == CC_SLTE_JustDestination ) )

typedef enum CcStepLinkTypeEnum {
    CC_SLTE_None, /* Step link not found, uninitialized, not parsed yet, or error happened. */
    CC_SLTE_InitialPosition, /* Position at which a piece was located before it started moving. */
    CC_SLTE_Reposition, /* In trance-journey, dark Shaman's distant starting field; separated by , (comma). */
    CC_SLTE_Next, /* Step immediately following previous, separated by . (dot). */
    CC_SLTE_Distant, /* Step not immediately following previous, separated by .. (double-dot). */
    CC_SLTE_Destination, /* Step to destination field, separated by - (hyphen). */
    CC_SLTE_JustDestination, /* Just destination field, no separators, no other steps. */
} CcStepLinkTypeEnum;

char const * cc_step_link_type_symbol( CcStepLinkTypeEnum sle );

#define CC_MAX_LEN_STEP_LINK_TYPE_SYMBOL (2)


typedef struct CcStep {
    CcStepLinkTypeEnum link; /* Type of a link to previous step. */
    CcPos field; /* Field of a step. */
    CcSideEffect side_effect; /* Side-effect structure. */

    CcSideEffectLink * tentative__d; /* Possible side-effects (e.g. displacements when building a path), a linked list. */

    struct CcStep * next; /* Next step in a linked list. */
} CcStep;

CcStep * cc_step__new( CcStepLinkTypeEnum link,
                       CcPos field,
                       CcSideEffect side_effect );

CcStep * cc_step_initial_no_side_effect__new( CcPos field );

CcStep * cc_step_initial__new( CcPos field,
                               CcSideEffect side_effect );

CcStep * cc_step_next_no_side_effect__new( CcPos field );

CcStep * cc_step_next__new( CcPos field,
                            CcSideEffect side_effect );

CcStep * cc_step_append( CcStep ** steps__iod_a,
                         CcStepLinkTypeEnum link,
                         CcPos field,
                         CcSideEffect side_effect );

CcStep * cc_step_append_next_no_side_effect( CcStep ** steps__iod_a,
                                             CcPos field );

CcStep * cc_step_duplicate_all__new( CcStep * steps,
                                     bool include_tentative );

CcStep * cc_step_extend( CcStep ** steps__iod_a,
                         CcStep ** steps__d_n );

size_t cc_step_count( CcStep * steps, bool do_momentum );

CcStep * cc_step_fetch_initial( CcStep * steps );

CcStep * cc_step_fetch_destination( CcStep * steps );

CcSideEffect * cc_step_fetch_last_side_effect( CcStep * steps );

bool cc_step_free_all( CcStep ** steps__f );

// static size_t _cc_step_sum_len_all_tentative( CcStep * steps );
char * cc_step_all_to_string__new( CcStep * steps );


CcStep * cc_step_none__new( CcStepLinkTypeEnum link, CcPos field );

CcStep * cc_step_capture__new( CcStepLinkTypeEnum link, CcPos field,
                               CcPieceTagType piece );

CcStep * cc_step_displacement__new( CcStepLinkTypeEnum link, CcPos field,
                                    CcPieceTagType piece,
                                    CcPos destination );

CcStep * cc_step_en_passant__new( CcStepLinkTypeEnum link, CcPos field,
                                  CcPieceTagType private,
                                  CcPos distant );

CcStep * cc_step_castle__new( CcStepLinkTypeEnum link, CcPos field,
                              CcPieceTagType rook,
                              CcPos start,
                              CcPos destination );

CcStep * cc_step_promote__new( CcStepLinkTypeEnum link, CcPos field,
                               CcPieceTagType captured,
                               CcPieceTagType promoted_to );

CcStep * cc_step_tag_for_promotion__new( CcStepLinkTypeEnum link, CcPos field,
                                         CcPieceTagType captured );

CcStep * cc_step_convert__new( CcStepLinkTypeEnum link, CcPos field,
                               CcPieceTagType piece );

CcStep * cc_step_failed_conversion__new( CcStepLinkTypeEnum link, CcPos field );

CcStep * cc_step_demote__new( CcStepLinkTypeEnum link, CcPos field,
                              CcPieceTagType piece,
                              CcPos distant );

CcStep * cc_step_resurrect__new( CcStepLinkTypeEnum link, CcPos field,
                                 CcPieceTagType piece,
                                 CcPos destination );

CcStep * cc_step_failed_resurrection__new( CcStepLinkTypeEnum link, CcPos field );


CcStep * cc_step_none_append( CcStep ** steps__iod_a,
                              CcStepLinkTypeEnum link,
                              CcPos field );

CcStep * cc_step_capture_append( CcStep ** steps__iod_a,
                                 CcStepLinkTypeEnum link,
                                 CcPos field,
                                 CcPieceTagType piece );

CcStep * cc_step_displacement_append( CcStep ** steps__iod_a,
                                      CcStepLinkTypeEnum link,
                                      CcPos field,
                                      CcPieceTagType piece,
                                      CcPos destination );

CcStep * cc_step_en_passant_append( CcStep ** steps__iod_a,
                                    CcStepLinkTypeEnum link,
                                    CcPos field,
                                    CcPieceTagType private,
                                    CcPos distant );

CcStep * cc_step_castle_append( CcStep ** steps__iod_a,
                                CcStepLinkTypeEnum link,
                                CcPos field,
                                CcPieceTagType rook,
                                CcPos start,
                                CcPos destination );

CcStep * cc_step_promote_append( CcStep ** steps__iod_a,
                                 CcStepLinkTypeEnum link,
                                 CcPos field,
                                 CcPieceTagType captured,
                                 CcPieceTagType promoted_to );

CcStep * cc_step_tag_for_promotion_append( CcStep ** steps__iod_a,
                                           CcStepLinkTypeEnum link,
                                           CcPos field,
                                           CcPieceTagType captured );

CcStep * cc_step_convert_append( CcStep ** steps__iod_a,
                                 CcStepLinkTypeEnum link,
                                 CcPos field,
                                 CcPieceTagType piece );

CcStep * cc_step_failed_conversion_append( CcStep ** steps__iod_a,
                                           CcStepLinkTypeEnum link,
                                           CcPos field );

CcStep * cc_step_demote_append( CcStep ** steps__iod_a,
                                CcStepLinkTypeEnum link,
                                CcPos field,
                                CcPieceTagType piece,
                                CcPos distant );

CcStep * cc_step_resurrect_append( CcStep ** steps__iod_a,
                                   CcStepLinkTypeEnum link,
                                   CcPos field,
                                   CcPieceTagType piece,
                                   CcPos destination );

CcStep * cc_step_failed_resurrection_append( CcStep ** steps__iod_a,
                                             CcStepLinkTypeEnum link,
                                             CcPos field );


#endif /* __CC_STEP_H__ */
