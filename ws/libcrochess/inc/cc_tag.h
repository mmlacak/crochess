// Copyright (c) 2021 Mario Mlačak, mmlacak@gmail.com
// Licensed under GNU GPL v3+ license. See LICENSING, COPYING files for details.

#ifndef __CC_TAG_H__
#define __CC_TAG_H__

#include <stdbool.h>

#include "cc_piece.h"

//
// Tag enum

// TODO :: DOCS
#define CC_TAG_IS_CAN_RUSH(pte) ( ( (pte) == CC_PTE_DarkGrenadier_CanRush )         \
                               || ( (pte) == CC_PTE_DarkScout_CanRush )             \
                               || ( (pte) == CC_PTE_DarkPawn_CanRush )              \
                               || ( (pte) == CC_PTE_LightPawn_CanRush )             \
                               || ( (pte) == CC_PTE_LightScout_CanRush )            \
                               || ( (pte) == CC_PTE_LightGrenadier_CanRush ) )

// TODO :: DOCS
#define CC_TAG_IS_RUSHED_PREVIOUS(pte) ( ( (pte) == CC_PTE_DarkGrenadier_RushedPrevious )       \
                                      || ( (pte) == CC_PTE_DarkScout_RushedPrevious )           \
                                      || ( (pte) == CC_PTE_DarkPawn_RushedPrevious )            \
                                      || ( (pte) == CC_PTE_LightPawn_RushedPrevious )           \
                                      || ( (pte) == CC_PTE_LightScout_RushedPrevious )          \
                                      || ( (pte) == CC_PTE_LightGrenadier_RushedPrevious ) )

// TODO :: DOCS
#define CC_TAG_IS_RUSHED_CURRENT(pte) ( ( (pte) == CC_PTE_DarkGrenadier_RushedCurrent )         \
                                     || ( (pte) == CC_PTE_DarkScout_RushedCurrent )             \
                                     || ( (pte) == CC_PTE_DarkPawn_RushedCurrent )              \
                                     || ( (pte) == CC_PTE_LightPawn_RushedCurrent )             \
                                     || ( (pte) == CC_PTE_LightScout_RushedCurrent )            \
                                     || ( (pte) == CC_PTE_LightGrenadier_RushedCurrent ) )

// #define CC_TAG_IS_RUSHED(pte) ( ( (pte) == CC_TE_EnPassant_Previous )     \
//                                  || ( (pte) == CC_TE_EnPassant_Current ) )
// TODO :: DOCS
#define CC_TAG_IS_RUSHED(pte) ( ( CC_TAG_IS_RUSHED_PREVIOUS(pte) )     \
                             || ( CC_TAG_IS_RUSHED_CURRENT(pte) ) )

// TODO :: DOCS
#define CC_TAG_IS_DELAYED_PROMOTION(pte) ( ( (pte) == CC_PTE_DarkPawn_DelayedPromotion )       \
                                        || ( (pte) == CC_PTE_LightPawn_DelayedPromotion ) )

// TODO :: DOCS
#define CC_TAG_IS_CAN_CASTLE(pte) ( ( (pte) == CC_PTE_DarkKing_CanCastle )      \
                                 || ( (pte) == CC_PTE_DarkRook_CanCastle )      \
                                 || ( (pte) == CC_PTE_LightRook_CanCastle )     \
                                 || ( (pte) == CC_PTE_LightKing_CanCastle ) )

// #define CC_TAG_IS_VALID(pte) ( ( CC_TE_None < (pte) ) && ( (pte) <= CC_TE_EnPassant_Current ) )
// TODO :: DOCS
#define CC_TAG_IS_VALID(pte) ( ( CC_TAG_IS_CAN_RUSH(pte) )              \
                            || ( CC_TAG_IS_RUSHED(pte) )                \
                            || ( CC_TAG_IS_DELAYED_PROMOTION(pte) )     \
                            || ( CC_TAG_IS_CAN_CASTLE(pte) ) )

// #define CC_TAG_IS_ENUMERATOR(pte) ( ( CC_TE_None <= (pte) ) && ( (pte) <= CC_TE_EnPassant_Current ) )
// TODO :: DOCS
#define CC_TAG_IS_ENUMERATOR(pte) ( ( (pte) == CC_PTE_None )     \
                                 || ( CC_TAG_IS_VALID(pte) ) )

// #define CC_TAG_IS_PERSISTENT(pte) ( ( (pte) == CC_TE_CanRush )                \
//                                  || ( (pte) == CC_TE_CanCastle )              \
//                                  || ( (pte) == CC_TE_DelayedPromotion ) )
// TODO :: DOCS
#define CC_TAG_IS_PERSISTENT(pte) ( ( CC_TAG_IS_CAN_RUSH(pte) )                \
                                 || ( CC_TAG_IS_CAN_CASTLE(pte) )              \
                                 || ( CC_TAG_IS_DELAYED_PROMOTION(pte) ) )

#define CC_TAG_CHAR_NONE ' '
#define CC_TAG_CHAR_INVALID '?'
#define CC_TAG_CHAR_PIECE '!'

#define CC_TAG_CHAR_CAN_RUSH '*'
#define CC_TAG_CHAR_CAN_CASTLE '&'
#define CC_TAG_CHAR_DELAYED_PROMOTION '='

#define CC_TAG_CHAR_RUSHED_PREVIOUS ':'
#define CC_TAG_CHAR_RUSHED_CURRENT ';'

// TODO :: DELETE :: DOCS
// typedef enum CcTagEnum {
//     CC_TE_None = 0, /* No tag applies. */
//
//     CC_TE_CanRush, /* Pawn can rush, persistent tag. */
//     CC_TE_CanCastle, /* Rooks, Kings can castle, persistent tag. */
//     CC_TE_DelayedPromotion, /* Pawn delayed promotion, persistent tag. */
//
//     CC_TE_EnPassant_Previous, /* A private rushed in previous turn, this is en passant opportunity tag. */
//     CC_TE_EnPassant_Current, /* A private rushed in current turn (in a previous ply), this will become en passant opportunity for opponent in the very next turn. */
// } CcTagEnum;

// TODO :: DELETE :: DOCS
// typedef unsigned char CcTagType;

char cc_tag_as_char( CcPieceTagType ptt );

// CcPieceTagType cc_tag_from_char( char c ); // TODO :: DELETE : DOCS

// bool cc_tag_is_congruent( CcPieceTagType ptt_1, CcPieceTagType ptt_2 ); // TODO :: DELETE : DOCS

//
// Losing tag enum

#define CC_LOSING_TAG_IS_ENUMERATOR(ltt) ( ( CC_LTE_NoneLost <= (ltt) ) && ( (ltt) <= CC_LTE_DelayedPromotionLost ) )

#define CC_LOSING_TAG_IS_VALID(ltt) ( ( CC_LTE_NoneLost < (ltt) ) && ( (ltt) <= CC_LTE_DelayedPromotionLost ) )

typedef enum CcLosingTagEnum {
    CC_LTE_NoneLost = 0, // CC_TE_None, /* No tag has been lost. */

    CC_LTE_RushingTagLost = 1, // CC_TE_CanRush, /* Pawn lost its ability to rush. */
    CC_LTE_CastlingTagLost = 2, // CC_TE_CanCastle, /* Rook (King) lost its ability to castle. */
    CC_LTE_DelayedPromotionLost = 3, // CC_TE_DelayedPromotion, /* Pawn lost its delayed promotion opportunity. */
} CcLosingTagEnum;

#define CC_MAX_LEN_LOSING_TAG_SYMBOL (2)

typedef unsigned char CcLosingTagType; // == CcTagType

char const * cc_losing_tag_symbol( CcLosingTagType ltt );

char const * cc_losing_tag_as_string( CcLosingTagType ltt,
                                      bool capitalize,
                                      bool no_tag );

// TODO :: DOCS
CcLosingTagType cc_convert_tag_to_losing( CcPieceTagType ptt );

// CcTagType cc_convert_tag_from_losing( CcLosingTagType ltt );
// TODO :: FIX :: DOCS
CcPieceTagType cc_set_piece_tag_from_losing( CcPieceTagType ptt,
                                             CcLosingTagType ltt,
                                             bool override_conflicting_tag );

// bool cc_losing_tag_is_congruent( CcLosingTagType ltt_1, CcLosingTagType ltt_2 ); // TODO :: DELETE :: DOCS



#endif /* __CC_TAG_H__ */
