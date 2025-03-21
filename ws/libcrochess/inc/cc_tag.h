// Copyright (c) 2021 Mario Mlačak, mmlacak@gmail.com
// Licensed under GNU GPL v3+ license. See LICENSING, COPYING files for details.

#ifndef __CC_TAG_H__
#define __CC_TAG_H__

#include <stdbool.h>

//
// Tag enum

#define CC_TAG_IS_ENUMERATOR(te) ( ( CC_TE_None <= (te) ) && ( (te) <= CC_TE_EnPassant_Current ) )

#define CC_TAG_IS_VALID(te) ( ( CC_TE_None < (te) ) && ( (te) <= CC_TE_EnPassant_Current ) )

#define CC_TAG_IS_PERSISTENT(te) ( ( (te) == CC_TE_CanRush )                \
                                || ( (te) == CC_TE_CanCastle )              \
                                || ( (te) == CC_TE_DelayedPromotion ) )

#define CC_TAG_IS_EN_PASSANT(te) ( ( (te) == CC_TE_EnPassant_Previous )     \
                                || ( (te) == CC_TE_EnPassant_Current ) )

#define CC_TAG_CHAR_NONE ' '
#define CC_TAG_CHAR_INVALID '?'

#define CC_TAG_CHAR_CAN_RUSH 'R'
#define CC_TAG_CHAR_CAN_CASTLE 'C'
#define CC_TAG_CHAR_DELAYED_PROMOTION 'P'

#define CC_TAG_CHAR_EN_PASSANT_PREVIOUS 'E'
#define CC_TAG_CHAR_EN_PASSANT_CURRENT 'e'

typedef enum CcTagEnum {
    CC_TE_None = 0, /* No tag applies. */

    CC_TE_CanRush, /* Pawn can rush, persistent tag. */
    CC_TE_CanCastle, /* Rooks, Kings can castle, persistent tag. */
    CC_TE_DelayedPromotion, /* Pawn delayed promotion, persistent tag. */

    CC_TE_EnPassant_Previous, /* A private rushed in previous turn, this is en passant opportunity tag. */
    CC_TE_EnPassant_Current, /* A private rushed in current turn (in a previous ply), this will become en passant opportunity for opponent in the very next turn. */
} CcTagEnum;

typedef unsigned char CcTagType;

char cc_tag_as_char( CcTagType ct );

CcTagType cc_tag_from_char( char c );

bool cc_tag_is_congruent( CcTagType ct_1, CcTagType ct_2 );

//
// Losing tag enum

#define CC_LOSING_TAG_IS_ENUMERATOR(ltt) ( ( CC_LTE_NoneLost <= (ltt) ) && ( (ltt) <= CC_LTE_DelayedPromotionLost ) )

#define CC_LOSING_TAG_IS_VALID(ltt) ( ( CC_LTE_NoneLost < (ltt) ) && ( (ltt) <= CC_LTE_DelayedPromotionLost ) )

typedef enum CcLosingTagEnum {
    CC_LTE_NoneLost = CC_TE_None, /* No tag has been lost. */

    CC_LTE_RushingTagLost = CC_TE_CanRush, /* Pawn lost its ability to rush. */
    CC_LTE_CastlingTagLost = CC_TE_CanCastle, /* Rook (King) lost its ability to castle. */
    CC_LTE_DelayedPromotionLost = CC_TE_DelayedPromotion, /* Pawn lost its delayed promotion opportunity. */

    /* En passant tags are semi-permanent, they last for a turn, and are removed simply by playing another move. */
} CcLosingTagEnum;

#define CC_MAX_LEN_LOSING_TAG_SYMBOL (2)

typedef CcTagType CcLosingTagType; // unsigned char

char const * cc_losing_tag_symbol( CcLosingTagType ltt );

char const * cc_losing_tag_as_string( CcLosingTagType ltt,
                                      bool capitalize,
                                      bool no_tag );

CcLosingTagType cc_convert_tag_to_losing( CcTagType te );

CcTagType cc_convert_tag_from_losing( CcLosingTagType ltt );

bool cc_losing_tag_is_congruent( CcLosingTagType ltt_1, CcLosingTagType ltt_2 );



#endif /* __CC_TAG_H__ */
