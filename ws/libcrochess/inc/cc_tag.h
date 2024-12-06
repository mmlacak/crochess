// Copyright (c) 2021 Mario Mlačak, mmlacak@gmail.com
// Licensed under GNU GPL v3+ license. See LICENSING, COPYING files for details.

#ifndef __CC_TAG_H__
#define __CC_TAG_H__

#include <stdbool.h>

//
// Tag enum

// TODO :: warning: comparison of unsigned expression in ‘>= 0’ is always true [-Wtype-limits] (in CC_TE_None <= ...)
#define CC_TAG_IS_ENUMERATOR(te) ( ( CC_TE_None <= (te) ) && ( (te) <= CC_TE_DelayedPromotion ) )

#define CC_TAG_IS_VALID(te) ( ( CC_TE_None < (te) ) && ( (te) <= CC_TE_DelayedPromotion ) )

#define CC_TAG_IS_PERSISTENT(te) ( ( (te) == CC_TE_CanRush )               \
                                || ( (te) == CC_TE_CanCastle )             \
                                || ( (te) == CC_TE_DelayedPromotion ) )

#define CC_TAG_CHAR_NONE ' '
#define CC_TAG_CHAR_INVALID '?'

#define CC_TAG_CHAR_CAN_RUSH 'R'
#define CC_TAG_CHAR_CAN_CASTLE 'C'
#define CC_TAG_CHAR_DELAYED_PROMOTION 'P'

typedef enum CcTagEnum {
    CC_TE_None = 0, /* No tag applies. */

    CC_TE_CanRush, /* Pawn can rush, persistent tag. */
    CC_TE_CanCastle, /* Rooks, Kings can castle, persistent tag. */
    CC_TE_DelayedPromotion, /* Pawn delayed promotion, persistent tag. */
} CcTagEnum;

typedef unsigned char CcTagType;

char cc_tag_as_char( CcTagType ct );

CcTagType cc_tag_from_char( char c );

//
// Losing tag enum

#define CC_LOSING_TAG_IS_ENUMERATOR(ltt) ( ( CC_LTE_NoneLost <= (ltt) ) && ( (ltt) <= CC_LTE_DelayedPromotionLost ) )

#define CC_LOSING_TAG_IS_VALID(ltt) ( ( CC_LTE_NoneLost < (ltt) ) && ( (ltt) <= CC_LTE_DelayedPromotionLost ) )

typedef enum CcLosingTagEnum {
    CC_LTE_NoneLost = CC_TE_None, /* No tag has been lost. */

    CC_LTE_RushingTagLost = CC_TE_CanRush, /* Pawn lost its ability to rush. */
    CC_LTE_CastlingTagLost = CC_TE_CanCastle, /* Rook (King) lost its ability to castle. */
    CC_LTE_DelayedPromotionLost = CC_TE_DelayedPromotion, /* Pawn lost its delayed promotion opportunity. */
} CcLosingTagEnum;

#define CC_MAX_LEN_LOSING_TAG_SYMBOL (2)

typedef CcTagType CcLosingTagType; // unsigned char

char const * cc_losing_tag_symbol( CcLosingTagType ltt );

char const * cc_losing_tag_as_string( CcLosingTagType ltt,
                                      bool capitalize,
                                      bool no_tag );

CcLosingTagType cc_tag_to_losing( CcTagType te );

CcTagType cc_tag_from_losing( CcLosingTagType ltt );



#endif /* __CC_TAG_H__ */
