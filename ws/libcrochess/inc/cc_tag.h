// Copyright (c) 2021 Mario Mlačak, mmlacak@gmail.com
// Licensed under GNU GPL v3+ license. See LICENSING, COPYING files for details.

#ifndef __CC_TAG_H__
#define __CC_TAG_H__

#include <stdbool.h>


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

// TODO :: warning: comparison of unsigned expression in ‘>= 0’ is always true [-Wtype-limits] (in CC_TE_None <= ...)
#define CC_TAG_IS_ENUMERATOR(te) ( ( CC_TE_None <= (te) ) && ( (te) <= CC_TE_DelayedPromotion ) )

#define CC_TAG_IS_VALID(te) ( ( CC_TE_None < (te) ) && ( (te) <= CC_TE_DelayedPromotion ) )

#define CC_TAG_IS_PERSISTENT(te) ( ( (te) == CC_TE_CanRush )               \
                                || ( (te) == CC_TE_CanCastle )             \
                                || ( (te) == CC_TE_DelayedPromotion ) )

char cc_tag_as_char( CcTagType ct );

CcTagType cc_tag_from_char( char c );


#endif /* __CC_TAG_H__ */
