// Copyright (c) 2021 Mario Mlačak, mmlacak@gmail.com
// Licensed under GNU GPL v3+ license. See LICENSING, COPYING files for details.

#ifndef __CC_TAG_H__
#define __CC_TAG_H__

#include <stdbool.h>

#include "cc_piece.h"

//
// Tag chars

#define CC_TAG_CHAR_NONE ' '
#define CC_TAG_CHAR_INVALID '?'
#define CC_TAG_CHAR_PIECE '!'

#define CC_TAG_CHAR_CAN_RUSH '^'
#define CC_TAG_CHAR_CAN_CASTLE '&'
#define CC_TAG_CHAR_DELAYED_PROMOTION '='

#define CC_TAG_CHAR_RUSHED_PREVIOUS ':'
#define CC_TAG_CHAR_RUSHED_CURRENT ';'

char cc_tag_as_char( CcPieceTagType ptt );

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

typedef unsigned char CcLosingTagType;

char const * cc_losing_tag_symbol( CcLosingTagType ltt );

char const * cc_losing_tag_as_string( CcLosingTagType ltt,
                                      bool capitalize,
                                      bool no_tag );

CcLosingTagType cc_convert_tag_to_losing( CcPieceTagType ptt );

CcPieceTagType cc_set_piece_tag_from_losing( CcPieceTagType ptt,
                                             CcLosingTagType ltt,
                                             bool override_conflicting_tag );


#endif /* __CC_TAG_H__ */
