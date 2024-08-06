// Copyright (c) 2021 Mario Mlačak, mmlacak@gmail.com
// Licensed under GNU GPL v3+ license. See LICENSING, COPYING files for details.

#ifndef __CC_TAG_H__
#define __CC_TAG_H__

#include <stdbool.h>


// TODO :: DOCS
#define CC_TAG_IS_EQUAL(te1,te2) ( (te1) == (te2) )

// TODO :: DOCS
#define CC_TAG_IS_NONE(te) ( (te) == (int)CC_TE_None )

// TODO :: DOCS
#define CC_TAG_IS_VALID(te) ( ( CC_TE_None <= (te) ) && ( (te) <= CC_TE_PawnSacrifice ) )

// TODO :: DOCS
#define CC_TAG_EXISTS(te) ( ( CC_TE_None < (te) ) && ( (te) <= CC_TE_PawnSacrifice ) )

#define CC_TAG_CAN_RUSH(te) ( (te) == (int)CC_TE_CanRush )

#define CC_TAG_CAN_CASTLE(te) ( (te) == (int)CC_TE_CanCastle )

#define CC_TAG_CAN_PROMOTE(te) ( (te) == (int)CC_TE_DelayedPromotion )

#define CC_TAG_CAN_EN_PASSANT(te) ( (te) == (int)CC_TE_EnPassant )

#define CC_TAG_CAN_PAWN_SACRIFICE(te) ( (te) == (int)CC_TE_PawnSacrifice )

#define CC_TAG_IS_PERSISTENT(te) ( ( (te) == (int)CC_TE_CanRush )               \
                                || ( (te) == (int)CC_TE_CanCastle )             \
                                || ( (te) == (int)CC_TE_DelayedPromotion ) )

#define CC_TAG_IS_TEMPORARY(te) ( ( (te) == (int)CC_TE_EnPassant )         \
                               || ( (te) == (int)CC_TE_PawnSacrifice ) )


// TODO :: DOCS
#define CC_TAG_VALUE_MASK (0x07) // <!> Keep in sync with CcTagEnum enumerators.

// TODO :: DOCS
#define CC_MOVE_STARTER_MASK (0x80)

// TODO :: DOCS
#define CC_TAG_VALUE(te) ( (te) & CC_TAG_VALUE_MASK )

// TODO :: DOCS
#define CC_MOVE_STARTER_FLAG(te) ( (te) & CC_MOVE_STARTER_MASK )

// TODO :: DOCS
#define CC_SET_MOVE_STARTER_FLAG(te) ( (te) | CC_MOVE_STARTER_MASK )


#define CC_TAG_CHAR_NONE ' '
#define CC_TAG_CHAR_INVALID '?'

#define CC_TAG_CHAR_CAN_RUSH 'R'
#define CC_TAG_CHAR_CAN_CASTLE 'C'
#define CC_TAG_CHAR_DELAYED_PROMOTION 'P'

#define CC_TAG_CHAR_EN_PASSANT 'E'
#define CC_TAG_CHAR_PAWN_SACRIFICE 'S'

// TODO :: DOCS
#define CC_TAG_CHAR_MOVE_STARTER_CAN_RUSH 'r'
#define CC_TAG_CHAR_MOVE_STARTER_CAN_CASTLE 'c'
#define CC_TAG_CHAR_MOVE_STARTER_DELAYED_PROMOTION 'p'

// TODO :: DOCS
#define CC_TAG_CHAR_MOVE_STARTER_EN_PASSANT 'e'
#define CC_TAG_CHAR_MOVE_STARTER_PAWN_SACRIFICE 's'

// TODO :: DOCS
typedef enum CcTagEnum {
    CC_TE_None = 0, /* No tag applies. */

    CC_TE_CanRush, /* Pawn can rush, persistent tag. */
    CC_TE_CanCastle, /* Rooks, Kings can castle, persistent tag. */
    CC_TE_DelayedPromotion, /* Pawn delayed promotion, persistent tag. */

    /* En passant tag, semi-persistent. Gained in a move, used or lost in the
    very next one. */
    CC_TE_EnPassant,

    /* Pawn sacrifice tag, non-persistent. Gained in a move, used or lost in
    the very same move. */
    CC_TE_PawnSacrifice = 5,

    /* Piece has started a move, thus cannot return to its starting position.
    This house-keeping flag is obtained after the first ply is finished, and
    follows the piece for the remainder of the move. */
    CC_TE_MoveStarterFlag = 0x80,
} CcTagEnum;

// TODO :: DOCS
typedef unsigned char CcTagType;

// TODO :: DOCS
char cc_tag_as_char( CcTagType ct );

// TODO :: DOCS
CcTagType cc_tag_from_char( char c );


// TODO :: DOCS
typedef enum CcLosingTagEnum {
    CC_LTE_NoneLost = (int)CC_TE_None, /* No tag has been lost. */

    CC_LTE_RushingTagLost = (int)CC_TE_CanRush, /* Pawn lost its ability to rush. */
    CC_LTE_CastlingTagLost = (int)CC_TE_CanCastle, /* Rook (King) lost its ability to castle. */
    CC_LTE_DelayedPromotionLost = (int)CC_TE_DelayedPromotion, /* Pawn lost its delayed promotion opportunity. */
} CcLosingTagEnum;

#define CC_MAX_LEN_LOSING_TAG (2)

// TODO :: DOCS
char const * cc_losing_tag_symbol( CcLosingTagEnum lte );

// TODO :: DOCS
char const * cc_losing_tag_as_string( CcLosingTagEnum lte,
                                      bool capitalize,
                                      bool no_tag );

// TODO :: DOCS
CcLosingTagEnum cc_tag_to_losing( CcTagType te );

// TODO :: DOCS
CcTagType cc_tag_from_losing( CcLosingTagEnum lte );


#endif /* __CC_TAG_H__ */
