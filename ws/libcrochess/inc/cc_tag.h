// Copyright (c) 2021 Mario Mlačak, mmlacak@gmail.com
// Licensed under GNU GPL v3+ license. See LICENSING, COPYING files for details.

#ifndef __CC_TAG_H__
#define __CC_TAG_H__

#include <stdbool.h>


// TODO :: warning: comparison of unsigned expression in ‘>= 0’ is always true [-Wtype-limits] (in CC_TE_None <= ...)
#define CC_TAG_IS_ENUMERATOR(te) ( ( CC_TE_None <= (te) ) && ( (te) <= CC_TE_DelayedPromotion ) )

#define CC_TAG_IS_VALID(te) ( ( CC_TE_None < (te) ) && ( (te) <= CC_TE_PawnSacrifice ) )

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

    /* En passant tag, semi-persistent. Gained in a move, used or lost in the
    very next one. */
    // TODO :: MOVE into CcGame --> DELETE
    // CC_TE_EnPassant,

    /* Pawn sacrifice tag, non-persistent. Gained in a move, used or lost in
    the very same move. */
    // TODO :: MOVE into CcGame --> DELETE
    // CC_TE_PawnSacrifice = 5,

    /* Piece has started a move, thus cannot return to its starting position.
    This house-keeping flag is obtained after the first ply is finished, and
    follows the piece for the remainder of the move. */
    // \TODO :: MOVE into CcGame --> SPLIT flag into --> starting position + starting piece
    //       :: ADD --> checks if both flags meet after move is finished
    //       :: ADD --> reset after each move performed
    // CC_TE_MoveStarterFlag = 0x80,
} CcTagEnum;

typedef unsigned char CcTagType;

char cc_tag_as_char( CcTagType ct );

CcTagType cc_tag_from_char( char c );


#endif /* __CC_TAG_H__ */
