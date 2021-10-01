// Copyright (c) 2021 Mario Mlačak, mmlacak@gmail.com
// Licensed under GNU GPL v3+ license. See LICENSE, COPYING files for details.

#ifndef __CC_TAG_H__
#define __CC_TAG_H__

/**
    @file cc_tag.h
    @brief Tag enumeration, and related functions.

    Tag is a link between a piece and field at which it stands.
    Every piece can have only one tag applied at any given time.

    Persistant tags are valid until used or lost, e.g. until piece
    is moved, activated, converted, captured, displaced, teleported,
    demoted (if figure), promoted (if Pawn).
*/

// TODO :: DOCS
#define CC_TAG_EXISTS(te) ( (te) != CC_TE_None )

// TODO :: DOCS
#define CC_TAG_CAN_RUSH(te) ( (te) == CC_TE_CanRush )

// TODO :: DOCS
#define CC_TAG_CAN_CASTLE(te) ( (te) == CC_TE_CanCastle )

// TODO :: DOCS
#define CC_TAG_CAN_PROMOTE(te) ( (te) == CC_TE_DelayedPromotion )

// TODO :: DOCS
#define CC_TAG_CAN_EN_PASSANT(te) ( (te) == CC_TE_EnPassant )

// TODO :: DOCS
#define CC_TAG_CAN_PAWN_SACRIFICE(te) ( (te) == CC_TE_PawnSacrifice )

// TODO :: DOCS
#define CC_TAG_IS_LASTING(te) ( ( (te) == CC_TE_CanRush )           \
                             || ( (te) == CC_TE_CanCastle )         \
                             || ( (te) == CC_TE_DelayedPromotion ) )


/**
    Enumerates all tags, used in all variants.

    Tag `CC_TE_None` is used for e.g. empty on-board fields, any off-board field.
*/
typedef enum CcTagEnum
{
    CC_TE_None, /**< No tag applies. */

    CC_TE_CanRush, /**< Pawn can rush, persistant tag. */
    CC_TE_CanCastle, /**< Rooks, Kings can castle, persistant tag. */
    CC_TE_DelayedPromotion, /**< Pawn delayed promotion, persistant tag. */

    CC_TE_EnPassant, /**< En passant tag, semi-persistant. Gained in a move, used or lost in the very next one. */

    CC_TE_PawnSacrifice, /**< Pawn sacrifice tag, non-persistant. Gained in a move, used or lost in the very same move. */
} CcTagEnum;


/**
    Function returning tag char, based on tag enum.

    @param ct Tag enum.

    @return Tag char, uppercase if valid, space otherwise.
*/
char cc_tag_as_char( CcTagEnum const ct );


#endif /* __CC_TAG_H__ */
