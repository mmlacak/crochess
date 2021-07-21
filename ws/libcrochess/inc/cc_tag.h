// Copyright (c) 2021 Mario Mlačak, mmlacak@gmail.com
// Licensed under 3-clause (modified) BSD license. See LICENSE for details.

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
