// Copyright (c) 2021 Mario Mlačak, mmlacak@gmail.com
// Licensed under GNU GPL v3+ license. See LICENSING, COPYING files for details.

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
    Macro expression to evaluate whether given tag is valid (i.e. not None).

    @param te Tag enum.

    @return `true` if valid tag, `false` otherwise.
*/
#define CC_TAG_EXISTS(te) ( (te) != CC_TE_None )

/**
    Macro expression to evaluate whether given tag is denoting "can rush" ability.

    @param te Tag enum.

    @return `true` if "can rush" tag, `false` otherwise.
*/
#define CC_TAG_CAN_RUSH(te) ( (te) == CC_TE_CanRush )

/**
    Macro expression to evaluate whether given tag is denoting "can castle" ability.

    @param te Tag enum.

    @return `true` if "can castle" tag, `false` otherwise.
*/
#define CC_TAG_CAN_CASTLE(te) ( (te) == CC_TE_CanCastle )

/**
    Macro expression to evaluate whether given tag is denoting "can be promoted" option.

    @param te Tag enum.

    @return `true` if "can be promoted" tag, `false` otherwise.
*/
#define CC_TAG_CAN_PROMOTE(te) ( (te) == CC_TE_DelayedPromotion )

/**
    Macro expression to evaluate whether given tag is denoting "can be captured by en passant" option.

    @param te Tag enum.

    @return `true` if "can be captured by en passant" tag, `false` otherwise.
*/
#define CC_TAG_CAN_EN_PASSANT(te) ( (te) == CC_TE_EnPassant )

/**
    Macro expression to evaluate whether given tag is denoting "can capture after Pawn sacrifice" ability.

    @param te Tag enum.

    @return `true` if "can capture after Pawn sacrifice" tag, `false` otherwise.
*/
#define CC_TAG_CAN_PAWN_SACRIFICE(te) ( (te) == CC_TE_PawnSacrifice )

/**
    Macro expression to evaluate whether given tag can last for several moves.

    @param te Tag enum.

    @return `true` if lasting tag, `false` otherwise.
*/
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
char cc_tag_as_char( CcTagEnum ct );


#endif /* __CC_TAG_H__ */
