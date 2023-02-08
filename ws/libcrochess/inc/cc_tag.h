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

    Values enumerated in losing tag are the same as in ordinary tag.
    So, conversion between tags changes just type, not value.
*/

/**
    Macro expression to evaluate whether given tag is valid (i.e. not None).

    @param te Tag enum, i.e. one of `CcTagEnum`, `CcLosingTagEnum` values.

    @see CcTagEnum, CcLosingTagEnum

    @return `true` if valid tag, `false` otherwise.
*/
#define CC_TAG_EXISTS(te) ( (te) != (int)CC_TE_None )

/**
    Macro expression to evaluate whether given tag is denoting "can rush" ability.

    @param te Tag enum, i.e. one of `CcTagEnum`, `CcLosingTagEnum` values.

    @see CcTagEnum, CcLosingTagEnum

    @return `true` if "can rush" tag, `false` otherwise.
*/
#define CC_TAG_CAN_RUSH(te) ( (te) == (int)CC_TE_CanRush )

/**
    Macro expression to evaluate whether given tag is denoting "can castle" ability.

    @param te Tag enum, i.e. one of `CcTagEnum`, `CcLosingTagEnum` values.

    @see CcTagEnum, CcLosingTagEnum

    @return `true` if "can castle" tag, `false` otherwise.
*/
#define CC_TAG_CAN_CASTLE(te) ( (te) == (int)CC_TE_CanCastle )

/**
    Macro expression to evaluate whether given tag is denoting "can be promoted" option.

    @param te Tag enum, i.e. one of `CcTagEnum`, `CcLosingTagEnum` values.

    @see CcTagEnum, CcLosingTagEnum

    @return `true` if "can be promoted" tag, `false` otherwise.
*/
#define CC_TAG_CAN_PROMOTE(te) ( (te) == (int)CC_TE_DelayedPromotion )

/**
    Macro expression to evaluate whether given tag is denoting "can be captured by en passant" option.

    @param te Tag enum, i.e. one of `CcTagEnum`, `CcLosingTagEnum` values.

    @see CcTagEnum, CcLosingTagEnum

    @return `true` if "can be captured by en passant" tag, `false` otherwise.
*/
#define CC_TAG_CAN_EN_PASSANT(te) ( (te) == (int)CC_TE_EnPassant )

/**
    Macro expression to evaluate whether given tag is denoting "can capture after Pawn sacrifice" ability.

    @param te Tag enum, i.e. one of `CcTagEnum`, `CcLosingTagEnum` values.

    @see CcTagEnum, CcLosingTagEnum

    @return `true` if "can capture after Pawn sacrifice" tag, `false` otherwise.
*/
#define CC_TAG_CAN_PAWN_SACRIFICE(te) ( (te) == (int)CC_TE_PawnSacrifice )

/**
    Macro expression to evaluate whether given tag can last for several moves.

    @param te Tag enum, i.e. one of `CcTagEnum`, `CcLosingTagEnum` values.

    @see CcTagEnum, CcLosingTagEnum

    @return `true` if lasting tag, `false` otherwise.
*/
#define CC_TAG_IS_LASTING(te) ( ( (te) == (int)CC_TE_CanRush )               \
                             || ( (te) == (int)CC_TE_CanCastle )             \
                             || ( (te) == (int)CC_TE_DelayedPromotion ) )

/**
    Macro expression to evaluate whether given tag is temporarily,
    i.e. lasts at most a single move.

    @param te Tag enum, i.e. one of `CcTagEnum`, `CcLosingTagEnum` values.

    @see CcTagEnum, CcLosingTagEnum

    @return `true` if temporarily tag, `false` otherwise.
*/
#define CC_TAG_IS_TEMPORARILY(te) ( ( (te) == (int)CC_TE_EnPassant )         \
                                 || ( (te) == (int)CC_TE_PawnSacrifice ) )

/**
    Macro expression to check if given tag is none, or can be lost,
    i.e. lasts at least a single move.

    @param te Tag enum, i.e. one of `CcTagEnum`, `CcLosingTagEnum` values.

    @see CcTagEnum, CcLosingTagEnum

    @return `true` if tag can be lost, or is `CC_TE_None`; `false` otherwise.
*/
#define CC_TAG_CAN_BE_LOST(te) CC_TAG_IS_LASTING((te))


#define CC_TAG_CHAR_NONE ' '
#define CC_TAG_CHAR_INVALID '?'

#define CC_TAG_CHAR_CAN_RUSH 'R'
#define CC_TAG_CHAR_CAN_CASTLE 'C'
#define CC_TAG_CHAR_DELAYED_PROMOTION 'P'

#define CC_TAG_CHAR_EN_PASSANT 'E'
#define CC_TAG_CHAR_PAWN_SACRIFICE 'S'


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

/**
    Function returning tag enum, based on tag char.

    @param c A char.

    @return Tag enum if valid `char`, `CC_TE_None` otherwise.
*/
CcTagEnum cc_tag_from_char( char c );

/**
    Enumerates only tags that can be lost, used in all variants.

    Values enumerated in losing tag are the same as in ordinary tag.
    So, conversion between tags changes just type, not value.

    When converting from ordinary tag enum, `CC_LTE_None` is used for
    all values not enumerated here.
*/
typedef enum CcLosingTagEnum
{
    CC_LTE_None = (int)CC_TE_None, /**< No tag applies. */

    CC_LTE_CanRush = (int)CC_TE_CanRush, /**< Pawn can rush, persistant tag. */
    CC_LTE_CanCastle = (int)CC_TE_CanCastle, /**< Rooks, Kings can castle, persistant tag. */
    CC_LTE_DelayedPromotion = (int)CC_TE_DelayedPromotion, /**< Pawn delayed promotion, persistant tag. */
} CcLosingTagEnum;

/**
    Maximum length of a losing-tag symbol.
*/
#define CC_MAX_LEN_LOSING_TAG (2)

/**
    Function returning string, based on lost tag.

    @param lte Lost tag.

    @return Valid string pointer.
            String can be empty, if tag cannot be lost.
*/
char const * cc_losing_tag_as_string( CcLosingTagEnum lte );

/**
    Converts ordinary tag into lost tag.

    @param te Ordinary tag.

    Ordinary tag values without equivalent losing tag value are converted into `CC_LTE_None` instead.

    @return Losing tag.
*/
CcLosingTagEnum cc_tag_to_losing( CcTagEnum te );

/**
    Converts losing tag into ordinary tag.

    @param lte Losing tag.

    @return Ordinary tag.
*/
CcTagEnum cc_tag_from_losing( CcLosingTagEnum lte );


#endif /* __CC_TAG_H__ */
