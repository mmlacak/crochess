// Copyright (c) 2021, 2022 Mario Mlačak, mmlacak@gmail.com
// Licensed under GNU GPL v3+ license. See LICENSING, COPYING files for details.

#ifndef __CC_PARSE_DEFS_H__
#define __CC_PARSE_DEFS_H__

#include <stdlib.h> // atoi

/**
    @file cc_parse_defs.h
    @brief Macro, enum definitions.
*/


#define CC_IS_PLY_GATHER(char_c) ( ( (char_c) == '[' ) || ( (char_c) == ']' ) )
#define CC_IS_PLY_GATHER_START(char_c) ( (char_c) == '[' )
#define CC_IS_PLY_GATHER_END(char_c) ( (char_c) == ']' )

#define CC_IS_PIECE_SYMBOL(char_c) ( isupper( (char_c) ) )

#define CC_MAX_LEN_STEP (3)
#define CC_MAX_LEN_DISAMBIGUATION (3)
#define CC_MAX_LEN_DISAMBIGUATION_STEP (6)


/**
    Ply link enumeration.

    This enumerates different ways plies can cascade,
    and directly corresponds to cascading plies separators and terminators.
*/
typedef enum CcPlyLinkEnum
{
    CC_PLE_None, /**< Ply link not found, uninitialized, or error happened. */
    CC_PLE_StartingPly, /**< Just first ply, standalone or starting a cascade. */
    CC_PLE_CascadingPly, /**< Just one ply, continuing cascade. Corresponds to `~`. */
    CC_PLE_Teleportation, /**< Teleportation of piece. Corresponds to `|`. */
    CC_PLE_FailedTeleportation, /**< Failed teleportation, corresponds to `||`. */
    CC_PLE_TranceJourney, /**< Trance-journey, corresponds to `@`. */
    CC_PLE_DualTranceJourney, /**< Double trance-journey, corresponds to `@@`. */
    CC_PLE_FailedTranceJourney, /**< Failed trance-journey, corresponds to `@@@`. */
    CC_PLE_PawnSacrifice, /**< Pawn sacrifice, corresponds to `;;`. */
} CcPlyLinkEnum;

/**
    Losing tag enumeration.

    This enumerates all delayed opportunities, which can be lost.
*/
typedef enum CcLosingTagEnum
{
    CC_LTE_None, /**< Losing tag not found, uninitialized, or error happened. */
    CC_LTE_Promotion, /**< Losing promotion, corresponds to == (dual equal sign). */
    CC_LTE_Rushing, /**< Losing ability to rush, corresponds to :: (double-colon). */
    CC_LTE_Castling, /**< Losing ability to castle, corresponds to && (double-ampersand). */
} CcLosingTagEnum;

/**
    Step link enumeration.
*/
typedef enum CcStepLinkEnum
{
    CC_SLE_None, /**< Step link not found, uninitialized, or error happened. */
    CC_SLE_Start, /**< Position from which a piece started moving. */
    CC_SLE_Reposition, /**< In trance-journey, dark Shaman's distant starting field; separated by , (comma). */
    CC_SLE_Next, /**< Step immediately following previous, separated by . (dot). */
    CC_SLE_Distant, /**< Step not immediately following previous, separated by .. (double-dot). */
    CC_SLE_Destination, /**< Step to destination field, separated by - (hyphen). */
} CcStepLinkEnum;

/**
    Side-effects enumeration.

    This enumerates all side-effects, except losing tags.
*/
typedef enum CcSideEffectEnum
{
    CC_SEE_None, /**< Side-effect not found, uninitialized, or error happened. */
    CC_SEE_Capturing, /**< Capturing, corresponds to * (asterisk). */
    CC_SEE_Displacement, /**< Trance-journey displacement, correspondes to < (less-than). */
    CC_SEE_EnPassant, /**< En passant, corresponds to : (colon). */
    CC_SEE_Castling, /**< Castling, corresponds to & (ampersand). */
    CC_SEE_Promotion, /**< Promotion, corresponds to = (equal sign). */
    CC_SEE_PromotionNoSign, /**< Promotion, without sign. */
    CC_SEE_TagForPromotion, /**< Tag for promotion, corresponds to = (equal sign). */
    CC_SEE_Conversion, /**< Conversion, corresponds to % (percent sign). */
    CC_SEE_FailedConversion, /**< Failed conversion, corresponds to %% (double percent sign). */
    CC_SEE_DemotingToPawn, /**< Syzygy, demoting to Pawn, corresponds to > (greater-than sign). */
    CC_SEE_Resurrection, /**< Syzygy, resurrection, corresponds to $ (dollar-sign). */
    CC_SEE_FailedResurrection, /**< Syzygy, failed resurrection, corresponds to $$ (dual dollar-sign). */
} CcSideEffectEnum;


char const * cc_losing_tag_as_string( CcLosingTagEnum lte );


#endif /* __CC_PARSE_DEFS_H__ */
