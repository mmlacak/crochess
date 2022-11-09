// Copyright (c) 2021, 2022 Mario Mlačak, mmlacak@gmail.com
// Licensed under GNU GPL v3+ license. See LICENSING, COPYING files for details.

#ifndef __CC_PARSE_DEFS_H__
#define __CC_PARSE_DEFS_H__


/**
    @file cc_parse_defs.h
    @brief Macro, enum definitions.
*/


#define CC_IS_PLY_GATHER(char_c) ( ( (char_c) == '[' ) || ( (char_c) == ']' ) )
#define CC_IS_PLY_GATHER_START(char_c) ( (char_c) == '[' )
#define CC_IS_PLY_GATHER_END(char_c) ( (char_c) == ']' )

#define CC_IS_PIECE_SYMBOL(char_c) ( isupper( (char_c) ) )

#define CC_MAX_LEN_STEP_POS (3)
#define CC_MAX_LEN_DISAMBIGUATION (3)
#define CC_MAX_LEN_DISAMBIGUATION_STEP (6)


#define CC_IS_PLY_NONE(ple) ( (ple) == CC_PLE_None )
#define CC_IS_PLY_STARTING(ple) ( (ple) == CC_PLE_StartingPly )
#define CC_IS_PLY_CASCADING(ple) ( (ple) == CC_PLE_CascadingPly )

#define CC_IS_PLY_TELEPORTATION(ple) ( (ple) == CC_PLE_Teleportation )
#define CC_IS_PLY_FAILED_TELEPORTATION(ple) ( (ple) == CC_PLE_FailedTeleportation )

#define CC_IS_PLY_TRANCE_JOURNEY(ple) ( (ple) == CC_PLE_TranceJourney )
#define CC_IS_PLY_DUAL_TRANCE_JOURNEY(ple) ( (ple) == CC_PLE_DualTranceJourney )
#define CC_IS_PLY_FAILED_TRANCE_JOURNEY(ple) ( (ple) == CC_PLE_FailedTranceJourney )

#define CC_IS_PLY_PAWN_SACRIFICE(ple) ( (ple) == CC_PLE_PawnSacrifice )


#define CC_IS_PLY_VALID(ple) ( (ple) != CC_PLE_None )

#define CC_IS_PLY_ANY_TELEPORTATION(ple) ( ( (ple) == CC_PLE_Teleportation )           \
                                        || ( (ple) == CC_PLE_FailedTeleportation )

#define CC_IS_PLY_ANY_TRANCE_JOURNEY(ple) ( ( (ple) == CC_PLE_TranceJourney )           \
                                         || ( (ple) == CC_PLE_DualTranceJourney )       \
                                         || ( (ple) == CC_PLE_FailedTranceJourney ) )


// /**
//     Ply link enumeration.

//     This enumerates different ways plies can cascade,
//     and directly corresponds to cascading plies separators and terminators.
// */
// typedef enum CcPlyLinkEnum
// {
//     CC_PLE_None, /**< Ply link not found, uninitialized, or error happened. */
//     CC_PLE_StartingPly, /**< Just first ply, standalone or starting a cascade. */
//     CC_PLE_CascadingPly, /**< Just one ply, continuing cascade. Corresponds to `~`. */
//     CC_PLE_Teleportation, /**< Teleportation of piece. Corresponds to `|`. */
//     CC_PLE_FailedTeleportation, /**< Failed teleportation, corresponds to `||`. */
//     CC_PLE_TranceJourney, /**< Trance-journey, corresponds to `@`. */
//     CC_PLE_DualTranceJourney, /**< Double trance-journey, corresponds to `@@`. */
//     CC_PLE_FailedTranceJourney, /**< Failed trance-journey, corresponds to `@@@`. */
//     CC_PLE_PawnSacrifice, /**< Pawn sacrifice, corresponds to `;;`. */
// } CcPlyLinkEnum;

// /**
//     Losing tag enumeration.

//     This enumerates all delayed opportunities, which can be lost.
// */
// typedef enum CcLosingTagEnum
// {
//     CC_LTE_None, /**< Losing tag not found, uninitialized, or error happened. */
//     CC_LTE_Promotion, /**< Losing promotion, corresponds to == (dual equal sign). */
//     CC_LTE_Rushing, /**< Losing ability to rush, corresponds to :: (double-colon). */
//     CC_LTE_Castling, /**< Losing ability to castle, corresponds to && (double-ampersand). */
// } CcLosingTagEnum;

// /**
//     Step link enumeration.
// */
// typedef enum CcStepLinkEnum
// {
//     CC_SLE_None, /**< Step link not found, uninitialized, or error happened. */
//     CC_SLE_Start, /**< Position from which a piece started moving. */
//     CC_SLE_Reposition, /**< In trance-journey, dark Shaman's distant starting field; separated by , (comma). */
//     CC_SLE_Next, /**< Step immediately following previous, separated by . (dot). */
//     CC_SLE_Distant, /**< Step not immediately following previous, separated by .. (double-dot). */
//     CC_SLE_Destination, /**< Step to destination field, separated by - (hyphen). */
// } CcStepLinkEnum;


// char const * cc_losing_tag_as_string( CcLosingTagEnum lte );


#endif /* __CC_PARSE_DEFS_H__ */
