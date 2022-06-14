// Copyright (c) 2021, 2022 Mario Mlačak, mmlacak@gmail.com
// Licensed under GNU GPL v3+ license. See LICENSING, COPYING files for details.

#ifndef __CC_PARSE_H__
#define __CC_PARSE_H__

#include "cc_piece.h"
#include "cc_pos.h"

/**
    @file cc_parse.h
    @brief Functions separating a move (algebraic notation string) into list of enums, sub-strings.
*/


/**
    Step link enumeration.
*/
typedef enum CcStepLinkEnum
{
    CC_SLE_Start, /**< Position from which a piece started moving. */
    CC_SLE_Reposition, /**< In trance-journey, dark Shaman's distant starting field; separated by , (comma). */
    CC_SLE_Next, /**< Step immediately following previous, separated by . (dot). */
    CC_SLE_Distant, /**< Step not immediately following previous, separated by .. (double-dot). */
    CC_SLE_Destination, /**< Step to destination field, separated by - (hyphen). */
} CcStepLinkEnum;

/**
    Ply link enumeration.

    This enumerates different ways plies can cascade,
    and directly corresponds to cascading plies separators and terminators.
*/
typedef enum CcPlyLinkEnum
{
    CC_PLE_Ply, /**< Just one ply, starting or continuing cascade. If cascading, corresponds to `~`. */
    CC_PLE_Teleportation, /**< Teleportation of piece. Corresponds to `|`. */
    CC_PLE_FailedTeleportation, /**< Failed teleportation, corresponds to `||`. */
    CC_PLE_TranceJourney, /**< Trance-journey, corresponds to `@`. */
    CC_PLE_DualTranceJourney, /**< Double trance-journey, corresponds to `@@`. */
    CC_PLE_FailedTranceJourney, /**< Failed trance-journey, corresponds to `@@@`. */
    CC_PLE_PawnSacrifice, /**< Pawn sacrifice, corresponds to `;;`. */
} CcPlyLinkEnum;


/**
    Ply algebraic notation structure.
*/
typedef struct CcPlyANs
{
    char * link_an_str; /**< A link, algebraic notation for cascading. Can be `NULL`, in which case `CC_PLE_Ply` is assumed. */
    CcPlyLinkEnum link; /**< Type of a link between this ply and previous (if in a cascade).  */

    char * ply_an_str; /**< A ply, algebraic notation for a complete movement of a piece. */
    CcPieceEnum piece; /**< A piece being moved in this ply. */

    CcPos start; /**< Starting field; parsed, calculated, or copied from destination field of a previous ply. */

    // TODO :: not needed ?
    //
    // struct CcPlyANs * next; /**< Next ply in a cascade. */
} CcPlyANs;


#endif /* __CC_PARSE_H__ */
