// Copyright (c) 2021 Mario Mlačak, mmlacak@gmail.com
// Licensed under GNU GPL v3+ license. See LICENSING, COPYING files for details.

#ifndef __CC_DO_MOVES_H__
#define __CC_DO_MOVES_H__

#include "cc_chessboard.h"
#include "cc_move.h"

/**
    @file cc_do_moves.h
    @brief Applying moves, plies, steps to chessboard.
*/


/**
    Apply move enumeration.
*/
typedef enum CcDoMoveEnum
{
    CC_DME_DoOnlyCurrentMove,
    CC_DME_DoOnlyLastMove,
    CC_DME_DoAllMoves,
} CcDoMoveEnum;


/**
    Function returning linkage of a next ply in a cascade.

    @param ply A ply.

    @return Linkage if successful (and if there is next ply in a cascade), `NULL` otherwise.
*/
CcPlyLinkEnum * cc_get_next_ply_link( CcPly * restrict ply );

/**
    Checks if linkage of a next ply in a cascade is teleportation.

    @param ply A ply.

    @note
    Currently, teleporting linkages are:
    - `CC_PLE_Teleportation`
    - `CC_PLE_FailedTeleportation`

    @return `true` if next ply is teleporting, `false` otherwise.
*/
bool cc_is_teleporting_next( CcPly * restrict ply );

/**
    Removes all temporarily tags.

    @param cb__io _Input/output_ parameter, a chessboard to be altered.

    @note
    Temporarily tags are those which are used (or lost) in the same or the very next move,
    those are `CC_TE_EnPassant`, `CC_TE_PawnSacrifice`.

    @note
    Function is meant to clean-up tags after every move, so temporarily tags can't be
    used after expiration.

    @return `true` if successful, `false` otherwise.
*/
bool cc_remove_all_temporarily_tags( CcChessboard * restrict cb__io );


/**
    Applies step to chessboard.

    @param cb__io _Input/output_ parameter, a chessboard to be altered.
    @param move Grand-parent move which owns this step.
    @param ply Parent ply which owns this step.
    @param step A step being applied.

    @return `true` if successful, `false` otherwise.
*/
bool cc_do_step( CcChessboard * restrict cb__io,
                 CcMove * restrict move,
                 CcPly * restrict ply,
                 CcStep * restrict step );

/**
    Applies ply to chessboard.

    @param cb__io _Input/output_ parameter, a chessboard to be altered.
    @param move Parent move which owns this step.
    @param ply A ply being applied.

    @return `true` if successful, `false` otherwise.
*/
bool cc_do_ply( CcChessboard * restrict cb__io,
                CcMove * restrict move,
                CcPly * restrict ply );

/**
    Applies move(s) to chessboard.

    @param cb__io _Input/output_ parameter, a chessboard to be altered.
    @param moves A move(s) being applied.
    @param do_spec Flag, which move(s) are to be applied.

    @return `true` if successful, `false` otherwise.
*/
bool cc_do_moves( CcChessboard * restrict cb__io,
                  CcMove * restrict moves,
                  CcDoMoveEnum do_spec );


#endif /* __CC_DO_MOVES_H__ */
