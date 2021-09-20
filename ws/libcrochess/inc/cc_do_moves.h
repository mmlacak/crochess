// Copyright (c) 2021 Mario Mlačak, mmlacak@gmail.com
// Licensed under 3-clause (modified) BSD license. See LICENSE for details.

#ifndef __CC_DO_MOVES_H__
#define __CC_DO_MOVES_H__

#include "cc_chessboard.h"
#include "cc_move.h"

/**
    @file cc_do_moves.h
    @brief Enumeration, and related functions applying transformations to chessboard.
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
CcPlyLinkEnum * cc_get_next_ply_link( CcPly const * const restrict ply );

/**
    Checks if linkage of a next ply in a cascade is teleportation.

    @param ply A ply.

    @note
    Currently, teleporting linkages are:
    - `CC_PLE_Teleportation`
    - `CC_PLE_FailedTeleportation`

    @return `true` if next ply is teleporting, `false` otherwise.
*/
bool cc_is_teleporting_next( CcPly const * const restrict ply );


/**
    Applies step to chessboard.

    @param cb_io A chessboard to be altered.
    @param move Grand-parent move which owns this step.
    @param ply Parent ply which owns this step.
    @param step A step being applied.

    @return `true` if successful, `false` otherwise.
*/
bool cc_do_step( CcChessboard * const restrict cb_io,
                 CcMove const * const restrict move,
                 CcPly const * const restrict ply,
                 CcStep const * const restrict step );

/**
    Applies ply to chessboard.

    @param cb_io A chessboard to be altered.
    @param move Parent move which owns this step.
    @param ply A ply being applied.

    @return `true` if successful, `false` otherwise.
*/
bool cc_do_ply( CcChessboard * const restrict cb_io,
                CcMove const * const restrict move,
                CcPly const * const restrict ply );

/**
    Applies move(s) to chessboard.

    @param cb_io A chessboard to be altered.
    @param moves A move(s) being applied.
    @param do_spec Flag, which move(s) are to be applied.

    @return `true` if successful, `false` otherwise.
*/
bool cc_do_moves( CcChessboard * const restrict cb_io,
                  CcMove const * const restrict moves,
                  CcDoMoveEnum const do_spec );


#endif /* __CC_DO_MOVES_H__ */
