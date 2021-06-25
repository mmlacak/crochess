// Copyright (c) 2021 Mario Mlačak, mmlacak@gmail.com
// Licensed under 3-clause (modified) BSD license. See LICENSE for details.

#ifndef __CC_PRINT_MOVES_H__
#define __CC_PRINT_MOVES_H__

// #include "cc_piece.h"
#include "cc_chessboard.h"

#include "cc_move.h"
#include "cc_ply.h"
#include "cc_step.h"


typedef enum CcPrintMoveEnum
{
    CC_PME_PrintOnlyCurrentMove,
    CC_PME_PrintOnlyLastMove,
    CC_PME_PrintAllMoves,
} CcPrintMoveEnum;


char * cc_print_step( CcChessboard * const restrict cb,
                      CcMove const * const restrict move,
                      CcPly const * const restrict ply,
                      CcStep const * const restrict step );

char * cc_print_ply( CcChessboard * const restrict cb,
                     CcMove const * const restrict move,
                     CcPly const * const restrict ply );

char * cc_print_moves( CcChessboard * const restrict cb,
                       CcMove const * const restrict moves,
                       CcPrintMoveEnum do_spec );


#endif /* __CC_PRINT_MOVES_H__ */
