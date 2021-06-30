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


char cc_format_pos_file( int i );
char * cc_format_pos_rank_new( int j );

char * cc_format_side_effect_new(   CcChessboard const * const restrict cb,
                                    CcMove const * const restrict move,
                                    CcPly const * const restrict ply,
                                    CcStep const * const restrict step,
                                    CcSideEffect const * const restrict side_effect );

char * cc_format_step_new( CcChessboard const * const restrict cb,
                           CcMove const * const restrict move,
                           CcPly const * const restrict ply,
                           CcStep const * const restrict step );

char * cc_format_ply_new( CcChessboard const * const restrict cb,
                          CcMove const * const restrict move,
                          CcPly const * const restrict ply );

char * cc_format_move_new( CcChessboard const * const restrict cb,
                           CcMove const * const restrict move );


#endif /* __CC_PRINT_MOVES_H__ */
