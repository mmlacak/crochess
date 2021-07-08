// Copyright (c) 2021 Mario Mlačak, mmlacak@gmail.com
// Licensed under 3-clause (modified) BSD license. See LICENSE for details.

#ifndef __CC_PRINT_MOVES_H__
#define __CC_PRINT_MOVES_H__

// #include "cc_piece.h"
#include "cc_chessboard.h"

#include "cc_move.h"
#include "cc_ply.h"
#include "cc_step.h"


typedef enum CcFormatMoveScopeEnum
{
    CC_FMSE_FormatOnlyCurrentMove,
    CC_FMSE_FormatOnlyLastMove,
    CC_FMSE_FormatAllMoves,
} CcFormatMoveScopeEnum;

typedef enum CcWrapPlyInSquareBracketsEnum
{
    CC_WPISB_Never,
    CC_WPISB_IfCascading_HasSteps,
    CC_WPISB_IfCascading,
    CC_WPISB_Always,
} CcWrapPlyInSquareBracketsEnum;

bool cc_if_wrap_ply_in_square_brackets( CcWrapPlyInSquareBracketsEnum wrap,
                                        CcMove const * const restrict move,
                                        CcPly const * const restrict ply,
                                        bool default_value );

typedef struct CcFormatMove
{
    CcFormatMoveScopeEnum scope;
    CcFormatStepUsageEnum usage;
    bool do_format_with_pawn_symbol;
    bool do_dark_pieces_uppercase;
    CcWrapPlyInSquareBracketsEnum wrap;
    bool default_wrap;
} CcFormatMove;

CcFormatMove cc_format_move( CcFormatMoveScopeEnum scope,
                             CcFormatStepUsageEnum usage,
                             bool do_format_with_pawn_symbol,
                             bool do_dark_pieces_uppercase,
                             CcWrapPlyInSquareBracketsEnum wrap,
                             bool default_wrap );

CcFormatMove cc_format_move_user( CcFormatMoveScopeEnum scope );
CcFormatMove cc_format_move_output( CcFormatMoveScopeEnum scope );
CcFormatMove cc_format_move_debug( CcFormatMoveScopeEnum scope );


char cc_format_pos_file( int i );
char * cc_format_pos_rank_new( int j );

char * cc_format_side_effect_new( CcChessboard const * const restrict cb,
                                  CcMove const * const restrict move,
                                  CcPly const * const restrict ply,
                                  CcStep const * const restrict step,
                                  CcSideEffect const * const restrict side_effect,
                                  CcFormatMove const format_move );

char * cc_format_step_new( CcChessboard const * const restrict cb,
                           CcMove const * const restrict move,
                           CcPly const * const restrict ply,
                           CcStep const * const restrict step,
                           CcFormatMove const format_move );

char * cc_format_ply_new( CcChessboard const * const restrict cb,
                          CcMove const * const restrict move,
                          CcPly const * const restrict ply,
                          CcFormatMove const format_move );

char * cc_format_move_new( CcChessboard const * const restrict cb,
                           CcMove const * const restrict move,
                           CcFormatMove const format_move );


#endif /* __CC_PRINT_MOVES_H__ */
