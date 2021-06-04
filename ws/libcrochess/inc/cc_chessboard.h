// Copyright (c) 2021 Mario Mlačak, mmlacak@gmail.com
// Licensed under 3-clause (modified) BSD license. See LICENSE for details.

#ifndef __CC_CHESS_BOARD_H__
#define __CC_CHESS_BOARD_H__

#include "cc_piece.h"
#include "cc_tag.h"
#include "cc_variant.h"


typedef struct CcChessboard
{
    CcVariantEnum type;
    unsigned int size;

    CcPieceEnum board[ CC_VARIANT_BOARD_SIZE_MAXIMUM ][ CC_VARIANT_BOARD_SIZE_MAXIMUM ];
    CcTagEnum tags[ CC_VARIANT_BOARD_SIZE_MAXIMUM ][ CC_VARIANT_BOARD_SIZE_MAXIMUM ];
} CcChessboard;


bool cc_is_field_light( int i, int j );

CcChessboard * cc_chessboard_new( CcVariantEnum const ve, bool do_setup );
bool cc_chessboard_init( CcChessboard * const restrict cb, CcVariantEnum const ve, bool do_setup );
bool cc_chessboard_clear( CcChessboard * const restrict cb );
bool cc_chessboard_setup( CcChessboard * const restrict cb );

bool cc_chessboard_is_on_board( CcChessboard const * const restrict cb, int i, int j );
CcPieceEnum cc_chessboard_get_piece( CcChessboard const * const restrict cb, int i, int j );
CcTagEnum cc_chessboard_get_tag( CcChessboard const * const restrict cb, int i, int j );
bool cc_chessboard_set_piece_tag( CcChessboard * const restrict cb, int i, int j, CcPieceEnum pe, CcTagEnum ct );
bool cc_chessboard_set_piece( CcChessboard * const restrict cb, int i, int j, CcPieceEnum pe );
bool cc_chessboard_set_tag( CcChessboard * const restrict cb, int i, int j, CcTagEnum tt );

// static char * cc_chessboard_get_divider_new( CcChessboard const * const restrict cb );
// static char * cc_chessboard_get_horizontal_ruler_new( CcChessboard const * const restrict cb );
char * cc_chessboard_as_string_new( CcChessboard const * const restrict cb, bool is_board_or_chips );
bool cc_chessboard_print( CcChessboard const * const restrict cb, bool is_board_or_chips );


#endif /* __CC_CHESS_BOARD_H__ */
