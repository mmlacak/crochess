// Copyright (c) 2021 Mario Mlačak, mmlacak@gmail.com
// Licensed under 3-clause (modified) BSD license. See LICENSE for details.

#ifndef __CHESS_BOARD_H__
#define __CHESS_BOARD_H__

#include "piece_type.h"
#include "tag_type.h"
#include "board_type.h"


typedef struct Chessboard
{
    BoardType type;
    unsigned int size;

    PieceType board[ BOARD_SIZE_MAXIMUM ][ BOARD_SIZE_MAXIMUM ];
    TagType chips[ BOARD_SIZE_MAXIMUM ][ BOARD_SIZE_MAXIMUM ];
} Chessboard;


bool is_field_light( int i, int j );

Chessboard * brd_alloc_new( BoardType const bt );
bool brd_init( Chessboard * const restrict cb, BoardType const bt );
bool brd_clear( Chessboard * const restrict cb );
bool brd_is_on_board( Chessboard const * const restrict cb, int i, int j );

bool brd_set_piece_chip( Chessboard * const restrict cb, int i, int j, PieceType pt, TagType ct );
bool brd_set_piece( Chessboard * const restrict cb, int i, int j, PieceType pt );

// static char * brd_get_divider_alloc( Chessboard const * const restrict cb );
// static char * brd_get_horizontal_ruler_alloc( Chessboard const * const restrict cb );
char * brd_as_string_alloc( Chessboard const * const restrict cb, bool is_board_or_chips );


#endif /* __CHESS_BOARD_H__ */
