// Copyright (c) 2021 Mario Mlačak, mmlacak@gmail.com
// Licensed under 3-clause (modified) BSD license. See LICENSE for details.

#ifndef __BOARD_H__
#define __BOARD_H__

#include "piece_type.h"
#include "chip_type.h"
#include "board_type.h"


typedef struct Board
{
    BoardType type;
    unsigned int size;

    PieceType board[ BOARD_SIZE_MAXIMUM ][ BOARD_SIZE_MAXIMUM ];
    ChipType chips[ BOARD_SIZE_MAXIMUM ][ BOARD_SIZE_MAXIMUM ];
} Board;


bool is_field_light( int i, int j );

Board * brd_alloc_new(BoardType const bt);
bool brd_init( Board * const restrict b, BoardType const bt );
bool brd_clear(Board * const restrict b);
bool brd_is_on_board( Board const * const restrict b, int i, int j );

bool brd_set_piece_chip( Board * const restrict b, int i, int j, PieceType pt, ChipType ct );
bool brd_set_piece( Board * const restrict b, int i, int j, PieceType pt );


#endif /* __BOARD_H__ */
