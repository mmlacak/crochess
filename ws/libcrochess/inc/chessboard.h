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
    TagType tags[ BOARD_SIZE_MAXIMUM ][ BOARD_SIZE_MAXIMUM ];
} Chessboard;


bool is_field_light( int i, int j );

Chessboard * cb_alloc( BoardType const bt );
bool cb_init( Chessboard * const restrict cb, BoardType const bt );
bool cb_clear( Chessboard * const restrict cb );
bool cb_setup( Chessboard * const restrict cb );

bool cb_is_on_board( Chessboard const * const restrict cb, int i, int j );
bool cb_set_piece_chip( Chessboard * const restrict cb, int i, int j, PieceType pt, TagType ct );
bool cb_set_piece( Chessboard * const restrict cb, int i, int j, PieceType pt );

// static char * cb_get_divider_alloc( Chessboard const * const restrict cb );
// static char * cb_get_horizontal_ruler_alloc( Chessboard const * const restrict cb );
char * cb_as_string_alloc( Chessboard const * const restrict cb, bool is_board_or_chips );
bool cb_print( Chessboard const * const restrict cb, bool is_board_or_chips );


#endif /* __CHESS_BOARD_H__ */
