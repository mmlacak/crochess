// Copyright (c) 2021 Mario Mlačak, mmlacak@gmail.com
// Licensed under 3-clause (modified) BSD license. See LICENSE for details.

#ifndef __CHESS_BOARD_H__
#define __CHESS_BOARD_H__

#include "cc_piece.h"
#include "cc_tag.h"
#include "cc_variant.h"


typedef struct Chessboard
{
    CcVariantEnum type;
    unsigned int size;

    CcPieceEnum board[ CC_VARIANT_BOARD_SIZE_MAXIMUM ][ CC_VARIANT_BOARD_SIZE_MAXIMUM ];
    CcTagEnum tags[ CC_VARIANT_BOARD_SIZE_MAXIMUM ][ CC_VARIANT_BOARD_SIZE_MAXIMUM ];
} Chessboard;


bool is_field_light( int i, int j );

Chessboard * cb_new_alx( CcVariantEnum const be, bool do_setup );
bool cb_init( Chessboard * const restrict cb, CcVariantEnum const be, bool do_setup );
bool cb_clear( Chessboard * const restrict cb );
bool cb_setup( Chessboard * const restrict cb );

bool cb_is_on_board( Chessboard const * const restrict cb, int i, int j );
CcPieceEnum cb_get_piece( Chessboard const * const restrict cb, int i, int j );
CcTagEnum cb_get_tag( Chessboard const * const restrict cb, int i, int j );
bool cb_set_piece_tag( Chessboard * const restrict cb, int i, int j, CcPieceEnum pe, CcTagEnum ct );
bool cb_set_piece( Chessboard * const restrict cb, int i, int j, CcPieceEnum pe );
bool cb_set_tag( Chessboard * const restrict cb, int i, int j, CcTagEnum tt );

// static char * cb_get_divider_alx( Chessboard const * const restrict cb );
// static char * cb_get_horizontal_ruler_alx( Chessboard const * const restrict cb );
char * cb_as_string_alx( Chessboard const * const restrict cb, bool is_board_or_chips );
bool cb_print( Chessboard const * const restrict cb, bool is_board_or_chips );


#endif /* __CHESS_BOARD_H__ */
