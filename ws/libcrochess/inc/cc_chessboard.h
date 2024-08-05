// Copyright (c) 2021 Mario Mlačak, mmlacak@gmail.com
// Licensed under GNU GPL v3+ license. See LICENSING, COPYING files for details.

#ifndef __CC_CHESS_BOARD_H__
#define __CC_CHESS_BOARD_H__

#include "cc_defines.h"
#include "cc_piece.h"
#include "cc_tag.h"
#include "cc_variant.h"


extern char const CC_CHESSBOARD_SEPARATORS_SETUP_FROM_STRING[];


typedef struct CcChessboard
{
    CcVariantEnum type;
    cc_uint size;

    CcPieceEnum board[ CC_VARIANT_BOARD_SIZE_MAXIMUM ][ CC_VARIANT_BOARD_SIZE_MAXIMUM ];
    CcTagEnum tags[ CC_VARIANT_BOARD_SIZE_MAXIMUM ][ CC_VARIANT_BOARD_SIZE_MAXIMUM ];
} CcChessboard;


CcChessboard * cc_chessboard__new( CcVariantEnum ve, bool do_setup );

bool cc_chessboard_init( CcChessboard * cb__io,
                         CcVariantEnum ve,
                         bool do_setup );

bool cc_chessboard_is_size_valid( CcChessboard * cb );

bool cc_chessboard_clear( CcChessboard * cb__io );

bool cc_chessboard_setup( CcChessboard * cb__io );


bool cc_chessboard_copy( CcChessboard * into__io,
                         CcChessboard * from );

CcChessboard * cc_chessboard_duplicate__new( CcChessboard * from );

bool cc_chessboard_free_all( CcChessboard ** cb__f );

bool cc_chessboard_is_coord_on_board( CcChessboard * cb, int coord );

bool cc_chessboard_is_pos_on_board( CcChessboard * cb, int i, int j );

bool cc_chessboard_is_disambiguation_on_board( CcChessboard * cb, int i, int j );

bool cc_chessboard_is_coord_safe_off_board( CcChessboard * cb, int coord );

bool cc_chessboard_is_pos_safe_off_board( CcChessboard * cb, int i, int j );

bool cc_chessboard_is_disambiguation_safe_off_board( CcChessboard * cb, int i, int j );

bool cc_chessboard_is_field_on_light_side( CcChessboard * cb, int j );

bool cc_chessboard_is_field_on_dark_side( CcChessboard * cb, int j );

int cc_chessboard_promoting_rank( CcChessboard * cb, bool is_light );

int cc_chessboard_figure_rank( CcChessboard * cb, bool is_light );

CcPieceEnum cc_chessboard_get_piece( CcChessboard * cb, int i, int j );

CcTagEnum cc_chessboard_get_tag( CcChessboard * cb, int i, int j );

bool cc_chessboard_set_piece_tag( CcChessboard * cb__io,
                                  int i,
                                  int j,
                                  CcPieceEnum pe,
                                  CcTagEnum ct );

bool cc_chessboard_set_piece( CcChessboard * cb__io,
                              int i,
                              int j,
                              CcPieceEnum pe );

bool cc_chessboard_set_tag( CcChessboard * cb__io,
                            int i,
                            int j,
                            CcTagEnum tt );


bool cc_chessboard_is_equal( CcChessboard * cb, CcChessboard * cb_2 );


// static char * cc_chessboard_get_divider__new( CcChessboard * cb );

// static char * cc_chessboard_get_horizontal_ruler__new( CcChessboard * cb );

char * cc_chessboard_as_string__new( CcChessboard * cb,
                                     bool is_board_or_tag );

// TODO :: move out
bool cc_chessboard_print( CcChessboard * cb,
                          bool is_board_or_tag );

CcChessboard * cc_chessboard_clear_from_string__new( CcChessboard * cb,
                                                     char const * setup );


#endif /* __CC_CHESS_BOARD_H__ */
