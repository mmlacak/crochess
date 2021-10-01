// Copyright (c) 2021 Mario Mlačak, mmlacak@gmail.com
// Licensed under GNU GPL v3+ license. See LICENSE, COPYING files for details.

#ifndef __CC_CHESS_BOARD_H__
#define __CC_CHESS_BOARD_H__

#include "cc_piece.h"
#include "cc_tag.h"
#include "cc_variant.h"

/**
    @file cc_chessboard.h
    @brief Chessboard structure, and related functions.
*/


/**
    Chessboard structure, used for all variants.
*/
typedef struct CcChessboard
{
    CcVariantEnum type; /**< Chess variant to play. */
    unsigned int size; /**< Actual size of a board used for a given variant. */

    CcPieceEnum board[ CC_VARIANT_BOARD_SIZE_MAXIMUM ][ CC_VARIANT_BOARD_SIZE_MAXIMUM ]; /**< Holds pieces in place. */
    CcTagEnum tags[ CC_VARIANT_BOARD_SIZE_MAXIMUM ][ CC_VARIANT_BOARD_SIZE_MAXIMUM ]; /**< Holds tags for pieces at their respective position. */
} CcChessboard;


/**
    Function returning if given position is light field, or dark.
    Position itself can be off-board, i.e. on a virtual board, extending exisiting one.

    @param i File, position along horizontal axis.
    @param j Rank, position along vertical axis.

    @return `true` if position is light, `false` otherwise.
*/
bool cc_is_field_light( int const i, int const j );


/**
    Function allocating a new chessboard, initialized for a given variant.

    @param ve Variant to play.
    @param do_setup Wheather to set-up pieces to their initial positions.
                    If `false`, chessboard returned is empty.

    @return A newly allocated chessboard is successful, `NULL` otherwise.
*/
CcChessboard * cc_chessboard_new( CcVariantEnum const ve, bool const do_setup );

/**
    (Re-)initializes a chessboard, for a new variant.

    @param cb_io Chessboard to (re-)initialize.
    @param ve New variant to play, can be different to the one found in `cb_io->type`.
    @param do_setup Wheather to set-up pieces to their initial positions.
                    If `false`, chessboard is cleared of all pieces.

    @return `true` if chessboard is succesfully (re-)initialized, `false` otherwise.
*/
bool cc_chessboard_init( CcChessboard * const restrict cb_io,
                         CcVariantEnum const ve,
                         bool const do_setup );

/**
    Clears a chessboard of all pieces.

    @param cb_io Chessboard to clear.

    @return `true` if chessboard is succesfully cleared, `false` otherwise.
*/
bool cc_chessboard_clear( CcChessboard * const restrict cb_io );

/**
    Sets up pieces on a chessboard to their initial positions.

    @param cb_io Chessboard to set-up.

    @return `true` if chessboard is succesfully set up, `false` otherwise.
*/
bool cc_chessboard_setup( CcChessboard * const restrict cb_io );


/**
    Copies a chessboard to another one.

    @param into_io Chessboard to copy into.
    @param from Chessboard to copy from.

    @return `true` if chessboard is succesfully copied, `false` otherwise.
*/
bool cc_chessboard_copy( CcChessboard * const restrict into_io,
                         CcChessboard const * const restrict from );

/**
    Duplicates a chessboard, by allocating a new one and copying into it from a given chessboard.

    @param from Chessboard to copy from.

    @return A newly allocated chessboard if succesfully duplicated, `NULL` otherwise.
*/
CcChessboard * cc_chessboard_duplicate_new( CcChessboard const * const restrict from );

/**
    Frees chessboard, and all allocated resources.

    @param cb__f A chessboard.

    @return `true` if successful, `false` otherwise.
*/
bool cc_chessboard_free_all( CcChessboard ** const restrict cb__f );

/**
    Function returning if given coordinate belongs to a board.

    @param cb Chessboard.
    @param coord A coordinate.

    @return `true` if coordinate is on-board, `false` otherwise.
*/
bool cc_chessboard_is_coord_on_board( CcChessboard const * const restrict cb,
                                      int const coord );

/**
    Function returning if given position belongs to a board.

    @param cb Chessboard.
    @param i File, position along horizontal axis.
    @param j Rank, position along vertical axis.

    @return `true` if position is on-board, `false` otherwise.
*/
bool cc_chessboard_is_pos_on_board( CcChessboard const * const restrict cb,
                                    int const i,
                                    int const j );

/**
    Function returning piece on a given position.

    @param cb Chessboard.
    @param i File, position along horizontal axis.
    @param j Rank, position along vertical axis.

    @return Piece if position is on-board, `CC_PE_None` otherwise.
*/
CcPieceEnum cc_chessboard_get_piece( CcChessboard const * const restrict cb,
                                     int const i,
                                     int const j );

/**
    Function returning tag for a given position.

    @param cb Chessboard.
    @param i File, position along horizontal axis.
    @param j Rank, position along vertical axis.

    @return Tag if position is on-board, `CC_TE_None` otherwise.
*/
CcTagEnum cc_chessboard_get_tag( CcChessboard const * const restrict cb,
                                 int const i,
                                 int const j );

/**
    Function setting piece and tag onto given position.

    @param cb_io Chessboard.
    @param i File, position along horizontal axis.
    @param j Rank, position along vertical axis.
    @param pe Piece to set.
    @param ct Tag to set.

    @return `true` if successful, `false` otherwise.
*/
bool cc_chessboard_set_piece_tag( CcChessboard * const restrict cb_io,
                                  int const i,
                                  int const j,
                                  CcPieceEnum const pe,
                                  CcTagEnum const ct );

/**
    Function setting piece onto given position.

    @warning
    Function resets tag on the same position to `CC_TE_None`.

    @param cb_io Chessboard.
    @param i File, position along horizontal axis.
    @param j Rank, position along vertical axis.
    @param pe Piece to set.

    @return `true` if successful, `false` otherwise.
*/
bool cc_chessboard_set_piece( CcChessboard * const restrict cb_io,
                              int const i,
                              int const j,
                              CcPieceEnum const pe );

/**
    Function setting tag onto given position.

    @note
    Function does not alter piece on the same position.

    @param cb_io Chessboard.
    @param i File, position along horizontal axis.
    @param j Rank, position along vertical axis.
    @param tt Tag to set.

    @return `true` if successful, `false` otherwise.
*/
bool cc_chessboard_set_tag( CcChessboard * const restrict cb_io,
                            int const i,
                            int const j,
                            CcTagEnum const tt );

// static char * cc_chessboard_get_divider_new( CcChessboard const * const restrict cb );
// static char * cc_chessboard_get_horizontal_ruler_new( CcChessboard const * const restrict cb );

/**
    Formats a newly allocated string to represent piece, tag positions on a given chessboard.

    @param cb Chessboard to display.
    @param is_board_or_tag Wheather pieces are displayed (if `true`), or tags (if `false`).

    @return A newly allocated string is successful, `NULL` otherwise.
*/
char * cc_chessboard_as_string_new( CcChessboard const * const restrict cb,
                                    bool const is_board_or_tag );

/**
    Prints string representing piece, tag positions on a given chessboard.

    @param cb Chessboard to display.
    @param is_board_or_tag Wheather pieces are displayed (if `true`), or tags (if `false`).

    @return `true` is successful, `false` otherwise.
*/
bool cc_chessboard_print( CcChessboard const * const restrict cb,
                          bool const is_board_or_tag );


#endif /* __CC_CHESS_BOARD_H__ */
