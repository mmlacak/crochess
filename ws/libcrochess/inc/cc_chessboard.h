// Copyright (c) 2021 Mario Mlačak, mmlacak@gmail.com
// Licensed under GNU GPL v3+ license. See LICENSING, COPYING files for details.

#ifndef __CC_CHESS_BOARD_H__
#define __CC_CHESS_BOARD_H__

#include "cc_piece.h"
#include "cc_tag.h"
#include "cc_variant.h"

/**
    @file cc_chessboard.h
    @brief Storage for pieces, tags on board.
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
bool cc_is_field_light( int i, int j );


/**
    Function allocating a new chessboard, initialized for a given variant.

    @param ve Variant to play.
    @param do_setup Wheather to set-up pieces to their initial positions.
                    If `false`, chessboard returned is empty.

    @return A newly allocated chessboard is successful, `NULL` otherwise.
*/
CcChessboard * cc_chessboard_new( CcVariantEnum ve, bool do_setup );

/**
    (Re-)initializes a chessboard, for a new variant.

    @param cb__io Chessboard to (re-)initialize.
    @param ve New variant to play, can be different to the one found in `cb__io->type`.
    @param do_setup Wheather to set-up pieces to their initial positions.
                    If `false`, chessboard is cleared of all pieces.

    @return `true` if chessboard is succesfully (re-)initialized, `false` otherwise.
*/
bool cc_chessboard_init( CcChessboard * restrict cb__io,
                         CcVariantEnum ve,
                         bool do_setup );

/**
    Checks if chessboard size is valid, i.e. the same as defined for variant being played.

    @param cb Chessboard to check.

    @return `true` if chessboard size is valid, `false` otherwise.
*/
bool cc_chessboard_is_size_valid( CcChessboard * restrict cb );

/**
    Clears a chessboard of all pieces.

    @param cb__io Chessboard to clear.

    @return `true` if chessboard is succesfully cleared, `false` otherwise.
*/
bool cc_chessboard_clear( CcChessboard * restrict cb__io );

/**
    Sets up pieces on a chessboard to their initial positions.

    @param cb__io Chessboard to set-up.

    @return `true` if chessboard is succesfully set up, `false` otherwise.
*/
bool cc_chessboard_setup( CcChessboard * restrict cb__io );


/**
    Copies a chessboard to another one.

    @param into__io Chessboard to copy into.
    @param from Chessboard to copy from.

    @return `true` if chessboard is succesfully copied, `false` otherwise.
*/
bool cc_chessboard_copy( CcChessboard * restrict into__io,
                         CcChessboard * restrict from );

/**
    Duplicates a chessboard, by allocating a new one and copying into it from a given chessboard.

    @param from Chessboard to copy from.

    @return A newly allocated chessboard if succesfully duplicated, `NULL` otherwise.
*/
CcChessboard * cc_chessboard_duplicate_new( CcChessboard * restrict from );

/**
    Frees chessboard, and all allocated resources.

    @param cb__f A chessboard.

    @return `true` if successful, `false` otherwise.
*/
bool cc_chessboard_free_all( CcChessboard ** restrict cb__f );

/**
    Function returning if given coordinate belongs to a board.

    @param cb Chessboard.
    @param coord A coordinate.

    @return `true` if coordinate is on-board, `false` otherwise.
*/
bool cc_chessboard_is_coord_on_board( CcChessboard * restrict cb,
                                      int coord );

/**
    Function returning if given position belongs to a board.

    @param cb Chessboard.
    @param i File, position along horizontal axis.
    @param j Rank, position along vertical axis.

    @return `true` if position is on-board, `false` otherwise.
*/
bool cc_chessboard_is_pos_on_board( CcChessboard * restrict cb,
                                    int i,
                                    int j );

/**
    Function returning piece on a given position.

    @param cb Chessboard.
    @param i File, position along horizontal axis.
    @param j Rank, position along vertical axis.

    @return Piece if position is on-board, `CC_PE_None` otherwise.
*/
CcPieceEnum cc_chessboard_get_piece( CcChessboard * restrict cb,
                                     int i,
                                     int j );

/**
    Function returning tag for a given position.

    @param cb Chessboard.
    @param i File, position along horizontal axis.
    @param j Rank, position along vertical axis.

    @return Tag if position is on-board, `CC_TE_None` otherwise.
*/
CcTagEnum cc_chessboard_get_tag( CcChessboard * restrict cb,
                                 int i,
                                 int j );

/**
    Function setting piece and tag onto given position.

    @param cb__io Chessboard.
    @param i File, position along horizontal axis.
    @param j Rank, position along vertical axis.
    @param pe Piece to set.
    @param ct Tag to set.

    @return `true` if successful, `false` otherwise.
*/
bool cc_chessboard_set_piece_tag( CcChessboard * restrict cb__io,
                                  int i,
                                  int j,
                                  CcPieceEnum pe,
                                  CcTagEnum ct );

/**
    Function setting piece onto given position.

    @warning
    Function resets tag on the same position to `CC_TE_None`.

    @param cb__io Chessboard.
    @param i File, position along horizontal axis.
    @param j Rank, position along vertical axis.
    @param pe Piece to set.

    @return `true` if successful, `false` otherwise.
*/
bool cc_chessboard_set_piece( CcChessboard * restrict cb__io,
                              int i,
                              int j,
                              CcPieceEnum pe );

/**
    Function setting tag onto given position.

    @note
    Function does not alter piece on the same position.

    @param cb__io Chessboard.
    @param i File, position along horizontal axis.
    @param j Rank, position along vertical axis.
    @param tt Tag to set.

    @return `true` if successful, `false` otherwise.
*/
bool cc_chessboard_set_tag( CcChessboard * restrict cb__io,
                            int i,
                            int j,
                            CcTagEnum tt );

// static char * cc_chessboard_get_divider_new( CcChessboard * restrict cb );
// static char * cc_chessboard_get_horizontal_ruler_new( CcChessboard * restrict cb );

/**
    Formats a newly allocated string to represent piece, tag positions on a given chessboard.

    @param cb Chessboard to display.
    @param is_board_or_tag Wheather pieces are displayed (if `true`), or tags (if `false`).

    @return A newly allocated string is successful, `NULL` otherwise.
*/
char * cc_chessboard_as_string_new( CcChessboard * restrict cb,
                                    bool is_board_or_tag );

/**
    Prints string representing piece, tag positions on a given chessboard.

    @param cb Chessboard to display.
    @param is_board_or_tag Wheather pieces are displayed (if `true`), or tags (if `false`).

    @return `true` is successful, `false` otherwise.
*/
bool cc_chessboard_print( CcChessboard * restrict cb,
                          bool is_board_or_tag );


#endif /* __CC_CHESS_BOARD_H__ */
