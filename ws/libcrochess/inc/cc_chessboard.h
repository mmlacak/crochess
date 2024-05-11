// Copyright (c) 2021 Mario Mlačak, mmlacak@gmail.com
// Licensed under GNU GPL v3+ license. See LICENSING, COPYING files for details.

#ifndef __CC_CHESS_BOARD_H__
#define __CC_CHESS_BOARD_H__

#include "cc_defines.h"
#include "cc_piece.h"
#include "cc_tag.h"
#include "cc_variant.h"

/**
    @file cc_chessboard.h
    @brief Storage for pieces, tags on board.
*/

/**
    Separators constant, used to tokenize string setup.
*/
extern char const CC_CHESSBOARD_SEPARATORS_SETUP_FROM_STRING[];


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
    Function allocating a new chessboard, initialized for a given variant.

    @param ve Variant to play.
    @param do_setup Whether to set-up pieces to their initial positions.
                    If `false`, chessboard returned is empty.

    @return A newly allocated chessboard if successful, `NULL` otherwise.
*/
CcChessboard * cc_chessboard__new( CcVariantEnum ve, bool do_setup );

/**
    (Re-)initializes a chessboard, for a new variant.

    @param cb__io Chessboard to (re-)initialize.
    @param ve New variant to play, can be different to the one found in `cb__io->type`.
    @param do_setup Whether to set-up pieces to their initial positions.
                    If `false`, chessboard is cleared of all pieces.

    @return `true` if chessboard is successfully (re-)initialized, `false` otherwise.
*/
bool cc_chessboard_init( CcChessboard * cb__io,
                         CcVariantEnum ve,
                         bool do_setup );

/**
    Checks if chessboard size is valid, i.e. the same as defined for variant being played.

    @param cb Chessboard to check.

    @note
    Actual board size cannot be tested, 2D array is allocated only once, at the size of the largest variant.
    Chessboard size just limits which portion of board is used for a particular variant.

    @return `true` if chessboard size is valid, `false` otherwise.
*/
bool cc_chessboard_is_size_valid( CcChessboard * cb );

/**
    Clears a chessboard of all pieces.

    @param cb__io Chessboard to clear.

    @return `true` if chessboard is successfully cleared, `false` otherwise.
*/
bool cc_chessboard_clear( CcChessboard * cb__io );

/**
    Sets up pieces on a chessboard to their initial positions.

    @param cb__io Chessboard to set-up.

    @return `true` if chessboard is successfully set up, `false` otherwise.
*/
bool cc_chessboard_setup( CcChessboard * cb__io );


/**
    Copies a chessboard to another one.

    @param into__io Chessboard to copy into.
    @param from Chessboard to copy from.

    @return `true` if chessboard is successfully copied, `false` otherwise.
*/
bool cc_chessboard_copy( CcChessboard * into__io,
                         CcChessboard * from );

/**
    Duplicates a chessboard, by allocating a new one and copying into it from a given chessboard.

    @param from Chessboard to copy from.

    @return A newly allocated chessboard if successfully duplicated, `NULL` otherwise.
*/
CcChessboard * cc_chessboard_duplicate__new( CcChessboard * from );

/**
    Frees chessboard, and all allocated resources.

    @param cb__f A chessboard.

    @return `true` if successful, `false` otherwise.
*/
bool cc_chessboard_free_all( CcChessboard ** cb__f );

/**
    Function returning if given coordinate belongs to a board.

    @param cb Chessboard.
    @param coord A coordinate.

    @return `true` if coordinate is on-board, `false` otherwise.
*/
bool cc_chessboard_is_coord_on_board( CcChessboard * cb, int coord );

/**
    Function returning if given position belongs to a board.

    @param cb Chessboard.
    @param i File, position along horizontal axis.
    @param j Rank, position along vertical axis.

    @return `true` if position is on-board, `false` otherwise.
*/
bool cc_chessboard_is_pos_on_board( CcChessboard * cb, int i, int j );

/**
    Function returning if at least one coordinate of given disambiguation belongs to a board.

    @param cb Chessboard.
    @param i File, position along horizontal axis.
    @param j Rank, position along vertical axis.

    @return `true` if disambiguation is on-board, `false` otherwise.
*/
bool cc_chessboard_is_disambiguation_on_board( CcChessboard * cb, int i, int j );

/**
    Function checks if given coordinate is within safe off-board boundaries.

    @param cb Chessboard.
    @param coord A coordinate.

    @note
    Coordinate is safely off-board if there could be a movement (e.g. trance-journey),
    which would place piece back on-board.

    @return `true` if coordinate is safe off-board, `false` otherwise.
*/
bool cc_chessboard_is_coord_safe_off_board( CcChessboard * cb, int coord );

/**
    Function returning if given position is within safe off-board boundaries.

    @param cb Chessboard.
    @param i File, position along horizontal axis.
    @param j Rank, position along vertical axis.

    @note
    Position is safely off-board if there could be a movement (e.g. trance-journey),
    which would place piece back on-board.

    @return `true` if position is safe off-board, `false` otherwise.
*/
bool cc_chessboard_is_pos_safe_off_board( CcChessboard * cb, int i, int j );

/**
    Function returning if at least one coordinate of given disambiguation is within safe off-board boundaries.

    @param cb Chessboard.
    @param i File, position along horizontal axis.
    @param j Rank, position along vertical axis.

    @note
    Coordinate is safely off-board if there could be a movement (e.g. trance-journey),
    which would place piece back on-board.

    @return `true` if disambiguation is safe off-board, `false` otherwise.
*/
bool cc_chessboard_is_disambiguation_safe_off_board( CcChessboard * cb, int i, int j );

/**
    Function returning if given position is on a light side of board.

    @param cb Chessboard.
    @param j Rank, position along vertical axis.

    @return `true` if position is on a light side, `false` otherwise.
*/
bool cc_chessboard_is_field_on_light_side( CcChessboard * cb, int j );

/**
    Function returning if given position is on a dark side of board.

    @param cb Chessboard.
    @param j Rank, position along vertical axis.

    @return `true` if position is on a dark side, `false` otherwise.
*/
bool cc_chessboard_is_field_on_dark_side( CcChessboard * cb, int j );

/**
    Function returns rank of a promoting row.

    @param cb Chessboard.
    @param is_light Flag, whether it is for light or dark player.

    @return Rank of a promoting row if successful, `CC_INVALID_COORD` otherwise.
*/
int cc_chessboard_promoting_rank( CcChessboard * cb, bool is_light );

/**
    Function returns rank of a figure row.

    @param cb Chessboard.
    @param is_light Flag, whether it is for light or dark player.

    @return Rank of a figure row if successful, `CC_INVALID_COORD` otherwise.
*/
int cc_chessboard_figure_rank( CcChessboard * cb, bool is_light );

/**
    Function returning piece on a given position.

    @param cb Chessboard.
    @param i File, position along horizontal axis.
    @param j Rank, position along vertical axis.

    @return Piece if position is on-board, `CC_PE_None` otherwise.
*/
CcPieceEnum cc_chessboard_get_piece( CcChessboard * cb, int i, int j );

/**
    Function returning tag for a given position.

    @param cb Chessboard.
    @param i File, position along horizontal axis.
    @param j Rank, position along vertical axis.

    @return Tag if position is on-board, `CC_TE_None` otherwise.
*/
CcTagEnum cc_chessboard_get_tag( CcChessboard * cb, int i, int j );

/**
    Function setting piece and tag onto given position.

    @param cb__io Chessboard.
    @param i File, position along horizontal axis.
    @param j Rank, position along vertical axis.
    @param pe Piece to set.
    @param ct Tag to set.

    @return `true` if successful, `false` otherwise.
*/
bool cc_chessboard_set_piece_tag( CcChessboard * cb__io,
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
bool cc_chessboard_set_piece( CcChessboard * cb__io,
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
bool cc_chessboard_set_tag( CcChessboard * cb__io,
                            int i,
                            int j,
                            CcTagEnum tt );


/**
    Function checks if piece is blocked at given position.

    @param cb Chessboard.
    @param i File, position along horizontal axis.
    @param j Rank, position along vertical axis.
    @param piece A piece.

    @see CcMaybeBoolEnum

    @return
    One of `CcMaybeBoolEnum` values:
    - `CC_MBE_True` if piece is blocked at given position,
    - `CC_MBE_False` if piece is not blocked,
    - `CC_MBE_Void` in case of error (given chessboard was `NULL`).
*/
CcMaybeBoolEnum cc_chessboard_is_blocked_at( CcChessboard * cb,
                                             int i,
                                             int j,
                                             CcPieceEnum piece );

/**
    Function checks if a piece can capture at given position.

    @param cb Chessboard.
    @param i File, position along horizontal axis.
    @param j Rank, position along vertical axis.
    @param piece Capturing piece.

    @see CcMaybeBoolEnum

    @return
    One of `CcMaybeBoolEnum` values:
    - `CC_MBE_True` if a piece can capture at given position,
    - `CC_MBE_False` if no capture is possible,
    - `CC_MBE_Void` in case of error (given chessboard was `NULL`).
*/
CcMaybeBoolEnum cc_chessboard_can_capture_at( CcChessboard * cb,
                                              int i,
                                              int j,
                                              CcPieceEnum piece );


/**
    Compares two chessboards field-by-field.

    @param cb Chessboard to copy into.
    @param cb_2 Chessboard to copy from.

    @return `true` if chessboards are equal, `false` otherwise.
*/
bool cc_chessboard_is_equal( CcChessboard * cb, CcChessboard * cb_2 );


// static char * cc_chessboard_get_divider__new( CcChessboard * cb );
// static char * cc_chessboard_get_horizontal_ruler__new( CcChessboard * cb );

/**
    Formats a newly allocated string to represent piece, tag positions on a given chessboard.

    @param cb Chessboard to display.
    @param is_board_or_tag Whether pieces are displayed (if `true`), or tags (if `false`).

    @return A newly allocated string if successful, `NULL` otherwise.
*/
char * cc_chessboard_as_string__new( CcChessboard * cb,
                                     bool is_board_or_tag );

// TODO :: move out
//
/**
    Prints string representing piece, tag positions on a given chessboard.

    @param cb Chessboard to display.
    @param is_board_or_tag Whether pieces are displayed (if `true`), or tags (if `false`).

    @return `true` if successful, `false` otherwise.
*/
bool cc_chessboard_print( CcChessboard * cb,
                          bool is_board_or_tag );
//
// TODO :: move out

CcChessboard * cc_chessboard_clear_from_string__new( CcChessboard * cb,
                                                     char const * setup );


#endif /* __CC_CHESS_BOARD_H__ */
