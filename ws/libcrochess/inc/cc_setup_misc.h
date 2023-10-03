// Copyright (c) 2023 Mario Mlačak, mmlacak@gmail.com
// Licensed under GNU GPL v3+ license. See LICENSING, COPYING files for details.

#ifndef __CC_SETUP_MISC_H__
#define __CC_SETUP_MISC_H__

#include "cc_setup_board.h"
// #include "cc_setup_tags.h"

/**
    @file cc_setup_misc.h
    @brief Misc functions related to setup, or dependant on a variant being played, size of board, etc.
*/


/**
    Minimum distance King can travel when castling.

    Does not depend on variant being played.
*/
#define CC_KING_MIN_CASTLING_DISTANCE (2)


/**
    Function returns file of a figure in an initial setup of a chessboard.

    @param ve A variant.
    @param pe A figure.
    @param search_queen_side_first Flag, whether to search Queen-side, or
                                   King-side first.

    @note
    Function returns valid results only for figures in first (or last) row in an
    initial setup of chessboard. Pawns, Scouts, Grenadiers, and Monoliths are not
    covered, they'll always return invalid value (`CC_INVALID_COORD`).

    @return File if figure is found in a first/last row of initial setup,
            `CC_INVALID_COORD` otherwise.
*/
int cc_get_figure_initial_file( CcVariantEnum ve,
                                CcPieceEnum pe,
                                bool search_queen_side_first );

/**
    Function returns rank of figures in an initial setup of a chessboard.

    @param ve A variant.
    @param is_light Flag, whether to return light, or dark figure's rank.

    @return Rank of figures in initial setup if valid variant is given,
            `CC_INVALID_COORD` otherwise.
*/
int cc_get_initial_figure_rank( CcVariantEnum ve, bool is_light );

/**
    Function returns file of Kings in an initial setup of a chessboard.

    @param ve A variant.

    @return Kng's file in initial setup if valid variant is given,
            `CC_INVALID_COORD` otherwise.
*/
int cc_get_kings_initial_file( CcVariantEnum ve );

/**
    Function returns maximum castling distance Kings can make.

    @param ve A variant.

    @return Maximum Kng's castling distance if valid variant is given,
            `CC_INVALID_COORD` otherwise.
*/
int cc_get_kings_max_castling_distance( CcVariantEnum ve );

/**
    Function checks if position is valid step-field for castling King.

    @param ve A variant.
    @param is_light Flag, whether to check castling position for light, or dark King.
    @param i File, position along horizontal axis.
    @param j Rank, position along vertical axis.
    @param min_i__o _Output_, lower bound on King's castling file.
    @param max_i__o _Output_, upper bound on King's castling file.

    @note
    _Output_ parameters `min_i__o`, and `max_i__o` contain either Queen-, or King-side
    castling bounds, depending to which a given position (`i`, and `j` parameters) belong,
    i.e. if function returns `true`, it's guaranteed for     `min_i__o` <= `i` <= `max_i__o`
    to hold true, as well.

    @return `true` if position is valid step-field for castling King,
            `false` otherwise.
*/
bool cc_check_pos_is_king_castling_step( CcVariantEnum ve,
                                         bool is_light,
                                         int i,
                                         int j,
                                         int * restrict min_i__o,
                                         int * restrict max_i__o );


#endif /* __CC_SETUP_MISC_H__ */
