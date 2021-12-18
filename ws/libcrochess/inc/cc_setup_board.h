// Copyright (c) 2021 Mario Mlačak, mmlacak@gmail.com
// Licensed under GNU GPL v3+ license. See LICENSING, COPYING files for details.

#ifndef __CC_SETUP_BOARD_H__
#define __CC_SETUP_BOARD_H__

#include "cc_piece.h"
#include "cc_variant.h"

/**
    @file cc_setup_board.h
    @brief Board constant setups, and related functions.
*/


/**
    Classical Chess setup.
*/
extern CcPieceEnum const CC_SETUP_BOARD_CLASSICAL_CHESS[ CC_VARIANT_BOARD_SIZE_CLASSICAL_CHESS ][ CC_VARIANT_BOARD_SIZE_CLASSICAL_CHESS ];

/**
    Croatian Ties setup.
*/
extern CcPieceEnum const CC_SETUP_BOARD_CROATIAN_TIES[ CC_VARIANT_BOARD_SIZE_CROATIAN_TIES ][ CC_VARIANT_BOARD_SIZE_CROATIAN_TIES ];

/**
    Mayan Ascendancy setup.
*/
extern CcPieceEnum const CC_SETUP_BOARD_MAYAN_ASCENDANCY[ CC_VARIANT_BOARD_SIZE_MAYAN_ASCENDANCY ][ CC_VARIANT_BOARD_SIZE_MAYAN_ASCENDANCY ];

/**
    Age of Aquarius setup.
*/
extern CcPieceEnum const CC_SETUP_BOARD_AGE_OF_AQUARIUS[ CC_VARIANT_BOARD_SIZE_AGE_OF_AQUARIUS ][ CC_VARIANT_BOARD_SIZE_AGE_OF_AQUARIUS ];

/**
    Miranda's Veil setup.
*/
extern CcPieceEnum const CC_SETUP_BOARD_MIRANDAS_VEIL[ CC_VARIANT_BOARD_SIZE_MIRANDAS_VEIL ][ CC_VARIANT_BOARD_SIZE_MIRANDAS_VEIL ];

/**
    Nineteen setup.
*/
extern CcPieceEnum const CC_SETUP_BOARD_NINETEEN[ CC_VARIANT_BOARD_SIZE_NINETEEN ][ CC_VARIANT_BOARD_SIZE_NINETEEN ];

/**
    Hemera's Dawn setup.
*/
extern CcPieceEnum const CC_SETUP_BOARD_HEMERAS_DAWN[ CC_VARIANT_BOARD_SIZE_HEMERAS_DAWN ][ CC_VARIANT_BOARD_SIZE_HEMERAS_DAWN ];

/**
    Tamoanchan Revisited setup.
*/
extern CcPieceEnum const CC_SETUP_BOARD_TAMOANCHAN_REVISITED[ CC_VARIANT_BOARD_SIZE_TAMOANCHAN_REVISITED ][ CC_VARIANT_BOARD_SIZE_TAMOANCHAN_REVISITED ];

/**
    Conquest of Tlalocan setup.
*/
extern CcPieceEnum const CC_SETUP_BOARD_CONQUEST_OF_TLALOCAN[ CC_VARIANT_BOARD_SIZE_CONQUEST_OF_TLALOCAN ][ CC_VARIANT_BOARD_SIZE_CONQUEST_OF_TLALOCAN ];

/**
    Discovery setup.
*/
extern CcPieceEnum const CC_SETUP_BOARD_DISCOVERY[ CC_VARIANT_BOARD_SIZE_DISCOVERY ][ CC_VARIANT_BOARD_SIZE_DISCOVERY ];

/**
    One setup.
*/
extern CcPieceEnum const CC_SETUP_BOARD_ONE[ CC_VARIANT_BOARD_SIZE_ONE ][ CC_VARIANT_BOARD_SIZE_ONE ];


/**
    Function returning setup for a board, based on given variant.

    @param ve A variant.

    @return Pointer to setup if successful, `NULL` otherwise.
*/
CcPieceEnum const * cc_setup_board_get( CcVariantEnum ve );

/**
    Function checks if piece is present in an initial setup of a chessboard.

    @param ve A variant.
    @param pe A piece.

    @return `true` if piece is found in an initial setup, `false` otherwise.
*/
bool cc_setup_board_has_piece( CcVariantEnum ve, CcPieceEnum pe );

/**
    Function returns file of a figure in an initial setup of a chessboard.

    @param ve A variant.
    @param pe A figure.
    @param search_left_first Flag, whether to search left (Queen-side),
                             or right (King-side) first.

    @note
    Function returns valid results only for figures in first (or last) row in an
    initial setup of chessboard. Pawns and Monoliths are not covered, they'll
    always return invalid value (`CC_INVALID_OFF_BOARD_COORD_MIN`).

    @return File if figure is found in a first/last row of initial setup,
            `CC_INVALID_OFF_BOARD_COORD_MIN` otherwise.
*/
int cc_setup_board_get_figure_row_initial_file( CcVariantEnum ve,
                                                CcPieceEnum pe,
                                                bool search_left_first );


#endif /* __CC_SETUP_BOARD_H__ */
