// Copyright (c) 2021 Mario Mlačak, mmlacak@gmail.com
// Licensed under 3-clause (modified) BSD license. See LICENSE for details.

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

    @param ve Variant.

    @return Pointer to setup if successful, `NULL` otherwise.
*/
CcPieceEnum const * cc_board_setup_get( CcVariantEnum const ve );


#endif /* __CC_SETUP_BOARD_H__ */
