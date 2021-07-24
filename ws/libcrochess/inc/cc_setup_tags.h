// Copyright (c) 2021 Mario Mlačak, mmlacak@gmail.com
// Licensed under 3-clause (modified) BSD license. See LICENSE for details.

#ifndef __CC_SETUP_TAGS_H__
#define __CC_SETUP_TAGS_H__

#include "cc_tag.h"
#include "cc_variant.h"


/**
    @file cc_setup_tags.h
    @brief Tag constant setups, and related functions.
*/


/**
    Classical Chess initial tags.
*/
extern CcTagEnum const CC_SETUP_TAGS_CLASSICAL_CHESS[ CC_VARIANT_BOARD_SIZE_CLASSICAL_CHESS ][ CC_VARIANT_BOARD_SIZE_CLASSICAL_CHESS ];

/**
    Croatian Ties initial tags.
*/
extern CcTagEnum const CC_SETUP_TAGS_CROATIAN_TIES[ CC_VARIANT_BOARD_SIZE_CROATIAN_TIES ][ CC_VARIANT_BOARD_SIZE_CROATIAN_TIES ];

/**
    Mayan Ascendancy initial tags.
*/
extern CcTagEnum const CC_SETUP_TAGS_MAYAN_ASCENDANCY[ CC_VARIANT_BOARD_SIZE_MAYAN_ASCENDANCY ][ CC_VARIANT_BOARD_SIZE_MAYAN_ASCENDANCY ];

/**
    Age of Aquarius initial tags.
*/
extern CcTagEnum const CC_SETUP_TAGS_AGE_OF_AQUARIUS[ CC_VARIANT_BOARD_SIZE_AGE_OF_AQUARIUS ][ CC_VARIANT_BOARD_SIZE_AGE_OF_AQUARIUS ];

/**
    Miranda's Veil initial tags.
*/
extern CcTagEnum const CC_SETUP_TAGS_MIRANDAS_VEIL[ CC_VARIANT_BOARD_SIZE_MIRANDAS_VEIL ][ CC_VARIANT_BOARD_SIZE_MIRANDAS_VEIL ];

/**
    Nineteen initial tags.
*/
extern CcTagEnum const CC_SETUP_TAGS_NINETEEN[ CC_VARIANT_BOARD_SIZE_NINETEEN ][ CC_VARIANT_BOARD_SIZE_NINETEEN ];

/**
    Hemera's Dawn initial tags.
*/
extern CcTagEnum const CC_SETUP_TAGS_HEMERAS_DAWN[ CC_VARIANT_BOARD_SIZE_HEMERAS_DAWN ][ CC_VARIANT_BOARD_SIZE_HEMERAS_DAWN ];

/**
    Tamoanchan Revisited initial tags.
*/
extern CcTagEnum const CC_SETUP_TAGS_TAMOANCHAN_REVISITED[ CC_VARIANT_BOARD_SIZE_TAMOANCHAN_REVISITED ][ CC_VARIANT_BOARD_SIZE_TAMOANCHAN_REVISITED ];

/**
    Conquest of Tlalocan initial tags.
*/
extern CcTagEnum const CC_SETUP_TAGS_CONQUEST_OF_TLALOCAN[ CC_VARIANT_BOARD_SIZE_CONQUEST_OF_TLALOCAN ][ CC_VARIANT_BOARD_SIZE_CONQUEST_OF_TLALOCAN ];

/**
    Discovery initial tags.
*/
extern CcTagEnum const CC_SETUP_TAGS_DISCOVERY[ CC_VARIANT_BOARD_SIZE_DISCOVERY ][ CC_VARIANT_BOARD_SIZE_DISCOVERY ];

/**
    One initial tags.
*/
extern CcTagEnum const CC_SETUP_TAGS_ONE[ CC_VARIANT_BOARD_SIZE_ONE ][ CC_VARIANT_BOARD_SIZE_ONE ];


/**
    Function returning setup for a tags, based on given variant.

    @param ve Variant.

    @return Pointer to setup if successful, `NULL` otherwise.
*/
CcTagEnum const * cc_tags_setup_get( CcVariantEnum const ve );


#endif /* __CC_SETUP_TAGS_H__ */
