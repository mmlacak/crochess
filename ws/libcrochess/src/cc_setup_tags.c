// Copyright (c) 2021 Mario Mlačak, mmlacak@gmail.com
// Licensed under GNU GPL v3+ license. See LICENSING, COPYING files for details.

#include <stdlib.h>

#include "cc_tag.h"
#include "cc_variant.h"
#include "cc_setup_tags.h"

/**
    @file cc_setup_tags.c
    @brief Tag setups data arrays, and related functions.
*/


static const int n = CC_TE_None;
static const int R = CC_TE_CanRush;
static const int C = CC_TE_CanCastle;
// static const int P = CC_TE_DelayedPromotion; // Not used, so to silence [-Wunused-const-variable] ...


CcTagEnum const CC_SETUP_TAGS_CLASSICAL_CHESS[ CC_VARIANT_BOARD_SIZE_CLASSICAL_CHESS ][ CC_VARIANT_BOARD_SIZE_CLASSICAL_CHESS ] = {
    { C, n, n, n, C, n, n, C },
    { R, R, R, R, R, R, R, R },
    { n, n, n, n, n, n, n, n },
    { n, n, n, n, n, n, n, n },
    { n, n, n, n, n, n, n, n },
    { n, n, n, n, n, n, n, n },
    { R, R, R, R, R, R, R, R },
    { C, n, n, n, C, n, n, C },
};

CcTagEnum const CC_SETUP_TAGS_CROATIAN_TIES[ CC_VARIANT_BOARD_SIZE_CROATIAN_TIES ][ CC_VARIANT_BOARD_SIZE_CROATIAN_TIES ] = {
    { C, n, n, n, n, C, n, n, n, C },
    { R, R, R, R, R, R, R, R, R, R },
    { n, n, n, n, n, n, n, n, n, n },
    { n, n, n, n, n, n, n, n, n, n },
    { n, n, n, n, n, n, n, n, n, n },
    { n, n, n, n, n, n, n, n, n, n },
    { n, n, n, n, n, n, n, n, n, n },
    { n, n, n, n, n, n, n, n, n, n },
    { R, R, R, R, R, R, R, R, R, R },
    { C, n, n, n, n, C, n, n, n, C },
};

CcTagEnum const CC_SETUP_TAGS_MAYAN_ASCENDANCY[ CC_VARIANT_BOARD_SIZE_MAYAN_ASCENDANCY ][ CC_VARIANT_BOARD_SIZE_MAYAN_ASCENDANCY ] = {
    { C, n, n, n, n, n, C, n, n, n, n, C },
    { R, R, R, R, R, R, R, R, R, R, R, R },
    { n, n, n, n, n, n, n, n, n, n, n, n },
    { n, n, n, n, n, n, n, n, n, n, n, n },
    { n, n, n, n, n, n, n, n, n, n, n, n },
    { n, n, n, n, n, n, n, n, n, n, n, n },
    { n, n, n, n, n, n, n, n, n, n, n, n },
    { n, n, n, n, n, n, n, n, n, n, n, n },
    { n, n, n, n, n, n, n, n, n, n, n, n },
    { n, n, n, n, n, n, n, n, n, n, n, n },
    { R, R, R, R, R, R, R, R, R, R, R, R },
    { C, n, n, n, n, n, C, n, n, n, n, C },
};

CcTagEnum const CC_SETUP_TAGS_AGE_OF_AQUARIUS[ CC_VARIANT_BOARD_SIZE_AGE_OF_AQUARIUS ][ CC_VARIANT_BOARD_SIZE_AGE_OF_AQUARIUS ] = {
    { C, n, n, n, n, n, n, C, n, n, n, n, n, C },
    { R, R, R, R, R, R, R, R, R, R, R, R, R, R },
    { n, n, n, n, n, n, n, n, n, n, n, n, n, n },
    { n, n, n, n, n, n, n, n, n, n, n, n, n, n },
    { n, n, n, n, n, n, n, n, n, n, n, n, n, n },
    { n, n, n, n, n, n, n, n, n, n, n, n, n, n },
    { n, n, n, n, n, n, n, n, n, n, n, n, n, n },
    { n, n, n, n, n, n, n, n, n, n, n, n, n, n },
    { n, n, n, n, n, n, n, n, n, n, n, n, n, n },
    { n, n, n, n, n, n, n, n, n, n, n, n, n, n },
    { n, n, n, n, n, n, n, n, n, n, n, n, n, n },
    { n, n, n, n, n, n, n, n, n, n, n, n, n, n },
    { R, R, R, R, R, R, R, R, R, R, R, R, R, R },
    { C, n, n, n, n, n, n, C, n, n, n, n, n, C },
};

CcTagEnum const CC_SETUP_TAGS_MIRANDAS_VEIL[ CC_VARIANT_BOARD_SIZE_MIRANDAS_VEIL ][ CC_VARIANT_BOARD_SIZE_MIRANDAS_VEIL ] = {
    { C, n, n, n, n, n, n, n, C, n, n, n, n, n, n, C },
    { R, R, R, R, R, R, R, R, R, R, R, R, R, R, R, R },
    { n, n, n, n, n, n, n, n, n, n, n, n, n, n, n, n },
    { n, n, n, n, n, n, n, n, n, n, n, n, n, n, n, n },
    { n, n, n, n, n, n, n, n, n, n, n, n, n, n, n, n },
    { n, n, n, n, n, n, n, n, n, n, n, n, n, n, n, n },
    { n, n, n, n, n, n, n, n, n, n, n, n, n, n, n, n },
    { n, n, n, n, n, n, n, n, n, n, n, n, n, n, n, n },
    { n, n, n, n, n, n, n, n, n, n, n, n, n, n, n, n },
    { n, n, n, n, n, n, n, n, n, n, n, n, n, n, n, n },
    { n, n, n, n, n, n, n, n, n, n, n, n, n, n, n, n },
    { n, n, n, n, n, n, n, n, n, n, n, n, n, n, n, n },
    { n, n, n, n, n, n, n, n, n, n, n, n, n, n, n, n },
    { n, n, n, n, n, n, n, n, n, n, n, n, n, n, n, n },
    { R, R, R, R, R, R, R, R, R, R, R, R, R, R, R, R },
    { C, n, n, n, n, n, n, n, C, n, n, n, n, n, n, C },
};

CcTagEnum const CC_SETUP_TAGS_NINETEEN[ CC_VARIANT_BOARD_SIZE_NINETEEN ][ CC_VARIANT_BOARD_SIZE_NINETEEN ] = {
    { n, C, n, n, n, n, n, n, n, C, n, n, n, n, n, n, C, n },
    { R, R, R, R, R, R, R, R, R, R, R, R, R, R, R, R, R, R },
    { R, R, R, R, R, R, R, R, R, R, R, R, R, R, R, R, R, R },
    { n, n, n, n, n, n, n, n, n, n, n, n, n, n, n, n, n, n },
    { n, n, n, n, n, n, n, n, n, n, n, n, n, n, n, n, n, n },
    { n, n, n, n, n, n, n, n, n, n, n, n, n, n, n, n, n, n },
    { n, n, n, n, n, n, n, n, n, n, n, n, n, n, n, n, n, n },
    { n, n, n, n, n, n, n, n, n, n, n, n, n, n, n, n, n, n },
    { n, n, n, n, n, n, n, n, n, n, n, n, n, n, n, n, n, n },
    { n, n, n, n, n, n, n, n, n, n, n, n, n, n, n, n, n, n },
    { n, n, n, n, n, n, n, n, n, n, n, n, n, n, n, n, n, n },
    { n, n, n, n, n, n, n, n, n, n, n, n, n, n, n, n, n, n },
    { n, n, n, n, n, n, n, n, n, n, n, n, n, n, n, n, n, n },
    { n, n, n, n, n, n, n, n, n, n, n, n, n, n, n, n, n, n },
    { n, n, n, n, n, n, n, n, n, n, n, n, n, n, n, n, n, n },
    { R, R, R, R, R, R, R, R, R, R, R, R, R, R, R, R, R, R },
    { R, R, R, R, R, R, R, R, R, R, R, R, R, R, R, R, R, R },
    { n, C, n, n, n, n, n, n, n, C, n, n, n, n, n, n, C, n },
};

CcTagEnum const CC_SETUP_TAGS_HEMERAS_DAWN[ CC_VARIANT_BOARD_SIZE_HEMERAS_DAWN ][ CC_VARIANT_BOARD_SIZE_HEMERAS_DAWN ] = {
    { n, C, n, n, n, n, n, n, n, n, C, n, n, n, n, n, n, n, C, n },
    { R, R, R, R, R, R, R, R, R, R, R, R, R, R, R, R, R, R, R, R },
    { R, R, R, R, R, R, R, R, R, R, R, R, R, R, R, R, R, R, R, R },
    { n, n, R, n, n, n, R, n, n, n, n, n, n, R, n, n, n, R, n, n },
    { n, n, n, R, n, R, n, n, n, n, n, n, n, n, R, n, R, n, n, n },
    { n, n, n, n, n, n, n, n, n, n, n, n, n, n, n, n, n, n, n, n },
    { n, n, n, n, n, n, n, n, n, n, n, n, n, n, n, n, n, n, n, n },
    { n, n, n, n, n, n, n, n, n, n, n, n, n, n, n, n, n, n, n, n },
    { n, n, n, n, n, n, n, n, n, n, n, n, n, n, n, n, n, n, n, n },
    { n, n, n, n, n, n, n, n, n, n, n, n, n, n, n, n, n, n, n, n },
    { n, n, n, n, n, n, n, n, n, n, n, n, n, n, n, n, n, n, n, n },
    { n, n, n, n, n, n, n, n, n, n, n, n, n, n, n, n, n, n, n, n },
    { n, n, n, n, n, n, n, n, n, n, n, n, n, n, n, n, n, n, n, n },
    { n, n, n, n, n, n, n, n, n, n, n, n, n, n, n, n, n, n, n, n },
    { n, n, n, n, n, n, n, n, n, n, n, n, n, n, n, n, n, n, n, n },
    { n, n, n, R, n, R, n, n, n, n, n, n, n, n, R, n, R, n, n, n },
    { n, n, R, n, n, n, R, n, n, n, n, n, n, R, n, n, n, R, n, n },
    { R, R, R, R, R, R, R, R, R, R, R, R, R, R, R, R, R, R, R, R },
    { R, R, R, R, R, R, R, R, R, R, R, R, R, R, R, R, R, R, R, R },
    { n, C, n, n, n, n, n, n, n, n, C, n, n, n, n, n, n, n, C, n },
};

CcTagEnum const CC_SETUP_TAGS_TAMOANCHAN_REVISITED[ CC_VARIANT_BOARD_SIZE_TAMOANCHAN_REVISITED ][ CC_VARIANT_BOARD_SIZE_TAMOANCHAN_REVISITED ] = {
    { n, C, n, n, n, n, n, n, n, n, n, C, n, n, n, n, n, n, n, n, C, n },
    { R, R, R, R, R, R, R, R, R, R, R, R, R, R, R, R, R, R, R, R, R, R },
    { R, R, R, R, R, R, R, R, R, R, R, R, R, R, R, R, R, R, R, R, R, R },
    { n, n, n, n, n, n, R, n, n, n, R, R, n, n, n, R, n, n, n, n, n, n },
    { n, n, n, n, n, n, n, R, n, R, n, n, R, n, R, n, n, n, n, n, n, n },
    { n, n, n, n, n, n, n, n, n, n, n, n, n, n, n, n, n, n, n, n, n, n },
    { n, n, n, n, n, n, n, n, n, n, n, n, n, n, n, n, n, n, n, n, n, n },
    { n, n, n, n, n, n, n, n, n, n, n, n, n, n, n, n, n, n, n, n, n, n },
    { n, n, n, n, n, n, n, n, n, n, n, n, n, n, n, n, n, n, n, n, n, n },
    { n, n, n, n, n, n, n, n, n, n, n, n, n, n, n, n, n, n, n, n, n, n },
    { n, n, n, n, n, n, n, n, n, n, n, n, n, n, n, n, n, n, n, n, n, n },
    { n, n, n, n, n, n, n, n, n, n, n, n, n, n, n, n, n, n, n, n, n, n },
    { n, n, n, n, n, n, n, n, n, n, n, n, n, n, n, n, n, n, n, n, n, n },
    { n, n, n, n, n, n, n, n, n, n, n, n, n, n, n, n, n, n, n, n, n, n },
    { n, n, n, n, n, n, n, n, n, n, n, n, n, n, n, n, n, n, n, n, n, n },
    { n, n, n, n, n, n, n, n, n, n, n, n, n, n, n, n, n, n, n, n, n, n },
    { n, n, n, n, n, n, n, n, n, n, n, n, n, n, n, n, n, n, n, n, n, n },
    { n, n, n, n, n, n, n, R, n, R, n, n, R, n, R, n, n, n, n, n, n, n },
    { n, n, n, n, n, n, R, n, n, n, R, R, n, n, n, R, n, n, n, n, n, n },
    { R, R, R, R, R, R, R, R, R, R, R, R, R, R, R, R, R, R, R, R, R, R },
    { R, R, R, R, R, R, R, R, R, R, R, R, R, R, R, R, R, R, R, R, R, R },
    { n, C, n, n, n, n, n, n, n, n, n, C, n, n, n, n, n, n, n, n, C, n },
};

CcTagEnum const CC_SETUP_TAGS_CONQUEST_OF_TLALOCAN[ CC_VARIANT_BOARD_SIZE_CONQUEST_OF_TLALOCAN ][ CC_VARIANT_BOARD_SIZE_CONQUEST_OF_TLALOCAN ] = {
    { n, C, n, n, n, n, n, n, n, n, n, n, C, n, n, n, n, n, n, n, n, n, C, n },
    { R, R, R, R, R, R, R, R, R, R, R, R, R, R, R, R, R, R, R, R, R, R, R, R },
    { R, R, R, R, R, R, R, R, R, R, R, R, R, R, R, R, R, R, R, R, R, R, R, R },
    { n, n, n, R, n, n, n, R, R, n, n, R, R, n, n, R, R, n, n, n, R, n, n, n },
    { n, n, n, n, R, n, R, n, n, R, n, R, R, n, R, n, n, R, n, R, n, n, n, n },
    { n, n, n, n, n, n, n, n, n, n, n, n, n, n, n, n, n, n, n, n, n, n, n, n },
    { n, n, n, n, n, n, n, n, n, n, n, n, n, n, n, n, n, n, n, n, n, n, n, n },
    { n, n, n, n, n, n, n, n, n, n, n, n, n, n, n, n, n, n, n, n, n, n, n, n },
    { n, n, n, n, n, n, n, n, n, n, n, n, n, n, n, n, n, n, n, n, n, n, n, n },
    { n, n, n, n, n, n, n, n, n, n, n, n, n, n, n, n, n, n, n, n, n, n, n, n },
    { n, n, n, n, n, n, n, n, n, n, n, n, n, n, n, n, n, n, n, n, n, n, n, n },
    { n, n, n, n, n, n, n, n, n, n, n, n, n, n, n, n, n, n, n, n, n, n, n, n },
    { n, n, n, n, n, n, n, n, n, n, n, n, n, n, n, n, n, n, n, n, n, n, n, n },
    { n, n, n, n, n, n, n, n, n, n, n, n, n, n, n, n, n, n, n, n, n, n, n, n },
    { n, n, n, n, n, n, n, n, n, n, n, n, n, n, n, n, n, n, n, n, n, n, n, n },
    { n, n, n, n, n, n, n, n, n, n, n, n, n, n, n, n, n, n, n, n, n, n, n, n },
    { n, n, n, n, n, n, n, n, n, n, n, n, n, n, n, n, n, n, n, n, n, n, n, n },
    { n, n, n, n, n, n, n, n, n, n, n, n, n, n, n, n, n, n, n, n, n, n, n, n },
    { n, n, n, n, n, n, n, n, n, n, n, n, n, n, n, n, n, n, n, n, n, n, n, n },
    { n, n, n, n, R, n, R, n, n, R, n, R, R, n, R, n, n, R, n, R, n, n, n, n },
    { n, n, n, R, n, n, n, R, R, n, n, R, R, n, n, R, R, n, n, n, R, n, n, n },
    { R, R, R, R, R, R, R, R, R, R, R, R, R, R, R, R, R, R, R, R, R, R, R, R },
    { R, R, R, R, R, R, R, R, R, R, R, R, R, R, R, R, R, R, R, R, R, R, R, R },
    { n, C, n, n, n, n, n, n, n, n, n, n, C, n, n, n, n, n, n, n, n, n, C, n },
};

CcTagEnum const CC_SETUP_TAGS_DISCOVERY[ CC_VARIANT_BOARD_SIZE_DISCOVERY ][ CC_VARIANT_BOARD_SIZE_DISCOVERY ] = {
    { n, C, n, n, n, n, n, n, n, n, n, n, C, n, n, n, n, n, n, n, n, n, C, n },
    { R, R, R, R, R, R, R, R, R, R, R, R, R, R, R, R, R, R, R, R, R, R, R, R },
    { R, R, R, R, R, R, R, R, R, R, R, R, R, R, R, R, R, R, R, R, R, R, R, R },
    { n, n, n, R, n, n, n, R, R, n, n, R, R, n, n, R, R, n, n, n, R, n, n, n },
    { n, n, n, n, R, n, R, n, n, R, n, R, R, n, R, n, n, R, n, R, n, n, n, n },
    { n, n, n, n, n, n, n, n, n, n, n, n, n, n, n, n, n, n, n, n, n, n, n, n },
    { n, n, n, n, n, n, n, n, n, n, n, n, n, n, n, n, n, n, n, n, n, n, n, n },
    { n, n, n, n, n, n, n, n, n, n, n, n, n, n, n, n, n, n, n, n, n, n, n, n },
    { n, n, n, n, n, n, n, n, n, n, n, n, n, n, n, n, n, n, n, n, n, n, n, n },
    { n, n, n, n, n, n, n, n, n, n, n, n, n, n, n, n, n, n, n, n, n, n, n, n },
    { n, n, n, n, n, n, n, n, n, n, n, n, n, n, n, n, n, n, n, n, n, n, n, n },
    { n, n, n, n, n, n, n, n, n, n, n, n, n, n, n, n, n, n, n, n, n, n, n, n },
    { n, n, n, n, n, n, n, n, n, n, n, n, n, n, n, n, n, n, n, n, n, n, n, n },
    { n, n, n, n, n, n, n, n, n, n, n, n, n, n, n, n, n, n, n, n, n, n, n, n },
    { n, n, n, n, n, n, n, n, n, n, n, n, n, n, n, n, n, n, n, n, n, n, n, n },
    { n, n, n, n, n, n, n, n, n, n, n, n, n, n, n, n, n, n, n, n, n, n, n, n },
    { n, n, n, n, n, n, n, n, n, n, n, n, n, n, n, n, n, n, n, n, n, n, n, n },
    { n, n, n, n, n, n, n, n, n, n, n, n, n, n, n, n, n, n, n, n, n, n, n, n },
    { n, n, n, n, n, n, n, n, n, n, n, n, n, n, n, n, n, n, n, n, n, n, n, n },
    { n, n, n, n, R, n, R, n, n, R, n, R, R, n, R, n, n, R, n, R, n, n, n, n },
    { n, n, n, R, n, n, n, R, R, n, n, R, R, n, n, R, R, n, n, n, R, n, n, n },
    { R, R, R, R, R, R, R, R, R, R, R, R, R, R, R, R, R, R, R, R, R, R, R, R },
    { R, R, R, R, R, R, R, R, R, R, R, R, R, R, R, R, R, R, R, R, R, R, R, R },
    { n, C, n, n, n, n, n, n, n, n, n, n, C, n, n, n, n, n, n, n, n, n, C, n },
};

CcTagEnum const CC_SETUP_TAGS_ONE[ CC_VARIANT_BOARD_SIZE_ONE ][ CC_VARIANT_BOARD_SIZE_ONE ] = {
    { n, C, n, n, n, n, n, n, n, n, n, n, n, C, n, n, n, n, n, n, n, n, n, n, C, n },
    { R, R, R, R, R, R, R, R, R, R, R, R, R, R, R, R, R, R, R, R, R, R, R, R, R, R },
    { R, R, R, R, R, R, R, R, R, R, R, R, R, R, R, R, R, R, R, R, R, R, R, R, R, R },
    { n, n, n, n, R, n, n, n, R, R, n, n, R, R, n, n, R, R, n, n, n, R, n, n, n, n },
    { n, n, n, n, n, R, n, R, n, n, R, n, R, R, n, R, n, n, R, n, R, n, n, n, n, n },
    { n, n, n, n, n, n, n, n, n, n, n, n, n, n, n, n, n, n, n, n, n, n, n, n, n, n },
    { n, n, n, n, n, n, n, n, n, n, n, n, n, n, n, n, n, n, n, n, n, n, n, n, n, n },
    { n, n, n, n, n, n, n, n, n, n, n, n, n, n, n, n, n, n, n, n, n, n, n, n, n, n },
    { n, n, n, n, n, n, n, n, n, n, n, n, n, n, n, n, n, n, n, n, n, n, n, n, n, n },
    { n, n, n, n, n, n, n, n, n, n, n, n, n, n, n, n, n, n, n, n, n, n, n, n, n, n },
    { n, n, n, n, n, n, n, n, n, n, n, n, n, n, n, n, n, n, n, n, n, n, n, n, n, n },
    { n, n, n, n, n, n, n, n, n, n, n, n, n, n, n, n, n, n, n, n, n, n, n, n, n, n },
    { n, n, n, n, n, n, n, n, n, n, n, n, n, n, n, n, n, n, n, n, n, n, n, n, n, n },
    { n, n, n, n, n, n, n, n, n, n, n, n, n, n, n, n, n, n, n, n, n, n, n, n, n, n },
    { n, n, n, n, n, n, n, n, n, n, n, n, n, n, n, n, n, n, n, n, n, n, n, n, n, n },
    { n, n, n, n, n, n, n, n, n, n, n, n, n, n, n, n, n, n, n, n, n, n, n, n, n, n },
    { n, n, n, n, n, n, n, n, n, n, n, n, n, n, n, n, n, n, n, n, n, n, n, n, n, n },
    { n, n, n, n, n, n, n, n, n, n, n, n, n, n, n, n, n, n, n, n, n, n, n, n, n, n },
    { n, n, n, n, n, n, n, n, n, n, n, n, n, n, n, n, n, n, n, n, n, n, n, n, n, n },
    { n, n, n, n, n, n, n, n, n, n, n, n, n, n, n, n, n, n, n, n, n, n, n, n, n, n },
    { n, n, n, n, n, n, n, n, n, n, n, n, n, n, n, n, n, n, n, n, n, n, n, n, n, n },
    { n, n, n, n, n, R, n, R, n, n, R, n, R, R, n, R, n, n, R, n, R, n, n, n, n, n },
    { n, n, n, n, R, n, n, n, R, R, n, n, R, R, n, n, R, R, n, n, n, R, n, n, n, n },
    { R, R, R, R, R, R, R, R, R, R, R, R, R, R, R, R, R, R, R, R, R, R, R, R, R, R },
    { R, R, R, R, R, R, R, R, R, R, R, R, R, R, R, R, R, R, R, R, R, R, R, R, R, R },
    { n, C, n, n, n, n, n, n, n, n, n, n, n, C, n, n, n, n, n, n, n, n, n, n, C, n },
};


CcTagEnum const * cc_setup_tags_get( CcVariantEnum ve ) {
    switch ( ve ) {
        case CC_VE_ClassicalChess : return (CcTagEnum const *)CC_SETUP_TAGS_CLASSICAL_CHESS;
        case CC_VE_CroatianTies : return (CcTagEnum const *)CC_SETUP_TAGS_CROATIAN_TIES;
        case CC_VE_MayanAscendancy : return (CcTagEnum const *)CC_SETUP_TAGS_MAYAN_ASCENDANCY;
        case CC_VE_AgeOfAquarius : return (CcTagEnum const *)CC_SETUP_TAGS_AGE_OF_AQUARIUS;
        case CC_VE_MirandasVeil : return (CcTagEnum const *)CC_SETUP_TAGS_MIRANDAS_VEIL;
        case CC_VE_Nineteen : return (CcTagEnum const *)CC_SETUP_TAGS_NINETEEN;
        case CC_VE_HemerasDawn : return (CcTagEnum const *)CC_SETUP_TAGS_HEMERAS_DAWN;
        case CC_VE_TamoanchanRevisited : return (CcTagEnum const *)CC_SETUP_TAGS_TAMOANCHAN_REVISITED;
        case CC_VE_ConquestOfTlalocan : return (CcTagEnum const *)CC_SETUP_TAGS_CONQUEST_OF_TLALOCAN;
        case CC_VE_Discovery : return (CcTagEnum const *)CC_SETUP_TAGS_DISCOVERY;
        case CC_VE_One : return (CcTagEnum const *)CC_SETUP_TAGS_ONE;

        default : return NULL;
    }
}
