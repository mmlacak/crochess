// Copyright (c) 2021 Mario Mlačak, mmlacak@gmail.com
// Licensed under 3-clause (modified) BSD license. See LICENSE for details.

#include <stdlib.h>

#include "tag_type.h"
#include "board_type.h"
#include "setup_tags.h"


static const int n = TT_None;
static const int R = TT_CanRush;
static const int C = TT_CanCastle;
static const int P = TT_DelayedPromotion;


TagType const SETUP_TAGS_CLASSICAL_CHESS[ BOARD_SIZE_CLASSICAL_CHESS ][ BOARD_SIZE_CLASSICAL_CHESS ] =
{
    { C, n, n, n, C, n, n, C },
    { R, R, R, R, R, R, R, R },
    { n, n, n, n, n, n, n, n },
    { n, n, n, n, n, n, n, n },
    { n, n, n, n, n, n, n, n },
    { n, n, n, n, n, n, n, n },
    { R, R, R, R, R, R, R, R },
    { C, n, n, n, C, n, n, C },
};

TagType const SETUP_TAGS_CROATIAN_TIES[ BOARD_SIZE_CROATIAN_TIES ][ BOARD_SIZE_CROATIAN_TIES ] =
{
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

TagType const SETUP_TAGS_MAYAN_ASCENDANCY[ BOARD_SIZE_MAYAN_ASCENDANCY ][ BOARD_SIZE_MAYAN_ASCENDANCY ] =
{
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

TagType const SETUP_TAGS_AGE_OF_AQUARIUS[ BOARD_SIZE_AGE_OF_AQUARIUS ][ BOARD_SIZE_AGE_OF_AQUARIUS ] =
{
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

TagType const SETUP_TAGS_MIRANDAS_VEIL[ BOARD_SIZE_MIRANDAS_VEIL ][ BOARD_SIZE_MIRANDAS_VEIL ] =
{
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

TagType const SETUP_TAGS_NINETEEN[ BOARD_SIZE_NINETEEN ][ BOARD_SIZE_NINETEEN ] =
{
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

TagType const SETUP_TAGS_HEMERAS_DAWN[ BOARD_SIZE_HEMERAS_DAWN ][ BOARD_SIZE_HEMERAS_DAWN ] =
{
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
    { n, n, n, P, n, P, n, n, n, n, n, n, n, n, P, n, P, n, n, n },
    { n, n, P, n, n, n, P, n, n, n, n, n, n, P, n, n, n, P, n, n },
    { P, P, P, P, P, P, P, P, P, P, P, P, P, P, P, P, P, P, P, P },
    { P, P, P, P, P, P, P, P, P, P, P, P, P, P, P, P, P, P, P, P },
    { n, C, n, n, n, n, n, n, n, n, C, n, n, n, n, n, n, n, C, n },
};

TagType const SETUP_TAGS_TAMOANCHAN_REVISITED[ BOARD_SIZE_TAMOANCHAN_REVISITED ][ BOARD_SIZE_TAMOANCHAN_REVISITED ] =
{
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

TagType const SETUP_TAGS_CONQUEST_OF_TLALOCAN[ BOARD_SIZE_CONQUEST_OF_TLALOCAN ][ BOARD_SIZE_CONQUEST_OF_TLALOCAN ] =
{
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

TagType const SETUP_TAGS_DISCOVERY[ BOARD_SIZE_DISCOVERY ][ BOARD_SIZE_DISCOVERY ] =
{
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

TagType const SETUP_TAGS_ONE[ BOARD_SIZE_ONE ][ BOARD_SIZE_ONE ] =
{
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


TagType const * get_tags_setup( BoardType const bt )
{
    switch ( bt )
    {
        case BT_ClassicalChess : return (TagType const *)SETUP_TAGS_CLASSICAL_CHESS;
        case BT_CroatianTies : return (TagType const *)SETUP_TAGS_CROATIAN_TIES;
        case BT_MayanAscendancy : return (TagType const *)SETUP_TAGS_MAYAN_ASCENDANCY;
        case BT_AgeOfAquarius : return (TagType const *)SETUP_TAGS_AGE_OF_AQUARIUS;
        case BT_MirandasVeil : return (TagType const *)SETUP_TAGS_MIRANDAS_VEIL;
        case BT_Nineteen : return (TagType const *)SETUP_TAGS_NINETEEN;
        case BT_HemerasDawn : return (TagType const *)SETUP_TAGS_HEMERAS_DAWN;
        case BT_TamoanchanRevisited : return (TagType const *)SETUP_TAGS_TAMOANCHAN_REVISITED;
        case BT_ConquestOfTlalocan : return (TagType const *)SETUP_TAGS_CONQUEST_OF_TLALOCAN;
        case BT_Discovery : return (TagType const *)SETUP_TAGS_DISCOVERY;
        case BT_One : return (TagType const *)SETUP_TAGS_ONE;

        default : return NULL;
    }
}
