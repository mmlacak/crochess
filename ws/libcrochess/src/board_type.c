// Copyright (c) 2021 Mario Mlačak, mmlacak@gmail.com
// Licensed under 3-clause (modified) BSD license. See LICENSE for details.

#include <stdbool.h>
#include <stdlib.h>
#include <string.h>

#include "str_utils.h"
#include "board_type.h"


BoardType bt_from_str(char const * const code)
{
    BoardType bt = BT_One;
    if ( !code ) return bt;

    char * lc = str_to_case_alloc(code, true);
    if ( !lc ) return bt;

    if ( !strcmp(lc, "cc") ) bt = BT_ClassicalChess;
    else if ( !strcmp(lc, "ct") ) bt = BT_CroatianTies;
    else if ( !strcmp(lc, "ma") ) bt = BT_MayanAscendancy;
    else if ( !strcmp(lc, "aoa") ) bt = BT_AgeOfAquarius;
    else if ( !strcmp(lc, "mv") ) bt = BT_MirandasVeil;
    else if ( !strcmp(lc, "n") ) bt = BT_Nineteen;
    else if ( !strcmp(lc, "hd") ) bt = BT_HemerasDawn;
    else if ( !strcmp(lc, "tr") ) bt = BT_TamoanchanRevisited;
    else if ( !strcmp(lc, "cot") ) bt = BT_ConquestOfTlalocan;
    else if ( !strcmp(lc, "d") ) bt = BT_Discovery;
    // else if ( !strcmp(lc, "o") ) bt = BT_One;
    // else bt = BT_One;

    free( lc );

    return bt;
}

char const * const bt_label(BoardType const bt)
{
    switch ( bt )
    {
        case BT_ClassicalChess : return "Classical Chess";
        case BT_CroatianTies : return "Croatian Ties";
        case BT_MayanAscendancy : return "Mayan Ascendancy";
        case BT_AgeOfAquarius : return "Age of Aquarius";
        case BT_MirandasVeil : return "Miranda’s Veil";
        case BT_Nineteen : return "Nineteen";
        case BT_HemerasDawn : return "Hemera’s Dawn";
        case BT_TamoanchanRevisited : return "Tamoanchan Revisited";
        case BT_ConquestOfTlalocan : return "Conquest of Tlalocan";
        case BT_Discovery : return "Discovery";
        case BT_One : return "One";

        default : return "";
    }
}

unsigned int bt_size(BoardType const bt)
{
    switch ( bt )
    {
        case BT_ClassicalChess : return BOARD_SIZE_CLASSICAL_CHESS;
        case BT_CroatianTies : return BOARD_SIZE_CROATIAN_TIES;
        case BT_MayanAscendancy : return BOARD_SIZE_MAYAN_ASCENDANCY;
        case BT_AgeOfAquarius : return BOARD_SIZE_AGE_OF_AQUARIUS;
        case BT_MirandasVeil : return BOARD_SIZE_MIRANDAS_VEIL;
        case BT_Nineteen : return BOARD_SIZE_NINETEEN;
        case BT_HemerasDawn : return BOARD_SIZE_HEMERAS_DAWN;
        case BT_TamoanchanRevisited : return BOARD_SIZE_TAMOANCHAN_REVISITED;
        case BT_ConquestOfTlalocan : return BOARD_SIZE_CONQUEST_OF_TLALOCAN;
        case BT_Discovery : return BOARD_SIZE_DISCOVERY;
        case BT_One : return BOARD_SIZE_ONE;

        default : return 0;
    }
}
