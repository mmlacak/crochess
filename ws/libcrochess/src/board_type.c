// Copyright (c) 2021 Mario Mlačak, mmlacak@gmail.com
// Licensed under 3-clause (modified) BSD license. See LICENSE for details.

#include <stdbool.h>
#include <string.h>

#include "str_utils.h"
#include "board_type.h"


BoardType board_from_str(char const * const code)
{
    char * lc = str_to_case_alloc(code, true);

    if ( !strcmp(lc, "cc") ) return BT_ClassicalChess;
    else if ( !strcmp(lc, "ct") ) return BT_CroatianTies;
    else if ( !strcmp(lc, "ma") ) return BT_MayanAscendancy;
    else if ( !strcmp(lc, "aoa") ) return BT_AgeOfAquarius;
    else if ( !strcmp(lc, "mv") ) return BT_MirandasVeil;
    else if ( !strcmp(lc, "n") ) return BT_Nineteen;
    else if ( !strcmp(lc, "hd") ) return BT_HemerasDawn;
    else if ( !strcmp(lc, "tr") ) return BT_TamoanchanRevisited;
    else if ( !strcmp(lc, "cot") ) return BT_ConquestOfTlalocan;
    else if ( !strcmp(lc, "d") ) return BT_Discovery;
    // else if ( !strcmp(lc, "o") ) return BT_One;
    else return BT_One;
}

char const * const board_label(BoardType const bt)
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

unsigned int board_size(BoardType const bt)
{
    switch ( bt )
    {
        case BT_ClassicalChess : return 8;
        case BT_CroatianTies : return 10;
        case BT_MayanAscendancy : return 12;
        case BT_AgeOfAquarius : return 14;
        case BT_MirandasVeil : return 16;
        case BT_Nineteen : return 18;
        case BT_HemerasDawn : return 20;
        case BT_TamoanchanRevisited : return 22;
        case BT_ConquestOfTlalocan : return 24;
        case BT_Discovery : return 24;
        case BT_One : return 26;

        default : return 0;
    }
}
