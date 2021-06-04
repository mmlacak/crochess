// Copyright (c) 2021 Mario Mlačak, mmlacak@gmail.com
// Licensed under 3-clause (modified) BSD license. See LICENSE for details.

#include <stdbool.h>
#include <stdlib.h>
#include <string.h>

#include "cc_str_utils.h"
#include "board_type.h"


char const * const BOARD_TYPE_CLASSICAL_CHESS_SYMBOL = "cc";
char const * const BOARD_TYPE_CROATIAN_TIES_SYMBOL = "ct";
char const * const BOARD_TYPE_MAYAN_ASCENDANCY_SYMBOL = "ma";
char const * const BOARD_TYPE_AGE_OF_AQUARIUS_SYMBOL = "aoa";
char const * const BOARD_TYPE_MIRANDAS_VEIL_SYMBOL = "mv";
char const * const BOARD_TYPE_NINETEEN_SYMBOL = "n";
char const * const BOARD_TYPE_HEMERAS_DAWN_SYMBOL = "hd";
char const * const BOARD_TYPE_TAMOANCHAN_REVISITED_SYMBOL = "tr";
char const * const BOARD_TYPE_CONQUEST_OF_TLALOCAN_SYMBOL = "cot";
char const * const BOARD_TYPE_DISCOVERY_SYMBOL = "d";
char const * const BOARD_TYPE_ONE_SYMBOL = "o";

char const * const BOARD_TYPE_SYMBOLS[] =
{
    BOARD_TYPE_CLASSICAL_CHESS_SYMBOL,
    BOARD_TYPE_CROATIAN_TIES_SYMBOL,
    BOARD_TYPE_MAYAN_ASCENDANCY_SYMBOL,
    BOARD_TYPE_AGE_OF_AQUARIUS_SYMBOL,
    BOARD_TYPE_MIRANDAS_VEIL_SYMBOL,
    BOARD_TYPE_NINETEEN_SYMBOL,
    BOARD_TYPE_HEMERAS_DAWN_SYMBOL,
    BOARD_TYPE_TAMOANCHAN_REVISITED_SYMBOL,
    BOARD_TYPE_CONQUEST_OF_TLALOCAN_SYMBOL,
    BOARD_TYPE_DISCOVERY_SYMBOL,
    BOARD_TYPE_ONE_SYMBOL,
};


bool bt_is_code( char const * const restrict code )
{
    if ( !code ) return false;

    char * lc = cc_str_to_case_new(code, true);
    if ( !lc ) return false;

    int const count = sizeof( BOARD_TYPE_SYMBOLS ) / sizeof( BOARD_TYPE_SYMBOLS[ 0 ] );
    for ( int i = 0; i < count; ++i )
    {
        char const * const sym = BOARD_TYPE_SYMBOLS[ i ];

        if ( !strcmp( sym, lc ) )
        {
            free( lc );
            return true;
        }
    }

    free( lc );
    return false;
}

BoardType bt_from_str(char const * const restrict code)
{
    BoardType bt = BT_One;
    if ( !code ) return bt;

    char * lc = cc_str_to_case_new(code, true);
    if ( !lc ) return bt;

    if ( !strcmp(lc, BOARD_TYPE_CLASSICAL_CHESS_SYMBOL) ) bt = BT_ClassicalChess;
    else if ( !strcmp(lc, BOARD_TYPE_CROATIAN_TIES_SYMBOL) ) bt = BT_CroatianTies;
    else if ( !strcmp(lc, BOARD_TYPE_MAYAN_ASCENDANCY_SYMBOL) ) bt = BT_MayanAscendancy;
    else if ( !strcmp(lc, BOARD_TYPE_AGE_OF_AQUARIUS_SYMBOL) ) bt = BT_AgeOfAquarius;
    else if ( !strcmp(lc, BOARD_TYPE_MIRANDAS_VEIL_SYMBOL) ) bt = BT_MirandasVeil;
    else if ( !strcmp(lc, BOARD_TYPE_NINETEEN_SYMBOL) ) bt = BT_Nineteen;
    else if ( !strcmp(lc, BOARD_TYPE_HEMERAS_DAWN_SYMBOL) ) bt = BT_HemerasDawn;
    else if ( !strcmp(lc, BOARD_TYPE_TAMOANCHAN_REVISITED_SYMBOL) ) bt = BT_TamoanchanRevisited;
    else if ( !strcmp(lc, BOARD_TYPE_CONQUEST_OF_TLALOCAN_SYMBOL) ) bt = BT_ConquestOfTlalocan;
    else if ( !strcmp(lc, BOARD_TYPE_DISCOVERY_SYMBOL) ) bt = BT_Discovery;
    // else if ( !strcmp(lc, BOARD_TYPE_ONE_SYMBOL) ) bt = BT_One;
    // else bt = BT_One;

    free( lc );

    return bt;
}

char const * bt_label(BoardType const bt)
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
