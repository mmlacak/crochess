// Copyright (c) 2021 Mario Mlačak, mmlacak@gmail.com
// Licensed under GNU GPL v3+ license. See LICENSE, COPYING files for details.

#include <stdbool.h>
#include <stdlib.h>
#include <string.h>

#include "cc_str_utils.h"
#include "cc_variant.h"

/**
    @file cc_variant.c
    @brief Variants symbols, and related functions.
*/


char const * const CC_VARIANT_CLASSICAL_CHESS_SYMBOL = "cc";
char const * const CC_VARIANT_CROATIAN_TIES_SYMBOL = "ct";
char const * const CC_VARIANT_MAYAN_ASCENDANCY_SYMBOL = "ma";
char const * const CC_VARIANT_AGE_OF_AQUARIUS_SYMBOL = "aoa";
char const * const CC_VARIANT_MIRANDAS_VEIL_SYMBOL = "mv";
char const * const CC_VARIANT_NINETEEN_SYMBOL = "n";
char const * const CC_VARIANT_HEMERAS_DAWN_SYMBOL = "hd";
char const * const CC_VARIANT_TAMOANCHAN_REVISITED_SYMBOL = "tr";
char const * const CC_VARIANT_CONQUEST_OF_TLALOCAN_SYMBOL = "cot";
char const * const CC_VARIANT_DISCOVERY_SYMBOL = "d";
char const * const CC_VARIANT_ONE_SYMBOL = "o";

char const * const CC_VARIANT_SYMBOLS[] =
{
    CC_VARIANT_CLASSICAL_CHESS_SYMBOL,
    CC_VARIANT_CROATIAN_TIES_SYMBOL,
    CC_VARIANT_MAYAN_ASCENDANCY_SYMBOL,
    CC_VARIANT_AGE_OF_AQUARIUS_SYMBOL,
    CC_VARIANT_MIRANDAS_VEIL_SYMBOL,
    CC_VARIANT_NINETEEN_SYMBOL,
    CC_VARIANT_HEMERAS_DAWN_SYMBOL,
    CC_VARIANT_TAMOANCHAN_REVISITED_SYMBOL,
    CC_VARIANT_CONQUEST_OF_TLALOCAN_SYMBOL,
    CC_VARIANT_DISCOVERY_SYMBOL,
    CC_VARIANT_ONE_SYMBOL,
};


bool cc_variant_str_is_symbol( char const * const restrict str )
{
    if ( !str ) return false;

    char * lc__o = cc_str_to_case_new( str, false );
    if ( !lc__o ) return false;

    int const count = sizeof( CC_VARIANT_SYMBOLS ) / sizeof( CC_VARIANT_SYMBOLS[ 0 ] );
    for ( int i = 0; i < count; ++i )
    {
        char const * const sym = CC_VARIANT_SYMBOLS[ i ];

        if ( !strcmp( sym, lc__o ) )
        {
            free( lc__o );
            return true;
        }
    }

    free( lc__o );
    return false;
}

CcVariantEnum cc_variant_from_symbol( char const * const restrict str )
{
    CcVariantEnum ve = CC_VE_One;
    if ( !str ) return ve;

    char * lc__o = cc_str_to_case_new( str, false );
    if ( !lc__o ) return ve;

    if ( !strcmp(lc__o, CC_VARIANT_CLASSICAL_CHESS_SYMBOL) ) ve = CC_VE_ClassicalChess;
    else if ( !strcmp(lc__o, CC_VARIANT_CROATIAN_TIES_SYMBOL) ) ve = CC_VE_CroatianTies;
    else if ( !strcmp(lc__o, CC_VARIANT_MAYAN_ASCENDANCY_SYMBOL) ) ve = CC_VE_MayanAscendancy;
    else if ( !strcmp(lc__o, CC_VARIANT_AGE_OF_AQUARIUS_SYMBOL) ) ve = CC_VE_AgeOfAquarius;
    else if ( !strcmp(lc__o, CC_VARIANT_MIRANDAS_VEIL_SYMBOL) ) ve = CC_VE_MirandasVeil;
    else if ( !strcmp(lc__o, CC_VARIANT_NINETEEN_SYMBOL) ) ve = CC_VE_Nineteen;
    else if ( !strcmp(lc__o, CC_VARIANT_HEMERAS_DAWN_SYMBOL) ) ve = CC_VE_HemerasDawn;
    else if ( !strcmp(lc__o, CC_VARIANT_TAMOANCHAN_REVISITED_SYMBOL) ) ve = CC_VE_TamoanchanRevisited;
    else if ( !strcmp(lc__o, CC_VARIANT_CONQUEST_OF_TLALOCAN_SYMBOL) ) ve = CC_VE_ConquestOfTlalocan;
    else if ( !strcmp(lc__o, CC_VARIANT_DISCOVERY_SYMBOL) ) ve = CC_VE_Discovery;
    //
    // <.> Not needed, CC_VE_One is default.
    //
    // else if ( !strcmp(lc__o, CC_VARIANT_ONE_SYMBOL) ) ve = CC_VE_One;
    // else ve = CC_VE_One;

    free( lc__o );
    return ve;
}

char const * cc_variant_label( CcVariantEnum const ve )
{
    switch ( ve )
    {
        case CC_VE_ClassicalChess : return "Classical Chess";
        case CC_VE_CroatianTies : return "Croatian Ties";
        case CC_VE_MayanAscendancy : return "Mayan Ascendancy";
        case CC_VE_AgeOfAquarius : return "Age of Aquarius";
        case CC_VE_MirandasVeil : return "Miranda’s Veil";
        case CC_VE_Nineteen : return "Nineteen";
        case CC_VE_HemerasDawn : return "Hemera’s Dawn";
        case CC_VE_TamoanchanRevisited : return "Tamoanchan Revisited";
        case CC_VE_ConquestOfTlalocan : return "Conquest of Tlalocan";
        case CC_VE_Discovery : return "Discovery";
        case CC_VE_One : return "One";

        default : return "???";
    }
}

unsigned int cc_variant_board_size( CcVariantEnum const ve )
{
    switch ( ve )
    {
        case CC_VE_ClassicalChess : return CC_VARIANT_BOARD_SIZE_CLASSICAL_CHESS;
        case CC_VE_CroatianTies : return CC_VARIANT_BOARD_SIZE_CROATIAN_TIES;
        case CC_VE_MayanAscendancy : return CC_VARIANT_BOARD_SIZE_MAYAN_ASCENDANCY;
        case CC_VE_AgeOfAquarius : return CC_VARIANT_BOARD_SIZE_AGE_OF_AQUARIUS;
        case CC_VE_MirandasVeil : return CC_VARIANT_BOARD_SIZE_MIRANDAS_VEIL;
        case CC_VE_Nineteen : return CC_VARIANT_BOARD_SIZE_NINETEEN;
        case CC_VE_HemerasDawn : return CC_VARIANT_BOARD_SIZE_HEMERAS_DAWN;
        case CC_VE_TamoanchanRevisited : return CC_VARIANT_BOARD_SIZE_TAMOANCHAN_REVISITED;
        case CC_VE_ConquestOfTlalocan : return CC_VARIANT_BOARD_SIZE_CONQUEST_OF_TLALOCAN;
        case CC_VE_Discovery : return CC_VARIANT_BOARD_SIZE_DISCOVERY;
        case CC_VE_One : return CC_VARIANT_BOARD_SIZE_ONE;

        default : return 0;
    }
}
