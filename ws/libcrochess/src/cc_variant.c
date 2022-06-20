// Copyright (c) 2021 Mario Mlačak, mmlacak@gmail.com
// Licensed under GNU GPL v3+ license. See LICENSING, COPYING files for details.

#include <ctype.h>
#include <stdlib.h>
#include <string.h>

#include "cc_defines.h"
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


bool cc_variant_from_symbol( char const * restrict str,
                             size_t max_len__d,
                             CcVariantEnum * ve__o )
{
    if ( !str ) return false;
    if ( !ve__o ) return false;

    if ( ( max_len__d > CC_MAX_LEN_ZERO_TERMINATED )
      && ( max_len__d < CC_MAX_LEN_VARIANT_SYMBOL ) )
        return false;

    char const * s = str;
    int ve = -1;

    if ( *s == 'a' ||  *s == 'A' )
    {
        ++s;

        if ( *s == 'o' ||  *s == 'O' )
        {
            ++s;

            if ( *s == 'a' ||  *s == 'A' )
                ve = CC_VE_AgeOfAquarius;
        }
    }
    else if ( *s == 'c' ||  *s == 'C' )
    {
        ++s;

        if ( *s == 'c' ||  *s == 'C' )
            ve = CC_VE_ClassicalChess;
        else if ( *s == 't' ||  *s == 'T' )
            ve = CC_VE_CroatianTies;
        else if ( *s == 'o' ||  *s == 'O' )
        {
            ++s;

            if ( *s == 't' ||  *s == 'T' )
                ve = CC_VE_ConquestOfTlalocan;
        }
    }
    else if ( *s == 'd' ||  *s == 'D' )
        ve = CC_VE_Discovery;
    else if ( *s == 'h' ||  *s == 'H' )
    {
        ++s;

        if ( *s == 'd' ||  *s == 'D' )
            ve = CC_VE_HemerasDawn;
    }
    else if ( *s == 'm' ||  *s == 'M' )
    {
        ++s;

        if ( *s == 'a' ||  *s == 'A' )
            ve = CC_VE_MayanAscendancy;
        else if ( *s == 'v' ||  *s == 'V' )
            ve = CC_VE_MirandasVeil;
    }
    else if ( *s == 'n' ||  *s == 'N' )
        ve = CC_VE_Nineteen;
    else if ( *s == 'o' ||  *s == 'O' )
        ve = CC_VE_One;
    else if ( *s == 't' ||  *s == 'T' )
    {
        ++s;

        if ( *s == 'r' ||  *s == 'R' )
            ve = CC_VE_TamoanchanRevisited;
    }

    ++s;

    if ( ve >= 0 )
    {
        if ( max_len__d > CC_MAX_LEN_VARIANT_SYMBOL )
            if ( !iscntrl( *s ) && !isspace( *s ) )
                return false;

        *ve__o = (CcVariantEnum)ve;
        return true;
    }

    return false;
}

char const * cc_variant_label( CcVariantEnum ve )
{
    switch ( ve )
    {
        case CC_VE_ClassicalChess : return "Classical Chess";
        case CC_VE_CroatianTies : return "Croatian Ties";
        case CC_VE_MayanAscendancy : return "Mayan Ascendancy";
        case CC_VE_AgeOfAquarius : return "Age of Aquarius";
        case CC_VE_MirandasVeil : return "Miranda's Veil";
        case CC_VE_Nineteen : return "Nineteen";
        case CC_VE_HemerasDawn : return "Hemera's Dawn";
        case CC_VE_TamoanchanRevisited : return "Tamoanchan Revisited";
        case CC_VE_ConquestOfTlalocan : return "Conquest of Tlalocan";
        case CC_VE_Discovery : return "Discovery";
        case CC_VE_One : return "One";

        default : return NULL;
    }
}

unsigned int cc_variant_board_size( CcVariantEnum ve )
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
