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

char const * const CC_VARIANT_SYMBOLS[] = {
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


size_t cc_variant_from_symbol( char const * str,
                               CcVariantEnum * ve__o ) {
    if ( !str ) return false;
    if ( !ve__o ) return false;

    char const * s = str;
    int ve = -1;
    size_t len = CC_LEN_VARIANT_SYMBOL_INVALID;

    if ( *s == 'a' ||  *s == 'A' ) {
        ++s;

        if ( *s == 'o' ||  *s == 'O' ) {
            ++s;

            if ( *s == 'a' ||  *s == 'A' ) { // "aoa"
                ve = CC_VE_AgeOfAquarius;
                len = 3;
            }
        }
    } else if ( *s == 'c' ||  *s == 'C' ) {
        ++s;

        if ( *s == 'c' ||  *s == 'C' ) { // "cc"
            ve = CC_VE_ClassicalChess;
            len = 2;
        } else if ( *s == 't' ||  *s == 'T' ) { // "ct"
            ve = CC_VE_CroatianTies;
            len = 2;
        } else if ( *s == 'o' ||  *s == 'O' ) {
            ++s;

            if ( *s == 't' ||  *s == 'T' ) { // "cot"
                ve = CC_VE_ConquestOfTlalocan;
                len = 3;
            }
        }
    } else if ( *s == 'd' ||  *s == 'D' ) { // "d"
        ve = CC_VE_Discovery;
        len = 1;
    } else if ( *s == 'h' ||  *s == 'H' ) {
        ++s;

        if ( *s == 'd' ||  *s == 'D' ) { // "hd"
            ve = CC_VE_HemerasDawn;
            len = 2;
        }
    } else if ( *s == 'm' ||  *s == 'M' ) {
        ++s;

        if ( *s == 'a' ||  *s == 'A' ) { // "ma"
            ve = CC_VE_MayanAscendancy;
            len = 2;
        } else if ( *s == 'v' ||  *s == 'V' ) { // "mv"
            ve = CC_VE_MirandasVeil;
            len = 2;
        }
    } else if ( *s == 'n' ||  *s == 'N' ) { // "n"
        ve = CC_VE_Nineteen;
        len = 1;
    } else if ( *s == 'o' ||  *s == 'O' ) { // "o"
        ve = CC_VE_One;
        len = 1;
    } else if ( *s == 't' ||  *s == 'T' ) {
        ++s;

        if ( *s == 'r' ||  *s == 'R' ) { // "tr"
            ve = CC_VE_TamoanchanRevisited;
            len = 2;
        }
    }

    if ( ve >= 0 ) {
        if ( isalnum( *++s ) )
            return CC_LEN_VARIANT_SYMBOL_INVALID;

        *ve__o = (CcVariantEnum)ve;
        return len;
    }

    return CC_LEN_VARIANT_SYMBOL_INVALID;
}

char const * cc_variant_symbol( CcVariantEnum ve ) {
    switch ( ve ) {
        case CC_VE_ClassicalChess : return CC_VARIANT_CLASSICAL_CHESS_SYMBOL;
        case CC_VE_CroatianTies : return CC_VARIANT_CROATIAN_TIES_SYMBOL;
        case CC_VE_MayanAscendancy : return CC_VARIANT_MAYAN_ASCENDANCY_SYMBOL;
        case CC_VE_AgeOfAquarius : return CC_VARIANT_AGE_OF_AQUARIUS_SYMBOL;
        case CC_VE_MirandasVeil : return CC_VARIANT_MIRANDAS_VEIL_SYMBOL;
        case CC_VE_Nineteen : return CC_VARIANT_NINETEEN_SYMBOL;
        case CC_VE_HemerasDawn : return CC_VARIANT_HEMERAS_DAWN_SYMBOL;
        case CC_VE_TamoanchanRevisited : return CC_VARIANT_TAMOANCHAN_REVISITED_SYMBOL;
        case CC_VE_ConquestOfTlalocan : return CC_VARIANT_CONQUEST_OF_TLALOCAN_SYMBOL;
        case CC_VE_Discovery : return CC_VARIANT_DISCOVERY_SYMBOL;
        case CC_VE_One : return CC_VARIANT_ONE_SYMBOL;

        default : return NULL;
    }
}

char const * cc_variant_label( CcVariantEnum ve ) {
    switch ( ve ) {
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

unsigned int cc_variant_board_size( CcVariantEnum ve ) {
    switch ( ve ) {
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

bool cc_variant_has_sideways_pawns( CcVariantEnum ve ) {
    switch ( ve ) {
        case CC_VE_Nineteen :
        case CC_VE_HemerasDawn :
        case CC_VE_TamoanchanRevisited :
        case CC_VE_ConquestOfTlalocan :
        case CC_VE_Discovery :
        case CC_VE_One :
            return true;

        default :
            return false;
    }
}

unsigned int cc_variant_rush_rank_limit( CcVariantEnum ve, bool is_piece_light ) {
    if ( is_piece_light ) {
        switch ( ve ) {
            case CC_VE_ClassicalChess : return CC_VARIANT_MAX_RUSH_RANK_CLASSICAL_CHESS_LIGHT;
            case CC_VE_CroatianTies : return CC_VARIANT_MAX_RUSH_RANK_CROATIAN_TIES_LIGHT;
            case CC_VE_MayanAscendancy : return CC_VARIANT_MAX_RUSH_RANK_MAYAN_ASCENDANCY_LIGHT;
            case CC_VE_AgeOfAquarius : return CC_VARIANT_MAX_RUSH_RANK_AGE_OF_AQUARIUS_LIGHT;
            case CC_VE_MirandasVeil : return CC_VARIANT_MAX_RUSH_RANK_MIRANDAS_VEIL_LIGHT;
            case CC_VE_Nineteen : return CC_VARIANT_MAX_RUSH_RANK_NINETEEN_LIGHT;
            case CC_VE_HemerasDawn : return CC_VARIANT_MAX_RUSH_RANK_HEMERAS_DAWN_LIGHT;
            case CC_VE_TamoanchanRevisited : return CC_VARIANT_MAX_RUSH_RANK_TAMOANCHAN_REVISITED_LIGHT;
            case CC_VE_ConquestOfTlalocan : return CC_VARIANT_MAX_RUSH_RANK_CONQUEST_OF_TLALOCAN_LIGHT;
            case CC_VE_Discovery : return CC_VARIANT_MAX_RUSH_RANK_DISCOVERY_LIGHT;
            case CC_VE_One : return CC_VARIANT_MAX_RUSH_RANK_ONE_LIGHT;

            default : return 0;
        }
    } else {
        switch ( ve ) {
            case CC_VE_ClassicalChess : return CC_VARIANT_MIN_RUSH_RANK_CLASSICAL_CHESS_DARK;
            case CC_VE_CroatianTies : return CC_VARIANT_MIN_RUSH_RANK_CROATIAN_TIES_DARK;
            case CC_VE_MayanAscendancy : return CC_VARIANT_MIN_RUSH_RANK_MAYAN_ASCENDANCY_DARK;
            case CC_VE_AgeOfAquarius : return CC_VARIANT_MIN_RUSH_RANK_AGE_OF_AQUARIUS_DARK;
            case CC_VE_MirandasVeil : return CC_VARIANT_MIN_RUSH_RANK_MIRANDAS_VEIL_DARK;
            case CC_VE_Nineteen : return CC_VARIANT_MIN_RUSH_RANK_NINETEEN_DARK;
            case CC_VE_HemerasDawn : return CC_VARIANT_MIN_RUSH_RANK_HEMERAS_DAWN_DARK;
            case CC_VE_TamoanchanRevisited : return CC_VARIANT_MIN_RUSH_RANK_TAMOANCHAN_REVISITED_DARK;
            case CC_VE_ConquestOfTlalocan : return CC_VARIANT_MIN_RUSH_RANK_CONQUEST_OF_TLALOCAN_DARK;
            case CC_VE_Discovery : return CC_VARIANT_MIN_RUSH_RANK_DISCOVERY_DARK;
            case CC_VE_One : return CC_VARIANT_MIN_RUSH_RANK_ONE_DARK;

            default : return 0;
        }
    }
}

bool cc_variant_is_rank_in_rush_limits( CcVariantEnum ve,
                                        bool is_piece_light,
                                        int rank ) {
    if ( is_piece_light ) {
        if ( rank < (int)CC_VARIANT_MIN_RUSH_RANK_LIGHT ) return false;
    } else {
        int max_rush_rank = (int)( cc_variant_board_size( ve ) - CC_VARIANT_RUSH_RANK_OFFSET );
        if ( max_rush_rank < rank ) return false;
    }

    int rush_rank_limit = (int)cc_variant_rush_rank_limit( ve, is_piece_light );

    if ( is_piece_light ) {
        return ( rank <= rush_rank_limit );
    } else {
        return ( rush_rank_limit <= rank );
    }
}
