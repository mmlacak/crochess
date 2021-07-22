// Copyright (c) 2021 Mario Mlačak, mmlacak@gmail.com
// Licensed under 3-clause (modified) BSD license. See LICENSE for details.

#ifndef __CC_VARIANT_H__
#define __CC_VARIANT_H__

#include <stdbool.h>


#define CC_VARIANT_BOARD_SIZE_CLASSICAL_CHESS (8)
#define CC_VARIANT_BOARD_SIZE_CROATIAN_TIES (10)
#define CC_VARIANT_BOARD_SIZE_MAYAN_ASCENDANCY (12)
#define CC_VARIANT_BOARD_SIZE_AGE_OF_AQUARIUS (14)
#define CC_VARIANT_BOARD_SIZE_MIRANDAS_VEIL (16)
#define CC_VARIANT_BOARD_SIZE_NINETEEN (18)
#define CC_VARIANT_BOARD_SIZE_HEMERAS_DAWN (20)
#define CC_VARIANT_BOARD_SIZE_TAMOANCHAN_REVISITED (22)
#define CC_VARIANT_BOARD_SIZE_CONQUEST_OF_TLALOCAN (24)
#define CC_VARIANT_BOARD_SIZE_DISCOVERY (24)
#define CC_VARIANT_BOARD_SIZE_ONE (26)

#define CC_VARIANT_BOARD_SIZE_MAXIMUM (26)


typedef enum CcVariantEnum
{
    CC_VE_ClassicalChess,
    CC_VE_CroatianTies,
    CC_VE_MayanAscendancy,
    CC_VE_AgeOfAquarius,
    CC_VE_MirandasVeil,
    CC_VE_Nineteen,
    CC_VE_HemerasDawn,
    CC_VE_TamoanchanRevisited,
    CC_VE_ConquestOfTlalocan,
    CC_VE_Discovery,
    CC_VE_One,
} CcVariantEnum;


extern char const * const CC_VARIANT_CLASSICAL_CHESS_SYMBOL;
extern char const * const CC_VARIANT_CROATIAN_TIES_SYMBOL;
extern char const * const CC_VARIANT_MAYAN_ASCENDANCY_SYMBOL;
extern char const * const CC_VARIANT_AGE_OF_AQUARIUS_SYMBOL;
extern char const * const CC_VARIANT_MIRANDAS_VEIL_SYMBOL;
extern char const * const CC_VARIANT_NINETEEN_SYMBOL;
extern char const * const CC_VARIANT_HEMERAS_DAWN_SYMBOL;
extern char const * const CC_VARIANT_TAMOANCHAN_REVISITED_SYMBOL;
extern char const * const CC_VARIANT_CONQUEST_OF_TLALOCAN_SYMBOL;
extern char const * const CC_VARIANT_DISCOVERY_SYMBOL;
extern char const * const CC_VARIANT_ONE_SYMBOL;

extern char const * const CC_VARIANT_SYMBOLS[];


bool cc_variant_str_is_symbol( char const * const restrict str );
CcVariantEnum cc_variant_from_symbol( char const * const restrict str );
char const * cc_variant_label( CcVariantEnum const ve );

unsigned int cc_variant_board_size( CcVariantEnum const ve );


#endif /* __CC_VARIANT_H__ */
