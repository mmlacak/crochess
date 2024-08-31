// Copyright (c) 2021 Mario Mlačak, mmlacak@gmail.com
// Licensed under GNU GPL v3+ license. See LICENSING, COPYING files for details.

#ifndef __CC_VARIANT_H__
#define __CC_VARIANT_H__

#include <stdbool.h>
#include <stddef.h>

#include "cc_defines.h"


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


#define CC_VARIANT_MAX_RUSH_RANK_CLASSICAL_CHESS_LIGHT (3)
#define CC_VARIANT_MIN_RUSH_RANK_CLASSICAL_CHESS_DARK (4)

#define CC_VARIANT_MAX_RUSH_RANK_CROATIAN_TIES_LIGHT (4)
#define CC_VARIANT_MIN_RUSH_RANK_CROATIAN_TIES_DARK (5)

#define CC_VARIANT_MAX_RUSH_RANK_MAYAN_ASCENDANCY_LIGHT (5)
#define CC_VARIANT_MIN_RUSH_RANK_MAYAN_ASCENDANCY_DARK (6)

#define CC_VARIANT_MAX_RUSH_RANK_AGE_OF_AQUARIUS_LIGHT (6)
#define CC_VARIANT_MIN_RUSH_RANK_AGE_OF_AQUARIUS_DARK (7)

#define CC_VARIANT_MAX_RUSH_RANK_MIRANDAS_VEIL_LIGHT (7)
#define CC_VARIANT_MIN_RUSH_RANK_MIRANDAS_VEIL_DARK (8)

#define CC_VARIANT_MAX_RUSH_RANK_NINETEEN_LIGHT (8)
#define CC_VARIANT_MIN_RUSH_RANK_NINETEEN_DARK (9)

#define CC_VARIANT_MAX_RUSH_RANK_HEMERAS_DAWN_LIGHT (9)
#define CC_VARIANT_MIN_RUSH_RANK_HEMERAS_DAWN_DARK (10)

#define CC_VARIANT_MAX_RUSH_RANK_TAMOANCHAN_REVISITED_LIGHT (10)
#define CC_VARIANT_MIN_RUSH_RANK_TAMOANCHAN_REVISITED_DARK (11)

#define CC_VARIANT_MAX_RUSH_RANK_CONQUEST_OF_TLALOCAN_LIGHT (11)
#define CC_VARIANT_MIN_RUSH_RANK_CONQUEST_OF_TLALOCAN_DARK (12)

#define CC_VARIANT_MAX_RUSH_RANK_DISCOVERY_LIGHT (11)
#define CC_VARIANT_MIN_RUSH_RANK_DISCOVERY_DARK (12)

#define CC_VARIANT_MAX_RUSH_RANK_ONE_LIGHT (12)
#define CC_VARIANT_MIN_RUSH_RANK_ONE_DARK (13)


#define CC_VARIANT_RUSH_RANK_OFFSET (3)

#define CC_VARIANT_MIN_RUSH_RANK_LIGHT (3)
#define CC_VARIANT_MAX_RUSH_RANK_LIGHT (12)

#define CC_VARIANT_MIN_RUSH_RANK_DARK (4)
#define CC_VARIANT_MAX_RUSH_RANK_DARK (22)


typedef enum CcVariantEnum {
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


#define CC_MAX_LEN_VARIANT_SYMBOL (3)

#define CC_LEN_VARIANT_SYMBOL_INVALID (0)

#define CC_VARIANT_IS_ENUMERATOR(ve) ( ( CC_VE_ClassicalChess <= (ve) ) && ( (ve) <= CC_VE_One ) ) // <!> Keep in-sync with CcVariantEnum.

#define CC_VARIANT_IS_VALID(ve) ( ( CC_VE_ClassicalChess <= (ve) ) && ( (ve) <= CC_VE_One ) ) // <!> Keep in-sync with CcVariantEnum.

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


size_t cc_variant_from_symbol( char const * str,
                               CcVariantEnum * ve__o );

char const * cc_variant_symbol( CcVariantEnum ve );

char const * cc_variant_label( CcVariantEnum ve );

cc_uint_t cc_variant_board_size( CcVariantEnum ve );

bool cc_variant_has_sideways_pawns( CcVariantEnum ve );

cc_uint_t cc_variant_rush_rank_limit( CcVariantEnum ve, bool is_piece_light );

bool cc_variant_is_rank_in_rush_limits( CcVariantEnum ve,
                                        bool is_piece_light,
                                        int rank );

int cc_variant_promoting_rank( CcVariantEnum ve, bool is_light );

int cc_variant_initial_figure_rank( CcVariantEnum ve, bool is_light );


#endif /* __CC_VARIANT_H__ */
