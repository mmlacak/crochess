// Copyright (c) 2021 Mario Mlačak, mmlacak@gmail.com
// Licensed under GNU GPL v3+ license. See LICENSING, COPYING files for details.

#ifndef __CC_VARIANT_H__
#define __CC_VARIANT_H__

#include <stdbool.h>
#include <stddef.h>

/**
    @file cc_variant.h
    @brief Variants constants, symbols, enumeration, and related functions.
*/


/** @defgroup variant_board_sizes The board sizes definitions
 *  The board sizes defined for each variant.

    @see cc_variant_board_size()
 *  @{
 */

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

/** @} */ // end of variant_board_sizes


/** @defgroup variant_rush_limits The rush limit definitions
 *  The rush limits for each variant define maximum (and minimum) rank light
    (and dark) privates can reach in a rush.

    @see cc_variant_rush_rank_limit()
 *  @{
 */

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

/** @} */ // end of variant_rush_limits


/**
    Enumeration of all variants.
*/
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


/**
    Maximum length of a symbol string (char array), used by any variant.
*/
#define CC_MAX_LEN_VARIANT_SYMBOL (3)

/**
    Length of an invalid symbol string (char array).
*/
#define CC_LEN_VARIANT_SYMBOL_INVALID (0)

/**
    Classical Chess symbol, short string.
*/
extern char const * const CC_VARIANT_CLASSICAL_CHESS_SYMBOL;

/**
    Croatian Ties symbol, short string.
*/
extern char const * const CC_VARIANT_CROATIAN_TIES_SYMBOL;

/**
    Mayaan Ascendancy symbol, short string.
*/
extern char const * const CC_VARIANT_MAYAN_ASCENDANCY_SYMBOL;

/**
    Age of Aquarius symbol, short string.
*/
extern char const * const CC_VARIANT_AGE_OF_AQUARIUS_SYMBOL;

/**
    Miranda's Veil symbol, short string.
*/
extern char const * const CC_VARIANT_MIRANDAS_VEIL_SYMBOL;

/**
    Nineteen symbol, short string.
*/
extern char const * const CC_VARIANT_NINETEEN_SYMBOL;

/**
    Hemera's Dawn symbol, short string.
*/
extern char const * const CC_VARIANT_HEMERAS_DAWN_SYMBOL;

/**
    Tamoanchan Revisited symbol, short string.
*/
extern char const * const CC_VARIANT_TAMOANCHAN_REVISITED_SYMBOL;

/**
    Conquest of Tlalocan symbol, short string.
*/
extern char const * const CC_VARIANT_CONQUEST_OF_TLALOCAN_SYMBOL;

/**
    Discovery symbol, short string.
*/
extern char const * const CC_VARIANT_DISCOVERY_SYMBOL;

/**
    One symbol, short string.
*/
extern char const * const CC_VARIANT_ONE_SYMBOL;

/**
    Array of all symbols, for all variants.
*/
extern char const * const CC_VARIANT_SYMBOLS[];


/**
    Function returns variant enum, based on a string.

    @param str String.
    @param ve__o _Output_, variant enum.

    @note
    Strings are compared case-insensitive.

    @note
    If `max_len__d` is given (i.e. > `0`), then it must also be > `CC_MAX_LEN_VARIANT_SYMBOL`.

    @return `true` if successful, `false` otherwise.

*/
size_t cc_variant_from_symbol( char const * str,
                               CcVariantEnum * ve__o );

/**
    Function returning variant symbol.

    @param ve Variant enum.

    @return Variant symbol, i.e. lowercase name abbreviation of a variant.
*/
char const * cc_variant_symbol( CcVariantEnum ve );

/**
    Function returning variant label.

    @param ve Variant enum.

    @return Variant label, i.e. capitalized name of a variant.
*/
char const * cc_variant_label( CcVariantEnum ve );

/**
    Function returning size of a board for a given variant.

    @param ve Variant enum.

    @return Size of a board used by given variant.
*/
unsigned int cc_variant_board_size( CcVariantEnum ve );

/**
    Function returns if Pawns can move sideways for a given variant.

    @param ve Variant enum.

    @return `true` if variant has sideways Pawns, `false` otherwise.
*/
bool cc_variant_has_sideways_pawns( CcVariantEnum ve );

/**
    Function returns rush limit, either maximum rank for light privates,
    or minimum rank for dark privates.

    @param ve Variant enum.
    @param is_piece_light Flag whether piece is light or dark.

    @return Rush limit for known variants, `0` otherwise.
*/
unsigned int cc_variant_rush_rank_limit( CcVariantEnum ve, bool is_piece_light );

/**
    Function checks if given rank is within rush limits;
    both upper and lower rush limits are checked.

    @param ve Variant enum.
    @param is_piece_light Flag whether piece is light or dark.
    @param rank Rank, position along vertical axis.

    @return `true` if within rush limits, `false` otherwise.
*/
bool cc_variant_is_rank_in_rush_limits( CcVariantEnum ve,
                                        bool is_piece_light,
                                        int rank );

#endif /* __CC_VARIANT_H__ */
