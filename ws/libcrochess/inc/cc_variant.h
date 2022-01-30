// Copyright (c) 2021 Mario Mlačak, mmlacak@gmail.com
// Licensed under GNU GPL v3+ license. See LICENSING, COPYING files for details.

#ifndef __CC_VARIANT_H__
#define __CC_VARIANT_H__

#include <stdbool.h>

/**
    @file cc_variant.h
    @brief Variants constants, symbols, enumeration, and related functions.
*/

/**
    Classical Chess board size.
*/
#define CC_VARIANT_BOARD_SIZE_CLASSICAL_CHESS (8)

/**
    Croatian Ties board size.
*/
#define CC_VARIANT_BOARD_SIZE_CROATIAN_TIES (10)

/**
    Mayan Ascendancy board size.
*/
#define CC_VARIANT_BOARD_SIZE_MAYAN_ASCENDANCY (12)

/**
    Age of Aquarius board size.
*/
#define CC_VARIANT_BOARD_SIZE_AGE_OF_AQUARIUS (14)

/**
    Miranda's Veil board size.
*/
#define CC_VARIANT_BOARD_SIZE_MIRANDAS_VEIL (16)

/**
    Nineteen board size.
*/
#define CC_VARIANT_BOARD_SIZE_NINETEEN (18)

/**
    Hemera's Dawn board size.
*/
#define CC_VARIANT_BOARD_SIZE_HEMERAS_DAWN (20)

/**
    Tamoanchan Revisited board size.
*/
#define CC_VARIANT_BOARD_SIZE_TAMOANCHAN_REVISITED (22)

/**
    Conquest of Tlalocan board size.
*/
#define CC_VARIANT_BOARD_SIZE_CONQUEST_OF_TLALOCAN (24)

/**
    Dicsovery board size.
*/
#define CC_VARIANT_BOARD_SIZE_DISCOVERY (24)

/**
    One board size.
*/
#define CC_VARIANT_BOARD_SIZE_ONE (26)

/**
    Maximum board size, used by any variant.
*/
#define CC_VARIANT_BOARD_SIZE_MAXIMUM (26)

/**
    Macro expression to evaluate whether given number is a valid chessboard size.

    @param size A number.

    @return `true` if valid chessboard size, `false` otherwise.
*/
#define CC_VARIANT_BOARD_SIZE_IS_VALID(size) ( ( (size) == CC_VARIANT_BOARD_SIZE_CLASSICAL_CHESS )      \
                                            || ( (size) == CC_VARIANT_BOARD_SIZE_CROATIAN_TIES )        \
                                            || ( (size) == CC_VARIANT_BOARD_SIZE_MAYAN_ASCENDANCY )     \
                                            || ( (size) == CC_VARIANT_BOARD_SIZE_AGE_OF_AQUARIUS )      \
                                            || ( (size) == CC_VARIANT_BOARD_SIZE_MIRANDAS_VEIL )        \
                                            || ( (size) == CC_VARIANT_BOARD_SIZE_NINETEEN )             \
                                            || ( (size) == CC_VARIANT_BOARD_SIZE_HEMERAS_DAWN )         \
                                            || ( (size) == CC_VARIANT_BOARD_SIZE_TAMOANCHAN_REVISITED ) \
                                            || ( (size) == CC_VARIANT_BOARD_SIZE_CONQUEST_OF_TLALOCAN ) \
                                            || ( (size) == CC_VARIANT_BOARD_SIZE_DISCOVERY )            \
                                            || ( (size) == CC_VARIANT_BOARD_SIZE_ONE ) )


/**
    Enumeration of all variants.
*/
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
    Function returning if string is a valid variant symbol.

    @param str String.

    @return `true` if `str` is variant symbol, `false` otherwise.
            Strings are compared case-insensitive.
*/
bool cc_variant_str_is_symbol( char const * restrict str );

/**
    Function returning variant enum, based on a string.

    @param str String.

    @return `CcVariantEnum` if `str` is valid variant symbol, `CC_VE_One` otherwise.
            Strings are compared case-insensitive.
*/
CcVariantEnum cc_variant_from_symbol( char const * restrict str );

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
    Function checks if given coordinate is valid for a board.

    @param ve A variant.
    @param coord A coordinate.

    @return `true` if coordinate is on-board, `false` otherwise.
*/
bool cc_variant_is_coord_on_board( CcVariantEnum ve, int coord );

/**
    Function checks if given position is valid for a board.

    @param ve A variant.
    @param i File, position along horizontal axis.
    @param j Rank, position along vertical axis.

    @return `true` if position is on-board, `false` otherwise.
*/
bool cc_variant_is_pos_on_board( CcVariantEnum ve, int i, int j );


#endif /* __CC_VARIANT_H__ */
