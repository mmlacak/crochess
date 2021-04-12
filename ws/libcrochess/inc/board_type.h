// Copyright (c) 2021 Mario Mlačak, mmlacak@gmail.com
// Licensed under 3-clause (modified) BSD license. See LICENSE for details.

#ifndef __BOARD_TYPE_H__
#define __BOARD_TYPE_H__


#define BOARD_SIZE_CLASSICAL_CHESS 8
#define BOARD_SIZE_CROATIAN_TIES 10
#define BOARD_SIZE_MAYAN_ASCENDANCY 12
#define BOARD_SIZE_AGE_OF_AQUARIUS 14
#define BOARD_SIZE_MIRANDAS_VEIL 16
#define BOARD_SIZE_NINETEEN 18
#define BOARD_SIZE_HEMERAS_DAWN 20
#define BOARD_SIZE_TAMOANCHAN_REVISITED 22
#define BOARD_SIZE_CONQUEST_OF_TLALOCAN 24
#define BOARD_SIZE_DISCOVERY 24
#define BOARD_SIZE_ONE 26

#define BOARD_SIZE_MAXIMUM 26


typedef enum BoardType
{
    BT_ClassicalChess,
    BT_CroatianTies,
    BT_MayanAscendancy,
    BT_AgeOfAquarius,
    BT_MirandasVeil,
    BT_Nineteen,
    BT_HemerasDawn,
    BT_TamoanchanRevisited,
    BT_ConquestOfTlalocan,
    BT_Discovery,
    BT_One,
} BoardType;


BoardType bt_from_str(char const * const code);
char const * const bt_label(BoardType const bt);

unsigned int bt_size(BoardType const bt);


#endif /* __BOARD_TYPE_H__ */
