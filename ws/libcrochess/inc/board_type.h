// Copyright (c) 2021 Mario Mlačak, mmlacak@gmail.com
// Licensed under 3-clause (modified) BSD license. See LICENSE for details.

#ifndef __BOARD_TYPE_H__
#define __BOARD_TYPE_H__


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


BoardType board_from_str(char const * const code);
char const * const board_label(BoardType const bt);

unsigned int board_size(BoardType const bt);


#endif /* __BOARD_TYPE_H__ */
