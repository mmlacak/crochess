// Copyright (c) 2021 Mario Mlačak, mmlacak@gmail.com
// Licensed under 3-clause (modified) BSD license. See LICENSE for details.

#ifndef __CHIP_TYPE_H__
#define __CHIP_TYPE_H__


typedef enum ChipType
{
    CT_None,
    CT_CanRush, // Pawns
    CT_CanCastle, // Rooks, Kings
    CT_TagForPromotion, // Pawn
} ChipType;




#endif /* __CHIP_TYPE_H__ */
