// Copyright (c) 2021 Mario Mlačak, mmlacak@gmail.com
// Licensed under 3-clause (modified) BSD license. See LICENSE for details.

#ifndef __TAG_TYPE_H__
#define __TAG_TYPE_H__


typedef enum TagType
{
    TT_None,
    TT_CanRush, // Pawns
    TT_CanCastle, // Rooks, Kings
    TT_DelayedPromotion, // Pawn
} TagType;


char tt_as_char(TagType const ct);


#endif /* __TAG_TYPE_H__ */
