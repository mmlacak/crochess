// Copyright (c) 2021 Mario Mlačak, mmlacak@gmail.com
// Licensed under 3-clause (modified) BSD license. See LICENSE for details.

#ifndef __CC_TAG_H__
#define __CC_TAG_H__


typedef enum CcTagEnum
{
    CC_TE_None,
    CC_TE_CanRush, // Pawns
    CC_TE_CanCastle, // Rooks, Kings
    CC_TE_DelayedPromotion, // Pawn

    // TODO
    // pawn sacrifice
    // en passant
} CcTagEnum;


char cc_tag_as_char( CcTagEnum const ct );


#endif /* __CC_TAG_H__ */
