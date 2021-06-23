// Copyright (c) 2021 Mario Mlačak, mmlacak@gmail.com
// Licensed under 3-clause (modified) BSD license. See LICENSE for details.

#ifndef __CC_TAG_H__
#define __CC_TAG_H__


typedef enum CcTagEnum
{
    CC_TE_None,

    // Persistant, i.e. valid until lost.
    CC_TE_CanRush, // Pawns
    CC_TE_CanCastle, // Rooks, Kings
    CC_TE_DelayedPromotion, // Pawn

    CC_TE_EnPassant, // Gained in a move, used or lost in next.

    CC_TE_PawnSacrifice, // Gained in a move, used or lost in the very same move.
} CcTagEnum;


char cc_tag_as_char( CcTagEnum const ct );


#endif /* __CC_TAG_H__ */
