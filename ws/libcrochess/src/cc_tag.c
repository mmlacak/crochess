// Copyright (c) 2021 Mario Mlačak, mmlacak@gmail.com
// Licensed under 3-clause (modified) BSD license. See LICENSE for details.

#include "cc_tag.h"


/**
    @file cc_tag.c
    @brief Tag related functions.
*/


// https://stackoverflow.com/questions/15927583/how-to-suppress-warning-control-reaches-end-of-non-void-function
#pragma GCC diagnostic push
#pragma GCC diagnostic ignored "-Wreturn-type"

char cc_tag_as_char( CcTagEnum const ct )
{
    switch ( ct )
    {
        // gcc doesn't check if all current options in enum are covered.

        case CC_TE_None : return ' ';
        case CC_TE_CanRush : return 'R';
        case CC_TE_CanCastle : return 'C';
        case CC_TE_DelayedPromotion : return 'P';
        case CC_TE_EnPassant : return 'E';
        case CC_TE_PawnSacrifice : return 'S';

        // default : return ' '; // Won't be suitable --> make compilers complain.
    }
}

#pragma GCC diagnostic pop
