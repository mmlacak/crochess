// Copyright (c) 2021 Mario Mlačak, mmlacak@gmail.com
// Licensed under 3-clause (modified) BSD license. See LICENSE for details.

#include "cc_tag.h"


char cc_tag_as_char( CcTagEnum const ct )
{
    switch ( ct )
    {
        case CC_TE_None : return ' ';
        case CC_TE_CanRush : return 'R';
        case CC_TE_CanCastle : return 'C';
        case CC_TE_DelayedPromotion : return 'P';

        default : return ' ';
    }
}
