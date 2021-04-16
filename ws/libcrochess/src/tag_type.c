// Copyright (c) 2021 Mario Mlačak, mmlacak@gmail.com
// Licensed under 3-clause (modified) BSD license. See LICENSE for details.

#include "tag_type.h"


char tt_as_char(TagType const ct)
{
    switch ( ct )
    {
        case TT_None : return ' ';
        case TT_CanRush : return 'R';
        case TT_CanCastle : return 'C';
        case TT_DelayedPromotion : return 'P';

        default : return ' ';
    }
}
