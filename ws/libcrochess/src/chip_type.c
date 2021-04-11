// Copyright (c) 2021 Mario Mlačak, mmlacak@gmail.com
// Licensed under 3-clause (modified) BSD license. See LICENSE for details.

#include "chip_type.h"


char chip_as_char(ChipType const ct)
{
    switch ( ct )
    {
        case CT_None : return ' ';
        case CT_CanRush : return 'R';
        case CT_CanCastle : return 'C';
        case CT_TagForPromotion : return 'P';

        default : return ' ';
    }
}
