// Copyright (c) 2021 Mario Mlačak, mmlacak@gmail.com
// Licensed under GNU GPL v3+ license. See LICENSING, COPYING files for details.

#include "cc_defines.h"
#include "cc_pos.h"


CcPos cc_pos( int const i, int const j )
{
    CcPos pos = { .i = i, .j = j };
    return pos;
}

CcPos cc_pos_empty()
{
    return cc_pos( CC_INVALID_OFF_BOARD_COORD_MIN, CC_INVALID_OFF_BOARD_COORD_MIN );
}
