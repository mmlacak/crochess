// Copyright (c) 2021, 2022 Mario Mlačak, mmlacak@gmail.com
// Licensed under GNU GPL v3+ license. See LICENSING, COPYING files for details.

#include "cc_rules_misc.h"


bool cc_check_valid_draw_offer_exists( CcMoves * restrict moves,
                                       CcGameStatusEnum player_to_play )
{
    if ( !moves ) return false;
    if ( !CC_GAME_STATUS_IS_TURN( player_to_play ) ) return false;

    CcMoves * m = moves;

    while ( m->next ) m = m->next; // rewind to last

    while ( m )
    {
        m->an;
    }

    return false;
}
