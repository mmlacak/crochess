// Copyright (c) 2021, 2022 Mario Mlačak, mmlacak@gmail.com
// Licensed under GNU GPL v3+ license. See LICENSING, COPYING files for details.

#include "cc_str_utils.h"

#include "cc_pos_defs.h"

#include "cc_rules_misc.h"
#include "cc_rules_path.h"


bool cc_check_valid_draw_offer_exists( CcMove * moves,
                                       CcGameStatusEnum gse ) {
    if ( !moves ) return false;
    if ( !CC_GAME_STATUS_IS_TURN( gse ) ) return false;

    CcMove * m = moves;
    CC_FASTFORWARD( m );

    while ( m ) {
        if ( CC_MOVE_STATUS_IS_DRAW_OFFER_REVOKED( m->status ) )
            return false;
        else if ( CC_MOVE_STATUS_IS_DRAW_OFFER( m->status ) )
            return true;

        // Skip two moves, because draw offer is made by one player.
        m = m->prev__w;
        if ( m )
            m = m->prev__w;
        else
            break;
    }

    return false;
}
