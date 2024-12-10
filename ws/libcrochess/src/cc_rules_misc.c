// Copyright (c) 2021, 2022 Mario Mlačak, mmlacak@gmail.com
// Licensed under GNU GPL v3+ license. See LICENSING, COPYING files for details.

#include "cc_str_utils.h"

#include "cc_pos_defs.h"

#include "cc_rules_misc.h"
#include "cc_rules_path.h"


#define CC_DRAW_OFFER_LEN (3)

#define CC_DRAW_OFFER_CANCELED (-1)
#define CC_DRAW_OFFER_NOT_FOUND (0)
#define CC_DRAW_OFFER_FOUND (1)


static int _cc_an_str_ends_with_draw_offer( char const * an_start,
                                            char const * an_end__d,
                                            size_t max_len__d ) {
    if ( !an_start ) return CC_DRAW_OFFER_NOT_FOUND;

    size_t len = cc_str_len( an_start, an_end__d, max_len__d );
    if ( len < CC_DRAW_OFFER_LEN ) return CC_DRAW_OFFER_NOT_FOUND;

    char const * c = an_start + len - 1;

    while ( !*c )
        if ( an_start < c )
            --c;
        else
            break;

    if ( c < an_start + CC_DRAW_OFFER_LEN - 1 )
        return CC_DRAW_OFFER_NOT_FOUND;

    // Do mind: reverse order!
    if ( *c == ')' ) {
        if ( *--c == '=' ) {
            if ( *--c == '(' ) {
                // "(=)" draw offered
                return CC_DRAW_OFFER_FOUND;
            }
        } else if ( *c == '-' ) {
            if ( *--c == '(' ) {
                // "(-)" draw offer canceled
                return CC_DRAW_OFFER_CANCELED;
            }
        }
    }

    return CC_DRAW_OFFER_NOT_FOUND;
}


bool cc_check_valid_draw_offer_exists( CcMove * moves,
                                       CcGameStatusEnum gse ) {
    if ( !moves ) return false;
    if ( !CC_GAME_STATUS_IS_TURN( gse ) ) return false;

    int draw_offer = CC_DRAW_OFFER_NOT_FOUND;
    CcMove * m = moves;
    CC_FASTFORWARD( m );

    while ( m ) {
        draw_offer = _cc_an_str_ends_with_draw_offer( m->notation,
                                                     NULL,
                                                     CC_MAX_LEN_ZERO_TERMINATED );

        if ( draw_offer == CC_DRAW_OFFER_CANCELED )
            return false;
        else if ( draw_offer == CC_DRAW_OFFER_FOUND )
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
