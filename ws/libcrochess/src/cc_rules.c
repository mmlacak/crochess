// Copyright (c) 2021, 2022 Mario Mlačak, mmlacak@gmail.com
// Licensed under GNU GPL v3+ license. See LICENSING, COPYING files for details.

#include <ctype.h>

#include "cc_str_utils.h"
#include "cc_rules.h"


bool cc_make_move( char const * restrict move_an_str,
                   CcGame ** restrict game__io,
                   CcParseMsg ** restrict parse_msgs__io )
{
    if ( !move_an_str ) return false;

    if ( !game__io ) return false;
    if ( !*game__io ) return false;

    CcGame * g = *game__io;

    if ( !g->chessboard ) return false;
    if ( !g->moves ) return false;

    if ( !parse_msgs__io ) return false;

    if ( !CC_GAME_STATUS_IS_TURN( g->status ) )
    {
        cc_parse_msg_append_or_init_format( parse_msgs__io,
                                            CC_PMTE_Error,
                                            CC_MAX_LEN_ZERO_TERMINATED,
                                            "Game is finished." );
        return false;
    }

    char const * m = move_an_str;

    if ( *m == '#' )
    {
        ++m;

        if ( iscntrl( *m ) || isspace( *m ) )
        {
            g->status = cc_game_status_next( g->status, true, true );
            return true;
        }
        else
        {
            cc_parse_msg_append_or_init_format( parse_msgs__io,
                                                CC_PMTE_Error,
                                                CC_MAX_LEN_ZERO_TERMINATED,
                                                "Invalid char(s) after self-checkmate." );
            return false;
        }
    }


    // TODO


    return false;
}
