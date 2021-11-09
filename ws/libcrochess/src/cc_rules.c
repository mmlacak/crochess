// Copyright (c) 2021 Mario Mlačak, mmlacak@gmail.com
// Licensed under GNU GPL v3+ license. See LICENSING, COPYING files for details.

#include "cc_parse_move.h"
#include "cc_rules.h"


bool cc_rules_do_moves( CcGame ** restrict game_io__r,
                        CcMove ** const restrict moves__n,
                        CcDoMoveEnum dme )
{
    if ( !game_io__r ) return false;
    if ( !*game_io__r ) return false;
    if ( !((*game_io__r)->chessboard) ) return false;

    if ( !moves__n ) return false;
    if ( !*moves__n ) return false;

    if ( !CC_GAME_STATUS_IS_TURN( (*game_io__r)->status ) ) return false;

    CcGame * gm__t = cc_game_duplicate_all_new( *game_io__r );
    if ( !gm__t ) return false;

    if ( dme == CC_DME_DoAllMoves )
    {
        CcMove * mv = *moves__n;

        while ( mv )
        {
            if ( !cc_do_moves( gm__t->chessboard, mv, CC_DME_DoOnlyCurrentMove ) )
            {
                cc_game_free_all( &gm__t );
                return false;
            }

// TODO :: flags for status
            gm__t->status = cc_game_status_next( gm__t->status, false, false, false );

            mv = mv->next;
        }
    }
    else
    {
        if ( !cc_do_moves( gm__t->chessboard, *moves__n, dme ) )
        {
            cc_game_free_all( &gm__t );
            return false;
        }

// TODO :: flags for status
        gm__t->status = cc_game_status_next( gm__t->status, false, false, false );
    }

    if ( !cc_move_append_or_init( &( gm__t->moves ), moves__n ) )
    {
        cc_game_free_all( &gm__t );
        return false;
    }

    if ( !cc_game_free_all( game_io__r ) )
    {
        cc_game_free_all( &gm__t );
        return false;
    }

    *game_io__r = gm__t;

    return true;
}


bool cc_rules_make_move( CcGame ** const restrict game_io__r,
                         char const * const restrict move_an,
                         CcParseMsg ** const restrict parse_msgs_io )
{
    if ( !game_io__r ) return false;
    if ( !*game_io__r ) return false;
    if ( !((*game_io__r)->chessboard) ) return false;

    if ( !move_an ) return false;

    if ( !CC_GAME_STATUS_IS_TURN( (*game_io__r)->status ) ) return false;

    CcMove * move__t = NULL;

    if ( !cc_parse_move( move_an, *game_io__r, &move__t, parse_msgs_io ) )
    {
        cc_move_free_all_moves( &move__t );
        return false;
    }

    if ( !cc_rules_do_moves( game_io__r, &move__t, CC_DME_DoOnlyCurrentMove ) )
    {
        cc_parse_msg_init_or_append_format( parse_msgs_io,
                                            CC_PME_Error,
                                            "Error while making move '%s'.",
                                            move_an );

        cc_move_free_all_moves( &move__t );
        return false;
    }

    return true;
}
