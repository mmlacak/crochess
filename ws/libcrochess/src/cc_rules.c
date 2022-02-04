// Copyright (c) 2021 Mario Mlačak, mmlacak@gmail.com
// Licensed under GNU GPL v3+ license. See LICENSING, COPYING files for details.

#include "cc_parse_move.h"
#include "cc_rules.h"


bool cc_rules_do_moves( CcGame ** restrict game__io_r,
                        CcMove ** restrict moves__n,
                        CcDoMoveEnum dme )
{
    if ( !game__io_r ) return false;
    if ( !*game__io_r ) return false;
    if ( !((*game__io_r)->chessboard) ) return false;

    if ( !moves__n ) return false;
    if ( !*moves__n ) return false;

    if ( !CC_GAME_STATUS_IS_TURN( (*game__io_r)->status ) ) return false;

    CcGame * gm__t = cc_game_duplicate_all_new( *game__io_r );
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
            gm__t->status = cc_game_status_next( gm__t->status, false, false );

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
        gm__t->status = cc_game_status_next( gm__t->status, false, false );
    }

    if ( !cc_move_extend_or_init( &( gm__t->moves ), moves__n ) )
    {
        cc_game_free_all( &gm__t );
        return false;
    }

    if ( !cc_game_free_all( game__io_r ) )
    {
        cc_game_free_all( &gm__t );
        return false;
    }

    *game__io_r = gm__t; // Ownership transfer --> gm__t is now weak pointer.

    return true;
}


// bool cc_rules_make_move( CcGame ** restrict game__io_r,
//                          char const * restrict move_str,
//                          CcParseMsg ** restrict parse_msgs__io )
// {
//     if ( !game__io_r ) return false;
//     if ( !*game__io_r ) return false;
//     if ( !((*game__io_r)->chessboard) ) return false;
//     if ( !move_str ) return false;

//     if ( !CC_GAME_STATUS_IS_TURN( (*game__io_r)->status ) ) return false;

//     CcMove * move__t = NULL;

//     if ( !cc_parse_move( move_str, *game__io_r, &move__t, parse_msgs__io ) )
//     {
//         cc_moves_free_all( &move__t );
//         return false;
//     }

//     if ( !cc_rules_do_moves( game__io_r, &move__t, CC_DME_DoOnlyCurrentMove ) )
//     {
//         cc_parse_msg_append_or_init_format( parse_msgs__io,
//                                             CC_PME_Error,
//                                             "Error while making move '%s'.",
//                                             move_str );

//         cc_moves_free_all( &move__t );
//         return false;
//     }

//     return true;
// }
