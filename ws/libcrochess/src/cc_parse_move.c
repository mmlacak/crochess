// Copyright (c) 2021, 2022 Mario Mlačak, mmlacak@gmail.com
// Licensed under GNU GPL v3+ license. See LICENSING, COPYING files for details.

#include <ctype.h>
// #include <stdarg.h>

// #include <string.h>
// #include <stdio.h>

#include "cc_rules_misc.h"
#include "cc_parse_ply.h"
#include "cc_parse_move.h"


static bool cc_check_standalone_status( char const char_an,
                                        CcMove ** restrict temp__n,
                                        CcMove ** restrict move__o,
                                        CcParseMsg ** restrict parse_msgs__iod,
                                        CcMoveStatusEnum mse,
                                        size_t max_len__d,
                                        char const * restrict msg, ... ) {
    if ( iscntrl( char_an ) || isspace( char_an ) ) {
        ( *temp__n )->status = mse;

        // Ownership transfer.
        *move__o = *temp__n;
        *temp__n = NULL;

        return true;
    } else {
        va_list args;
        va_start( args, msg );

        cc_parse_msg_append_fmt( parse_msgs__iod, CC_PMTE_Error, max_len__d, msg, args );

        va_end( args );
        return false;
    }
}


bool cc_parse_move( char const * restrict move_an,
                    CcGame * restrict game,
                    CcMove ** restrict move__o,
                    CcParseMsg ** restrict parse_msgs__iod ) {
    if ( !move_an ) return false;
    if ( !game ) return false;
    if ( !move__o || *move__o ) return false;
    if ( !parse_msgs__iod ) return false;

    if ( !game->chessboard ) return false;

    if ( !CC_GAME_STATUS_IS_TURN( game->status ) ) {
        char const * msg =
            ( game->status == CC_GSE_None ) ? "Game is not initialized.\n"
                                            : "Game is finished.\n";

        cc_parse_msg_append_fmt( parse_msgs__iod, CC_PMTE_Error, CC_MAX_LEN_ZERO_TERMINATED, msg );
        return false;
    }

    CcMove * move__t = cc_move__new( move_an, CC_MAX_LEN_ZERO_TERMINATED, NULL, CC_MSE_None );
    if ( !move__t ) return false;

    char const * m_an = move_an;
    if ( *m_an == '#' ) {
        if ( *++m_an == '#' ) {
            // "##" resign
            return cc_check_standalone_status( *++m_an, &move__t, move__o, parse_msgs__iod, CC_MSE_Resign, CC_MAX_LEN_ZERO_TERMINATED, "Invalid char(s) after resign.\n" );
        } else {
            // "#" self-checkmate

            // TODO :: Do check if opponent is really (self-)checkmated.
            //         Self- is optional, since both players could overlook checkmate,
            //         this is option to rectify such a situation.

            return cc_check_standalone_status( *m_an, &move__t, move__o, parse_msgs__iod, CC_MSE_SelfCheckmate, CC_MAX_LEN_ZERO_TERMINATED, "Invalid char(s) after self-checkmate.\n" );
        }
    }

    if ( *m_an == '(' ) {
        if ( *++m_an == '=' ) {
            if ( *++m_an == '=' ) {
                if ( *++m_an == ')' ) {
                    // "(==)" draw offer accepted

                    if ( cc_check_valid_draw_offer_exists( game->moves, game->status ) ) {
                        return cc_check_standalone_status( *++m_an, &move__t, move__o, parse_msgs__iod, CC_MSE_DrawAccepted, CC_MAX_LEN_ZERO_TERMINATED, "Invalid char(s) after accepted draw.\n" );
                    } else {
                        cc_parse_msg_append_fmt( parse_msgs__iod, CC_PMTE_Error, CC_MAX_LEN_ZERO_TERMINATED, "No valid opponent's draw offer found.\n" );
                        return false;
                    }
                }
                // <i> Draw-by-rules should be issued by arbiter, not players;
                //     i.e. should be issued by server, not clients.
                //
                // else if ( *m_an == '=' )
                // {
                //     if ( *++m_an == ')' )
                //     {
                //         // "(===)" draw by rules
                //
                //         return cc_check_standalone_status( *++m_an,
                //                                            &move__t,
                //                                            move__o,
                //                                            parse_msgs__iod,
                //                                            CC_MSE_DrawByRules,
                //                                            CC_MAX_LEN_ZERO_TERMINATED,
                //                                            "Invalid char(s) after draw by rules.\n" );
                //     }
                // }
            }
        }

        cc_parse_msg_append_fmt( parse_msgs__iod, CC_PMTE_Error, CC_MAX_LEN_ZERO_TERMINATED, "Invalid char(s) within draw; draw offer cannot be issued standalone; draw-by-rules only by arbiter, not players.\n" );
        return false;
    }

    CcPly * plies__t = NULL;

    if ( !cc_parse_plies( move__t->notation, game, &plies__t, parse_msgs__iod ) ) {
        cc_ply_free_all( &plies__t );
        cc_move_free_all( &move__t );
        return false;
    }


    // TODO :: DEBUG :: DELETE
    //
    // {
    //     char * plies_str__a = cc_ply_all_to_short_string__new( plies__t );

    //     cc_str_print( plies_str__a, NULL, 0, "Plies: '%s'.\n", 0, NULL );

    //     CC_FREE( plies_str__a );
    // }
    //
    // TODO :: DEBUG :: DELETE






    // TODO :: post-plies status




    move__t->plies = plies__t; // Ownership transfer.
    plies__t = NULL;

    *move__o = move__t; // Ownership transfer.
    move__t = NULL;

    return true;
}
