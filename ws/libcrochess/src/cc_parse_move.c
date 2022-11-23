// Copyright (c) 2021, 2022 Mario Mlačak, mmlacak@gmail.com
// Licensed under GNU GPL v3+ license. See LICENSING, COPYING files for details.

#include <ctype.h>
// #include <stdarg.h>

// #include <string.h>
// #include <stdio.h>

#include "cc_rules_misc.h"
#include "cc_parse_move.h"


static bool cc_check_pre_plies_status( char const char_an,
                                       CcGame * restrict game__io,
                                       CcParseMsg ** restrict parse_msgs__io,
                                       bool is_resign,
                                       bool is_end,
                                       bool is_won,
                                       size_t max_len__d,
                                       char const * restrict msg, ... )
{
    // if ( !game__io ) return false;

    // if ( !game__io->chessboard ) return false;
    // if ( !game__io->moves ) return false;

    // if ( !parse_msgs__io ) return false;
    // if ( !msg ) return false;

    if ( iscntrl( char_an ) || isspace( char_an ) )
    {
        if ( is_resign )
            game__io->status = cc_game_resign( game__io->status );
        else
            game__io->status = cc_game_status_next( game__io->status, is_end, is_won );

        return true;
    }
    else
    {
        va_list args;
        va_start( args, msg );

        cc_parse_msg_append_format_if( parse_msgs__io, CC_PMTE_Error, max_len__d, msg, args );

        va_end( args );

        return false;
    }
}


bool cc_parse_move( char const * restrict move_an,
                    CcGame * restrict game,
                    CcMove ** restrict move__o,
                    CcParseMsg ** restrict parse_msgs__io )
{
    if ( !move_an ) return false;
    if ( !game ) return false;
    if ( !move__o ) return false;
    if ( *move__o ) return false;
    if ( !parse_msgs__io ) return false;

    if ( !game->chessboard ) return false;

    if ( !CC_GAME_STATUS_IS_TURN( game->status ) )
    {
        char const * msg =
            ( game->status == CC_GSE_None ) ? "Game is not initialized.\n"
                                            : "Game is finished.\n";

        cc_parse_msg_append_format_if( parse_msgs__io,
                                       CC_PMTE_Error,
                                       CC_MAX_LEN_ZERO_TERMINATED,
                                       msg );
        return false;
    }

    char const * m_an = move_an;

    if ( *m_an == '#' )
    {
        if ( *++m_an == '#' )
        {
            // "##" resign
            return cc_check_pre_plies_status( *++m_an, game, parse_msgs__io, true, true, false,
                                              CC_MAX_LEN_ZERO_TERMINATED,
                                              "Invalid char(s) after resign.\n" );
        }
        else
        {
            // "#" self-checkmate

// TODO :: Do check if opponent is really (self-)checkmated.
//         Self- is optional, since both players could overlook checkmate,
//         this is option to rectify such a situation.

            return cc_check_pre_plies_status( *m_an, game, parse_msgs__io, false, true, true,
                                              CC_MAX_LEN_ZERO_TERMINATED,
                                              "Invalid char(s) after self-checkmate.\n" );
        }
    }

    if ( *m_an == '(' )
    {
        if ( *++m_an == '=' )
        {
            if ( *++m_an == '=' )
            {
                if ( *++m_an == ')' )
                {
                    // "(==)" draw offer accepted

                    if ( cc_check_valid_draw_offer_exists( game->moves, game->status ) )
                    {
                        return cc_check_pre_plies_status( *++m_an, game, parse_msgs__io, false, true, false,
                                                          CC_MAX_LEN_ZERO_TERMINATED,
                                                          "Invalid char(s) after accepted draw.\n" );
                    }
                    else
                    {
                        cc_parse_msg_append_format_if( parse_msgs__io,
                                                       CC_PMTE_Error,
                                                       CC_MAX_LEN_ZERO_TERMINATED,
                                                       "No valid opponent's draw offer found.\n" );
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
                //         return cc_check_pre_plies_status( *++m_an, game, parse_msgs__io, false, true, false,
                //                                           CC_MAX_LEN_ZERO_TERMINATED,
                //                                           "Invalid char(s) after draw by rules.\n" );
                //     }
                // }
            }
        }

        cc_parse_msg_append_format_if( parse_msgs__io,
                                       CC_PMTE_Error,
                                       CC_MAX_LEN_ZERO_TERMINATED,
                                       "Invalid char(s) within draw; draw offer cannot be issued standalone; draw-by-rules only by arbiter, not players.\n" );
        return false;
    }





// TODO :: post-plies status



    return true;
}
