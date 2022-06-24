// Copyright (c) 2021, 2022 Mario Mlačak, mmlacak@gmail.com
// Licensed under GNU GPL v3+ license. See LICENSING, COPYING files for details.

#include <ctype.h>
#include <stdarg.h>

#include "cc_str_utils.h"
#include "cc_rules.h"


static bool cc_check_move_precondition( char const char_an,
                                        CcGame * restrict game__io,
                                        CcParseMsg ** restrict parse_msgs__io,
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
        game__io->status = cc_game_status_next( game__io->status, true, true );
        return true;
    }
    else
    {
        va_list args;
        va_start( args, msg );

        cc_parse_msg_append_or_init_format( parse_msgs__io, CC_PMTE_Error, max_len__d, msg, args );

        va_end( args );

        return false;
    }
}


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
        if ( *++m == '#' )
        {
            return cc_check_move_precondition( *++m, g, parse_msgs__io,
                                               CC_MAX_LEN_ZERO_TERMINATED,
                                               "Invalid char(s) after resign." );
        }

        return cc_check_move_precondition( *m, g, parse_msgs__io,
                                           CC_MAX_LEN_ZERO_TERMINATED,
                                           "Invalid char(s) after self-checkmate." );
    }

    if ( *m == '(' )
    {
        if ( *++m == '=' )
        {
            if ( *++m == '=' )
            {
                if ( *++m == ')' )
                {
                    return cc_check_move_precondition( *++m, g, parse_msgs__io,
                                                       CC_MAX_LEN_ZERO_TERMINATED,
                                                       "Invalid char(s) after accepted draw." );
                }
                else if ( *m == '=' )
                {
                    if ( *++m == ')' )
                    {
                        return cc_check_move_precondition( *++m, g, parse_msgs__io,
                                                           CC_MAX_LEN_ZERO_TERMINATED,
                                                           "Invalid char(s) after draw by rules." );
                    }
                }
            }
        }

        cc_parse_msg_append_or_init_format( parse_msgs__io,
                                            CC_PMTE_Error,
                                            CC_MAX_LEN_ZERO_TERMINATED,
                                            "Invalid char(s) within draw." );
        return false;
    }



    // TODO


    return false;
}
