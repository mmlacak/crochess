// Copyright (c) 2021, 2022 Mario Mlačak, mmlacak@gmail.com
// Licensed under GNU GPL v3+ license. See LICENSING, COPYING files for details.

#include <ctype.h>
#include <stdarg.h>

#include <string.h>
#include <stdio.h>

#include "cc_str_utils.h"
#include "cc_parse.h"
#include "cc_rules_misc.h"
#include "cc_rules.h"


static bool cc_check_pre_plies_status( char const char_an,
                                       CcGame * restrict game__io,
                                       CcParseMsgs ** restrict parse_msgs__io,
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

        cc_parse_msgs_append_or_init_format( parse_msgs__io, CC_PMTE_Error, max_len__d, msg, args );

        va_end( args );

        return false;
    }
}


bool cc_make_move( char const * restrict move_an_str,
                   CcGame ** restrict game__io,
                   CcParseMsgs ** restrict parse_msgs__io )
{
    if ( !move_an_str ) return false;
    if ( !parse_msgs__io ) return false;

    if ( !game__io ) return false;
    if ( !*game__io ) return false;

    CcGame * g = *game__io;

    if ( !g->chessboard ) return false;
    // if ( !g->moves ) return false;

    if ( !CC_GAME_STATUS_IS_TURN( g->status ) )
    {
        char const * msg =
            ( g->status == CC_GSE_None ) ? "Game is not initialized.\n"
                                         : "Game is finished.\n";

        cc_parse_msgs_append_or_init_format( parse_msgs__io,
                                             CC_PMTE_Error,
                                             CC_MAX_LEN_ZERO_TERMINATED,
                                             msg );
        return false;
    }

    char const * m = move_an_str;

    if ( *m == '#' )
    {
        if ( *++m == '#' )
        {
            // "##" resign
            return cc_check_pre_plies_status( *++m, g, parse_msgs__io, true, true, false,
                                              CC_MAX_LEN_ZERO_TERMINATED,
                                              "Invalid char(s) after resign.\n" );
        }
        else
        {
            // "#" self-checkmate

// TODO :: Do check if opponent is really (self-)checkmated.
//         Self- is optional, since both players could overlook checkmate,
//         this is option to rectify such situation.

            return cc_check_pre_plies_status( *m, g, parse_msgs__io, false, true, true,
                                              CC_MAX_LEN_ZERO_TERMINATED,
                                              "Invalid char(s) after self-checkmate.\n" );
        }
    }

    if ( *m == '(' )
    {
        if ( *++m == '=' )
        {
            if ( *++m == '=' )
            {
                if ( *++m == ')' )
                {
                    // "(==)" draw offer accepted

                    if ( cc_check_valid_draw_offer_exists( g->moves, g->status ) )
                    {
                        return cc_check_pre_plies_status( *++m, g, parse_msgs__io, false, true, false,
                                                          CC_MAX_LEN_ZERO_TERMINATED,
                                                          "Invalid char(s) after accepted draw.\n" );
                    }
                    else
                    {
                        cc_parse_msgs_append_or_init_format( parse_msgs__io,
                                                             CC_PMTE_Error,
                                                             CC_MAX_LEN_ZERO_TERMINATED,
                                                             "No valid opponent's draw offer found.\n" );
                        return false;
                    }
                }
                // <i> Draw-by-rules should be issued by arbiter, not players;
                //     i.e. should be issued by server, not clients.
                //
                // else if ( *m == '=' )
                // {
                //     if ( *++m == ')' )
                //     {
                //         // "(===)" draw by rules
                //
                //         return cc_check_pre_plies_status( *++m, g, parse_msgs__io, false, true, false,
                //                                           CC_MAX_LEN_ZERO_TERMINATED,
                //                                           "Invalid char(s) after draw by rules.\n" );
                //     }
                // }
            }
        }

        cc_parse_msgs_append_or_init_format( parse_msgs__io,
                                             CC_PMTE_Error,
                                             CC_MAX_LEN_ZERO_TERMINATED,
                                             "Invalid char(s) within draw; draw offer cannot be issued standalone; draw-by-rules only by arbiter, not players.\n" );
        return false;
    }

    char const * start = NULL;
    char const * end = NULL;

    CcPlyLinkEnum ple = CC_PLE_StartingPly;
    char piece_symbol = ' ';
    CcPieceEnum piece = CC_PE_None;
    char const * c = NULL;

    printf( " --- --- ---\n" );
    while ( cc_ply_iter( m, &start, &end ) )
    {
        cc_str_print( start, end, 8192, "Ply: '%s'.\n", "" );

        ple = cc_starting_ply_link( start );
        c = start + cc_ply_link_len( ple );

        cc_str_print( start, c, 128, "Ply link: '%s'", " --> %d.\n", ple );

        if ( cc_ply_piece_symbol( c, &piece_symbol ) )
        {
            piece = cc_piece_from_symbol( piece_symbol, CC_GAME_STATUS_IS_LIGHT_TURN( g->status ) );

            printf( "Piece: '%c' --> %d.\n", piece_symbol, piece );
        }
        else
        {
            cc_parse_msgs_append_or_init_format( parse_msgs__io,
                                                 CC_PMTE_Error,
                                                 CC_MAX_LEN_ZERO_TERMINATED,
                                                 "Invalid piece symbol '%c'.\n",
                                                 piece_symbol );
            return false;
        }


        printf( " ... ... ...\n" );
    }
    printf( " --- --- ---\n" );

// TODO :: loop over plies


// TODO :: post-plies status



    if ( !cc_moves_append_or_init( &( g->moves ), move_an_str, CC_MAX_LEN_ZERO_TERMINATED ) )
        return false;

// TODO :: determine ending status
// TODO :: determine winning status
    g->status = cc_game_status_next( g->status, false, false );

    return true;
}
