// Copyright (c) 2021, 2022 Mario Mlačak, mmlacak@gmail.com
// Licensed under GNU GPL v3+ license. See LICENSING, COPYING files for details.

#include <ctype.h>
// #include <stdarg.h>

// #include <string.h>
// #include <stdio.h> // TODO :: TEMP :: DEBUG :: printf

#include "cc_rules_misc.h"
#include "cc_parse_ply.h"
#include "cc_parse_move.h"


static bool _cc_check_standalone_status( char const char_an,
                                         CcMove ** temp__n,
                                         CcMove ** move__o,
                                         CcParseMsg ** parse_msgs__iod,
                                         CcMoveStatusEnum mse,
                                         size_t max_len__d,
                                         char const * msg, ... ) {
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


bool cc_parse_move( char const * move_an,
                    CcGame * game,
                    CcMove ** move__o,
                    CcParseMsg ** parse_msgs__iod ) {
    if ( !move_an ) return false;
    if ( !game ) return false;
    if ( !game->chessboard ) return false;
    // if ( !game->moves ) return false; // Currently not initialized.
    if ( !move__o || *move__o ) return false;
    if ( !parse_msgs__iod ) return false;

    if ( !CC_GAME_STATUS_IS_TURN( game->status ) ) {
        char const * msg =
            ( game->status == CC_GSE_None ) ? "Game is not initialized.\n"
                                            : "Game is finished.\n";

        cc_parse_msg_append_fmt( parse_msgs__iod, CC_PMTE_Error, CC_MAX_LEN_ZERO_TERMINATED, msg );
        return false;
    }

    bool is_turn_light = ( game->status == CC_GSE_Turn_Light );

    cc_uint_t board_size = cc_variant_board_size( game->chessboard->type );
    if ( !CC_IS_BOARD_SIZE_VALID( board_size ) ) return false;

    CcMove * move__t = cc_move__new( move_an, CC_MAX_LEN_ZERO_TERMINATED, NULL, CC_MSE_None );
    if ( !move__t ) return false;

    char const * m_an = move_an;

    if ( *m_an == '#' ) {
        if ( *++m_an == '#' ) {
            // "##" resign
            return _cc_check_standalone_status( *++m_an, &move__t, move__o, parse_msgs__iod, CC_MSE_Resign, CC_MAX_LEN_ZERO_TERMINATED, "Invalid char(s) after resign.\n" );
        } else {
            // "#" self-checkmate

            // TODO :: Do check if opponent is really (self-)checkmated.
            //         Self- is optional, since both players could overlook checkmate,
            //         this is option to rectify such a situation.

            return _cc_check_standalone_status( *m_an, &move__t, move__o, parse_msgs__iod, CC_MSE_SelfCheckmate, CC_MAX_LEN_ZERO_TERMINATED, "Invalid char(s) after self-checkmate.\n" );
        }
    }

    if ( *m_an == '(' ) {
        if ( *++m_an == '=' ) {
            if ( *++m_an == '=' ) {
                if ( *++m_an == ')' ) {
                    // "(==)" draw offer accepted

                    if ( cc_check_valid_draw_offer_exists( game->moves, game->status ) ) {
                        return _cc_check_standalone_status( *++m_an, &move__t, move__o, parse_msgs__iod, CC_MSE_DrawAccepted, CC_MAX_LEN_ZERO_TERMINATED, "Invalid char(s) after accepted draw.\n" );
                    } else {
                        cc_parse_msg_append_fmt( parse_msgs__iod, CC_PMTE_Error, CC_MAX_LEN_ZERO_TERMINATED, "No valid opponent's draw offer found.\n" );
                        return false;
                    }
                }
                // Draw-by-rules should be issued by arbiter, not players;
                //     i.e. should be issued by server, not clients.
                //
                // else if ( *m_an == '=' )
                // {
                //     if ( *++m_an == ')' )
                //     {
                //         // "(===)" draw by rules
                //
                //         return _cc_check_standalone_status( *++m_an,
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

    if ( !cc_parse_plies( move__t->notation, is_turn_light, board_size,
                          &plies__t,
                          parse_msgs__iod ) ) {
        cc_ply_free_all( &plies__t );
        cc_move_free_all( &move__t );
        return false;
    }


    // { // TODO :: DEBUG :: DELETE
    //     char * plies_str__a = cc_ply_all_to_string__new( plies__t );

    //     cc_str_print( plies_str__a, NULL, 0, "Plies: '%s'.\n", 0, NULL );

    //     CC_FREE( plies_str__a );
    // } // TODO :: DEBUG :: DELETE


    //
    // Post-plies status.

    while ( *m_an != '\0' ) {
        if ( ( *m_an == '+' ) || ( *m_an == '#' ) || ( *m_an == '(' ) )
            break;

        ++m_an;
    }

    if ( *m_an == '#' ) {
        // # checkmate
        move__t->status = CC_MSE_Checkmate;
    } else if ( *m_an == '+' ) {
        if ( *++m_an == '+' ) {
            // ++ checkmate
            move__t->status = CC_MSE_Checkmate;
        } else {
            // + check
            move__t->status = CC_MSE_Check;
        }
    }

    if ( ( move__t->status == CC_MSE_None ) || ( move__t->status == CC_MSE_Check ) ) {
        if ( *m_an == '(' ) {
            if ( *++m_an == '=' ) {
                if ( *++m_an == ')' ) {
                    // (=) draw offer
                    if ( move__t->status == CC_MSE_Check )
                        move__t->status = CC_MSE_Check_DrawOffer;
                    else
                        move__t->status = CC_MSE_DrawOffer;
                }
            } else if ( *m_an == ')' ) {
                // () draw offer withdrawn
                if ( move__t->status == CC_MSE_Check )
                    move__t->status = CC_MSE_Check_DrawOffer_Revoked;
                else
                    move__t->status = CC_MSE_DrawOffer_Revoked;
            }
        }
    }

    //
    // Ownership transfer.

    move__t->plies = plies__t; // Ownership transfer.
    // plies__t = NULL; // Not really needed.

    *move__o = move__t; // Ownership transfer.
    // move__t = NULL; // Not really needed.

    return true;
}
