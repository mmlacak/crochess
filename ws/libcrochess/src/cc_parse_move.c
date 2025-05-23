// Copyright (c) 2021, 2022 Mario Mlačak, mmlacak@gmail.com
// Licensed under GNU GPL v3+ license. See LICENSING, COPYING files for details.

#include <ctype.h>
// #include <stdarg.h>

// #include <string.h>

#include "cc_defines.h"
#include "cc_checks.h"
#include "cc_parse_ply.h"
#include "cc_parse_move.h"


static CcMaybeBoolEnum _cc_parse_standalone_status( char const * move_an,
                                                    CcMove ** move__io,
                                                    CcParseMsg ** parse_msgs__iod ) {
    // result <-- standalone move status:
    // CC_MBE_Void <-- not encountered && it's ok
    // CC_MBE_False <-- encountered && error
    // CC_MBE_True <-- encountered && parsed successfully
    CcMaybeBoolEnum result = CC_MBE_Void;

    CcMove * move__t = *move__io;
    char const * m_an = move_an;

    if ( *m_an == '#' ) {
        if ( *++m_an == '#' ) {
            // "##" resign
            move__t->status = CC_MSE_Resign;
            ++m_an;
        } else {
            // "#" self-checkmate
            move__t->status = CC_MSE_SelfCheckmate;
        }

        result = CC_MBE_True;
    }

    if ( *m_an == '(' ) {
        result = CC_MBE_False;

        if ( move__t->status == CC_MSE_None ) {
            if ( *++m_an == '=' ) {
                if ( *++m_an == '=' ) {
                    if ( *++m_an == ')' ) {
                        // "(==)" draw offer accepted
                        move__t->status = CC_MSE_DrawAccepted;
                        result = CC_MBE_True;
                        ++m_an;
                    }
                    // Draw-by-rules should be issued by arbiter, not players;
                    //     i.e. should be issued by server, not clients.
                    //
                    // else if ( *m_an == '=' ) {
                    //     if ( *++m_an == ')' ) {
                    //         // "(===)" draw by rules
                    //     }
                    // }
                }
            }
        }
    }

    if ( result == CC_MBE_True )
        if ( ( *m_an != ' ' ) && ( *m_an != '_' ) && ( *m_an != '\0' ) )
            result = CC_MBE_False;

    if ( result == CC_MBE_False )
        cc_parse_msg_append_fmt( parse_msgs__iod, CC_PMTE_Error, CC_MAX_LEN_BUFFER, "Malformed standalone status, in move '%s'.\n", move_an );

    return result;
}

static CcMaybeBoolEnum _cc_parse_move_status( char const * move_an,
                                              CcMove ** move__io,
                                              CcParseMsg ** parse_msgs__iod ) {
    if ( ( *move__io )->status != CC_MSE_None ) // Move containing standalone status shouldn't have plies parsed, let alone post-plies status.
        return CC_MBE_False;

    // result <-- (post-plies) move status:
    // CC_MBE_Void <-- not encountered && it's ok
    // CC_MBE_False <-- encountered && error
    // CC_MBE_True <-- encountered && parsed successfully
    CcMaybeBoolEnum result = CC_MBE_Void;

    CcMove * move__t = *move__io;
    char const * m_an = move_an;

    while ( *m_an != '\0' ) {
        if ( ( *m_an == '+' ) || ( *m_an == '#' ) || ( *m_an == '(' ) )
            break;

        ++m_an;
    }

    char const * status_an = m_an;

    if ( *m_an == '#' ) {
        // # checkmate
        move__t->status = CC_MSE_Checkmate;
        result = CC_MBE_True;
        ++m_an;
    } else if ( *m_an == '+' ) {
        if ( *++m_an == '+' ) {
            // ++ checkmate
            move__t->status = CC_MSE_Checkmate;
            ++m_an;
        } else {
            // + check
            move__t->status = CC_MSE_Check;
        }

        result = CC_MBE_True;
    }

    if ( ( move__t->status == CC_MSE_None ) ||
         ( move__t->status == CC_MSE_Check ) ||
         ( move__t->status == CC_MSE_Checkmate ) ) {
        if ( *m_an == '(' ) {
            result = CC_MBE_False;

            if ( move__t->status != CC_MSE_Checkmate ) {
                if ( *++m_an == '=' ) {
                    if ( *++m_an == ')' ) {
                        // (=) draw offer
                        if ( move__t->status == CC_MSE_Check )
                            move__t->status = CC_MSE_Check_DrawOffer;
                        else
                            move__t->status = CC_MSE_DrawOffer;

                        result = CC_MBE_True;
                        ++m_an;
                    }
                } else if ( *m_an == ')' ) {
                    // () draw offer withdrawn
                    if ( move__t->status == CC_MSE_Check )
                        move__t->status = CC_MSE_Check_DrawOffer_Revoked;
                    else
                        move__t->status = CC_MSE_DrawOffer_Revoked;

                    result = CC_MBE_True;
                    ++m_an;
                }
            }
        }
    }

    if ( result == CC_MBE_True )
        if ( ( *m_an != ' ' ) && ( *m_an != '_' ) && ( *m_an != '\0' ) )
            result = CC_MBE_False;

    if ( result == CC_MBE_False )
        cc_parse_msg_append_fmt( parse_msgs__iod, CC_PMTE_Error, CC_MAX_LEN_BUFFER, "Malformed (or standalone) move status '%s' encountered after plies; in move '%s'.\n", status_an, move_an );

    return result;
}


bool cc_parse_move( char const * move_an,
                    CcGame * game,
                    CcMove ** move__o,
                    CcParseMsg ** parse_msgs__iod ) {
    if ( !move_an ) return false;
    if ( !game ) return false;
    if ( !game->chessboard ) return false;
    // if ( !game->moves ) return false; // Might not be initialized.
    if ( !move__o || *move__o ) return false;
    if ( !parse_msgs__iod ) return false;

    if ( !CC_GAME_STATUS_IS_TURN( game->status ) ) {
        char const * msg =
            ( game->status == CC_GSE_None ) ? "Game is not initialized.\n"
                                            : "Game is finished.\n";

        cc_parse_msg_append_fmt( parse_msgs__iod, CC_PMTE_Error, CC_MAX_LEN_BUFFER, msg );
        return false;
    }

    bool is_turn_light = ( game->status == CC_GSE_Turn_Light );

    cc_uint_t board_size = cc_variant_board_size( game->chessboard->type );
    if ( !CC_IS_BOARD_SIZE_VALID( board_size ) ) return false;

    CcMove * move__t = cc_move__new( move_an, CC_MAX_LEN_BUFFER, NULL, CC_MSE_None );
    if ( !move__t ) return false;

    //
    // Parsing standalone status.

    // result <-- standalone move status:
    // CC_MBE_Void <-- not encountered && it's ok
    // CC_MBE_False <-- encountered && error
    // CC_MBE_True <-- encountered && parsed successfully
    CcMaybeBoolEnum result_sms = _cc_parse_standalone_status( move_an, &move__t, parse_msgs__iod );

    if ( result_sms == CC_MBE_True ) {
        *move__o = move__t; // Ownership transfer.
        // move__t = NULL; // Not really needed.

        return true;
    } else if ( result_sms == CC_MBE_False ) {
        cc_move_free_all( &move__t );
        return false;
    }

    //
    // Parsing plies.

    CcPly * plies__t = NULL;

    if ( !cc_parse_plies( move__t->notation, is_turn_light, board_size,
                          &plies__t,
                          parse_msgs__iod ) ) {
        cc_ply_free_all( &plies__t );
        cc_move_free_all( &move__t );
        return false;
    }

    #ifdef __CC_DEBUG__
    {
        char * plies_str__a = cc_ply_all_to_string__new( plies__t );

        CC_STR_PRINT( plies_str__a, NULL, 0, "Plies: '%s'.\n", 0, NULL );

        CC_FREE( plies_str__a );
    }
    #endif // __CC_DEBUG__

    move__t->plies = plies__t; // Ownership transfer.
    // plies__t = NULL; // Not really needed.

    //
    // Post-plies status.

    if ( _cc_parse_move_status( move_an, &move__t, parse_msgs__iod ) == CC_MBE_False ) {
        cc_move_free_all( &move__t );
        return false;
    }

    //
    // Ownership transfer.

    *move__o = move__t; // Ownership transfer.
    // move__t = NULL; // Not really needed.

    return true;
}
