// Copyright (c) 2021 Mario Mlačak, mmlacak@gmail.com
// Licensed under GNU GPL v3+ license. See LICENSING, COPYING files for details.

// #include <stdbool.h>
#include <ctype.h>
// #include <stdlib.h>
// #include <string.h>
// #include <stdio.h>

#include "cc_rules.h"

#include "cc_parse_msg.h"
#include "cc_parse_utils.h"
#include "cc_parse_move.h"


// CcPly * cc_parse_ply( char const * const restrict ply_str,
//                       CcChessboard const * const restrict cb,
//                       CcParseMsg ** parse_msgs_io )
// {
//     if ( !ply_str ) return NULL;
//     if ( !cb ) return NULL;
//     if ( !parse_msgs_io ) return NULL;


// // TODO
//     return NULL;
// }

bool cc_parse_move( char const * const restrict move_str,
                    CcGame const * const restrict game,
                    CcMove ** const restrict move_o,
                    CcParseMsg ** const restrict parse_msgs_io )
{
    if ( !move_str ) return false;
    if ( !game ) return false;
    if ( !move_o ) return false;
    if ( !parse_msgs_io ) return false;

    if ( !CC_GAME_STATUS_IS_TURN( game->status ) ) return false;

    CcChessboard const * const cb = game->chessboard;

    char * ply_an__o = cc_parse_utils_next_ply_str_new( move_str );
    if ( !ply_an__o ) return false;

    CcPlyLinkEnum ple = CC_PLE_Ply;
    char piece_symbol = ' ';
    CcPieceEnum pe = CC_PE_None;

    // First piece in a first ply *always* has the color of a player on-turn.
// TODO :: FIX :: is_piece_light !!!
    bool is_piece_light = CC_GAME_STATUS_IS_LIGHT_TURN( game->status );
// TODO :: FIX :: is_piece_light !!!

    char const * steps_str = NULL;

    do
    {
        if ( !cc_parse_utils_get_ply_link( ply_an__o, &ple ) )
        {
            cc_parse_msg_init_or_append_format( parse_msgs_io,
                                                CC_PME_Error,
                                                "Ply link not found in '%s'.",
                                                ply_an__o );
            free( ply_an__o );
            return false;
        }

        if ( !cc_parse_utils_get_ply_piece_symbol( ply_an__o, &piece_symbol ) )
        {
            cc_parse_msg_init_or_append_format( parse_msgs_io,
                                                CC_PME_Error,
                                                "Piece symbol not found in '%s'.",
                                                ply_an__o );
            free( ply_an__o );
            return false;
        }

        steps_str = cc_parse_utils_get_steps_str( ply_an__o );
        if ( steps_str )
        {
            char * step_an__o = cc_parse_utils_next_step_str_new( steps_str );
            CcStepLinkEnum sle = CC_SLE_Destination;

            if ( step_an__o )
            {
                do
                {
                    if ( !cc_parse_utils_get_step_link( ply_an__o, step_an__o, &sle ) )
                    {
                        cc_parse_msg_init_or_append_format( parse_msgs_io,
                                                            CC_PME_Error,
                                                            "Step link not found in '%s', within '%s'.",
                                                            step_an__o,
                                                            ply_an__o );
                        free( step_an__o );
                        free( ply_an__o );
                        return false;
                    }

                    char * fields_an__o = cc_parse_utils_step_fields_str_new( step_an__o );
                    if ( !fields_an__o )
                    {
                        cc_parse_msg_init_or_append_format( parse_msgs_io,
                                                            CC_PME_Error,
                                                            "Movement not found in '%s', within '%s'.",
                                                            step_an__o,
                                                            ply_an__o );
                        free( step_an__o );
                        free( ply_an__o );
                        return false;
                    }

                    int disamb_step_i;
                    int disamb_step_j;
                    int step_i;
                    int step_j;

                    if ( !cc_parse_utils_get_fields( fields_an__o,
                                                     cb,
                                                     &disamb_step_i,
                                                     &disamb_step_j,
                                                     &step_i,
                                                     &step_j ) )
                    {
                        cc_parse_msg_init_or_append_format( parse_msgs_io,
                                                            CC_PME_Error,
                                                            "Movement not found in '%s', within '%s', within '%s'.",
                                                            fields_an__o,
                                                            step_an__o,
                                                            ply_an__o );
                        free( fields_an__o );
                        free( step_an__o );
                        free( ply_an__o );
                        return false;
                    }

                    free( fields_an__o );
                    fields_an__o = NULL;

                    char const * side_effects = cc_parse_utils_side_effect_str( step_an__o );
                    if ( !side_effects )
                    {
                        cc_parse_msg_init_or_append_format( parse_msgs_io,
                                                            CC_PME_Error,
                                                            "Side-effects not found in '%s', within '%s'.",
                                                            step_an__o,
                                                            ply_an__o );
                        free( step_an__o );
                        free( ply_an__o );
                        return false;
                    }

                    CcSideEffect se = cc_side_effect_none();
                    if ( !cc_parse_utils_get_side_effect( step_an__o,
                                                          cb,
                                                          pe,
                                                          step_i,
                                                          step_j,
                                                          &se ) )
                    {
                        cc_parse_msg_init_or_append_format( parse_msgs_io,
                                                            CC_PME_Error,
                                                            "Side-effects not found in '%s', within '%s', within '%s'.",
                                                            side_effects,
                                                            step_an__o,
                                                            ply_an__o );
                        free( step_an__o );
                        free( ply_an__o );
                        return false;
                    }

                    free( step_an__o );
                    step_an__o = cc_parse_utils_next_step_str_new( NULL );
                }
                while ( step_an__o );
            }
        }


// // TODO :: FIX :: is_piece_light !!!
//         is_piece_light = !is_piece_light;
// // TODO :: FIX :: is_piece_light !!!

        free( ply_an__o );
        ply_an__o = cc_parse_utils_next_ply_str_new( NULL );
    }
    while ( ply_an__o );



// TODO
    return true;
}
