// Copyright (c) 2021 Mario Mlačak, mmlacak@gmail.com
// Licensed under GNU GPL v3+ license. See LICENSING, COPYING files for details.

// // #include <stdbool.h>
// #include <ctype.h>
// // #include <stdlib.h>
// // #include <string.h>
// // #include <stdio.h>

// #include "cc_defines.h"
// #include "cc_rule_steps.h"

// #include "cc_parse_msg.h"
// #include "cc_parse_utils.h"
// #include "cc_parse_move.h"


// bool cc_parse_move( char const * restrict move_str,
//                     CcGame * restrict game,
//                     CcMove ** restrict move__o,
//                     CcParseMsg ** restrict parse_msgs__io )
// {
//     if ( !move_str ) return false;
//     if ( !game ) return false;
//     if ( !move__o ) return false;
//     if ( *move__o ) return false;
//     if ( !parse_msgs__io ) return false;

//     if ( !CC_GAME_STATUS_IS_TURN( game->status ) )
//     {
//         cc_parse_msg_append_or_init_format( parse_msgs__io,
//                                             CC_PME_Error,
//                                             "Cannot make a move in a finished game." );
//         return false;
//     }

//     CcChessboard * cb = game->chessboard;

//     char * ply_an__t = NULL;
//     if ( !cc_parse_utils_ply_str_iter_new( move_str, &ply_an__t, true ) )
//     {
//         cc_parse_msg_append_or_init_format( parse_msgs__io,
//                                             CC_PME_Error,
//                                             "Ply not found in '%s'.",
//                                             move_str );
//         return false;
//     }

//     CcPlyLinkEnum ple = CC_PLE_Ply;
//     char piece_symbol = ' ';
//     CcPieceEnum pe = CC_PE_None;
//     CcPly * plies__t = NULL;

//     // First piece in a first ply *always* has the color of a player on-turn.
// // TODO :: not exactly --> Monolith
//     bool is_piece_light = CC_GAME_STATUS_IS_LIGHT_TURN( game->status );

//     char const * steps_str = NULL;

//     do
//     {
//         if ( !cc_parse_utils_get_ply_link( ply_an__t, &ple ) )
//         {
//             cc_parse_msg_append_or_init_format( parse_msgs__io,
//                                                 CC_PME_Error,
//                                                 "Ply link not found in '%s', within '%s'.",
//                                                 ply_an__t,
//                                                 move_str );
//             CC_FREE( ply_an__t );

//             return cc_game_move_data_free_all( NULL, NULL, NULL, &plies__t, NULL, false );
//         }

//         if ( !cc_parse_utils_get_ply_piece_symbol( ply_an__t, &piece_symbol ) )
//         {
//             cc_parse_msg_append_or_init_format( parse_msgs__io,
//                                                 CC_PME_Error,
//                                                 "Piece symbol not found in '%s', within '%s'.",
//                                                 ply_an__t,
//                                                 move_str );
//             CC_FREE( ply_an__t );

//             return cc_game_move_data_free_all( NULL, NULL, NULL, &plies__t, NULL, false );
//         }

//         steps_str = cc_parse_utils_get_steps_str( ply_an__t );
//         if ( !steps_str )
//         {
//             cc_parse_msg_append_or_init_format( parse_msgs__io,
//                                                 CC_PME_Error,
//                                                 "Step(s) not found in '%s', within '%s'.",
//                                                 ply_an__t,
//                                                 move_str );
//             CC_FREE( ply_an__t );

//             return cc_game_move_data_free_all( NULL, NULL, NULL, &plies__t, NULL, false );
//         }

//         char * step_an__t = cc_parse_utils_next_step_str_new( steps_str );
//         if ( !step_an__t )
//         {
//             cc_parse_msg_append_or_init_format( parse_msgs__io,
//                                                 CC_PME_Error,
//                                                 "Step not found in '%s', within '%s'.",
//                                                 ply_an__t,
//                                                 move_str );
//             CC_FREE( ply_an__t );

//             return cc_game_move_data_free_all( NULL, NULL, NULL, &plies__t, NULL, false );
//         }

//         CcStepLinkEnum sle = CC_SLE_Destination;
//         CcStep * steps__t = NULL;
//         int disamb_i = CC_INVALID_OFF_BOARD_COORD_MIN;
//         int disamb_j = CC_INVALID_OFF_BOARD_COORD_MIN;

//         do
//         {
//             if ( !cc_parse_utils_get_step_link( ply_an__t, step_an__t, &sle ) )
//             {
//                 cc_parse_msg_append_or_init_format( parse_msgs__io,
//                                                     CC_PME_Error,
//                                                     "Step link not found in '%s', within '%s', within '%s'.",
//                                                     step_an__t,
//                                                     ply_an__t,
//                                                     move_str );
//                 CC_FREE( step_an__t );
//                 CC_FREE( ply_an__t );

//                 return cc_game_move_data_free_all( NULL, NULL, NULL, &plies__t, &steps__t, false );
//             }

//             char * fields_an__t = cc_parse_utils_step_fields_str_new( step_an__t );
//             if ( !fields_an__t )
//             {
//                 cc_parse_msg_append_or_init_format( parse_msgs__io,
//                                                     CC_PME_Error,
//                                                     "Movement not found in '%s', within '%s', within '%s'.",
//                                                     step_an__t,
//                                                     ply_an__t,
//                                                     move_str );
//                 CC_FREE( step_an__t );
//                 CC_FREE( ply_an__t );

//                 return cc_game_move_data_free_all( NULL, NULL, NULL, &plies__t, &steps__t, false );
//             }

//             int disamb_step_i;
//             int disamb_step_j;
//             int step_i;
//             int step_j;

//             if ( !cc_parse_utils_get_fields( fields_an__t,
//                                              cb,
//                                              &disamb_step_i,
//                                              &disamb_step_j,
//                                              &step_i,
//                                              &step_j ) )
//             {
//                 cc_parse_msg_append_or_init_format( parse_msgs__io,
//                                                     CC_PME_Error,
//                                                     "Movement not found in '%s', within '%s', within '%s', within '%s'.",
//                                                     fields_an__t,
//                                                     step_an__t,
//                                                     ply_an__t,
//                                                     move_str );
//                 CC_FREE( fields_an__t );
//                 CC_FREE( step_an__t );
//                 CC_FREE( ply_an__t );

//                 return cc_game_move_data_free_all( NULL, NULL, NULL, &plies__t, &steps__t, false );
//             }

//             if ( cc_chessboard_is_coord_on_board( game->chessboard, disamb_step_i )
//               || cc_chessboard_is_coord_on_board( game->chessboard, disamb_step_j ) )
//             {
//                 if ( cc_chessboard_is_coord_on_board( game->chessboard, disamb_i )
//                   || cc_chessboard_is_coord_on_board( game->chessboard, disamb_j ) )
//                 {
//                     cc_parse_msg_append_or_init_format( parse_msgs__io,
//                                                         CC_PME_Error,
//                                                         "More than one disambiguation encountered, in '%s', within '%s', within '%s', within '%s'.",
//                                                         fields_an__t,
//                                                         step_an__t,
//                                                         ply_an__t,
//                                                         move_str );
//                     CC_FREE( fields_an__t );
//                     CC_FREE( step_an__t );
//                     CC_FREE( ply_an__t );

//                     return cc_game_move_data_free_all( NULL, NULL, NULL, &plies__t, &steps__t, false );
//                 }
//                 else
//                 {
//                     disamb_i = disamb_step_i;
//                     disamb_j = disamb_step_j;
//                 }
//             }

//             CC_FREE_NULL( &fields_an__t );

//             char const * side_effects = cc_parse_utils_side_effect_str( step_an__t );
//             if ( !side_effects )
//             {
//                 cc_parse_msg_append_or_init_format( parse_msgs__io,
//                                                     CC_PME_Error,
//                                                     "Side-effects not found in '%s', within '%s', within '%s'.",
//                                                     step_an__t,
//                                                     ply_an__t,
//                                                     move_str );
//                 CC_FREE( step_an__t );
//                 CC_FREE( ply_an__t );

//                 return cc_game_move_data_free_all( NULL, NULL, NULL, &plies__t, &steps__t, false );
//             }

//             CcSideEffect se = cc_side_effect_none();
//             if ( !cc_parse_utils_get_side_effect( step_an__t,
//                                                   cb,
//                                                   pe,
//                                                   step_i,
//                                                   step_j,
//                                                   &se ) )
//             {
//                 cc_parse_msg_append_or_init_format( parse_msgs__io,
//                                                     CC_PME_Error,
//                                                     "Side-effects not found in '%s', within '%s', within '%s', within '%s'.",
//                                                     side_effects,
//                                                     step_an__t,
//                                                     ply_an__t,
//                                                     move_str );
//                 CC_FREE( step_an__t );
//                 CC_FREE( ply_an__t );

//                 return cc_game_move_data_free_all( NULL, NULL, NULL, &plies__t, &steps__t, false );
//             }

//             if ( !cc_step_append_or_init( &steps__t, sle, step_i, step_j, se, CC_FSUE_User ) )
//             {
//                 CC_FREE( step_an__t );
//                 CC_FREE( ply_an__t );

//                 return cc_game_move_data_free_all( NULL, NULL, NULL, &plies__t, &steps__t, false );
//             }

//             CC_FREE( step_an__t );
//             step_an__t = cc_parse_utils_next_step_str_new( NULL );
//         }
//         while ( step_an__t );

// // TODO :: USE :: disamb_i, disamb_j

// // TODO :: compare is_piece_light to actual piece, on actual starting position


// // // TODO :: FIX :: is_piece_light !!!
// //         is_piece_light = !is_piece_light;
// // // TODO :: FIX :: is_piece_light !!!

// // TODO :: FIX :: is_piece_light --> piece_symbol --> pe !!!
//         if ( !cc_ply_append_or_init( &plies__t, ple, pe, &steps__t ) )
//         {
//             CC_FREE( ply_an__t );

//             return cc_game_move_data_free_all( NULL, NULL, NULL, &plies__t, &steps__t, false );
//         }
// // TODO :: FIX :: is_piece_light --> piece_symbol --> pe !!!

//         CC_FREE( ply_an__t );
//     }
//     while ( cc_parse_utils_ply_str_iter_new( move_str, &ply_an__t, false ) );

//     CcMoveStatusEnum mse = CC_MSE_None;

//     *move__o = cc_move_on_new( move_str, CC_MPSE_None, &plies__t, mse );

// TODO
//     return true;
// }
