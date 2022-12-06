// Copyright (c) 2021, 2022 Mario Mlačak, mmlacak@gmail.com
// Licensed under GNU GPL v3+ license. See LICENSING, COPYING files for details.

// #include <ctype.h>
// #include <stdarg.h>

// #include <string.h>
// #include <stdio.h>

// #include "cc_defines.h"
// #include "cc_str_utils.h"
// #include "cc_path_gens.h"
// #include "cc_parse_utils.h"
// #include "cc_rules_misc.h"
// #include "cc_rules.h"


// static bool cc_check_standalone_status( char const char_an,
//                                        CcGame * restrict game__io,
//                                        CcParseMsg ** restrict parse_msgs__iod,
//                                        bool is_resign,
//                                        bool is_end,
//                                        bool is_won,
//                                        size_t max_len__d,
//                                        char const * restrict msg, ... )
// {
//     // if ( !game__io ) return false;

//     // if ( !game__io->chessboard ) return false;
//     // if ( !game__io->moves ) return false;

//     // if ( !parse_msgs__iod ) return false;
//     // if ( !msg ) return false;

//     if ( iscntrl( char_an ) || isspace( char_an ) )
//     {
//         if ( is_resign )
//             game__io->status = cc_game_resign( game__io->status );
//         else
//             game__io->status = cc_game_status_next( game__io->status, is_end, is_won );

//         return true;
//     }
//     else
//     {
//         va_list args;
//         va_start( args, msg );

//         cc_parse_msg_append_fmt_if( parse_msgs__iod, CC_PMTE_Error, max_len__d, msg, args );

//         va_end( args );

//         return false;
//     }
// }


// static bool cc_construct_full_path( CcChessboard * restrict cb_before_activation,
//                                     char const * restrict ply_start_str,
//                                     char const * restrict ply_end_str,
//                                     bool is_starting_ply,
//                                     CcPieceEnum activator,
//                                     CcPos starting,
//                                     CcPos destination,
//                                     char piece_symbol,
//                                     bool is_light_piece,
//                                     bool include_opponent,
//                                     CcSteps * restrict steps,
//                                     CcParseMsg ** restrict parse_msgs__iod,
//                                     CcPos * restrict start__o,
//                                     CcPosLink ** restrict path__o,
//                                     size_t * restrict path_count__o )
// {
//     if ( !cb_before_activation ) return false;
//     if ( !ply_start_str ) return false;
//     if ( !ply_end_str ) return false;
//     if ( !parse_msgs__iod ) return false;
//     if ( !start__o ) return false;
//     if ( !path__o ) return false;
//     if ( *path__o ) return false;
//     if ( !path_count__o ) return false;

//     // CcPos start_cb = CC_POS_CAST_INVALID;
//     // CcPosLink * path__a = NULL;
//     // size_t path_count = 0;
//     CcPieceEnum piece = cc_piece_from_symbol( piece_symbol, is_light_piece );

//     *path_count__o = 0;

//     while ( cc_piece_pos_iter( cb_before_activation, starting, piece, include_opponent, start__o ) )
//     {
//         CcPieceEnum pe_start = cc_chessboard_get_piece( cb_before_activation, start__o->i, start__o->j );

//         if ( ( is_starting_ply
//                 && ( !cc_piece_is_equal( piece_symbol, is_light_piece, pe_start ) ) )
//             || ( !is_starting_ply
//                 && ( !cc_piece_has_congruent_type( piece_symbol, pe_start ) ) ) )
//         {
//             cc_char_8 start_str = CC_CHAR_8_EMPTY;
//             cc_pos_to_short_string( *start__o, &start_str );

//             char * ply_str__a = cc_str_copy__new( ply_start_str,
//                                                     ply_end_str,
//                                                     CC_MAX_LEN_ZERO_TERMINATED );

//             cc_parse_msg_append_fmt_if( parse_msgs__iod,
//                                             CC_PMTE_Error,
//                                             CC_MAX_LEN_ZERO_TERMINATED,
//                                             "Piece '%c' not found at '%s', in ply '%s'.\n",
//                                             piece_symbol,
//                                             start_str,
//                                             ply_str__a );

//             CC_FREE( ply_str__a );

// //             cc_step_free_all( &steps );
// //             cc_chessboard_free_all( &cb_before_activation );
//             return false;
//         }

//         CcPosLink * path__t =
//             is_starting_ply ? cc_longest_path__new( cb_before_activation, activator, *start__o, destination )
//                             : cc_shortest_path__new( cb_before_activation, activator, *start__o, destination );

//         // #ifdef __CC_STR_PRINT_INFO__
//         // {
//         //     char * path_str__a = cc_pos_link_to_short_string__new( path__t );
//         //     if ( path_str__a )
//         //     {
//         //         CC_PRINTF_IF_INFO( "Path: '%s'.\n", path_str__a );
//         //         CC_FREE( path_str__a );
//         //     }
//         // }
//         // #endif // __CC_STR_PRINT_INFO__


// // TODO :: side-effects
//         if ( cc_steps_are_congruent( steps, path__t ) )
//         {
//             *path__o = path__t;
//             ++path_count__o;
//         }

//     } // while ( cc_piece_pos_iter( ... ) )

//     return true;
// }



// static bool cc_append_steps( CcGame * restrict game,
//                              char const * restrict ply_start_str,
//                              char const * restrict ply_end_str,
//                              CcChessboard * restrict cb_before_activation,
//                              CcSteps ** restrict steps__io,
//                              CcParseMsg ** restrict parse_msgs__iod )
// {
//     if ( !game ) return false;
//     if ( !ply_start_str ) return false;
//     if ( !ply_end_str ) return false;
//     if ( !cb_before_activation ) return false;
//     if ( !steps__io ) return false;
//     if ( !parse_msgs__iod ) return false;

//     char const * c_str = ply_start_str;

//     char const * step_start_str = NULL;
//     char const * step_end_str = NULL;

//     while ( cc_iter_step( ply_start_str, ply_end_str, &step_start_str, &step_end_str ) )
//     {
//         CcStepLinkEnum sle = cc_parse_step_link( step_start_str );
//         c_str = step_start_str + cc_step_link_len( sle );

//         //
//         // Step position.

//         CcPos pos = CC_POS_CAST_INVALID;
//         char const * pos_str_end = NULL;

//         if ( !cc_parse_pos( c_str, &pos, &pos_str_end ) )
//         {
//             char * ply_str__a = cc_str_copy__new( ply_start_str, ply_end_str, CC_MAX_LEN_ZERO_TERMINATED );
//             char * step_str__a = cc_str_copy__new( step_start_str, step_end_str, CC_MAX_LEN_ZERO_TERMINATED );

//             cc_parse_msg_append_fmt_if( parse_msgs__iod,
//                                             CC_PMTE_Error,
//                                             CC_MAX_LEN_ZERO_TERMINATED,
//                                             "Invalid position in step '%s', in ply '%s'.\n",
//                                             step_str__a,
//                                             ply_str__a );

//             CC_FREE( step_str__a );
//             CC_FREE( ply_str__a );
//             return false;
//         }

//         if ( pos_str_end ) c_str = pos_str_end;

//         //
//         // Side-effect.

//         CcSideEffectEnum see = CC_SEE_None;
//         char const * side_effect_str = cc_find_side_effect_type( c_str, step_end_str, &see );

//         CcSideEffect se = CC_SIDE_EFFECT_CAST_INVALID;

// // TODO :: side-effects
// // TODO :: losing tags within side-effects

//         CcSteps * step__w = cc_steps_append_if( steps__io, sle, pos, se );
//         if ( !step__w ) return false;
//     }

//     return true;
// }

// // static bool cc_do_make_plies( char const * restrict move_an_str,
// //                               CcGame * restrict game__io,
// //                               CcParseMsg ** restrict parse_msgs__iod )
// // {
// //     if ( !move_an_str ) return false;
// //     if ( !game__io ) return false;
// //     if ( !parse_msgs__iod ) return false;

// //     if ( !game__io->chessboard ) return false;
// //     // if ( !game__io->moves ) return false;

// //     CcChessboard * cb__a = cc_chessboard_duplicate__new( game__io->chessboard );

// //     char const * ply_start_str = NULL;
// //     char const * ply_end_str = NULL;
// //     int momentum = 0;

// //     CcPieceEnum previous_piece = CC_PE_None;
// //     CcPieceEnum activator = CC_PE_None;

// //     CcPieceEnum cascaded_piece = CC_PE_None;
// //     CcPos cascading_field = CC_POS_CAST_INVALID;
// //     CcPos destination_field = CC_POS_CAST_INVALID;

// // // TODO :: check if castling --> handle as a special case

// //     CC_PRINTF_IF_INFO( "Move: '%s'.\n\n", move_an_str );

// //     while ( cc_iter_ply( move_an_str, &ply_start_str, &ply_end_str ) )
// //     {
// //         bool ply_has_steps = cc_ply_has_steps( ply_start_str, ply_end_str );

// // // TODO :: check if en passant --> handle as a special case

// //         CC_STR_PRINT_IF_INFO( ply_start_str, ply_end_str, 8192, "Ply: '%s', ", "steps: %d.\n", ply_has_steps );

// //         //
// //         // Ply link.

// //         CcPlyLinkEnum ple = cc_parse_ply_link( ply_start_str );
// //         bool is_any_trance_journey = CC_PLY_LINK_IS_ANY_TRANCE_JOURNEY( ple );
// //         char const * c_str = ply_start_str + cc_ply_link_len( ple );

// //         CC_STR_PRINT_IF_INFO( ply_start_str, c_str, 128, "Ply link: '%s'", " --> %d.\n", ple );

// //         if ( CC_CHAR_IS_PLY_GATHER_START( *c_str ) ) ++c_str; // Move past '['.

// //         //
// //         // Piece symbol.

// //         char piece_symbol = ' ';

// //         if ( !cc_fetch_piece_symbol( c_str, &piece_symbol, true, true ) )
// //         {
// //             cc_parse_msg_append_fmt_if( parse_msgs__iod,
// //                                             CC_PMTE_Error,
// //                                             CC_MAX_LEN_ZERO_TERMINATED,
// //                                             "Invalid piece symbol '%c'.\n",
// //                                             piece_symbol );

// //             cc_chessboard_free_all( &cb__a );
// //             return false;
// //         }

// //         if ( CC_CHAR_IS_PIECE_SYMBOL( *c_str ) ) ++c_str;

// //         //
// //         // Losing tag.

// //         CcLosingTagEnum lte_an = cc_parse_losing_tag( c_str );

// //         if ( lte_an != CC_LTE_None )
// //         {
// //             CC_PRINTF_IF_INFO( "Losing tag: '%c%c' --> %d.\n", *c_str, *(c_str+1), lte_an );
// //             c_str += cc_losing_tag_len( lte_an );
// //         }

// //         //
// //         // Disambiguation.

// //         cc_char_8 disambiguation_c8 = CC_CHAR_8_EMPTY;

// //         char const * end_da_str = cc_parse_starting_pos_str( c_str, ply_end_str, true, &disambiguation_c8 );

// //         CC_STR_PRINT_IF_INFO( disambiguation_c8, NULL, CC_MAX_LEN_CHAR_8, "Disambiguation: '%s'", ", pointer: '%p'.\n", end_da_str ); // TODO :: maybe check error (?)

// //         int file_da = CC_INVALID_COORD;
// //         int rank_da = CC_INVALID_COORD;

// //         if ( !cc_convert_coords( disambiguation_c8, &file_da, &rank_da ) &&
// //              ( CC_IS_COORD_ON_BOARD( game__io->chessboard->size, file_da ) ||
// //                CC_IS_COORD_ON_BOARD( game__io->chessboard->size, rank_da ) ) )
// //         {
// //             char * ply_str__a = cc_str_copy__new( ply_start_str, ply_end_str, CC_MAX_LEN_ZERO_TERMINATED );

// //             cc_parse_msg_append_fmt_if( parse_msgs__iod,
// //                                             CC_PMTE_Error,
// //                                             CC_MAX_LEN_ZERO_TERMINATED,
// //                                             "Invalid char(s) in disambiguation '%s', in ply '%s'.\n",
// //                                             disambiguation_c8,
// //                                             ply_str__a );

// //             CC_FREE( ply_str__a );
// //             cc_chessboard_free_all( &cb__a );
// //             return false;
// //         }

// //         CC_PRINTF_IF_INFO( "Disambiguation file, rank: %d, %d.\n", file_da, rank_da );

// //         if ( end_da_str ) c_str = end_da_str;

// //         //
// //         // Position (destination / starting position).

// //         // Destination if ply doesn't have steps, otherwise starting position
// //         // (in which case disambiguation_c8 must be empty).
// //         cc_char_8 position_c8 = CC_CHAR_8_EMPTY;

// //         char const * end_pos_str = cc_parse_starting_pos_str( c_str,
// //                                                     ply_end_str,
// //                                                     false,
// //                                                     &position_c8 );

// //         CC_STR_PRINT_IF_INFO( position_c8, NULL, CC_MAX_LEN_CHAR_8, "Pos: '%s'", ", pointer: '%p'.\n", end_pos_str ); // TODO :: maybe check error (?)

// //         int file_pos = CC_INVALID_COORD;
// //         int rank_pos = CC_INVALID_COORD;

// //         if ( !cc_convert_coords( position_c8, &file_pos, &rank_pos ) &&
// //              ( CC_IS_COORD_ON_BOARD( game__io->chessboard->size, file_pos ) ||
// //                CC_IS_COORD_ON_BOARD( game__io->chessboard->size, rank_pos ) ) )
// //         {
// //             char * ply_str__a = cc_str_copy__new( ply_start_str,
// //                                                   ply_end_str,
// //                                                   CC_MAX_LEN_ZERO_TERMINATED );

// //             cc_parse_msg_append_fmt_if( parse_msgs__iod,
// //                                             CC_PMTE_Error,
// //                                             CC_MAX_LEN_ZERO_TERMINATED,
// //                                             "Invalid char(s) in position '%s', in ply '%s'.\n",
// //                                             position_c8,
// //                                             ply_str__a );

// //             CC_FREE( ply_str__a );
// //             cc_chessboard_free_all( &cb__a );
// //             return false;
// //         }

// //         CC_PRINTF_IF_INFO( "Pos file, rank: %d, %d.\n", file_pos, rank_pos );

// //         if ( end_pos_str ) c_str = end_pos_str;

// //         //
// //         // Steps, from notation.

// //         CcSteps * steps__a = NULL;
// //         bool has_disambiguation = ( disambiguation_c8[ 0 ] != '\0' );
// //         bool has_position = ( position_c8[ 0 ] != '\0' );

// //         if ( ply_has_steps )
// //         {
// //             if ( has_disambiguation && has_position )
// //             {
// //                 char * ply_str__a = cc_str_copy__new( ply_start_str,
// //                                                       ply_end_str,
// //                                                       CC_MAX_LEN_ZERO_TERMINATED );

// //                 cc_parse_msg_append_fmt_if( parse_msgs__iod,
// //                                                 CC_PMTE_Error,
// //                                                 CC_MAX_LEN_ZERO_TERMINATED,
// //                                                 "Disambiguation '%s' preceedes starting position '%s', in ply '%s'.\n",
// //                                                 disambiguation_c8,
// //                                                 position_c8,
// //                                                 ply_str__a );

// //                 CC_FREE( ply_str__a );

// //                 cc_step_free_all( &steps__a );
// //                 cc_chessboard_free_all( &cb__a );
// //                 return false;
// //             }
// //             else if ( has_disambiguation )
// //             {
// //                 steps__a = cc_steps__new( CC_SLE_Start,
// //                                           cc_pos( file_da, rank_da ),
// //                                           CC_SIDE_EFFECT_CAST_INVALID );
// //             }
// //             else if ( has_position )
// //             {
// //                 steps__a = cc_steps__new( CC_SLE_Start,
// //                                           cc_pos( file_pos, rank_pos ),
// //                                           CC_SIDE_EFFECT_CAST_INVALID );
// //             }

// //             if ( !cc_append_steps( game__io,
// //                                    c_str,
// //                                    ply_end_str,
// //                                    cb__a,
// //                                    &steps__a,
// //                                    parse_msgs__iod ) )
// //             {
// //                 // <i> Parse msgs are added within cc_append_steps().

// //                 cc_step_free_all( &steps__a );
// //                 cc_chessboard_free_all( &cb__a );
// //                 return false;
// //             }
// //         }
// //         else // if ( !ply_has_steps )
// //         {
// //             if ( has_disambiguation )
// //             {
// //                 steps__a = cc_steps__new( CC_SLE_Start,
// //                                           cc_pos( file_da, rank_da ),
// //                                           CC_SIDE_EFFECT_CAST_INVALID );
// //             }

// //             if ( has_position )
// //             {
// //                 CcSteps * step__w = cc_steps_append_if( &steps__a,
// //                                                         CC_SLE_Destination,
// //                                                         cc_pos( file_pos, rank_pos ),
// //                                                         CC_SIDE_EFFECT_CAST_INVALID );

// //                 if ( !step__w )
// //                 {
// //                     cc_step_free_all( &steps__a );
// //                     cc_chessboard_free_all( &cb__a );
// //                     return false;
// //                 }
// //             }
// //             else
// //             {
// //                 char * ply_str__a = cc_str_copy__new( ply_start_str,
// //                                                       ply_end_str,
// //                                                       CC_MAX_LEN_ZERO_TERMINATED );

// //                 cc_parse_msg_append_fmt_if( parse_msgs__iod,
// //                                                 CC_PMTE_Error,
// //                                                 CC_MAX_LEN_ZERO_TERMINATED,
// //                                                 "Destination not found, in ply '%s'.\n",
// //                                                 ply_str__a );

// //                 CC_FREE( ply_str__a );

// //                 cc_step_free_all( &steps__a );
// //                 cc_chessboard_free_all( &cb__a );
// //                 return false;
// //             }
// //         }

// //         // ++c_str; // Not used afterwards, so ...

// //         #ifdef __CC_STR_PRINT_INFO__
// //         {
// //             char * steps_str__a = cc_steps_to_short_string__new( steps__a );
// //             if ( steps_str__a )
// //             {
// //                 CC_PRINTF_IF_INFO( "Steps: '%s'.\n", steps_str__a );
// //                 CC_FREE( steps_str__a );
// //             }
// //         }
// //         #endif // __CC_STR_PRINT_INFO__

// //         //
// //         // Piece, from symbol.

// //         bool is_starting_ply = ( ple == CC_PLE_StartingPly );
// //         bool include_opponent = !is_starting_ply;

// //         bool is_light_piece_temp = CC_GAME_STATUS_IS_LIGHT_TURN( game__io->status );
// //         CcPieceEnum piece_temp = cc_piece_from_symbol( piece_symbol, is_light_piece_temp );
// //         CcPieceEnum activator_temp = CC_PIECE_IS_WAVE( piece_temp ) ? activator : piece_temp;

// //         CC_PRINTF_IF_INFO( "Piece, from symbol: '%c' --> %d.\n", piece_symbol, piece_temp );

// //         //
// //         // Starting, ending position, based on notation.

// //         CcPos start_an = CC_POS_CAST_INVALID;
// //         CcPos end_an = CC_POS_CAST_INVALID;
// //         CcSteps * steps = steps__a;

// //         if ( steps->step_link == CC_SLE_Start )
// //             start_an = steps->pos;

// //         while ( steps && steps->next ) steps = steps->next; // rewind
// //         end_an = steps->pos;

// //         cc_char_8 temp = CC_CHAR_8_EMPTY;

// //         if ( cc_pos_to_short_string( start_an, &temp ) )
// //             CC_PRINTF_IF_INFO( "Found starting: '%s'.\n", temp );

// //         cc_str_clear( temp, CC_MAX_LEN_CHAR_8 );
// //         if ( cc_pos_to_short_string( end_an, &temp ) )
// //             CC_PRINTF_IF_INFO( "Found end: '%s'.\n", temp );

// //         //
// //         // Construct full path. Find true start.

// //         CcPos start_cb = CC_POS_CAST_INVALID;
// //         CcPosLink * path__a = NULL;
// //         size_t path_count = 0;

// //         while ( cc_piece_pos_iter( cb__a, start_an, piece_temp, include_opponent, &start_cb ) )
// //         {
// //             cc_char_8 start_str = CC_CHAR_8_EMPTY;
// //             if ( cc_pos_to_short_string( start_cb, &start_str ) )
// //                 CC_PRINTF_IF_INFO( "Try start: '%s'.\n", start_str );

// //             CcPieceEnum pe_start = cc_chessboard_get_piece( cb__a, start_cb.i, start_cb.j );
// //             bool is_light_piece = CC_GAME_STATUS_IS_LIGHT_TURN( game__io->status );

// //             if ( ( is_starting_ply && ( !cc_piece_is_equal( piece_symbol, is_light_piece, pe_start ) ) ) ||
// //                  ( !is_starting_ply && ( !cc_piece_has_congruent_type( piece_symbol, pe_start ) ) ) )
// //             {
// //                 char * ply_str__a = cc_str_copy__new( ply_start_str, ply_end_str, CC_MAX_LEN_ZERO_TERMINATED );

// //                 cc_parse_msg_append_fmt_if( parse_msgs__iod,
// //                                                 CC_PMTE_Error,
// //                                                 CC_MAX_LEN_ZERO_TERMINATED,
// //                                                 "Piece '%c' not found at '%s', in ply '%s'.\n",
// //                                                 piece_symbol,
// //                                                 start_str,
// //                                                 ply_str__a );

// //                 CC_FREE( ply_str__a );

// //                 cc_step_free_all( &steps__a );
// //                 cc_chessboard_free_all( &cb__a );
// //                 return false;
// //             }

// //             CcPosLink * path__t =
// //                 is_starting_ply ? cc_longest_path__new( cb__a, activator_temp, start_cb, end_an )
// //                                 : cc_shortest_path__new( cb__a, activator_temp, start_cb, end_an );

// //             #ifdef __CC_STR_PRINT_INFO__
// //             {
// //                 char * path_str__a = cc_pos_link_to_short_string__new( path__t );
// //                 if ( path_str__a )
// //                 {
// //                     CC_PRINTF_IF_INFO( "Path: '%s'.\n", path_str__a );
// //                     CC_FREE( path_str__a );
// //                 }
// //             }
// //             #endif // __CC_STR_PRINT_INFO__

// // // TODO :: side-effects
// //             if ( cc_steps_are_congruent( steps__a, path__t ) )
// //             {
// //                 CC_PRINTF_IF_INFO( "Found it!\n" );

// //                 path__a = path__t;
// //                 ++path_count;
// //             }

// //         } // while ( cc_piece_pos_iter( ... ) )

// //         //
// //         // Check if only one path --> do move.

// //         CcPieceEnum piece_cb = CC_PE_None;

// //         if ( path_count == 1 )
// //         {
// //             CcPosLink * start_path = path__a;
// //             CcPos start_pos = start_path->pos;

// //             CcPosLink * pl = path__a;
// //             int step_count = 0;
// //             CcPos pos_2 = CC_POS_INVALID;
// //             CcPos pos_3 = CC_POS_INVALID;

// //             while ( pl->next )
// //             {
// //                 if ( step_count == 1 )
// //                     pos_2 = pl->pos;
// //                 else if ( step_count == 2 )
// //                     pos_3 = pl->pos;

// //                 pl = pl->next;
// //                 ++step_count;
// //             }

// //             CcPosLink * end_path = pl;
// //             CcPos end_pos = end_path->pos;

// //             //
// //             // Piece, from starting position.

// //             piece_cb = cc_chessboard_get_piece( cb__a, start_pos.i, start_pos.j );
// //             CcTagEnum te_cb = cc_chessboard_get_tag( cb__a, start_pos.i, start_pos.j );

// //             if ( cc_check_tag_is_lost( lte_an, te_cb ) )
// //             {
// //                 char * ply_str__a = cc_str_copy__new( ply_start_str, ply_end_str, CC_MAX_LEN_ZERO_TERMINATED );
// //                 char const * lte_str = cc_losing_tag_as_string( lte_an );

// //                 cc_char_8 start_str = CC_CHAR_8_EMPTY;
// //                 cc_pos_to_short_string( start_pos, &start_str );

// //                 cc_parse_msg_append_fmt_if( parse_msgs__iod,
// //                                                 CC_PMTE_Error,
// //                                                 CC_MAX_LEN_ZERO_TERMINATED,
// //                                                 "Piece '%c' at '%s' didn't lost specified tag '%s', in ply '%s'.\n",
// //                                                 piece_symbol,
// //                                                 start_str,
// //                                                 lte_str,
// //                                                 ply_str__a );

// //                 CC_FREE( ply_str__a );

// //                 cc_step_free_all( &steps__a );
// //                 cc_chessboard_free_all( &cb__a );
// //                 return false;
// //             }

// //             if ( is_starting_ply )
// //                 momentum = step_count;
// //             else if ( is_any_trance_journey )
// //                 /* Nothing to do here, just to escape subtraction. */ ;
// //             else if ( !CC_PIECE_IS_WEIGHTLESS( piece_cb ) )
// //                 momentum -= step_count;

// //             CC_PRINTF_IF_INFO( "Piece: '%c' --> %d, with momentum %d.\n", piece_symbol, piece_cb, momentum );

// //             if ( ( !is_any_trance_journey ) && ( momentum < 0 ) )
// //             {
// //                 char * ply_str__a = cc_str_copy__new( ply_start_str, ply_end_str, CC_MAX_LEN_ZERO_TERMINATED );

// //                 cc_char_8 start_str = CC_CHAR_8_EMPTY;
// //                 cc_pos_to_short_string( start_pos, &start_str );

// //                 cc_parse_msg_append_fmt_if( parse_msgs__iod,
// //                                                 CC_PMTE_Error,
// //                                                 CC_MAX_LEN_ZERO_TERMINATED,
// //                                                 "Piece '%c' at '%s' exhausted more (%d) than received momentum, in ply '%s'.\n",
// //                                                 piece_symbol,
// //                                                 start_str,
// //                                                 momentum,
// //                                                 ply_str__a );

// //                 CC_FREE( ply_str__a );

// //                 cc_step_free_all( &steps__a );
// //                 cc_chessboard_free_all( &cb__a );
// //                 return false;
// //             }

// //             cascaded_piece = cc_chessboard_get_piece( cb__a, end_pos.i, end_pos.j );
// //             cascading_field = end_pos;
// //             destination_field = end_pos;

// //             CC_PRINTF_IF_INFO( "Cascaded piece: '%c' --> %d, %d.\n", cc_piece_as_char( cascaded_piece ), cascading_field.i, cascading_field.j );

// //             if ( !CC_PIECE_IS_WAVE( piece_cb ) )
// //                 activator = piece_cb;

// // // TODO :: tags
// // // TODO :: teleport
// //             if ( cc_chessboard_set_piece_tag( cb__a, start_pos.i, start_pos.j, previous_piece, CC_TE_None ) )
// //             {
// //                 CC_PRINTF_IF_INFO( "Previous ply is done!\n" );

// //                 if ( !CC_PIECE_IS_PAWN( previous_piece ) && !cc_delete_en_passant_tag( cb__a ) )
// //                 {
// //                     CC_PRINTF_IF_INFO( "Error deleting en passant tag.\n" );

// //                     cc_step_free_all( &steps__a );
// //                     cc_chessboard_free_all( &cb__a );
// //                     return false;
// //                 }
// //             }
// //             else
// //             {
// //                 char * ply_str__a = cc_str_copy__new( ply_start_str, ply_end_str, CC_MAX_LEN_ZERO_TERMINATED );

// //                 cc_parse_msg_append_fmt_if( parse_msgs__iod,
// //                                                 CC_PMTE_Error,
// //                                                 CC_MAX_LEN_ZERO_TERMINATED,
// //                                                 "Chessboard not updated, with ply '%s'.\n",
// //                                                 ply_str__a );

// //                 CC_FREE( ply_str__a );

// //                 cc_step_free_all( &steps__a );
// //                 cc_chessboard_free_all( &cb__a );
// //                 return false;
// //             }

// // // TODO :: step iterator for (step) side-effects (?)
// //             if ( CC_PIECE_IS_SHAMAN( piece_cb ) )
// //             {
// //                 CcPos step = cc_pos_subtract( pos_2, start_pos, 1 );

// //                 if ( cc_is_step_shamans_capture( piece_cb, step ) )
// //                 {
// //                     pl = path__a;

// //                     while ( pl )
// //                     {
// //                         if ( ( pl != start_path ) && ( pl != end_path ) )
// //                         {
// //                             CcPos pos = pl->pos;

// //                             if ( !cc_chessboard_set_piece_tag( cb__a, pos.i, pos.j, CC_PE_None, CC_TE_None ) )
// //                             {
// //                                 cc_step_free_all( &steps__a );
// //                                 cc_chessboard_free_all( &cb__a );
// //                                 return false;
// //                             }
// //                         }

// //                         if ( pl->next )
// //                             pl = pl->next;
// //                         else
// //                             break;
// //                     }
// //                 }
// //             }
// // // TODO :: step iterator for (step) side-effects (?)
// //         }
// //         else
// //         {
// //             char * ply_str__a = cc_str_copy__new( ply_start_str, ply_end_str, CC_MAX_LEN_ZERO_TERMINATED );
// //             char const * const fmt =
// //                 ( path_count == 0 ) ? "No path found, in ply '%s'.\n"
// //                                     : "More than one path found, in ply '%s'.\n";

// //             cc_parse_msg_append_fmt_if( parse_msgs__iod,
// //                                             CC_PMTE_Error,
// //                                             CC_MAX_LEN_ZERO_TERMINATED,
// //                                             fmt,
// //                                             ply_str__a );

// //             CC_FREE( ply_str__a );

// //             cc_step_free_all( &steps__a );
// //             cc_chessboard_free_all( &cb__a );
// //             return false;
// //         }

// //         previous_piece = piece_cb;

// //         if ( !CC_PIECE_IS_WAVE( piece_cb ) )
// //             activator = piece_cb;

// //         cc_step_free_all( &steps__a );

// //         if ( ply_end_str && *ply_end_str != '\0' )
// //             CC_PRINTF_IF_INFO( "\n" );
// //     } // while ( cc_iter_ply( ... ) )

// //     //
// //     // Writing last piece in a cascade onto last destination field.

// // // TODO :: tags
// // // TODO :: teleport
// //     if ( cc_chessboard_set_piece_tag( cb__a, destination_field.i, destination_field.j, previous_piece, CC_TE_None ) )
// //     {
// //         CC_PRINTF_IF_INFO( "Last ply is done!\n" );

// //         if ( !CC_PIECE_IS_PAWN( previous_piece ) && !cc_delete_en_passant_tag( cb__a ) )
// //         {
// //             CC_PRINTF_IF_INFO( "Error deleting en passant tag.\n" );

// //             cc_chessboard_free_all( &cb__a );
// //             return false;
// //         }
// //     }
// //     else
// //     {
// //         cc_parse_msg_append_fmt_if( parse_msgs__iod,
// //                                         CC_PMTE_Error,
// //                                         CC_MAX_LEN_ZERO_TERMINATED,
// //                                         "Chessboard not updated, with last destination in '%s'.\n",
// //                                         move_an_str );

// //         cc_chessboard_free_all( &cb__a );
// //         return false;
// //     }
// //     CC_PRINTF_IF_INFO( "-----------------------------------------------------------------------\n" );

// //     if ( cc_chessboard_free_all( &( game__io->chessboard ) ) )
// //     {
// //         game__io->chessboard = cb__a;
// //         return true;
// //     }
// //     else
// //     {
// //         cc_chessboard_free_all( &cb__a );
// //         return false;
// //     }
// // }


// static bool cc_make_plies( char const * restrict move_an_str,
//                            CcGame * restrict game,
//                            CcChessboard ** restrict cb__o,
//                            CcGameStatusEnum * restrict gse__o,
//                            CcParseMsg ** restrict parse_msgs__iod )
// {
//     if ( !move_an_str ) return false;
//     if ( !game ) return false;
//     if ( !cb__o ) return false;
//     if ( !gse__o ) return false;
//     if ( !parse_msgs__iod ) return false;

//     if ( !game->chessboard ) return false;

//     CcChessboard * cb__a = cc_chessboard_duplicate__new( game->chessboard );

//     char const * ply_start_str = NULL;
//     char const * ply_end_str = NULL;
//     int momentum = 0;

//     CcPieceEnum previous_piece = CC_PE_None;
//     CcPieceEnum activator = CC_PE_None;

//     CcPieceEnum cascaded_piece = CC_PE_None;
//     CcPos cascading_field = CC_POS_CAST_INVALID;
//     CcPos destination_field = CC_POS_CAST_INVALID;

// // TODO :: check if castling --> handle as a special case

//     while ( cc_iter_ply( move_an_str, &ply_start_str, &ply_end_str ) )
//     {
//         bool ply_has_steps = cc_ply_has_steps( ply_start_str, ply_end_str );

//         //
//         // Ply link.

//         CcPlyLinkEnum ple = cc_parse_ply_link( ply_start_str );
//         bool is_any_trance_journey = CC_PLY_LINK_IS_ANY_TRANCE_JOURNEY( ple );

//         if ( !CC_PLY_LINK_IS_VALID( ple ) )
//         {
//             cc_parse_msg_append_fmt_if( parse_msgs__iod,
//                                             CC_PMTE_Error,
//                                             CC_MAX_LEN_ZERO_TERMINATED,
//                                             "Invalid ply linkage at '%s'.\n",
//                                             ply_start_str );

//             cc_chessboard_free_all( &cb__a );
//             return false;
//         }

//         char const * c_str = ply_start_str + cc_ply_link_len( ple );

//         if ( CC_PLY_LINK_IS_TELEPORTATION( ple ) )
//         {
//             // TODO
//         }
//         else if ( CC_PLY_LINK_IS_FAILED_TELEPORTATION( ple ) )
//         {
//             // TODO
//         }
//         else if ( CC_PLY_LINK_IS_TRANCE_JOURNEY( ple ) )
//         {
//             // TODO
//         }
//         else if ( CC_PLY_LINK_IS_DUAL_TRANCE_JOURNEY( ple ) )
//         {
//             // TODO
//         }
//         else if ( CC_PLY_LINK_IS_FAILED_TRANCE_JOURNEY( ple ) )
//         {
//             // TODO
//         }
//         else if ( CC_PLY_LINK_IS_PAWN_SACRIFICE( ple ) )
//         {
//             // TODO
//         }
//         else
//         {
//             // Starting, or cascading, ply.

//             //
//             // Piece symbol.

//             char piece_symbol = ' ';

//             if ( !cc_fetch_piece_symbol( c_str, &piece_symbol, true, true ) )
//             {
//                 cc_parse_msg_append_fmt_if( parse_msgs__iod,
//                                                 CC_PMTE_Error,
//                                                 CC_MAX_LEN_ZERO_TERMINATED,
//                                                 "Invalid piece symbol '%c'.\n",
//                                                 piece_symbol );

//                 cc_chessboard_free_all( &cb__a );
//                 return false;
//             }

//             if ( CC_CHAR_IS_PIECE_SYMBOL( *c_str ) ) ++c_str;

//             //
//             // Losing tag.

//             CcLosingTagEnum lte_an = cc_parse_losing_tag( c_str );

//             if ( lte_an != CC_LTE_None )
//                 c_str += cc_losing_tag_len( lte_an );

//             //
//             // Disambiguation.

//             CcPos disambiguation = CC_POS_CAST_INVALID;
//             char const * disambiguation_str_end = NULL;

//             if ( !cc_parse_pos( c_str,
//                                    &disambiguation,
//                                    &disambiguation_str_end ) )
//             {
//                 char * ply_str__a = cc_str_copy__new( ply_start_str,
//                                                       ply_end_str,
//                                                       CC_MAX_LEN_ZERO_TERMINATED );

//                 cc_parse_msg_append_fmt_if( parse_msgs__iod,
//                                                 CC_PMTE_Error,
//                                                 CC_MAX_LEN_ZERO_TERMINATED,
//                                                 "Invalid char(s) in disambiguation, in ply '%s'.\n",
//                                                 ply_str__a );

//                 CC_FREE( ply_str__a );
//                 cc_chessboard_free_all( &cb__a );
//                 return false;
//             }

//             if ( disambiguation_str_end ) c_str = disambiguation_str_end;

//             //
//             // Position (destination / starting position).

//             // Destination if ply doesn't have steps, otherwise starting position
//             // (in which case disambiguation must be invalid).
//             CcPos position = CC_POS_CAST_INVALID;
//             char const * position_str_end = NULL;

//             if ( !cc_parse_pos( c_str, &position, &position_str_end ) )
//             {
//                 if ( CC_IS_COORD_2_ON_BOARD( game->chessboard->size,
//                                              disambiguation.i,
//                                              disambiguation.j ) )
//                 {
//                     position = disambiguation;
//                     disambiguation = CC_POS_CAST_INVALID;
//                 }
//                 else
//                 {
//                     char * ply_str__a = cc_str_copy__new( ply_start_str,
//                                                         ply_end_str,
//                                                         CC_MAX_LEN_ZERO_TERMINATED );

//                     cc_parse_msg_append_fmt_if( parse_msgs__iod,
//                                                     CC_PMTE_Error,
//                                                     CC_MAX_LEN_ZERO_TERMINATED,
//                                                     "Invalid char(s) in position, in ply '%s'.\n",
//                                                     ply_str__a );

//                     CC_FREE( ply_str__a );
//                     cc_chessboard_free_all( &cb__a );
//                     return false;
//                 }
//             }

//             if ( position_str_end ) c_str = position_str_end;

//             //
//             // Starting, destination positions.

//             CcPos starting_pos = CC_POS_CAST_INVALID;
//             CcPos destination_pos = CC_POS_CAST_INVALID;

//             if ( cc_pos_is_disambiguation( disambiguation ) )
//             {
//                 if ( ply_has_steps )
//                 {
//                     char * ply_str__a = cc_str_copy__new( ply_start_str,
//                                                           ply_end_str,
//                                                           CC_MAX_LEN_ZERO_TERMINATED );

//                     cc_parse_msg_append_fmt_if( parse_msgs__iod,
//                                                     CC_PMTE_Error,
//                                                     CC_MAX_LEN_ZERO_TERMINATED,
//                                                     "Ply has disambiguation and starting position at the same time, in '%s'.\n",
//                                                     ply_str__a );

//                     CC_FREE( ply_str__a );
//                     cc_chessboard_free_all( &cb__a );
//                     return false;
//                 }

//                 starting_pos = disambiguation;
//                 destination_pos = position;
//             }
//             else
//             {
//                 if ( ply_has_steps )
//                     starting_pos = position;
//                 else
//                     destination_pos = position;
//             }

//             //
//             // Steps, from notation.

//             CcSteps * steps__a = NULL;

//             if ( cc_chessboard_is_pos_on_board( game->chessboard, starting_pos.i, starting_pos.j ) )
//                 steps__a = cc_steps__new( CC_SLE_Start,
//                                           starting_pos,
//                                           CC_SIDE_EFFECT_CAST_INVALID );

//             if ( ply_has_steps )
//             {
//                 if ( !cc_append_steps( game,
//                                        c_str,
//                                        ply_end_str,
//                                        cb__a,
//                                        &steps__a,
//                                        parse_msgs__iod ) )
//                 {
//                     // <i> Parse msgs are added within cc_append_steps().

//                     cc_step_free_all( &steps__a );
//                     cc_chessboard_free_all( &cb__a );
//                     return false;
//                 }
//             }
//             else // if ( !ply_has_steps )
//             {
//                 CcSteps * step__w = cc_steps_append_if( &steps__a,
//                                                         CC_SLE_Destination,
//                                                         destination_pos,
//                                                         CC_SIDE_EFFECT_CAST_INVALID );

//                 if ( !step__w )
//                 {
//                     cc_step_free_all( &steps__a );
//                     cc_chessboard_free_all( &cb__a );
//                     return false;
//                 }
//             }

//             // ++c_str; // Not used afterwards, so ...

//             // #ifdef __CC_STR_PRINT_INFO__
//             // {
//             //     char * steps_str__a = cc_steps_to_short_string__new( steps__a );
//             //     if ( steps_str__a )
//             //     {
//             //         CC_PRINTF_IF_INFO( "Steps: '%s'.\n", steps_str__a );
//             //         CC_FREE( steps_str__a );
//             //     }
//             // }
//             // #endif // __CC_STR_PRINT_INFO__

//             //
//             // Piece, from symbol.

//             bool is_starting_ply = ( ple == CC_PLE_StartingPly );
//             bool include_opponent = !is_starting_ply;

//             bool is_light_piece_temp = CC_GAME_STATUS_IS_LIGHT_TURN( game->status );
//             CcPieceEnum piece_temp = cc_piece_from_symbol( piece_symbol, is_light_piece_temp );
//             CcPieceEnum activator_temp = CC_PIECE_IS_WAVE( piece_temp ) ? activator : piece_temp;

//             //
//             // Construct full path. Find true start.

//             CcPos start_cb = CC_POS_CAST_INVALID;
//             CcPosLink * path__a = NULL;
//             size_t path_count = 0;

//             while ( cc_piece_pos_iter( cb__a, starting_pos, piece_temp, include_opponent, &start_cb ) )
//             {
//                 CcPieceEnum pe_start = cc_chessboard_get_piece( cb__a, start_cb.i, start_cb.j );
//                 // bool is_light_piece = CC_GAME_STATUS_IS_LIGHT_TURN( game->status ); // TODO :: d'heck (?)

//                 if ( ( is_starting_ply
//                         && ( !cc_piece_is_equal( piece_symbol, is_light_piece_temp, pe_start ) ) )
//                   || ( !is_starting_ply
//                         && ( !cc_piece_has_congruent_type( piece_symbol, pe_start ) ) ) )
//                 {
//                     cc_char_8 start_str = CC_CHAR_8_EMPTY;
//                     cc_pos_to_short_string( start_cb, &start_str );

//                     char * ply_str__a = cc_str_copy__new( ply_start_str,
//                                                           ply_end_str,
//                                                           CC_MAX_LEN_ZERO_TERMINATED );

//                     cc_parse_msg_append_fmt_if( parse_msgs__iod,
//                                                     CC_PMTE_Error,
//                                                     CC_MAX_LEN_ZERO_TERMINATED,
//                                                     "Piece '%c' not found at '%s', in ply '%s'.\n",
//                                                     piece_symbol,
//                                                     start_str,
//                                                     ply_str__a );

//                     CC_FREE( ply_str__a );

//                     cc_step_free_all( &steps__a );
//                     cc_chessboard_free_all( &cb__a );
//                     return false;
//                 }

//                 CcPosLink * path__t =
//                     is_starting_ply ? cc_longest_path__new( cb__a, activator_temp, start_cb, destination_pos )
//                                     : cc_shortest_path__new( cb__a, activator_temp, start_cb, destination_pos );

//                 // #ifdef __CC_STR_PRINT_INFO__
//                 // {
//                 //     char * path_str__a = cc_pos_link_to_short_string__new( path__t );
//                 //     if ( path_str__a )
//                 //     {
//                 //         CC_PRINTF_IF_INFO( "Path: '%s'.\n", path_str__a );
//                 //         CC_FREE( path_str__a );
//                 //     }
//                 // }
//                 // #endif // __CC_STR_PRINT_INFO__

// // TODO :: side-effects
//                 if ( cc_steps_are_congruent( steps__a, path__t ) )
//                 {
//                     // CC_PRINTF_IF_INFO( "Found it!\n" );

//                     path__a = path__t;
//                     ++path_count;
//                 }

//             } // while ( cc_piece_pos_iter( ... ) )

//             //
//             // Check if only one path --> do move.

//             CcPieceEnum piece_cb = CC_PE_None;

//             if ( path_count == 1 )
//             {
//                 CcPosLink * start_path = path__a;
//                 CcPos start_pos = start_path->pos;

//                 CcPosLink * pl = path__a;
//                 int step_count = 0;
//                 CcPos pos_2 = CC_POS_INVALID;
//                 CcPos pos_3 = CC_POS_INVALID;

//                 while ( pl->next )
//                 {
//                     if ( step_count == 1 )
//                         pos_2 = pl->pos;
//                     else if ( step_count == 2 )
//                         pos_3 = pl->pos;

//                     pl = pl->next;
//                     ++step_count;
//                 }

//                 CcPosLink * end_path = pl;
//                 CcPos end_pos = end_path->pos;

//                 //
//                 // Piece, from starting position.

//                 piece_cb = cc_chessboard_get_piece( cb__a, start_pos.i, start_pos.j );
//                 CcTagEnum te_cb = cc_chessboard_get_tag( cb__a, start_pos.i, start_pos.j );

//                 if ( cc_check_tag_is_lost( lte_an, te_cb ) )
//                 {
//                     char * ply_str__a = cc_str_copy__new( ply_start_str, ply_end_str, CC_MAX_LEN_ZERO_TERMINATED );
//                     char const * lte_str = cc_losing_tag_as_string( lte_an );

//                     cc_char_8 start_str = CC_CHAR_8_EMPTY;
//                     cc_pos_to_short_string( start_pos, &start_str );

//                     cc_parse_msg_append_fmt_if( parse_msgs__iod,
//                                                     CC_PMTE_Error,
//                                                     CC_MAX_LEN_ZERO_TERMINATED,
//                                                     "Piece '%c' at '%s' didn't lost specified tag '%s', in ply '%s'.\n",
//                                                     piece_symbol,
//                                                     start_str,
//                                                     lte_str,
//                                                     ply_str__a );

//                     CC_FREE( ply_str__a );

//                     cc_step_free_all( &steps__a );
//                     cc_chessboard_free_all( &cb__a );
//                     return false;
//                 }

//                 if ( is_starting_ply )
//                     momentum = step_count;
//                 else if ( is_any_trance_journey )
//                     /* Nothing to do here, just to escape subtraction. */ ;
//                 else if ( !CC_PIECE_IS_WEIGHTLESS( piece_cb ) )
//                     momentum -= step_count;

//                 if ( ( !is_any_trance_journey ) && ( momentum < 0 ) )
//                 {
//                     char * ply_str__a = cc_str_copy__new( ply_start_str, ply_end_str, CC_MAX_LEN_ZERO_TERMINATED );

//                     cc_char_8 start_str = CC_CHAR_8_EMPTY;
//                     cc_pos_to_short_string( start_pos, &start_str );

//                     cc_parse_msg_append_fmt_if( parse_msgs__iod,
//                                                     CC_PMTE_Error,
//                                                     CC_MAX_LEN_ZERO_TERMINATED,
//                                                     "Piece '%c' at '%s' exhausted more (%d) than received momentum, in ply '%s'.\n",
//                                                     piece_symbol,
//                                                     start_str,
//                                                     momentum,
//                                                     ply_str__a );

//                     CC_FREE( ply_str__a );

//                     cc_step_free_all( &steps__a );
//                     cc_chessboard_free_all( &cb__a );
//                     return false;
//                 }

//                 cascaded_piece = cc_chessboard_get_piece( cb__a, end_pos.i, end_pos.j );
//                 cascading_field = end_pos;
//                 destination_field = end_pos;

//                 if ( !CC_PIECE_IS_WAVE( piece_cb ) )
//                     activator = piece_cb;

// // TODO :: tags
// // TODO :: teleport
//                 if ( cc_chessboard_set_piece_tag( cb__a, start_pos.i, start_pos.j, previous_piece, CC_TE_None ) )
//                 {
//                     if ( !CC_PIECE_IS_PAWN( previous_piece ) && !cc_delete_en_passant_tag( cb__a ) )
//                     {
//                         cc_step_free_all( &steps__a );
//                         cc_chessboard_free_all( &cb__a );
//                         return false;
//                     }
//                 }
//                 else
//                 {
//                     char * ply_str__a = cc_str_copy__new( ply_start_str, ply_end_str, CC_MAX_LEN_ZERO_TERMINATED );

//                     cc_parse_msg_append_fmt_if( parse_msgs__iod,
//                                                     CC_PMTE_Error,
//                                                     CC_MAX_LEN_ZERO_TERMINATED,
//                                                     "Chessboard not updated, with ply '%s'.\n",
//                                                     ply_str__a );

//                     CC_FREE( ply_str__a );

//                     cc_step_free_all( &steps__a );
//                     cc_chessboard_free_all( &cb__a );
//                     return false;
//                 }

// // TODO :: step iterator for (step) side-effects (?)
//                 if ( CC_PIECE_IS_SHAMAN( piece_cb ) )
//                 {
//                     CcPos step = cc_pos_subtract( pos_2, start_pos, 1 );

//                     if ( cc_is_step_shamans_capture( piece_cb, step ) )
//                     {
//                         pl = path__a;

//                         while ( pl )
//                         {
//                             if ( ( pl != start_path ) && ( pl != end_path ) )
//                             {
//                                 CcPos pos = pl->pos;

//                                 if ( !cc_chessboard_set_piece_tag( cb__a, pos.i, pos.j, CC_PE_None, CC_TE_None ) )
//                                 {
//                                     cc_step_free_all( &steps__a );
//                                     cc_chessboard_free_all( &cb__a );
//                                     return false;
//                                 }
//                             }

//                             if ( pl->next )
//                                 pl = pl->next;
//                             else
//                                 break;
//                         }
//                     }
//                 }
// // TODO :: step iterator for (step) side-effects (?)
//             }
//             else
//             {
//                 char * ply_str__a = cc_str_copy__new( ply_start_str, ply_end_str, CC_MAX_LEN_ZERO_TERMINATED );
//                 char const * const fmt =
//                     ( path_count == 0 ) ? "No path found, in ply '%s'.\n"
//                                         : "More than one path found, in ply '%s'.\n";

//                 cc_parse_msg_append_fmt_if( parse_msgs__iod,
//                                                 CC_PMTE_Error,
//                                                 CC_MAX_LEN_ZERO_TERMINATED,
//                                                 fmt,
//                                                 ply_str__a );

//                 CC_FREE( ply_str__a );

//                 cc_step_free_all( &steps__a );
//                 cc_chessboard_free_all( &cb__a );
//                 return false;
//             }




// //         previous_piece = piece_cb;

// //         if ( !CC_PIECE_IS_WAVE( piece_cb ) )
// //             activator = piece_cb;

// //         cc_step_free_all( &steps__a );

// //         if ( ply_end_str && *ply_end_str != '\0' )
// //             CC_PRINTF_IF_INFO( "\n" );
// //     } // while ( cc_iter_ply( ... ) )

// //     //
// //     // Writing last piece in a cascade onto last destination field.

// // // TODO :: tags
// // // TODO :: teleport
// //     if ( cc_chessboard_set_piece_tag( cb__a, destination_field.i, destination_field.j, previous_piece, CC_TE_None ) )
// //     {
// //         CC_PRINTF_IF_INFO( "Last ply is done!\n" );

// //         if ( !CC_PIECE_IS_PAWN( previous_piece ) && !cc_delete_en_passant_tag( cb__a ) )
// //         {
// //             CC_PRINTF_IF_INFO( "Error deleting en passant tag.\n" );

// //             cc_chessboard_free_all( &cb__a );
// //             return false;
// //         }
// //     }
// //     else
// //     {
// //         cc_parse_msg_append_fmt_if( parse_msgs__iod,
// //                                         CC_PMTE_Error,
// //                                         CC_MAX_LEN_ZERO_TERMINATED,
// //                                         "Chessboard not updated, with last destination in '%s'.\n",
// //                                         move_an_str );

// //         cc_chessboard_free_all( &cb__a );
// //         return false;
// //     }
// //     CC_PRINTF_IF_INFO( "-----------------------------------------------------------------------\n" );

// //     if ( cc_chessboard_free_all( &( game__io->chessboard ) ) )
// //     {
// //         game__io->chessboard = cb__a;
// //         return true;
// //     }
// //     else
// //     {
// //         cc_chessboard_free_all( &cb__a );
// //         return false;
// //     }
// // }







//         }




//     } // while ( cc_iter_ply( ... ) )


//     return true;
// }


// bool cc_make_move( char const * restrict move_an_str,
//                    CcGame * restrict game,
//                    CcChessboard ** restrict cb__o,
//                    CcGameStatusEnum * restrict gse__o,
//                    CcParseMsg ** restrict parse_msgs__iod )
// {
//     if ( !move_an_str ) return false;
//     if ( !game ) return false;
//     if ( !cb__o ) return false;
//     if ( !gse__o ) return false;
//     if ( !parse_msgs__iod ) return false;

//     if ( !game->chessboard ) return false;

//     if ( !CC_GAME_STATUS_IS_TURN( game->status ) )
//     {
//         char const * msg =
//             ( game->status == CC_GSE_None ) ? "Game is not initialized.\n"
//                                             : "Game is finished.\n";

//         cc_parse_msg_append_fmt_if( parse_msgs__iod,
//                                         CC_PMTE_Error,
//                                         CC_MAX_LEN_ZERO_TERMINATED,
//                                         msg );
//         return false;
//     }

//     char const * m_str = move_an_str;

//     if ( *m_str == '#' )
//     {
//         if ( *++m_str == '#' )
//         {
//             // "##" resign
//             return cc_check_standalone_status( *++m_str, game, parse_msgs__iod, true, true, false,
//                                               CC_MAX_LEN_ZERO_TERMINATED,
//                                               "Invalid char(s) after resign.\n" );
//         }
//         else
//         {
//             // "#" self-checkmate

// // TODO :: Do check if opponent is really (self-)checkmated.
// //         Self- is optional, since both players could overlook checkmate,
// //         this is option to rectify such a situation.

//             return cc_check_standalone_status( *m_str, game, parse_msgs__iod, false, true, true,
//                                               CC_MAX_LEN_ZERO_TERMINATED,
//                                               "Invalid char(s) after self-checkmate.\n" );
//         }
//     }

//     if ( *m_str == '(' )
//     {
//         if ( *++m_str == '=' )
//         {
//             if ( *++m_str == '=' )
//             {
//                 if ( *++m_str == ')' )
//                 {
//                     // "(==)" draw offer accepted

//                     if ( cc_check_valid_draw_offer_exists( game->moves, game->status ) )
//                     {
//                         return cc_check_standalone_status( *++m_str, game, parse_msgs__iod, false, true, false,
//                                                           CC_MAX_LEN_ZERO_TERMINATED,
//                                                           "Invalid char(s) after accepted draw.\n" );
//                     }
//                     else
//                     {
//                         cc_parse_msg_append_fmt_if( parse_msgs__iod,
//                                                         CC_PMTE_Error,
//                                                         CC_MAX_LEN_ZERO_TERMINATED,
//                                                         "No valid opponent's draw offer found.\n" );
//                         return false;
//                     }
//                 }
//                 // <i> Draw-by-rules should be issued by arbiter, not players;
//                 //     i.e. should be issued by server, not clients.
//                 //
//                 // else if ( *m_str == '=' )
//                 // {
//                 //     if ( *++m_str == ')' )
//                 //     {
//                 //         // "(===)" draw by rules
//                 //
//                 //         return cc_check_standalone_status( *++m_str, game, parse_msgs__iod, false, true, false,
//                 //                                           CC_MAX_LEN_ZERO_TERMINATED,
//                 //                                           "Invalid char(s) after draw by rules.\n" );
//                 //     }
//                 // }
//             }
//         }

//         cc_parse_msg_append_fmt_if( parse_msgs__iod,
//                                         CC_PMTE_Error,
//                                         CC_MAX_LEN_ZERO_TERMINATED,
//                                         "Invalid char(s) within draw; draw offer cannot be issued standalone; draw-by-rules only by arbiter, not players.\n" );
//         return false;
//     }

//     if ( !cc_make_plies( move_an_str, game, cb__o, gse__o, parse_msgs__iod ) )
//         return false;

// // // TODO :: post-plies status


// //     if ( !cc_moves_append_if( &( game__io->moves ), move_an_str, CC_MAX_LEN_ZERO_TERMINATED ) )
// //         return false;


// // // TODO :: determine ending status
// // // TODO :: determine winning status
// // // TODO :: determine if forced draw
// //     game__io->status = cc_game_status_next( game__io->status, false, false );

//     return true;
// }

// bool cc_apply_move( char const * restrict move_an_str,
//                     CcGame * restrict game__io,
//                     CcParseMsg ** restrict parse_msgs__iod )
// {
//     if ( !move_an_str ) return false;
//     if ( !game__io ) return false;
//     if ( !parse_msgs__iod ) return false;

//     if ( !game__io->chessboard ) return false;

//     CcChessboard * new__t = NULL;
//     CcGameStatusEnum gse = game__io->status;

//     if ( cc_make_move( move_an_str, game__io, &new__t, &gse, parse_msgs__iod ) )
//     {
//         if ( !cc_moves_append_if( &( game__io->moves ),
//                                   move_an_str,
//                                   CC_MAX_LEN_ZERO_TERMINATED ) )
//             return false;

//         CcChessboard * old__a = game__io->chessboard;
//         if ( !cc_chessboard_free_all( &old__a ) )
//             return false;

//         game__io->chessboard = new__t;
//         game__io->status = gse;

//         return true;
//     }
//     else
//         return false;
// }
