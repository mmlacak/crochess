// Copyright (c) 2021, 2022 Mario Mlačak, mmlacak@gmail.com
// Licensed under GNU GPL v3+ license. See LICENSING, COPYING files for details.

#include <ctype.h>
#include <stdarg.h>

#include <string.h>
#include <stdio.h>

#include "cc_defines.h"
#include "cc_str_utils.h"
#include "cc_steps.h"
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

        cc_parse_msgs_append_if_format( parse_msgs__io, CC_PMTE_Error, max_len__d, msg, args );

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

        cc_parse_msgs_append_if_format( parse_msgs__io,
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
//         this is option to rectify such a situation.

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
                        cc_parse_msgs_append_if_format( parse_msgs__io,
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

        cc_parse_msgs_append_if_format( parse_msgs__io,
                                        CC_PMTE_Error,
                                        CC_MAX_LEN_ZERO_TERMINATED,
                                        "Invalid char(s) within draw; draw offer cannot be issued standalone; draw-by-rules only by arbiter, not players.\n" );
        return false;
    }

    char const * ply_start = NULL;
    char const * ply_end = NULL;

    CcPlyLinkEnum ple = CC_PLE_None;
    char piece_symbol = ' ';
    CcPieceEnum piece = CC_PE_None;
    char const * c = NULL;

    char const * step_start = NULL;
    char const * step_end = NULL;
    bool ply_has_steps = false;

// TODO :: check if castling --> handle as a special case

    // CC_PRINTF_IF_INFO( "\n" );
    CC_PRINTF_IF_INFO( "Move: '%s'.\n\n", move_an_str ); // "\nMove: '%s'.\n\n"

    // <!> Reset before use, otherwise iterator will get confused.
    // ply_start = ply_end = NULL; // Currently not needed, variables are
                                   // defined and set just a few lines above.

    while ( cc_ply_iter( m, &ply_start, &ply_end ) )
    {
        ply_has_steps = cc_ply_has_steps( ply_start, ply_end );

// TODO :: check if en passant --> handle as a special case

        CC_STR_PRINT_IF_INFO( ply_start, ply_end, 8192, "Ply: '%s', ", "steps: %d.\n", ply_has_steps );

        ple = cc_starting_ply_link( ply_start );
        c = ply_start + cc_ply_link_len( ple );

        CC_STR_PRINT_IF_INFO( ply_start, c, 128, "Ply link: '%s'", " --> %d.\n", ple );

        if ( CC_IS_PLY_GATHER_START( *c ) ) ++c; // Move past '['.

        if ( !cc_ply_piece_symbol( c, &piece_symbol ) )
        {
            cc_parse_msgs_append_if_format( parse_msgs__io,
                                            CC_PMTE_Error,
                                            CC_MAX_LEN_ZERO_TERMINATED,
                                            "Invalid piece symbol '%c'.\n",
                                            piece_symbol );
            return false;
        }

        if ( CC_IS_PIECE_SYMBOL( *c ) ) ++c;

        CcLosingTagEnum lte = CC_LTE_Promotion;
        lte = cc_starting_losing_tag( c );

        if ( lte != CC_LTE_None )
        {
            CC_PRINTF_IF_INFO( "Losing tag: '%c%c' --> %d.\n", *c, *(c+1), lte );
            c += cc_losing_tag_len( lte );
        }

        char_8 disambiguation = CC_CHAR_8_EMPTY;

        char const * end_da = cc_starting_pos( c, ply_end, true, &disambiguation );

        CC_STR_PRINT_IF_INFO( disambiguation, NULL, CC_MAX_LEN_CHAR_8, "Disambiguation: '%s'", ", pointer: '%p'.\n", end_da ); // TODO :: maybe check error (?)

        int file_da = CC_INVALID_OFF_BOARD_COORD_MIN;
        int rank_da = CC_INVALID_OFF_BOARD_COORD_MIN;

        if ( !cc_convert_starting_pos( disambiguation, &file_da, &rank_da ) &&
             ( CC_IS_COORD_ON_BOARD( g->chessboard->size, file_da ) ||
               CC_IS_COORD_ON_BOARD( g->chessboard->size, rank_da ) ) )
        {
            char * ply__a = cc_str_copy__new( ply_start, ply_end, CC_MAX_LEN_ZERO_TERMINATED );

            cc_parse_msgs_append_if_format( parse_msgs__io,
                                            CC_PMTE_Error,
                                            CC_MAX_LEN_ZERO_TERMINATED,
                                            "Invalid char(s) in disambiguation '%s', in ply '%s'.\n",
                                            disambiguation,
                                            ply__a );

            CC_FREE( ply__a );
            return false;
        }

        CC_PRINTF_IF_INFO( "Disambiguation file, rank: %d, %d.\n", file_da, rank_da );

        if ( end_da ) c = end_da;

        // Destination if ply doesn't have steps, otherwise starting position
        // (in which case disambiguation must be empty).
        char_8 pos = CC_CHAR_8_EMPTY;

        char const * end_pos = cc_starting_pos( c, ply_end, false, &pos );

        CC_STR_PRINT_IF_INFO( pos, NULL, CC_MAX_LEN_CHAR_8, "Pos: '%s'", ", pointer: '%p'.\n", end_pos ); // TODO :: maybe check error (?)

        int file_pos = CC_INVALID_OFF_BOARD_COORD_MIN;
        int rank_pos = CC_INVALID_OFF_BOARD_COORD_MIN;

        if ( !cc_convert_starting_pos( pos, &file_pos, &rank_pos ) &&
             ( CC_IS_COORD_ON_BOARD( g->chessboard->size, file_pos ) ||
               CC_IS_COORD_ON_BOARD( g->chessboard->size, rank_pos ) ) )
        {
            char * ply__a = cc_str_copy__new( ply_start, ply_end, CC_MAX_LEN_ZERO_TERMINATED );

            cc_parse_msgs_append_if_format( parse_msgs__io,
                                            CC_PMTE_Error,
                                            CC_MAX_LEN_ZERO_TERMINATED,
                                            "Invalid char(s) in pos '%s', in ply '%s'.\n",
                                            pos,
                                            ply__a );

            CC_FREE( ply__a );
            return false;
        }

        CC_PRINTF_IF_INFO( "Pos file, rank: %d, %d.\n", file_pos, rank_pos );

        CcSteps * steps__a = NULL;

        if ( ply_has_steps )
        {
            if ( ( disambiguation[ 0 ] != '\0' ) && ( pos[ 0 ] != '\0' ) )
            {
                char * ply__a = cc_str_copy__new( ply_start, ply_end, CC_MAX_LEN_ZERO_TERMINATED );

                cc_parse_msgs_append_if_format( parse_msgs__io,
                                                CC_PMTE_Error,
                                                CC_MAX_LEN_ZERO_TERMINATED,
                                                "Disambiguation '%s' preceedes starting position '%s', in ply '%s'.\n",
                                                disambiguation,
                                                pos,
                                                ply__a );

                CC_FREE( ply__a );
                return false;
            }
            else if ( disambiguation[ 0 ] != '\0' )
            {
                steps__a = cc_steps__new( CC_SLE_Start, cc_pos( file_da, rank_da ) );
            }
            else if ( pos[ 0 ] != '\0' )
            {
                steps__a = cc_steps__new( CC_SLE_Start, cc_pos( file_pos, rank_pos ) );
            }

            if ( end_pos ) c = end_pos;

            // <!> Reset before use, otherwise iterator will get confused.
            step_start = step_end = NULL;

            while ( cc_step_iter( c, ply_end, &step_start, &step_end ) )
            {
                CC_STR_PRINT_IF_INFO( step_start, step_end, 8192, "Step: '%s'.\n", "" );

                CcStepLinkEnum sle = cc_starting_step_link( step_start );
                c = step_start + cc_step_link_len( sle );

                CC_STR_PRINT_IF_INFO( step_start, c, 128, "Step link: '%s'", " --> %d.\n", sle );

                char_8 pos = CC_CHAR_8_EMPTY;
                char const * p = pos;

                int file = CC_INVALID_OFF_BOARD_COORD_MIN;
                int rank = CC_INVALID_OFF_BOARD_COORD_MIN;

                CcSideEffectEnum see = CC_SEE_None;
                char const * se = cc_find_side_effect( c, step_end, &see );
                char const * pos_end = step_end;

                if ( se )
                {
                    CC_STR_PRINT_IF_INFO( se, step_end, 128, "Side-effect: '%s'", " --> %d.\n", see );
                    pos_end = se;
                }

                size_t pos_len = (size_t)( pos_end - c );
                size_t copied = cc_str_copy( c, pos_end, pos_len, pos, CC_MAX_LEN_CHAR_8 );

                if ( pos_len != copied )
                    CC_PRINTF_IF_INFO( "Check len? %zu != %zu\n", pos_len, copied );

                if ( !cc_convert_starting_pos( pos, &file, &rank ) ||
                     !CC_IS_POS_ON_BOARD( g->chessboard->size, file, rank ) )
                {
                    CC_PRINTF_IF_INFO( "Invalid step: '%s', '%s', '%s' .\n", c, pos, p );

                    char * ply__a = cc_str_copy__new( ply_start, ply_end, CC_MAX_LEN_ZERO_TERMINATED );
                    char * step__a = cc_str_copy__new( step_start, step_end, CC_MAX_LEN_ZERO_TERMINATED );

                    cc_parse_msgs_append_if_format( parse_msgs__io,
                                                    CC_PMTE_Error,
                                                    CC_MAX_LEN_ZERO_TERMINATED,
                                                    "Invalid char(s) in step '%s', in ply '%s'.\n",
                                                    step__a,
                                                    ply__a );

                    CC_FREE( step__a );
                    CC_FREE( ply__a );
                    return false;
                }

                CC_PRINTF_IF_INFO( "Step: %d, %d.\n", file, rank );

                CcSteps * step__w = cc_steps_append_if( &steps__a, sle, cc_pos( file, rank ) );

                if ( !step__w )
                {
                    cc_steps_free_all( &steps__a );
                    return false;
                }

            }
        }
        else // !ply_has_steps
        {
            if ( disambiguation[ 0 ] != '\0' )
            {
                steps__a = cc_steps__new( CC_SLE_Start, cc_pos( file_da, rank_da ) );
            }

            if ( pos[ 0 ] != '\0' )
            {
                CcSteps * step__w = cc_steps_append_if( &steps__a,
                                                        CC_SLE_Destination,
                                                        cc_pos( file_pos, rank_pos ) );

                if ( !step__w )
                {
                    cc_steps_free_all( &steps__a );
                    return false;
                }
            }
            else
            {
                char * ply__a = cc_str_copy__new( ply_start, ply_end, CC_MAX_LEN_ZERO_TERMINATED );

                cc_parse_msgs_append_if_format( parse_msgs__io,
                                                CC_PMTE_Error,
                                                CC_MAX_LEN_ZERO_TERMINATED,
                                                "Destination not found, in ply '%s'.\n",
                                                ply__a );

                CC_FREE( ply__a );
                cc_steps_free_all( &steps__a );
                return false;
            }
        }


// TODO :: movement

// TODO :: find starting position


        {
            bool is_light_piece = CC_GAME_STATUS_IS_LIGHT_TURN( g->status );
// TODO :: find if piece light, based on starting position, if ply is cascading ...
                // ( ple == CC_PLE_StartingPly ) ? CC_GAME_STATUS_IS_LIGHT_TURN( g->status )
                //                               : true; // TODO :: not really true
// TODO :: find if piece light, based on starting position, if ply is cascading ...

            piece = cc_piece_from_symbol( piece_symbol, is_light_piece );

            CC_PRINTF_IF_INFO( "Piece: '%c' --> %d.\n", piece_symbol, piece );

            ++c;
        }


        // if ( CC_IS_PLY_GATHER_END( *c ) ) ++c; // Move past ']'. // TODO (?)

        if ( ply_end && *ply_end != '\0' )
            CC_PRINTF_IF_INFO( "\n" );
    } // while ( cc_ply_iter( ... ) )
    CC_PRINTF_IF_INFO( "-----------------------------------------------------------------------\n" );

// TODO :: loop over plies


// TODO :: post-plies status



    if ( !cc_moves_append_if( &( g->moves ), move_an_str, CC_MAX_LEN_ZERO_TERMINATED ) )
        return false;

// TODO :: determine ending status
// TODO :: determine winning status
    g->status = cc_game_status_next( g->status, false, false );

    return true;
}
