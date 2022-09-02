// Copyright (c) 2021, 2022 Mario Mlačak, mmlacak@gmail.com
// Licensed under GNU GPL v3+ license. See LICENSING, COPYING files for details.

#include <ctype.h>
#include <stdarg.h>

#include <string.h>
#include <stdio.h>

#include "cc_defines.h"
#include "cc_str_utils.h"
#include "cc_steps.h"
#include "cc_steps_gen.h"
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


bool cc_do_make_move( char const * restrict move_an_str,
                      CcGame * restrict game__io,
                      CcParseMsgs ** restrict parse_msgs__io )
{
    if ( !move_an_str ) return false;
    if ( !game__io ) return false;
    if ( !parse_msgs__io ) return false;

    if ( !game__io->chessboard ) return false;
    // if ( !game__io->moves ) return false;

    if ( !CC_GAME_STATUS_IS_TURN( game__io->status ) )
    {
        char const * msg =
            ( game__io->status == CC_GSE_None ) ? "Game is not initialized.\n"
                                                : "Game is finished.\n";

        cc_parse_msgs_append_if_format( parse_msgs__io,
                                        CC_PMTE_Error,
                                        CC_MAX_LEN_ZERO_TERMINATED,
                                        msg );
        return false;
    }

    char const * m_str = move_an_str;

    if ( *m_str == '#' )
    {
        if ( *++m_str == '#' )
        {
            // "##" resign
            return cc_check_pre_plies_status( *++m_str, game__io, parse_msgs__io, true, true, false,
                                              CC_MAX_LEN_ZERO_TERMINATED,
                                              "Invalid char(s) after resign.\n" );
        }
        else
        {
            // "#" self-checkmate

// TODO :: Do check if opponent is really (self-)checkmated.
//         Self- is optional, since both players could overlook checkmate,
//         this is option to rectify such a situation.

            return cc_check_pre_plies_status( *m_str, game__io, parse_msgs__io, false, true, true,
                                              CC_MAX_LEN_ZERO_TERMINATED,
                                              "Invalid char(s) after self-checkmate.\n" );
        }
    }

    if ( *m_str == '(' )
    {
        if ( *++m_str == '=' )
        {
            if ( *++m_str == '=' )
            {
                if ( *++m_str == ')' )
                {
                    // "(==)" draw offer accepted

                    if ( cc_check_valid_draw_offer_exists( game__io->moves, game__io->status ) )
                    {
                        return cc_check_pre_plies_status( *++m_str, game__io, parse_msgs__io, false, true, false,
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
                // else if ( *m_str == '=' )
                // {
                //     if ( *++m_str == ')' )
                //     {
                //         // "(===)" draw by rules
                //
                //         return cc_check_pre_plies_status( *++m_str, game__io, parse_msgs__io, false, true, false,
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


    if ( !cc_do_make_plies( move_an_str, game__io, parse_msgs__io ) ) // move_an_str --> m_str (?)
        return false;



// TODO :: post-plies status


    if ( !cc_moves_append_if( &( game__io->moves ), move_an_str, CC_MAX_LEN_ZERO_TERMINATED ) )
        return false;

// TODO :: determine ending status
// TODO :: determine winning status
    game__io->status = cc_game_status_next( game__io->status, false, false );

    return true;
}

bool cc_do_make_plies( char const * restrict move_an_str,
                       CcGame * restrict game__io,
                       CcParseMsgs ** restrict parse_msgs__io )
{
    if ( !move_an_str ) return false;
    if ( !game__io ) return false;
    if ( !parse_msgs__io ) return false;

    if ( !game__io->chessboard ) return false;
    // if ( !game__io->moves ) return false;


    char const * m_str = move_an_str;
    CcChessboard * cb__a = cc_chessboard_duplicate__new( game__io->chessboard );

    char const * ply_start_str = NULL;
    char const * ply_end_str = NULL;

    CcPlyLinkEnum ple = CC_PLE_None;
    char piece_symbol = ' ';
    CcPieceEnum piece = CC_PE_None;
    CcPieceEnum activator = CC_PE_None;
    char const * c_str = NULL;

    char const * step_start_str = NULL;
    char const * step_end_str = NULL;
    bool ply_has_steps = false;

// TODO :: check if castling --> handle as a special case

    // CC_PRINTF_IF_INFO( "\n" );
    CC_PRINTF_IF_INFO( "Move: '%s'.\n\n", move_an_str ); // "\nMove: '%s'.\n\n"

    // <!> Reset before use, otherwise iterator will get confused.
    // ply_start_str = ply_end_str = NULL; // Currently not needed, variables are
    //                                     // defined and set just a few lines above.

    while ( cc_ply_iter( m_str, &ply_start_str, &ply_end_str ) )
    {
        ply_has_steps = cc_ply_has_steps( ply_start_str, ply_end_str );

// TODO :: check if en passant --> handle as a special case

        CC_STR_PRINT_IF_INFO( ply_start_str, ply_end_str, 8192, "Ply: '%s', ", "steps: %d.\n", ply_has_steps );

        ple = cc_starting_ply_link( ply_start_str );
        c_str = ply_start_str + cc_ply_link_len( ple );

        CC_STR_PRINT_IF_INFO( ply_start_str, c_str, 128, "Ply link: '%s'", " --> %d.\n", ple );

        if ( CC_IS_PLY_GATHER_START( *c_str ) ) ++c_str; // Move past '['.

        if ( !cc_ply_piece_symbol( c_str, &piece_symbol ) )
        {
            cc_parse_msgs_append_if_format( parse_msgs__io,
                                            CC_PMTE_Error,
                                            CC_MAX_LEN_ZERO_TERMINATED,
                                            "Invalid piece symbol '%c'.\n",
                                            piece_symbol );
            cc_chessboard_free_all( &cb__a );
            return false;
        }

        if ( CC_IS_PIECE_SYMBOL( *c_str ) ) ++c_str;

        CcLosingTagEnum lte = cc_starting_losing_tag( c_str );

        if ( lte != CC_LTE_None )
        {
            CC_PRINTF_IF_INFO( "Losing tag: '%c%c' --> %d.\n", *c_str, *(c_str+1), lte );
            c_str += cc_losing_tag_len( lte );
        }

        cc_char_8 disambiguation_c8 = CC_CHAR_8_EMPTY;

        char const * end_da_str = cc_starting_pos( c_str, ply_end_str, true, &disambiguation_c8 );

        CC_STR_PRINT_IF_INFO( disambiguation_c8, NULL, CC_MAX_LEN_CHAR_8, "Disambiguation: '%s'", ", pointer: '%p'.\n", end_da_str ); // TODO :: maybe check error (?)

        int file_da = CC_INVALID_OFF_BOARD_COORD_MIN;
        int rank_da = CC_INVALID_OFF_BOARD_COORD_MIN;

        if ( !cc_convert_starting_pos( disambiguation_c8, &file_da, &rank_da ) &&
             ( CC_IS_COORD_ON_BOARD( game__io->chessboard->size, file_da ) ||
               CC_IS_COORD_ON_BOARD( game__io->chessboard->size, rank_da ) ) )
        {
            char * ply_str__a = cc_str_copy__new( ply_start_str, ply_end_str, CC_MAX_LEN_ZERO_TERMINATED );

            cc_parse_msgs_append_if_format( parse_msgs__io,
                                            CC_PMTE_Error,
                                            CC_MAX_LEN_ZERO_TERMINATED,
                                            "Invalid char(s) in disambiguation '%s', in ply '%s'.\n",
                                            disambiguation_c8,
                                            ply_str__a );

            CC_FREE( ply_str__a );
            cc_chessboard_free_all( &cb__a );
            return false;
        }

        CC_PRINTF_IF_INFO( "Disambiguation file, rank: %d, %d.\n", file_da, rank_da );

        if ( end_da_str ) c_str = end_da_str;

        // Destination if ply doesn't have steps, otherwise starting position
        // (in which case disambiguation_c8 must be empty).
        cc_char_8 position_c8 = CC_CHAR_8_EMPTY;

        char const * end_pos_str = cc_starting_pos( c_str, ply_end_str, false, &position_c8 );

        CC_STR_PRINT_IF_INFO( position_c8, NULL, CC_MAX_LEN_CHAR_8, "Pos: '%s'", ", pointer: '%p'.\n", end_pos_str ); // TODO :: maybe check error (?)

        int file_pos = CC_INVALID_OFF_BOARD_COORD_MIN;
        int rank_pos = CC_INVALID_OFF_BOARD_COORD_MIN;

        if ( !cc_convert_starting_pos( position_c8, &file_pos, &rank_pos ) &&
             ( CC_IS_COORD_ON_BOARD( game__io->chessboard->size, file_pos ) ||
               CC_IS_COORD_ON_BOARD( game__io->chessboard->size, rank_pos ) ) )
        {
            char * ply_str__a = cc_str_copy__new( ply_start_str, ply_end_str, CC_MAX_LEN_ZERO_TERMINATED );

            cc_parse_msgs_append_if_format( parse_msgs__io,
                                            CC_PMTE_Error,
                                            CC_MAX_LEN_ZERO_TERMINATED,
                                            "Invalid char(s) in position '%s', in ply '%s'.\n",
                                            position_c8,
                                            ply_str__a );

            CC_FREE( ply_str__a );
            cc_chessboard_free_all( &cb__a );
            return false;
        }

        CC_PRINTF_IF_INFO( "Pos file, rank: %d, %d.\n", file_pos, rank_pos );

        CcSteps * steps__a = NULL;

        if ( ply_has_steps )
        {
            if ( ( disambiguation_c8[ 0 ] != '\0' ) && ( position_c8[ 0 ] != '\0' ) )
            {
                char * ply_str__a = cc_str_copy__new( ply_start_str, ply_end_str, CC_MAX_LEN_ZERO_TERMINATED );

                cc_parse_msgs_append_if_format( parse_msgs__io,
                                                CC_PMTE_Error,
                                                CC_MAX_LEN_ZERO_TERMINATED,
                                                "Disambiguation '%s' preceedes starting position '%s', in ply '%s'.\n",
                                                disambiguation_c8,
                                                position_c8,
                                                ply_str__a );

                CC_FREE( ply_str__a );
                cc_steps_free_all( &steps__a );
                cc_chessboard_free_all( &cb__a );
                return false;
            }
            else if ( disambiguation_c8[ 0 ] != '\0' )
            {
                steps__a = cc_steps__new( CC_SLE_Start, cc_pos( file_da, rank_da ) );
            }
            else if ( position_c8[ 0 ] != '\0' )
            {
                steps__a = cc_steps__new( CC_SLE_Start, cc_pos( file_pos, rank_pos ) );
            }

            if ( end_pos_str ) c_str = end_pos_str;

            // <!> Reset before use, otherwise iterator will get confused.
            step_start_str = step_end_str = NULL;

            while ( cc_step_iter( c_str, ply_end_str, &step_start_str, &step_end_str ) )
            {
                CC_STR_PRINT_IF_INFO( step_start_str, step_end_str, 8192, "Step: '%s'.\n", "" );

                CcStepLinkEnum sle = cc_starting_step_link( step_start_str );
                c_str = step_start_str + cc_step_link_len( sle );

                CC_STR_PRINT_IF_INFO( step_start_str, c_str, 128, "Step link: '%s'", " --> %d.\n", sle );

                cc_char_8 pos_c8 = CC_CHAR_8_EMPTY;
                char const * p_str = pos_c8;

                int file = CC_INVALID_OFF_BOARD_COORD_MIN;
                int rank = CC_INVALID_OFF_BOARD_COORD_MIN;

                CcSideEffectEnum see = CC_SEE_None;
                char const * side_effect_str = cc_find_side_effect( c_str, step_end_str, &see );
                char const * pos_end_str = step_end_str;

                if ( side_effect_str )
                {
                    CC_STR_PRINT_IF_INFO( side_effect_str, step_end_str, 128, "Side-effect: '%s'", " --> %d.\n", see );
                    pos_end_str = side_effect_str;
                }

                size_t pos_len = (size_t)( pos_end_str - c_str );
                size_t copied = cc_str_copy( c_str, pos_end_str, pos_len, pos_c8, CC_MAX_LEN_CHAR_8 );

                if ( pos_len != copied )
                    CC_PRINTF_IF_INFO( "Check len? %zu != %zu\n", pos_len, copied );

                if ( !cc_convert_starting_pos( pos_c8, &file, &rank ) ||
                     !CC_IS_POS_ON_BOARD( game__io->chessboard->size, file, rank ) )
                {
                    CC_PRINTF_IF_INFO( "Invalid step: '%s', '%s', '%s' .\n", c_str, pos_c8, p_str );

                    char * ply_str__a = cc_str_copy__new( ply_start_str, ply_end_str, CC_MAX_LEN_ZERO_TERMINATED );
                    char * step_str__a = cc_str_copy__new( step_start_str, step_end_str, CC_MAX_LEN_ZERO_TERMINATED );

                    cc_parse_msgs_append_if_format( parse_msgs__io,
                                                    CC_PMTE_Error,
                                                    CC_MAX_LEN_ZERO_TERMINATED,
                                                    "Invalid position in step '%s', in ply '%s'.\n",
                                                    step_str__a,
                                                    ply_str__a );

                    CC_FREE( step_str__a );
                    CC_FREE( ply_str__a );
                    cc_steps_free_all( &steps__a );
                    cc_chessboard_free_all( &cb__a );
                    return false;
                }

                CC_PRINTF_IF_INFO( "Step: %d, %d.\n", file, rank );

                CcSteps * step__w = cc_steps_append_if( &steps__a, sle, cc_pos( file, rank ) );

                if ( !step__w )
                {
                    cc_steps_free_all( &steps__a );
                    cc_chessboard_free_all( &cb__a );
                    return false;
                }

            }
        }
        else // if ( !ply_has_steps )
        {
            if ( disambiguation_c8[ 0 ] != '\0' )
            {
                steps__a = cc_steps__new( CC_SLE_Start, cc_pos( file_da, rank_da ) );
            }

            if ( position_c8[ 0 ] != '\0' )
            {
                CcSteps * step__w = cc_steps_append_if( &steps__a,
                                                        CC_SLE_Destination,
                                                        cc_pos( file_pos, rank_pos ) );

                if ( !step__w )
                {
                    cc_steps_free_all( &steps__a );
                    cc_chessboard_free_all( &cb__a );
                    return false;
                }
            }
            else
            {
                char * ply_str__a = cc_str_copy__new( ply_start_str, ply_end_str, CC_MAX_LEN_ZERO_TERMINATED );

                cc_parse_msgs_append_if_format( parse_msgs__io,
                                                CC_PMTE_Error,
                                                CC_MAX_LEN_ZERO_TERMINATED,
                                                "Destination not found, in ply '%s'.\n",
                                                ply_str__a );

                CC_FREE( ply_str__a );

                cc_steps_free_all( &steps__a );
                cc_chessboard_free_all( &cb__a );
                return false;
            }
        }

        char * steps_str__a = cc_steps_to_short_string__new( steps__a );
        if ( steps_str__a )
        {
            CC_PRINTF_IF_INFO( "Steps: '%s'.\n", steps_str__a );
            CC_FREE( steps_str__a );
        }

        bool is_starting_ply = ( ple == CC_PLE_StartingPly );

// TODO :: find if piece light, based on starting position, if ply is cascading ...
        bool is_light_piece =
            is_starting_ply ? CC_GAME_STATUS_IS_LIGHT_TURN( game__io->status )
                            : true; // TODO :: not really true
// TODO :: find if piece light, based on starting position, if ply is cascading ...

        bool include_opponent = !is_starting_ply;

        piece = cc_piece_from_symbol( piece_symbol, is_light_piece );

        CC_PRINTF_IF_INFO( "Piece: '%c' --> %d.\n", piece_symbol, piece );

        // ++c_str;

        CcPos start = CC_POS_INVALID_CAST ;
        CcPos end = CC_POS_INVALID_CAST;
        CcPosLink * path__a = NULL;

        // Position (i.e. position_c8) is destination if ply doesn't have steps,
        // otherwise starting position (in which case disambiguation_c8 must be empty).
        CcPos starting = ply_has_steps ? cc_pos( file_pos, rank_pos )
                                       : cc_pos( file_da, rank_da );

        CcSteps * steps = steps__a;
        while ( steps && steps->next ) steps = steps->next; // rewind
        end = steps->pos;

        cc_char_8 temp = CC_CHAR_8_EMPTY;

        if ( cc_pos_to_short_string( starting, &temp ) )
            CC_PRINTF_IF_INFO( "Found starting: '%s'.\n", temp );

        cc_str_clear( temp, CC_MAX_LEN_CHAR_8 );
        if ( cc_pos_to_short_string( end, &temp ) )
            CC_PRINTF_IF_INFO( "Found end: '%s'.\n", temp );

        while ( cc_piece_pos_iter( cb__a, starting, piece, include_opponent, &start ) )
        {
            cc_char_8 start_str = CC_CHAR_8_EMPTY;
            if ( cc_pos_to_short_string( start, &start_str ) )
                CC_PRINTF_IF_INFO( "Try start: '%s'.\n", start_str );

            if ( is_starting_ply )
                path__a = cc_longest_path__new( cb__a, activator, start, end );
            else
                path__a = cc_shortest_path__new( cb__a, activator, start, end );

            char * path_str__a = cc_pos_link_to_short_string__new( path__a );
            if ( path_str__a )
            {
                CC_PRINTF_IF_INFO( "Path: '%s'.\n", path_str__a );
                CC_FREE( path_str__a );
            }

// TOOD :: check if path__a is congruent with steps__a

            if ( cc_steps_are_congruent( steps__a, path__a ) )
            {
                CC_PRINTF_IF_INFO( "Found it!\n" );



                break;
            }

        }



// TODO :: movement

// TODO :: find starting position


        if ( !CC_PIECE_IS_WAVE( piece ) )
            activator = piece;

        // if ( CC_IS_PLY_GATHER_END( *c_str ) ) ++c_str; // Move past ']'. // TODO (?)


        cc_steps_free_all( &steps__a );

        if ( ply_end_str && *ply_end_str != '\0' )
            CC_PRINTF_IF_INFO( "\n" );
    } // while ( cc_ply_iter( ... ) )
    CC_PRINTF_IF_INFO( "-----------------------------------------------------------------------\n" );

// TODO :: loop over plies


    cc_chessboard_free_all( &cb__a );


    return true;
}
