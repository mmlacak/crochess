// Copyright (c) 2021 Mario Mlačak, mmlacak@gmail.com
// Licensed under 3-clause (modified) BSD license. See LICENSE for details.

#include <stdlib.h>
#include <string.h>
#include <stdio.h>

#include "cc_defines.h"
#include "cc_format_moves.h"
#include "cc_str_utils.h"

/**
    @file cc_format_moves.c
    @brief Functions to format move(s) as algebraic notation.
*/


CcFormatMove cc_format_move( CcFormatMoveScopeEnum scope,
                             CcFormatStepUsageEnum usage,
                             bool do_format_with_pawn_symbol,
                             bool do_dark_pieces_uppercase,
                             CcWrapPlyInSquareBracketsEnum wrap,
                             bool default_wrap )
{
    CcFormatMove fmt_mv_t = { .scope = scope,
                              .usage = usage,
                              .do_format_with_pawn_symbol = do_format_with_pawn_symbol,
                              .do_dark_pieces_uppercase = do_dark_pieces_uppercase,
                              .wrap = wrap,
                              .default_wrap = default_wrap };

    return fmt_mv_t;
}

CcFormatMove cc_format_move_user( CcFormatMoveScopeEnum scope )
{
    return cc_format_move( scope, CC_FSUE_User, false, true, CC_WPISB_Never, false );
}

CcFormatMove cc_format_move_output( CcFormatMoveScopeEnum scope )
{
    // return cc_format_move( scope, CC_FSUE_User, false, true, CC_WPISB_IfCascading_HasSteps, false );
    return cc_format_move( scope, CC_FSUE_Clarification, false, true, CC_WPISB_IfCascading_HasSteps, false );
    // return cc_format_move( scope, CC_FSUE_Clarification_NoOutput, false, true, CC_WPISB_IfCascading_HasSteps, false );
    // return cc_format_move( scope, CC_FSUE_Addition, false, true, CC_WPISB_IfCascading_HasSteps, false );
    // return cc_format_move( scope, CC_FSUE_Debug, false, true, CC_WPISB_IfCascading_HasSteps, false );
}

CcFormatMove cc_format_move_debug( CcFormatMoveScopeEnum scope )
{
    return cc_format_move( scope, CC_FSUE_Debug, true, false, CC_WPISB_Always, true );
}

bool cc_if_wrap_ply_in_square_brackets( CcMove const * const restrict move,
                                        CcPly const * const restrict ply,
                                        CcFormatMove const format_move )
{
    if ( format_move.wrap == CC_WPISB_Never ) return false;
    if ( format_move.wrap == CC_WPISB_Always ) return true;

    if ( !move ) return format_move.default_wrap;

    size_t ply_count = cc_move_ply_count( move );

    if ( format_move.wrap == CC_WPISB_IfCascading )
        return ( ply_count > 1 );

    if ( !ply ) return format_move.default_wrap;

    CcStep const * steps = ply->steps;
    size_t step_count = cc_step_count_usage( steps, format_move.usage );

    if ( format_move.wrap == CC_WPISB_IfCascading_HasSteps )
        return ( ( ply_count > 1 ) && ( step_count > 1 ) );

    return format_move.default_wrap;
}


char cc_format_pos_file( int i )
{
    if ( ( i < CC_MIN_BOARD_COORD ) || ( CC_MAX_BOARD_COORD < i ) ) return '?';

    return (char)('a' + i);
}

char * cc_format_pos_rank_new( int j )
{
    if ( ( j < CC_MIN_BOARD_COORD ) || ( CC_MAX_BOARD_COORD < j ) ) return NULL;

    // Unlike clang, gcc does not see that 0 <= j <= 25, so ...
    char * new = (char *)malloc( 4 );
    snprintf( new, 3, "%-hhu", (unsigned char)(j+1) );

    return new;
}

char * cc_format_side_effect_new( CcChessboard const * const restrict cb,
                                  CcMove const * const restrict move,
                                  CcPly const * const restrict ply,
                                  CcStep const * const restrict step,
                                  CcSideEffect const * const restrict side_effect,
                                  CcFormatMove const format_move )
{
    if ( !cb ) return NULL;
    if ( !move ) return NULL;
    if ( !ply ) return NULL;
    if ( !step ) return NULL;
    if ( !side_effect ) return NULL;

    cc_piece_fp_char_value_t fp_char_value =
        ( format_move.do_dark_pieces_uppercase ) ? cc_piece_symbol
                                                 : cc_piece_as_char;

    CcSideEffect const * const se = side_effect;
    char * result = NULL;

    switch ( se->type )
    {
        case CC_SEE_None : break;

        case CC_SEE_Capture :
        {
            if ( format_move.usage <= CC_FSUE_User )
            {
                result = cc_str_append_format_len_new( &result, BUFSIZ, "*" );
            }
            else
            {
                result = cc_str_append_format_len_new( &result,
                                                       BUFSIZ,
                                                       "*%c%s",
                                                       fp_char_value( se->capture.piece ),
                                                       ( se->capture.is_promo_tag_lost ) ? "==" : "" );
            }

            break;
        }

        case CC_SEE_Displacement :
        {
            bool is_user = ( format_move.usage <= CC_FSUE_User );

            char piece = fp_char_value( se->displacement.piece );
            char * is_promo_tag_lost = ( se->displacement.is_promo_tag_lost ) ? "==" : "";
            char file = cc_format_pos_file( se->displacement.dest_i );
            char * rank = cc_format_pos_rank_new( se->displacement.dest_j );

            if ( is_user )
            {
                if ( rank )
                {
                    result = cc_str_append_format_len_new(  &result,
                                                            BUFSIZ,
                                                            "<%c%s",
                                                            file,
                                                            rank );
                    free( rank );
                }
            }
            else
            {
                if ( rank )
                {
                    result = cc_str_append_format_len_new(  &result,
                                                            BUFSIZ,
                                                            "<%c%s%c%s",
                                                            piece,
                                                            is_promo_tag_lost,
                                                            file,
                                                            rank );
                    free( rank );
                }
            }

            break;
        }

        case CC_SEE_EnPassant :
        {
            if ( format_move.usage <= CC_FSUE_User )
            {
                result = cc_str_append_format_len_new( &result, BUFSIZ, ":" );
            }
            else if ( format_move.usage <= CC_FSUE_Clarification )
            {
                char * rank = cc_format_pos_rank_new( se->en_passant.dest_j );

                if ( rank )
                {
                    result = cc_str_append_format_len_new( &result, BUFSIZ, ":%s", rank );
                    free( rank );
                }
            }
            else
            {
                char piece = fp_char_value( se->en_passant.piece );
                char file = cc_format_pos_file( se->en_passant.dest_i );
                char * rank = cc_format_pos_rank_new( se->en_passant.dest_j );

                if ( rank )
                {
                    result = cc_str_append_format_len_new( &result, BUFSIZ, ":%c%c%s", piece, file, rank );
                    free( rank );
                }
            }

            break;
        }

        case CC_SEE_Castle :
        {
            if ( format_move.usage <= CC_FSUE_User )
            {
                result = cc_str_append_format_len_new( &result, BUFSIZ, "&" );
            }
            else if ( format_move.usage <= CC_FSUE_Clarification )
            {
                char file_2 = cc_format_pos_file( se->castle.dest_i );
                result = cc_str_append_format_len_new( &result, BUFSIZ, "&%c", file_2 );
            }
            else
            {
                char file_1 = cc_format_pos_file( se->castle.start_i );
                char * rank_1 = cc_format_pos_rank_new( se->castle.start_j );

                char file_2 = cc_format_pos_file( se->castle.dest_i );
                char * rank_2 = cc_format_pos_rank_new( se->castle.dest_j );

                if ( rank_1 && rank_2 )
                {
                    result = cc_str_append_format_len_new(  &result,
                                                            BUFSIZ,
                                                            "&%c%c%s-%c%s",
                                                            fp_char_value( se->castle.rook ),
                                                            file_1,
                                                            rank_1,
                                                            file_2,
                                                            rank_2 );
                }

                free( rank_1 );
                free( rank_2 );
            }

            break;
        }

        case CC_SEE_Promotion :
        {
            char * fmt = ( format_move.usage <= CC_FSUE_User ) ? "%c" : "=%c";

            result = cc_str_append_format_len_new(  &result,
                                                    BUFSIZ,
                                                    fmt,
                                                    fp_char_value( se->promote.piece ) );

            break;
        }

        case CC_SEE_TagForPromotion :
        {
            result = cc_str_append_format_len_new( &result, BUFSIZ, "=" );
            break;
        }

        case CC_SEE_Conversion :
        {
            if ( format_move.usage <= CC_FSUE_User )
            {
                result = cc_str_append_format_len_new( &result, BUFSIZ, "%%" );
            }
            else
            {
                result = cc_str_append_format_len_new(  &result,
                                                        BUFSIZ,
                                                        "%%%c%s",
                                                        fp_char_value( se->convert.piece ),
                                                        ( se->convert.is_promo_tag_lost ) ? "==" : "" );
            }

            break;
        }

        case CC_SEE_FailedConversion :
        {
            result = cc_str_append_format_len_new( &result, BUFSIZ, "%%%%" );
            break;
        }

        case CC_SEE_Demotion :
        {
            char file = cc_format_pos_file( se->demote.dest_i );
            char * rank = cc_format_pos_rank_new( se->demote.dest_j );

            if ( rank )
            {
                result = cc_str_append_format_len_new(  &result,
                                                        BUFSIZ,
                                                        ">%c%c%s",
                                                        fp_char_value( se->demote.piece ),
                                                        file,
                                                        rank );
                free( rank );
            }

            break;
        }

        case CC_SEE_Resurrection :
        {
            if ( cc_piece_is_lightweight( se->resurrect.piece )
                || ( format_move.usage >= CC_FSUE_Clarification ) )
            {
                char file = cc_format_pos_file( se->resurrect.dest_i );
                char * rank = cc_format_pos_rank_new( se->resurrect.dest_j );

                if ( rank )
                {
                    result = cc_str_append_format_len_new(  &result,
                                                            BUFSIZ,
                                                            "$%c%c%s",
                                                            fp_char_value( se->resurrect.piece ),
                                                            file,
                                                            rank );
                    free( rank );
                }
            }
            else
            {
                result = cc_str_append_format_len_new(  &result,
                                                        BUFSIZ,
                                                        "$%c",
                                                        fp_char_value( se->resurrect.piece ) );
            }

            break;
        }

        case CC_SEE_FailedResurrection :
        {
            result = cc_str_append_format_len_new( &result, BUFSIZ, "$$" );
            break;
        }
    }

    return result;
}

char * cc_format_step_new( CcChessboard const * const restrict cb,
                           CcMove const * const restrict move,
                           CcPly const * const restrict ply,
                           CcStep const * const restrict step,
                           CcFormatMove const format_move,
                           bool * const restrict has_preceding_step_io )
{
    if ( !cb ) return NULL;
    if ( !move ) return NULL;
    if ( !ply ) return NULL;
    if ( !step ) return NULL;
    if ( !has_preceding_step_io ) return NULL;

    char * result = NULL;

    if ( step->usage <= format_move.usage )
    {
        if ( *has_preceding_step_io )
        {
            switch ( step->link )
            {
                case CC_SLE_Start : break;
                case CC_SLE_Reposition : break;
                case CC_SLE_Next : result = cc_str_duplicate_len_new( ".", 1 ); break;
                case CC_SLE_Distant : result = cc_str_duplicate_len_new( "..", 2 ); break;
                case CC_SLE_Destination : result = cc_str_duplicate_len_new( "-", 1 ); break;
            }
        }

        if ( step->link == CC_SLE_Reposition )
            result = cc_str_duplicate_len_new( ",", 1 );

        *has_preceding_step_io = true;

        cc_str_append_char( &result, cc_format_pos_file( step->i ) );

        if ( !( cc_side_effect_enum_is_castling( step->side_effect.type )
                && ( step->usage <= CC_FSUE_Clarification ) ) )
        {
            char * rank = cc_format_pos_rank_new( step->j );
            result = cc_str_append_len_new( &result, &rank, BUFSIZ );
        }

        char * se = cc_format_side_effect_new( cb, move, ply, step, &(step->side_effect), format_move );
        result = cc_str_append_len_new( &result, &se, BUFSIZ );
    }

    return result;
}

char * cc_format_ply_new( CcChessboard const * const restrict cb,
                          CcMove const * const restrict move,
                          CcPly const * const restrict ply,
                          CcFormatMove const format_move )
{
    if ( !cb ) return NULL;
    if ( !move ) return NULL;
    if ( !ply ) return NULL;

    if ( ply->piece == CC_PE_None ) return NULL;

    cc_piece_fp_char_value_t fp_char_value = ( format_move.do_dark_pieces_uppercase )
                                             ? cc_piece_symbol
                                             : cc_piece_as_char;

    bool is_first_ply = ( ply == move->plies );
    char * ply_tilde = ( is_first_ply ) ? "" : "~";
    char * result = NULL;

    switch ( ply->link )
    {
        case CC_PLE_Ply : result = cc_str_duplicate_len_new( ply_tilde, 1 ); break;
        case CC_PLE_Teleportation : result = cc_str_duplicate_len_new( "|", 1 ); break;
        case CC_PLE_FailedTeleportation : result = cc_str_duplicate_len_new( "||", 2 ); break;
        case CC_PLE_TranceJourney : result = cc_str_duplicate_len_new( "@", 1 ); break;
        case CC_PLE_DualTranceJourney : result = cc_str_duplicate_len_new( "@@", 2 ); break;
        case CC_PLE_FailedTranceJourney : result = cc_str_duplicate_len_new( "@@@", 3 ); break;
        case CC_PLE_PawnSacrifice : result = cc_str_duplicate_len_new( "::", 2 ); break;
    }

    bool do_wrap = cc_if_wrap_ply_in_square_brackets( move, ply, format_move );

    if ( do_wrap )
        result = cc_str_concatenate_len_new( result, "[", BUFSIZ );

    if ( format_move.do_format_with_pawn_symbol )
        cc_str_append_char( &result, fp_char_value( ply->piece ) );
    else
    {
        if ( ( ply->piece != CC_PE_DarkPawn ) && ( ply->piece != CC_PE_LightPawn ) )
            cc_str_append_char( &result, fp_char_value( ply->piece ) );
    }

    CcStep const * step = ply->steps;

    bool has_preceding_step = false;

    while ( step )
    {
        char * new = cc_format_step_new( cb, move, ply, step, format_move, &has_preceding_step );
        char * appended = cc_str_concatenate_len_new( result, new, BUFSIZ );

        free( result );
        free( new );
        result = appended;

        step = step->next;
    }

    if ( do_wrap )
        result = cc_str_concatenate_len_new( result, "]", BUFSIZ );

    return result;
}

char * cc_format_move_new( CcChessboard const * const restrict cb,
                           CcMove const * const restrict move,
                           CcFormatMove const format_move )
{
    if ( !cb ) return NULL;
    if ( !move ) return NULL;
    if ( !move->plies ) return NULL;

    char * result = NULL;
    CcPly * ply = move->plies;

    while ( ply )
    {
        char * new = cc_format_ply_new( cb, move, ply, format_move );
        char * appended = cc_str_concatenate_len_new( result, new, BUFSIZ );

        free( result );
        free( new );
        result = appended;

        ply = ply->next;
    }

    char status = '\0';
    if ( move->status  == CC_MSE_Check ) status = '+';
    else if ( move->status  == CC_MSE_Checkmate ) status = '#';

    if ( status != '\0' ) cc_str_append_char( &result, status );

    return result;
}
