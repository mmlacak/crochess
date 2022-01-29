// Copyright (c) 2021 Mario Mlačak, mmlacak@gmail.com
// Licensed under GNU GPL v3+ license. See LICENSING, COPYING files for details.

#include <stdlib.h>
#include <string.h>
#include <stdio.h>

#include "cc_defines.h"
#include "cc_format_moves.h"
#include "cc_str_utils.h"

/**
    @file cc_format_moves.c
    @brief Format moves, plies, steps as algebraic notation strings.
*/


CcFormatRank cc_format_rank_zero()
{
    CcFormatRank rank = { .rank = { '\0', '\0', '\0', '\0' } };
    return rank;
}

bool cc_format_rank_is_zero( CcFormatRank rank )
{
    bool result = true;

    for ( int i = 0; i < CC_FORMAT_RANK_SIZE; ++i )
        result = ( rank.rank[ i ] == '\0' ) && result;

    return result;
}

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

bool cc_if_wrap_ply_in_square_brackets( CcMove * restrict move,
                                        CcPly * restrict ply,
                                        CcFormatMove format_move )
{
    if ( format_move.wrap == CC_WPISB_Never ) return false;
    if ( format_move.wrap == CC_WPISB_Always ) return true;

    if ( !move ) return format_move.default_wrap;

    size_t ply_count = cc_move_ply_count( move );

    if ( format_move.wrap == CC_WPISB_IfCascading )
        return ( ply_count > 1 );

    if ( !ply ) return format_move.default_wrap;

    CcStep * steps = ply->steps;
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
    char * an__a = (char *)malloc( 4 );
    if ( !an__a ) return NULL;

    snprintf( an__a, 3, "%-hhu", (unsigned char)( j + 1 ) );

    return an__a;
}

CcFormatRank cc_format_pos_rank( int j )
{
    CcFormatRank rank = cc_format_rank_zero();

    if ( ( j < CC_MIN_BOARD_COORD ) || ( CC_MAX_BOARD_COORD < j ) ) return rank;

    snprintf( rank.rank, CC_FORMAT_RANK_LENGTH, "%-hhu", (unsigned char)( j + 1 ) );

    return rank;
}

char const * cc_format_lost_tag( CcTagEnum te )
{
    switch ( te )
    {
        case CC_TE_CanRush : return "::";
        case CC_TE_CanCastle : return "&&";
        case CC_TE_DelayedPromotion : return "==";

        default : return "";
    }
}

char * cc_format_side_effect_new( CcSideEffect * restrict side_effect,
                                  CcFormatMove format_move )
{
    if ( !side_effect ) return NULL;

    cc_piece_fp_char_value_t fp_char_value =
        ( format_move.do_dark_pieces_uppercase ) ? cc_piece_symbol
                                                 : cc_piece_as_char;

    CcSideEffect * se = side_effect;
    char * an__a = NULL;

    switch ( se->type )
    {
        case CC_SEE_None : break;

        case CC_SEE_Capture :
        {
            if ( format_move.usage <= CC_FSUE_User )
            {
                an__a = cc_str_append_format_new( &an__a,
                                                  BUFSIZ,
                                                  "*" );
            }
            else
            {
                an__a = cc_str_append_format_new( &an__a,
                                                  BUFSIZ,
                                                  "*%c%s",
                                                  fp_char_value( se->capture.piece ),
                                                  cc_format_lost_tag( se->capture.lost_tag ));
            }

            break;
        }

        case CC_SEE_Displacement :
        {
            bool is_user = ( format_move.usage <= CC_FSUE_User );

            char piece = fp_char_value( se->displacement.piece );
            char const * lost_tag = cc_format_lost_tag( se->displacement.lost_tag );
            char file = cc_format_pos_file( se->displacement.dest_i );
            CcFormatRank rank = cc_format_pos_rank( se->displacement.dest_j );

            if ( is_user )
            {
                if ( !cc_format_rank_is_zero( rank ) )
                {
                    an__a = cc_str_append_format_new( &an__a,
                                                      BUFSIZ,
                                                      "<%c%s",
                                                      file,
                                                      rank.rank );
                }
            }
            else
            {
                if ( !cc_format_rank_is_zero( rank ) )
                {
                    an__a = cc_str_append_format_new( &an__a,
                                                      BUFSIZ,
                                                      "<%c%s%c%s",
                                                      piece,
                                                      lost_tag,
                                                      file,
                                                      rank.rank );
                }
            }

            break;
        }

        case CC_SEE_EnPassant :
        {
            if ( format_move.usage <= CC_FSUE_User )
            {
                an__a = cc_str_append_format_new( &an__a,
                                                  BUFSIZ,
                                                  ":" );
            }
            else if ( format_move.usage <= CC_FSUE_Clarification )
            {
                CcFormatRank rank = cc_format_pos_rank( se->en_passant.dest_j );

                if ( !cc_format_rank_is_zero( rank ) )
                {
                    an__a = cc_str_append_format_new( &an__a,
                                                      BUFSIZ,
                                                      ":%s",
                                                      rank.rank );
                }
            }
            else
            {
                char piece = fp_char_value( se->en_passant.pawn );
                char file = cc_format_pos_file( se->en_passant.dest_i );
                CcFormatRank rank = cc_format_pos_rank( se->en_passant.dest_j );

                if ( !cc_format_rank_is_zero( rank ) )
                {
                    an__a = cc_str_append_format_new( &an__a,
                                                      BUFSIZ,
                                                      ":%c%c%s",
                                                      piece,
                                                      file,
                                                      rank.rank );
                }
            }

            break;
        }

        case CC_SEE_Castle :
        {
            if ( format_move.usage <= CC_FSUE_User )
            {
                an__a = cc_str_append_format_new( &an__a,
                                                  BUFSIZ,
                                                  "&" );
            }
            else if ( format_move.usage <= CC_FSUE_Clarification )
            {
                char file_2 = cc_format_pos_file( se->castle.dest_i );
                an__a = cc_str_append_format_new( &an__a,
                                                  BUFSIZ,
                                                  "&%c",
                                                  file_2 );
            }
            else
            {
                char file_1 = cc_format_pos_file( se->castle.start_i );
                CcFormatRank rank_1 = cc_format_pos_rank( se->castle.start_j );

                char file_2 = cc_format_pos_file( se->castle.dest_i );
                CcFormatRank rank_2 = cc_format_pos_rank( se->castle.dest_j );

                if ( ( !cc_format_rank_is_zero( rank_1 ) )
                  && ( !cc_format_rank_is_zero( rank_2 ) ) )
                {
                    an__a = cc_str_append_format_new( &an__a,
                                                      BUFSIZ,
                                                      "&%c%c%s-%c%s",
                                                      fp_char_value( se->castle.rook ),
                                                      file_1,
                                                      rank_1.rank,
                                                      file_2,
                                                      rank_2.rank );
                }
            }

            break;
        }

        case CC_SEE_Promotion :
        {
            char const * fmt = ( format_move.usage <= CC_FSUE_User ) ? "%c" : "=%c";

            an__a = cc_str_append_format_new( &an__a,
                                              BUFSIZ,
                                              fmt,
                                              fp_char_value( se->promote.piece ) );

            break;
        }

        case CC_SEE_TagForPromotion :
        {
            an__a = cc_str_append_format_new( &an__a,
                                              BUFSIZ,
                                              "=" );
            break;
        }

        case CC_SEE_Conversion :
        {
            if ( format_move.usage <= CC_FSUE_User )
            {
                an__a = cc_str_append_format_new( &an__a,
                                                  BUFSIZ,
                                                  "%%" );
            }
            else
            {
                an__a = cc_str_append_format_new( &an__a,
                                                  BUFSIZ,
                                                  "%%%c%s",
                                                  fp_char_value( se->convert.piece ),
                                                  cc_format_lost_tag( se->convert.lost_tag ) );
            }

            break;
        }

        case CC_SEE_FailedConversion :
        {
            an__a = cc_str_append_format_new( &an__a,
                                              BUFSIZ,
                                              "%%%%" );
            break;
        }

        case CC_SEE_Demotion :
        {
            char file = cc_format_pos_file( se->demote.dest_i );
            CcFormatRank rank = cc_format_pos_rank( se->demote.dest_j );

            if ( format_move.usage <= CC_FSUE_User )
            {
                if ( !cc_format_rank_is_zero( rank ) )
                {
                    an__a = cc_str_append_format_new( &an__a,
                                                      BUFSIZ,
                                                      ">%c%s",
                                                      file,
                                                      rank.rank );
                }
            }
            else
            {
                if ( !cc_format_rank_is_zero( rank ) )
                {
                    an__a = cc_str_append_format_new( &an__a,
                                                      BUFSIZ,
                                                      ">%c%c%s",
                                                      fp_char_value( se->demote.piece ),
                                                      file,
                                                      rank.rank );
                }
            }

            break;
        }

        case CC_SEE_Resurrection :
        {
            if ( CC_PIECE_IS_WEIGHTLESS( se->resurrect.piece )
                || ( format_move.usage >= CC_FSUE_Clarification ) )
            {
                char file = cc_format_pos_file( se->resurrect.dest_i );
                CcFormatRank rank = cc_format_pos_rank( se->resurrect.dest_j );

                if ( !cc_format_rank_is_zero( rank ) )
                {
                    an__a = cc_str_append_format_new( &an__a,
                                                      BUFSIZ,
                                                      "$%c%c%s",
                                                      fp_char_value( se->resurrect.piece ),
                                                      file,
                                                      rank.rank );
                }
            }
            else
            {
                an__a = cc_str_append_format_new( &an__a,
                                                  BUFSIZ,
                                                  "$%c",
                                                  fp_char_value( se->resurrect.piece ) );
            }

            break;
        }

        case CC_SEE_FailedResurrection :
        {
            an__a = cc_str_append_format_new( &an__a,
                                              BUFSIZ,
                                              "$$" );
            break;
        }
    }

    return an__a;
}

char * cc_format_step_new( CcMove * restrict move,
                           CcPly * restrict ply,
                           CcStep * restrict step,
                           CcFormatMove format_move,
                           bool * restrict has_preceding_step__io )
{
    if ( !move ) return NULL;
    if ( !ply ) return NULL;
    if ( !step ) return NULL;
    if ( !has_preceding_step__io ) return NULL;

    char * an__a = NULL;

    if ( step->usage <= format_move.usage )
    {
        size_t step_count = cc_ply_step_count( ply, format_move.usage, true );

        if ( *has_preceding_step__io || ( step_count > 1 ) )
        {
            switch ( step->link )
            {
                case CC_SLE_Start : break;
                case CC_SLE_Reposition : break;
                case CC_SLE_Next : an__a = cc_str_duplicate_new( ".", false, 1 ); break;
                case CC_SLE_Distant : an__a = cc_str_duplicate_new( "..", false, 2 ); break;
                case CC_SLE_Destination : an__a = cc_str_duplicate_new( "-", false, 1 ); break;
            }
        }

        if ( step->link == CC_SLE_Reposition )
            an__a = cc_str_duplicate_new( ",", false, 1 );

        *has_preceding_step__io = true;

        an__a = cc_str_append_format_new( &an__a,
                                          BUFSIZ,
                                          "%c",
                                          cc_format_pos_file( step->i ) );

        if ( !( CC_SIDE_EFFECT_ENUM_IS_CASTLING( step->side_effect.type )
                && ( step->usage <= CC_FSUE_Clarification ) ) )
        {
            CcFormatRank rank = cc_format_pos_rank( step->j );
            an__a = cc_str_extend_new( &an__a,
                                       rank.rank,
                                       BUFSIZ );
        }

        char * se__t = cc_format_side_effect_new( &(step->side_effect), format_move );
        an__a = cc_str_append_new( &an__a,
                                   &se__t,
                                   BUFSIZ );
    }

    return an__a;
}

char * cc_format_ply_new( CcMove * restrict move,
                          CcPly * restrict ply,
                          CcFormatMove format_move )
{
    if ( !move ) return NULL;
    if ( !ply ) return NULL;

    if ( ply->piece == CC_PE_None ) return NULL;

    cc_piece_fp_char_value_t fp_char_value = ( format_move.do_dark_pieces_uppercase )
                                           ? cc_piece_symbol
                                           : cc_piece_as_char;

    bool is_first_ply = ( ply == move->plies );
    char const * ply_tilde = ( is_first_ply ) ? "" : "~";
    char * an__a = NULL;

    switch ( ply->link )
    {
        case CC_PLE_Ply : an__a = cc_str_duplicate_new( ply_tilde, false, 1 ); break;
        case CC_PLE_Teleportation : an__a = cc_str_duplicate_new( "|", false, 1 ); break;
        case CC_PLE_FailedTeleportation : an__a = cc_str_duplicate_new( "||", false, 2 ); break;
        case CC_PLE_TranceJourney : an__a = cc_str_duplicate_new( "@", false, 1 ); break;
        case CC_PLE_DualTranceJourney : an__a = cc_str_duplicate_new( "@@", false, 2 ); break;
        case CC_PLE_FailedTranceJourney : an__a = cc_str_duplicate_new( "@@@", false, 3 ); break;
        case CC_PLE_PawnSacrifice : an__a = cc_str_duplicate_new( ":::", false, 3 ); break;
    }

    bool do_wrap = cc_if_wrap_ply_in_square_brackets( move, ply, format_move );

    if ( do_wrap )
        an__a = cc_str_append_format_new( &an__a,
                                          BUFSIZ,
                                          "%c",
                                          '[' );

    if ( format_move.do_format_with_pawn_symbol )
        an__a = cc_str_append_format_new( &an__a,
                                          BUFSIZ,
                                          "%c",
                                          fp_char_value( ply->piece ) );
    else
    {
        if ( ( ply->piece != CC_PE_DarkPawn ) && ( ply->piece != CC_PE_LightPawn ) )
            an__a = cc_str_append_format_new( &an__a,
                                              BUFSIZ,
                                              "%c",
                                              fp_char_value( ply->piece ) );
    }

    CcStep * step = ply->steps;
    bool has_preceding_step = false;

    while ( step )
    {
        char * ply_an__t = cc_format_step_new( move, ply, step, format_move, &has_preceding_step );
        an__a = cc_str_append_new( &an__a,
                                   &ply_an__t,
                                   BUFSIZ );

        step = step->next;
    }

    if ( do_wrap )
        an__a = cc_str_append_format_new( &an__a,
                                          BUFSIZ,
                                          "%c",
                                          ']' );

    return an__a;
}

char * cc_format_move_new( CcMove * restrict move,
                           CcFormatMove format_move )
{
    if ( !move ) return NULL;
    if ( !move->plies ) return NULL;

    char * an__a = NULL;
    CcPly * ply = move->plies;

    while ( ply )
    {
        char * ply_an__t = cc_format_ply_new( move, ply, format_move );
        an__a = cc_str_append_new( &an__a,
                                   &ply_an__t,
                                   BUFSIZ );

        ply = ply->next;
    }

    char status = '\0';
    if ( move->status  == CC_MSE_Check ) status = '+';
    else if ( move->status  == CC_MSE_Checkmate ) status = '#';

    if ( status != '\0' )
        an__a = cc_str_append_format_new( &an__a,
                                          BUFSIZ,
                                          "%c",
                                          status );

    return an__a;
}
