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
    char * an__t = (char *)malloc( 4 );
    if ( !an__t ) return NULL;

    snprintf( an__t, 3, "%-hhu", (unsigned char)(j+1) );

    return an__t;
}

// TODO :: char[ 2 ] cc_format_pos_rank( int j )

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
    char * an__t = NULL;

    switch ( se->type )
    {
        case CC_SEE_None : break;

        case CC_SEE_Capture :
        {
            if ( format_move.usage <= CC_FSUE_User )
            {
                an__t = cc_str_append_format_new( &an__t,
                                                  BUFSIZ,
                                                  "*" );
            }
            else
            {
                an__t = cc_str_append_format_new( &an__t,
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
            char * rank__a = cc_format_pos_rank_new( se->displacement.dest_j );

            if ( is_user )
            {
                if ( rank__a )
                {
                    an__t = cc_str_append_format_new( &an__t,
                                                      BUFSIZ,
                                                      "<%c%s",
                                                      file,
                                                      rank__a );
                    CC_FREE( rank__a );
                }
            }
            else
            {
                if ( rank__a )
                {
                    an__t = cc_str_append_format_new( &an__t,
                                                      BUFSIZ,
                                                      "<%c%s%c%s",
                                                      piece,
                                                      lost_tag,
                                                      file,
                                                      rank__a );
                    CC_FREE( rank__a );
                }
            }

            break;
        }

        case CC_SEE_EnPassant :
        {
            if ( format_move.usage <= CC_FSUE_User )
            {
                an__t = cc_str_append_format_new( &an__t,
                                                  BUFSIZ,
                                                  ":" );
            }
            else if ( format_move.usage <= CC_FSUE_Clarification )
            {
                char * rank__a = cc_format_pos_rank_new( se->en_passant.dest_j );

                if ( rank__a )
                {
                    an__t = cc_str_append_format_new( &an__t,
                                                      BUFSIZ,
                                                      ":%s",
                                                      rank__a );
                    CC_FREE( rank__a );
                }
            }
            else
            {
                char piece = fp_char_value( se->en_passant.piece );
                char file = cc_format_pos_file( se->en_passant.dest_i );
                char * rank__a = cc_format_pos_rank_new( se->en_passant.dest_j );

                if ( rank__a )
                {
                    an__t = cc_str_append_format_new( &an__t,
                                                      BUFSIZ,
                                                      ":%c%c%s",
                                                      piece,
                                                      file,
                                                      rank__a );
                    CC_FREE( rank__a );
                }
            }

            break;
        }

        case CC_SEE_Castle :
        {
            if ( format_move.usage <= CC_FSUE_User )
            {
                an__t = cc_str_append_format_new( &an__t,
                                                  BUFSIZ,
                                                  "&" );
            }
            else if ( format_move.usage <= CC_FSUE_Clarification )
            {
                char file_2 = cc_format_pos_file( se->castle.dest_i );
                an__t = cc_str_append_format_new( &an__t,
                                                  BUFSIZ,
                                                  "&%c",
                                                  file_2 );
            }
            else
            {
                char file_1 = cc_format_pos_file( se->castle.start_i );
                char * rank_1__a = cc_format_pos_rank_new( se->castle.start_j );

                char file_2 = cc_format_pos_file( se->castle.dest_i );
                char * rank_2__a = cc_format_pos_rank_new( se->castle.dest_j );

                if ( rank_1__a && rank_2__a )
                {
                    an__t = cc_str_append_format_new( &an__t,
                                                      BUFSIZ,
                                                      "&%c%c%s-%c%s",
                                                      fp_char_value( se->castle.rook ),
                                                      file_1,
                                                      rank_1__a,
                                                      file_2,
                                                      rank_2__a );
                }

                CC_FREE( rank_1__a );
                CC_FREE( rank_2__a );
            }

            break;
        }

        case CC_SEE_Promotion :
        {
            char const * fmt = ( format_move.usage <= CC_FSUE_User ) ? "%c" : "=%c";

            an__t = cc_str_append_format_new( &an__t,
                                              BUFSIZ,
                                              fmt,
                                              fp_char_value( se->promote.piece ) );

            break;
        }

        case CC_SEE_TagForPromotion :
        {
            an__t = cc_str_append_format_new( &an__t,
                                              BUFSIZ,
                                              "=" );
            break;
        }

        case CC_SEE_Conversion :
        {
            if ( format_move.usage <= CC_FSUE_User )
            {
                an__t = cc_str_append_format_new( &an__t,
                                                  BUFSIZ,
                                                  "%%" );
            }
            else
            {
                an__t = cc_str_append_format_new( &an__t,
                                                  BUFSIZ,
                                                  "%%%c%s",
                                                  fp_char_value( se->convert.piece ),
                                                  cc_format_lost_tag( se->convert.lost_tag ) );
            }

            break;
        }

        case CC_SEE_FailedConversion :
        {
            an__t = cc_str_append_format_new( &an__t,
                                              BUFSIZ,
                                              "%%%%" );
            break;
        }

        case CC_SEE_Demotion :
        {
            char file = cc_format_pos_file( se->demote.dest_i );
            char * rank__a = cc_format_pos_rank_new( se->demote.dest_j );

            if ( format_move.usage <= CC_FSUE_User )
            {
                if ( rank__a )
                {
                    an__t = cc_str_append_format_new( &an__t,
                                                      BUFSIZ,
                                                      ">%c%s",
                                                      file,
                                                      rank__a );
                }
            }
            else
            {
                if ( rank__a )
                {
                    an__t = cc_str_append_format_new( &an__t,
                                                      BUFSIZ,
                                                      ">%c%c%s",
                                                      fp_char_value( se->demote.piece ),
                                                      file,
                                                      rank__a );
                }
            }

            CC_FREE( rank__a );
            break;
        }

        case CC_SEE_Resurrection :
        {
            if ( CC_PIECE_IS_WEIGHTLESS( se->resurrect.piece )
                || ( format_move.usage >= CC_FSUE_Clarification ) )
            {
                char file = cc_format_pos_file( se->resurrect.dest_i );
                char * rank__a = cc_format_pos_rank_new( se->resurrect.dest_j );

                if ( rank__a )
                {
                    an__t = cc_str_append_format_new( &an__t,
                                                      BUFSIZ,
                                                      "$%c%c%s",
                                                      fp_char_value( se->resurrect.piece ),
                                                      file,
                                                      rank__a );
                    CC_FREE( rank__a );
                }
            }
            else
            {
                an__t = cc_str_append_format_new( &an__t,
                                                  BUFSIZ,
                                                  "$%c",
                                                  fp_char_value( se->resurrect.piece ) );
            }

            break;
        }

        case CC_SEE_FailedResurrection :
        {
            an__t = cc_str_append_format_new( &an__t,
                                              BUFSIZ,
                                              "$$" );
            break;
        }
    }

    return an__t;
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

    char * an__t = NULL;

    if ( step->usage <= format_move.usage )
    {
        size_t step_count = cc_ply_step_count( ply, format_move.usage, true );

        if ( *has_preceding_step__io || ( step_count > 1 ) )
        {
            switch ( step->link )
            {
                case CC_SLE_Start : break;
                case CC_SLE_Reposition : break;
                case CC_SLE_Next : an__t = cc_str_duplicate_new( ".", false, 1 ); break;
                case CC_SLE_Distant : an__t = cc_str_duplicate_new( "..", false, 2 ); break;
                case CC_SLE_Destination : an__t = cc_str_duplicate_new( "-", false, 1 ); break;
            }
        }

        if ( step->link == CC_SLE_Reposition )
            an__t = cc_str_duplicate_new( ",", false, 1 );

        *has_preceding_step__io = true;

        an__t = cc_str_append_format_new( &an__t,
                                          BUFSIZ,
                                          "%c",
                                          cc_format_pos_file( step->i ) );

        if ( !( cc_side_effect_enum_is_castling( step->side_effect.type )
                && ( step->usage <= CC_FSUE_Clarification ) ) )
        {
            char * rank__t = cc_format_pos_rank_new( step->j );
            an__t = cc_str_append_new( &an__t,
                                       &rank__t,
                                       BUFSIZ );
        }

        char * se__t = cc_format_side_effect_new( &(step->side_effect), format_move );
        an__t = cc_str_append_new( &an__t,
                                   &se__t,
                                   BUFSIZ );
    }

    return an__t;
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
    char * an__t = NULL;

    switch ( ply->link )
    {
        case CC_PLE_Ply : an__t = cc_str_duplicate_new( ply_tilde, false, 1 ); break;
        case CC_PLE_Teleportation : an__t = cc_str_duplicate_new( "|", false, 1 ); break;
        case CC_PLE_FailedTeleportation : an__t = cc_str_duplicate_new( "||", false, 2 ); break;
        case CC_PLE_TranceJourney : an__t = cc_str_duplicate_new( "@", false, 1 ); break;
        case CC_PLE_DualTranceJourney : an__t = cc_str_duplicate_new( "@@", false, 2 ); break;
        case CC_PLE_FailedTranceJourney : an__t = cc_str_duplicate_new( "@@@", false, 3 ); break;
        case CC_PLE_PawnSacrifice : an__t = cc_str_duplicate_new( ":::", false, 3 ); break;
    }

    bool do_wrap = cc_if_wrap_ply_in_square_brackets( move, ply, format_move );

    if ( do_wrap )
        an__t = cc_str_append_format_new( &an__t,
                                          BUFSIZ,
                                          "%c",
                                          '[' );

    if ( format_move.do_format_with_pawn_symbol )
        an__t = cc_str_append_format_new( &an__t,
                                          BUFSIZ,
                                          "%c",
                                          fp_char_value( ply->piece ) );
    else
    {
        if ( ( ply->piece != CC_PE_DarkPawn ) && ( ply->piece != CC_PE_LightPawn ) )
            an__t = cc_str_append_format_new( &an__t,
                                              BUFSIZ,
                                              "%c",
                                              fp_char_value( ply->piece ) );
    }

    CcStep * step = ply->steps;
    bool has_preceding_step = false;

    while ( step )
    {
        char * ply_an__t = cc_format_step_new( move, ply, step, format_move, &has_preceding_step );
        an__t = cc_str_append_new( &an__t,
                                   &ply_an__t,
                                   BUFSIZ );

        step = step->next;
    }

    if ( do_wrap )
        an__t = cc_str_append_format_new( &an__t,
                                          BUFSIZ,
                                          "%c",
                                          ']' );

    return an__t;
}

char * cc_format_move_new( CcMove * restrict move,
                           CcFormatMove format_move )
{
    if ( !move ) return NULL;
    if ( !move->plies ) return NULL;

    char * an__t = NULL;
    CcPly * ply = move->plies;

    while ( ply )
    {
        char * ply_an__t = cc_format_ply_new( move, ply, format_move );
        an__t = cc_str_append_new( &an__t,
                                   &ply_an__t,
                                   BUFSIZ );

        ply = ply->next;
    }

    char status = '\0';
    if ( move->status  == CC_MSE_Check ) status = '+';
    else if ( move->status  == CC_MSE_Checkmate ) status = '#';

    if ( status != '\0' )
        an__t = cc_str_append_format_new( &an__t,
                                          BUFSIZ,
                                          "%c",
                                          status );

    return an__t;
}
