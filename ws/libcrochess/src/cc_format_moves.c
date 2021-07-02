// Copyright (c) 2021 Mario Mlačak, mmlacak@gmail.com
// Licensed under 3-clause (modified) BSD license. See LICENSE for details.

#include <stdlib.h>
#include <string.h>
#include <stdio.h>

#include "cc_defines.h"
#include "cc_format_moves.h"
#include "cc_str_utils.h"


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

char * cc_format_side_effect_new(   CcChessboard const * const restrict cb,
                                    CcMove const * const restrict move,
                                    CcPly const * const restrict ply,
                                    CcStep const * const restrict step,
                                    CcSideEffect const * const restrict side_effect )
{
    if ( !cb ) return NULL;
    if ( !move ) return NULL;
    if ( !ply ) return NULL;
    if ( !step ) return NULL;
    if ( !side_effect ) return NULL;

    CcSideEffect const * const se = side_effect;
    char * result = NULL;

    switch ( se->type )
    {
        case CC_SEE_None : break;

        case CC_SEE_Capture :
        {
            result = cc_str_append_format_len_new( &result,
                                                   BUFSIZ,
                                                   "*%c%s",
                                                   cc_piece_symbol( se->capture.piece ),
                                                   ( se->capture.is_promo_tag_lost ) ? "==" : "" );
            break;
        }

        case CC_SEE_Displacement :
        {
            char file = cc_format_pos_file( se->displacement.dest_i );
            char * rank = cc_format_pos_rank_new( se->displacement.dest_j );

            if ( rank )
            {
                result = cc_str_append_format_len_new(  &result,
                                                        BUFSIZ,
                                                        "<%c%s%c%s",
                                                        cc_piece_symbol( se->displacement.piece ),
                                                        ( se->displacement.is_promo_tag_lost ) ? "==" : "",
                                                        file,
                                                        rank );
                free( rank );
            }

            break;
        }

        case CC_SEE_EnPassant :
        {
            char file = cc_format_pos_file( se->en_passant.dest_i );
            char * rank = cc_format_pos_rank_new( se->en_passant.dest_j );

            if ( rank )
            {
                result = cc_str_append_format_len_new( &result, BUFSIZ, ":%c%s", file, rank );
                free( rank );
            }

            break;
        }

        case CC_SEE_Castle :
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
                                                        cc_piece_symbol( se->castle.rook ),
                                                        file_1,
                                                        rank_1,
                                                        file_2,
                                                        rank_2 );
            }

            free( rank_1 );
            free( rank_2 );
            break;
        }

        case CC_SEE_Promotion :
        {
            result = cc_str_append_format_len_new( &result,
                                                   BUFSIZ,
                                                   "=%c",
                                                   cc_piece_symbol( se->promote.piece ) );
            break;
        }

        case CC_SEE_TagForPromotion :
        {
            result = cc_str_append_format_len_new( &result, BUFSIZ, "=" );
            break;
        }

        case CC_SEE_Conversion :
        {
            result = cc_str_append_format_len_new( &result,
                                                   BUFSIZ,
                                                   "%%%c%s",
                                                   cc_piece_symbol( se->convert.piece ),
                                                   ( se->convert.is_promo_tag_lost ) ? "==" : "" );
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
                                                        cc_piece_symbol( se->demote.piece ),
                                                        file,
                                                        rank );
                free( rank );
            }

            break;
        }

        case CC_SEE_Resurrection :
        {
            char file = cc_format_pos_file( se->resurrect.dest_i );
            char * rank = cc_format_pos_rank_new( se->resurrect.dest_j );

            if ( rank )
            {
                result = cc_str_append_format_len_new(  &result,
                                                        BUFSIZ,
                                                        "$%c%c%s",
                                                        cc_piece_symbol( se->resurrect.piece ),
                                                        file,
                                                        rank );
                free( rank );
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
                           CcStep const * const restrict step )
{
    if ( !cb ) return NULL;
    if ( !move ) return NULL;
    if ( !ply ) return NULL;
    if ( !step ) return NULL;

    char * result = NULL;

    switch ( step->link )
    {
        case CC_SLE_Start : break;
        case CC_SLE_Next : result = cc_str_duplicate_len_new( ".", 1 ); break;
        case CC_SLE_Distant : result = cc_str_duplicate_len_new( "..", 2 ); break;
        case CC_SLE_Destination : result = cc_str_duplicate_len_new( "-", 1 ); break;
    }

    cc_str_append_char( &result, cc_format_pos_file( step->i ) );

    char * rank = cc_format_pos_rank_new( step->j );
    result = cc_str_append_len_new( &result, &rank, BUFSIZ );

    char * se = cc_format_side_effect_new( cb, move, ply, step, &(step->side_effect) );
    result = cc_str_append_len_new( &result, &se, BUFSIZ );

    return result;
}

char * cc_format_ply_new( CcChessboard const * const restrict cb,
                          CcMove const * const restrict move,
                          CcPly const * const restrict ply )
{
    if ( !cb ) return NULL;
    if ( !move ) return NULL;
    if ( !ply ) return NULL;

    if ( ply->piece == CC_PE_None ) return NULL;

    bool is_first_ply = ( ply == move->plies );
    char * ply_tilde = ( is_first_ply ) ? "" : "~";
    char * result = NULL;

    switch ( ply->link )
    {
        case CC_PLE_Ply : result = cc_str_duplicate_len_new( ply_tilde, 1 ); break;
        case CC_PLE_Teleportation : result = cc_str_duplicate_len_new( "|", 1 ); break;
        case CC_PLE_TeleportationWave : result = cc_str_duplicate_len_new( "|", 1 ); break;
        case CC_PLE_FailedTeleportationOblation : result = cc_str_duplicate_len_new( "||", 2 ); break;
        case CC_PLE_FailedTeleportation : result = cc_str_duplicate_len_new( "||", 2 ); break;
        case CC_PLE_TranceJourney : result = cc_str_duplicate_len_new( "@", 1 ); break;
        case CC_PLE_DualTranceJourney : result = cc_str_duplicate_len_new( "@@", 2 ); break;
        case CC_PLE_FailedTranceJourney : result = cc_str_duplicate_len_new( "@@@", 3 ); break;
        case CC_PLE_PawnSacrifice : result = cc_str_duplicate_len_new( "::", 2 ); break;
    }

// TODO
    // if  ( ( ply->piece != CC_PE_DarkPawn ) && ( ply->piece != CC_PE_LightPawn ) )
    //     cc_str_append_char( &result, cc_piece_symbol( ply->piece ) );
    cc_str_append_char( &result, cc_piece_symbol( ply->piece ) );

    CcStep * step = cc_ply_get_steps( ply );

    while ( step )
    {
        char * new = cc_format_step_new( cb, move, ply, step );
        char * appended = cc_str_concatenate_len_new( result, new, BUFSIZ );

        free( result );
        free( new );
        result = appended;

        step = step->next;
    }

    return result;
}

char * cc_format_move_new( CcChessboard const * const restrict cb,
                           CcMove const * const restrict move )
{
    if ( !cb ) return NULL;
    if ( !move ) return NULL;
    if ( !move->plies ) return NULL;

    char * result = NULL;
    CcPly * ply = move->plies;

    while ( ply )
    {
        char * new = cc_format_ply_new( cb, move, ply );
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
