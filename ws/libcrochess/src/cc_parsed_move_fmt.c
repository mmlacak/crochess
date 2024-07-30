// Copyright (c) 2021, 2024 Mario Mlačak, mmlacak@gmail.com
// Licensed under GNU GPL v3+ license. See LICENSING, COPYING files for details.

#include <stdlib.h>
#include <string.h>
#include <stdio.h>

#include "cc_defines.h"
#include "cc_parsed_move_fmt.h"
#include "cc_str_utils.h"

/**
    @file cc_format_moves.c
    @brief Format moves, plies, steps as algebraic notation strings.
*/


CcFormatRank cc_format_rank_zero( void ) {
    CcFormatRank rank = { .rank = { '\0', '\0', '\0', '\0' } };
    return rank;
}

bool cc_format_rank_is_zero( CcFormatRank rank ) {
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
                             bool default_wrap ) {
    CcFormatMove fmt_mv_t = { .scope = scope,
                              .usage = usage,
                              .do_format_with_pawn_symbol = do_format_with_pawn_symbol,
                              .do_dark_pieces_uppercase = do_dark_pieces_uppercase,
                              .wrap = wrap,
                              .default_wrap = default_wrap };

    return fmt_mv_t;
}

CcFormatMove cc_format_move_user( CcFormatMoveScopeEnum scope ) {
    return cc_format_move( scope, CC_FSUE_User, false, true, CC_WPISB_Never, false );
}

CcFormatMove cc_format_move_output( CcFormatMoveScopeEnum scope ) {
    // return cc_format_move( scope, CC_FSUE_User, false, true, CC_WPISB_IfCascading_HasSteps, false );
    return cc_format_move( scope, CC_FSUE_Clarification, false, true, CC_WPISB_IfCascading_HasSteps, false );
    // return cc_format_move( scope, CC_FSUE_Clarification_NoOutput, false, true, CC_WPISB_IfCascading_HasSteps, false );
    // return cc_format_move( scope, CC_FSUE_Addition, false, true, CC_WPISB_IfCascading_HasSteps, false );
    // return cc_format_move( scope, CC_FSUE_Debug, false, true, CC_WPISB_IfCascading_HasSteps, false );
}

CcFormatMove cc_format_move_debug( CcFormatMoveScopeEnum scope ) {
    return cc_format_move( scope, CC_FSUE_Debug, true, false, CC_WPISB_Always, true );
}

bool cc_if_wrap_ply_in_square_brackets( CcParsedMove * restrict move,
                                        CcParsedPly * restrict ply,
                                        CcFormatMove format_move ) {
    if ( format_move.wrap == CC_WPISB_Never ) return false;
    if ( format_move.wrap == CC_WPISB_Always ) return true;

    if ( !move ) return format_move.default_wrap;

    size_t ply_count = cc_parsed_move_plies_count( move );

    if ( format_move.wrap == CC_WPISB_IfCascading )
        return ( ply_count > 1 );

    if ( !ply ) return format_move.default_wrap;

    // CcParsedStep * steps = ply->steps;
    // size_t step_count = cc_step_count_usage( steps, format_move.usage );

    // if ( format_move.wrap == CC_WPISB_IfCascading_HasSteps )
    //     return ( ( ply_count > 1 ) && ( step_count > 1 ) );
    if ( format_move.wrap == CC_WPISB_IfCascading_HasSteps )
        return ( ( ply_count > 1 ) );

    return format_move.default_wrap;
}


char cc_format_pos_file( int i ) {
    if ( ( i < CC_MIN_BOARD_COORD ) || ( CC_MAX_BOARD_COORD < i ) ) return '?';

    return (char)('a' + i);
}

char * cc_format_pos_rank__new( int j ) {
    if ( ( j < CC_MIN_BOARD_COORD ) || ( CC_MAX_BOARD_COORD < j ) ) return NULL;

    // Unlike clang, gcc does not see that 0 <= j <= 25, so ...
    char * an__a = (char *)malloc( 4 );
    if ( !an__a ) return NULL;

    snprintf( an__a, 3, "%-hhu", (unsigned char)( j + 1 ) );

    return an__a;
}

CcFormatRank cc_format_pos_rank( int j ) {
    CcFormatRank rank = cc_format_rank_zero();

    if ( ( j < CC_MIN_BOARD_COORD ) || ( CC_MAX_BOARD_COORD < j ) ) return rank;

    snprintf( rank.rank, CC_FORMAT_RANK_LENGTH, "%-hhu", (unsigned char)( j + 1 ) );

    return rank;
}

char const * cc_format_lost_tag( CcLosingTagEnum lte ) {
    switch ( lte )     {
        case CC_LTE_CanRush : return "::";
        case CC_LTE_CanCastle : return "&&";
        case CC_LTE_DelayedPromotion : return "==";

        default : return "";
    }
}

char * cc_format_side_effect__new( CcParsedSideEffect * restrict side_effect,
                                   CcFormatMove format_move ) {
    if ( !side_effect ) return NULL;

    cc_piece_fp_char_value_t fp_char_value =
        ( format_move.do_dark_pieces_uppercase ) ? cc_piece_symbol
                                                 : cc_piece_as_char;

    CcParsedSideEffect * se = side_effect;
    char * an__a = NULL;

    switch ( se->type ) {
        case CC_PSEE_None : break;

        case CC_PSEE_Capture : {
            if ( format_move.usage <= CC_FSUE_User ) {
                an__a = cc_str_append_fmt__new( &an__a,
                                                BUFSIZ,
                                                "*" );
            } else {
                an__a = cc_str_append_fmt__new( &an__a,
                                                BUFSIZ,
                                                "*%c%s",
                                                fp_char_value( se->capture.piece ),
                                                cc_format_lost_tag( se->capture.lost_tag ));
            }

            break;
        }

        case CC_PSEE_Displacement : {
            bool is_user = ( format_move.usage <= CC_FSUE_User );

            char piece = fp_char_value( se->displacement.piece );
            char const * lost_tag = cc_format_lost_tag( se->displacement.lost_tag );
            char file = cc_format_pos_file( se->displacement.destination.i );
            CcFormatRank rank = cc_format_pos_rank( se->displacement.destination.j );

            if ( is_user ) {
                if ( !cc_format_rank_is_zero( rank ) ) {
                    an__a = cc_str_append_fmt__new( &an__a,
                                                    BUFSIZ,
                                                    "<%c%s",
                                                    file,
                                                    rank.rank );
                }
            } else {
                if ( !cc_format_rank_is_zero( rank ) ) {
                    an__a = cc_str_append_fmt__new( &an__a,
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

        case CC_PSEE_EnPassant : {
            if ( format_move.usage <= CC_FSUE_User ) {
                an__a = cc_str_append_fmt__new( &an__a,
                                                BUFSIZ,
                                                ":" );
            } else if ( format_move.usage <= CC_FSUE_Clarification ) {
                CcFormatRank rank = cc_format_pos_rank( se->en_passant.distant.j );

                if ( !cc_format_rank_is_zero( rank ) ) {
                    an__a = cc_str_append_fmt__new( &an__a,
                                                    BUFSIZ,
                                                    ":%s",
                                                    rank.rank );
                }
            } else {
                char piece = fp_char_value( se->en_passant.pawn );
                char file = cc_format_pos_file( se->en_passant.distant.i );
                CcFormatRank rank = cc_format_pos_rank( se->en_passant.distant.j );

                if ( !cc_format_rank_is_zero( rank ) ) {
                    an__a = cc_str_append_fmt__new( &an__a,
                                                    BUFSIZ,
                                                    ":%c%c%s",
                                                    piece,
                                                    file,
                                                    rank.rank );
                }
            }

            break;
        }

        case CC_PSEE_Castle : {
            if ( format_move.usage <= CC_FSUE_User ) {
                an__a = cc_str_append_fmt__new( &an__a,
                                                BUFSIZ,
                                                "&" );
            } else if ( format_move.usage <= CC_FSUE_Clarification ) {
                char file_2 = cc_format_pos_file( se->castle.destination.i );
                an__a = cc_str_append_fmt__new( &an__a,
                                                BUFSIZ,
                                                "&%c",
                                                file_2 );
            } else {
                char file_1 = cc_format_pos_file( se->castle.start.i );
                CcFormatRank rank_1 = cc_format_pos_rank( se->castle.start.j );

                char file_2 = cc_format_pos_file( se->castle.destination.i );
                CcFormatRank rank_2 = cc_format_pos_rank( se->castle.destination.j );

                if ( ( !cc_format_rank_is_zero( rank_1 ) )
                  && ( !cc_format_rank_is_zero( rank_2 ) ) )
                {
                    an__a = cc_str_append_fmt__new( &an__a,
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

        case CC_PSEE_Promotion : {
            char const * fmt = ( format_move.usage <= CC_FSUE_User ) ? "%c" : "=%c";

            an__a = cc_str_append_fmt__new( &an__a,
                                            BUFSIZ,
                                            fmt,
                                            fp_char_value( se->promote.promoted_to ) );

            break;
        }

        case CC_PSEE_TagForPromotion : {
            an__a = cc_str_append_fmt__new( &an__a,
                                            BUFSIZ,
                                            "=" );
            break;
        }

        case CC_PSEE_Conversion : {
            if ( format_move.usage <= CC_FSUE_User ) {
                an__a = cc_str_append_fmt__new( &an__a,
                                                BUFSIZ,
                                                "%%" );
            } else {
                an__a = cc_str_append_fmt__new( &an__a,
                                                BUFSIZ,
                                                "%%%c%s",
                                                fp_char_value( se->convert.piece ),
                                                cc_format_lost_tag( se->convert.lost_tag ) );
            }

            break;
        }

        case CC_PSEE_FailedConversion : {
            an__a = cc_str_append_fmt__new( &an__a,
                                            BUFSIZ,
                                            "%%%%" );
            break;
        }

        case CC_PSEE_Transparency : {
            an__a = cc_str_append_fmt__new( &an__a,
                                            BUFSIZ,
                                            "^%c",
                                            fp_char_value( se->transparency.piece ) );
            break;
        }

        case CC_PSEE_Divergence : {
            an__a = cc_str_append_fmt__new( &an__a,
                                            BUFSIZ,
                                            "/%c",
                                            fp_char_value( se->transparency.piece ) );
            break;
        }

        case CC_PSEE_DemoteToPawn : {
            char file = cc_format_pos_file( se->demote.distant.i );
            CcFormatRank rank = cc_format_pos_rank( se->demote.distant.j );

            if ( format_move.usage <= CC_FSUE_User ) {
                if ( !cc_format_rank_is_zero( rank ) ) {
                    an__a = cc_str_append_fmt__new( &an__a,
                                                    BUFSIZ,
                                                    ">%c%s",
                                                    file,
                                                    rank.rank );
                }
            } else {
                if ( !cc_format_rank_is_zero( rank ) ) {
                    an__a = cc_str_append_fmt__new( &an__a,
                                                    BUFSIZ,
                                                    ">%c%c%s",
                                                    fp_char_value( se->demote.piece ),
                                                    file,
                                                    rank.rank );
                }
            }

            break;
        }

        case CC_PSEE_Resurrection :
        case CC_PSEE_ResurrectingOpponent : {
            char * symbol = ( se->type == CC_PSEE_ResurrectingOpponent ) ? "$$" : "$";

            if ( CC_PIECE_IS_WEIGHTLESS( se->resurrect.piece )
                || ( format_move.usage >= CC_FSUE_Clarification ) ) {
                char file = cc_format_pos_file( se->resurrect.destination.i );
                CcFormatRank rank = cc_format_pos_rank( se->resurrect.destination.j );

                if ( !cc_format_rank_is_zero( rank ) ) {
                    an__a = cc_str_append_fmt__new( &an__a,
                                                    BUFSIZ,
                                                    "%s%c%c%s",
                                                    symbol,
                                                    fp_char_value( se->resurrect.piece ),
                                                    file,
                                                    rank.rank );
                }
            } else {
                an__a = cc_str_append_fmt__new( &an__a,
                                                BUFSIZ,
                                                "%s%c",
                                                symbol,
                                                fp_char_value( se->resurrect.piece ) );
            }

            break;
        }

        case CC_PSEE_FailedResurrection : {
            an__a = cc_str_append_fmt__new( &an__a,
                                            BUFSIZ,
                                            "$$$" );
            break;
        }
    }

    return an__a;
}

char * cc_format_step__new( CcParsedMove * restrict move,
                            CcParsedPly * restrict ply,
                            CcParsedStep * restrict step,
                            CcFormatMove format_move,
                            bool * restrict has_preceding_step__io ) {
    if ( !move ) return NULL;
    if ( !ply ) return NULL;
    if ( !step ) return NULL;
    if ( !has_preceding_step__io ) return NULL;

    char * an__a = NULL;

    /* if ( step->usage <= format_move.usage ) */ {
        // size_t step_count = cc_ply_step_count( ply, format_move.usage, true );
        size_t step_count = cc_parsed_ply_steps_count( ply );

        if ( *has_preceding_step__io || ( step_count > 1 ) ) {
            switch ( step->link ) {
                case CC_PSLE_None : break;
                case CC_PSLE_Start : break;
                case CC_PSLE_Reposition : break;
                case CC_PSLE_Next : an__a = cc_str_duplicate__new( ".", false, 1 ); break;
                case CC_PSLE_Distant : an__a = cc_str_duplicate__new( "..", false, 2 ); break;
                case CC_PSLE_Destination : an__a = cc_str_duplicate__new( "-", false, 1 ); break;
                case CC_PSLE_JustDestination : break;
            }
        }

        if ( step->link == CC_PSLE_Reposition )
            an__a = cc_str_duplicate__new( ",", false, 1 );

        *has_preceding_step__io = true;

        an__a = cc_str_append_fmt__new( &an__a,
                                        BUFSIZ,
                                        "%c",
                                        cc_format_pos_file( step->field.i ) );

        // if ( !( CC_PARSED_SIDE_EFFECT_ENUM_IS_CASTLING( step->side_effect.type )
        //         && ( step->usage <= CC_FSUE_Clarification ) ) ) {
        if ( !( CC_PARSED_SIDE_EFFECT_ENUM_IS_CASTLING( step->side_effect.type ) ) ) {
            // CcFormatRank rank = cc_format_pos_rank( step->field.j );
            // TODO :: cc_str_append__new ?
            // an__a = cc_str_extend__new( &an__a,
            //                             rank.rank,
            //                             BUFSIZ );
        }

        // char * se__t = cc_format_side_effect__new( &(step->side_effect), format_move );
        // TODO :: cc_str_extend__new ?
        // an__a = cc_str_append__new( &an__a,
        //                             &se__t,
        //                             BUFSIZ );
    }

    return an__a;
}

char * cc_format_ply__new( CcParsedMove * restrict move,
                           CcParsedPly * restrict ply,
                           CcFormatMove format_move ) {
    if ( !move ) return NULL;
    if ( !ply ) return NULL;

    if ( ply->piece == CC_PE_None ) return NULL;

    cc_piece_fp_char_value_t fp_char_value = ( format_move.do_dark_pieces_uppercase )
                                           ? cc_piece_symbol
                                           : cc_piece_as_char;

    bool is_first_ply = ( ply == move->plies );
    char const * ply_tilde = ( is_first_ply ) ? "" : "~";
    char * an__a = NULL;

    switch ( ply->link ) {
        case CC_PPLE_None : break;
        case CC_PPLE_StartingPly : break;
        case CC_PPLE_CascadingPly : an__a = cc_str_duplicate__new( ply_tilde, false, 1 ); break;
        case CC_PPLE_Teleportation : an__a = cc_str_duplicate__new( "|", false, 1 ); break;
        case CC_PPLE_TeleportationReemergence : an__a = cc_str_duplicate__new( "||", false, 2 ); break;
        case CC_PPLE_TeleportationOblation : an__a = cc_str_duplicate__new( "|||", false, 3 ); break;
        case CC_PPLE_TranceJourney : an__a = cc_str_duplicate__new( "@", false, 1 ); break;
        case CC_PPLE_DualTranceJourney : an__a = cc_str_duplicate__new( "@@", false, 2 ); break;
        case CC_PPLE_FailedTranceJourney : an__a = cc_str_duplicate__new( "@@@", false, 3 ); break;
        case CC_PPLE_PawnSacrifice : an__a = cc_str_duplicate__new( ":::", false, 3 ); break;
        case CC_PPLE_SenseJourney : an__a = cc_str_duplicate__new( "\"", false, 1 ); break;
        case CC_PPLE_FailedSenseJourney : an__a = cc_str_duplicate__new( "'", false, 1 ); break;
    }

    bool do_wrap = cc_if_wrap_ply_in_square_brackets( move, ply, format_move );

    if ( do_wrap )
        an__a = cc_str_append_fmt__new( &an__a,
                                        BUFSIZ,
                                        "%c",
                                        '[' );

    if ( format_move.do_format_with_pawn_symbol ) {
        an__a = cc_str_append_fmt__new( &an__a,
                                        BUFSIZ,
                                        "%c",
                                        fp_char_value( ply->piece ) );
    } else {
        if ( ( ply->piece != CC_PE_DarkPawn ) && ( ply->piece != CC_PE_LightPawn ) )
            an__a = cc_str_append_fmt__new( &an__a,
                                            BUFSIZ,
                                            "%c",
                                            fp_char_value( ply->piece ) );
    }

    CcParsedStep * step = ply->steps;
    bool has_preceding_step = false;

    while ( step ) {
        // char * ply_an__t = cc_format_step__new( move, ply, step, format_move, &has_preceding_step );
        // TODO :: cc_str_extend__new ?
        // an__a = cc_str_append__new( &an__a,
        //                             &ply_an__t,
        //                             BUFSIZ );

        step = step->next;
    }

    if ( do_wrap ) {
        an__a = cc_str_append_fmt__new( &an__a,
                                        BUFSIZ,
                                        "%c",
                                        ']' );
    }

    return an__a;
}

char * cc_format_move__new( CcParsedMove * restrict move,
                            CcFormatMove format_move ) {
    if ( !move ) return NULL;
    if ( !move->plies ) return NULL;

    char * an__a = NULL;
    CcParsedPly * ply = move->plies;

    while ( ply ) {
        // char * ply_an__t = cc_format_ply__new( move, ply, format_move );
        // TODO :: cc_str_extend__new ?
        // an__a = cc_str_append__new( &an__a,
        //                             &ply_an__t,
        //                             BUFSIZ );

        ply = ply->next;
    }

    char status = '\0';
    if ( move->status  == CC_PMSE_Check ) status = '+';
    else if ( move->status  == CC_PMSE_Checkmate ) status = '#';

    if ( status != '\0' ) {
        an__a = cc_str_append_fmt__new( &an__a,
                                        BUFSIZ,
                                        "%c",
                                        status );
    }

    return an__a;
}
