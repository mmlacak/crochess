// Copyright (c) 2022 Mario Mlačak, mmlacak@gmail.com
// Licensed under GNU GPL v3+ license. See LICENSING, COPYING files for details.

#include "cc_parse_utils.h"
#include "cc_parse_side_effect.h"


// static bool cc_parse_side_effect_error_msg( char const * restrict an_str,
//                                             char const * restrict step_end,
//                                             CcParseMsg ** restrict parse_msgs__iod,
//                                             char const * restrict fmt, ... )
// {
//     char * step_an__a = cc_str_copy__new( an_str, step_end, CC_MAX_LEN_ZERO_TERMINATED );

//     va_list args;
//     va_start( args, fmt );

//     cc_parse_msg_append_format_va_if( parse_msgs__iod,
//                                       CC_PMTE_Error,
//                                       CC_MAX_LEN_ZERO_TERMINATED,
//                                       fmt,
//                                       args );

//     va_end( args );

//     CC_FREE( step_an__a );

//     return false;
// }


bool cc_parse_side_effect( char const * restrict an_str,
                           char const * restrict step_end,
                           CcGame * restrict game,
                           CcChessboard * restrict cb,
                           CcPos step_pos,
                           CcSideEffect * restrict side_effect__o,
                           CcParseMsg ** restrict parse_msgs__iod )
{
    if ( !an_str ) return false;
    if ( !step_end ) return false;
    if ( !game ) return false;
    if ( !cb ) return false;
    if ( !side_effect__o ) return false;
    if ( !parse_msgs__iod ) return false;

    bool has_promotion_sign = true;

    CcSideEffectEnum see =
        cc_starting_side_effect_type( an_str, &has_promotion_sign );

    char const * se_an =
        an_str + cc_side_effect_type_len( see, has_promotion_sign );

    CcPieceEnum step_piece = cc_chessboard_get_piece( cb, step_pos.i, step_pos.j );

    switch ( see )
    {
        case CC_SEE_None :
        {
            *side_effect__o = cc_side_effect_none();
            return true;
        }

        case CC_SEE_Capture :
        {
            if ( !CC_PIECE_CAN_BE_CAPTURED( step_piece ) )
            {
                char * step_an__a = cc_str_copy__new( an_str, step_end, CC_MAX_LEN_ZERO_TERMINATED );

                cc_parse_msg_append_format_if( parse_msgs__iod,
                                               CC_PMTE_Error,
                                               CC_MAX_LEN_ZERO_TERMINATED,
                                               "Piece cannot be captured, in step '%s'.\n",
                                               step_an__a );
                CC_FREE( step_an__a );
                return false;
            }

            char piece_symbol = ' ';

            if ( cc_find_piece_symbol( se_an, &piece_symbol ) )
            {
                if ( !cc_piece_has_congruent_type( piece_symbol, step_piece ) )
                {
                    char * step_an__a = cc_str_copy__new( an_str, step_end, CC_MAX_LEN_ZERO_TERMINATED );

                    cc_parse_msg_append_format_if( parse_msgs__iod,
                                                   CC_PMTE_Error,
                                                   CC_MAX_LEN_ZERO_TERMINATED,
                                                   "Unrecognized piece symbol '%c' in a capturing side-effect, in step '%s'.\n",
                                                   step_an__a );
                    CC_FREE( step_an__a );
                    return false;
                }

                ++se_an;
            }

            CcTagEnum lte = cc_starting_losing_tag( se_an );

            *side_effect__o = cc_side_effect_capture( step_piece, lte );
            return true;
        }

        case CC_SEE_Displacement :
        {
            if ( !CC_PIECE_CAN_BE_DISPLACED( step_piece ) )
            {
                char * step_an__a = cc_str_copy__new( an_str, step_end, CC_MAX_LEN_ZERO_TERMINATED );

                cc_parse_msg_append_format_if( parse_msgs__iod,
                                               CC_PMTE_Error,
                                               CC_MAX_LEN_ZERO_TERMINATED,
                                               "Piece cannot be displaced, in step '%s'.\n",
                                               step_an__a );
                CC_FREE( step_an__a );
                return false;
            }

            char piece_symbol = ' ';

            if ( cc_find_piece_symbol( se_an, &piece_symbol ) )
            {
                if ( !cc_piece_has_congruent_type( piece_symbol, step_piece ) )
                {
                    char * step_an__a = cc_str_copy__new( an_str, step_end, CC_MAX_LEN_ZERO_TERMINATED );

                    cc_parse_msg_append_format_if( parse_msgs__iod,
                                                   CC_PMTE_Error,
                                                   CC_MAX_LEN_ZERO_TERMINATED,
                                                   "Unrecognized piece symbol '%c' in a displacing side-effect, in step '%s'.\n",
                                                   step_an__a );
                    CC_FREE( step_an__a );
                    return false;
                }

                ++se_an;
            }

            CcTagEnum lte = cc_starting_losing_tag( se_an );
            char const * pos_an = se_an + cc_losing_tag_len( lte );

            CcPos pos = CC_POS_CAST_INVALID;
            char const * pos_end_an = NULL;

            if ( !cc_starting_pos( pos_an, &pos, &pos_end_an ) )
            {
                char * step_an__a = cc_str_copy__new( an_str, step_end, CC_MAX_LEN_ZERO_TERMINATED );

                cc_parse_msg_append_format_if( parse_msgs__iod,
                                               CC_PMTE_Error,
                                               CC_MAX_LEN_ZERO_TERMINATED,
                                               "Error parsing side-effect position within step '%s'.\n",
                                               step_an__a );
                CC_FREE( step_an__a );
                return false;
            }

            *side_effect__o = cc_side_effect_displacement( step_piece, lte, pos );
            return true;
        }

        case CC_SEE_EnPassant :

        case CC_SEE_Castle :

        case CC_SEE_Promotion :
        {
            if ( !CC_PIECE_IS_PAWN( step_piece ) )
            {
                char * step_an__a = cc_str_copy__new( an_str, step_end, CC_MAX_LEN_ZERO_TERMINATED );

                cc_parse_msg_append_format_if( parse_msgs__iod,
                                               CC_PMTE_Error,
                                               CC_MAX_LEN_ZERO_TERMINATED,
                                               "Only Pawn can be promoted, in step '%s'.\n",
                                               step_an__a );
                CC_FREE( step_an__a );
                return false;
            }

            char piece_symbol = ' ';

            if ( !cc_find_piece_symbol( se_an, &piece_symbol ) )
            {
                char * step_an__a = cc_str_copy__new( an_str, step_end, CC_MAX_LEN_ZERO_TERMINATED );

                cc_parse_msg_append_format_if( parse_msgs__iod,
                                               CC_PMTE_Error,
                                               CC_MAX_LEN_ZERO_TERMINATED,
                                               "Unrecognized piece symbol '%c' in a promoting side-effect, in step '%s'.\n",
                                               step_an__a );
                CC_FREE( step_an__a );
                return false;
            }

            if ( !cc_piece_has_congruent_type( piece_symbol, step_piece ) )
            {
                char * step_an__a = cc_str_copy__new( an_str, step_end, CC_MAX_LEN_ZERO_TERMINATED );

                cc_parse_msg_append_format_if( parse_msgs__iod,
                                               CC_PMTE_Error,
                                               CC_MAX_LEN_ZERO_TERMINATED,
                                               "Piece symbol '%c' does not correspond to expected Pawn being promoted, in step '%s'.\n",
                                               step_an__a );
                CC_FREE( step_an__a );
                return false;
            }

            *side_effect__o = cc_side_effect_promote( step_piece );
            return true;
        }

        case CC_SEE_TagForPromotion :
        {
            if ( !CC_PIECE_IS_PAWN( step_piece ) )
            {
                char * step_an__a = cc_str_copy__new( an_str, step_end, CC_MAX_LEN_ZERO_TERMINATED );

                cc_parse_msg_append_format_if( parse_msgs__iod,
                                               CC_PMTE_Error,
                                               CC_MAX_LEN_ZERO_TERMINATED,
                                               "Only Pawn can be promoted, in step '%s'.\n",
                                               step_an__a );
                CC_FREE( step_an__a );
                return false;
            }

            *side_effect__o = cc_side_effect_tag_for_promotion();
            return true;
        }

        case CC_SEE_Conversion :

        case CC_SEE_FailedConversion :

        case CC_SEE_DemoteToPawn :

        case CC_SEE_Resurrection :

        case CC_SEE_FailedResurrection :

        default : return false;
    }
}
