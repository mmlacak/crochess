// Copyright (c) 2022 Mario Mlačak, mmlacak@gmail.com
// Licensed under GNU GPL v3+ license. See LICENSING, COPYING files for details.

#include "cc_parse_utils.h"
#include "cc_parse_side_effect.h"


// static bool cc_parse_side_effect_error_msg( char const * restrict side_effect_an,
//                                             char const * restrict step_end_an,
//                                             CcParseMsg ** restrict parse_msgs__iod,
//                                             char const * restrict fmt, ... )
// {
//     char * step_an__a = cc_str_copy__new( side_effect_an, step_end_an, CC_MAX_LEN_ZERO_TERMINATED );

//     va_list args;
//     va_start( args, fmt );

//     cc_parse_msg_append_fmt_va_if( parse_msgs__iod,
//                                       CC_PMTE_Error,
//                                       CC_MAX_LEN_ZERO_TERMINATED,
//                                       fmt,
//                                       args );

//     va_end( args );

//     CC_FREE( step_an__a );

//     return false;
// }


bool cc_parse_side_effect( char const * restrict side_effect_an,
                           char const * restrict step_start_an,
                           char const * restrict step_end_an,
                           CcGame * restrict game,
                           CcPosPieceTag last_ply_destination,
                           CcChessboard * restrict cb,
                           CcPos step_pos,
                           CcSideEffect * restrict side_effect__o,
                           CcParseMsg ** restrict parse_msgs__iod )
{
    if ( !side_effect_an ) return false;
    if ( !step_start_an ) return false;
    if ( !step_end_an ) return false;
    if ( !game ) return false;
    if ( !cb ) return false;
    if ( !side_effect__o ) return false;
    if ( !parse_msgs__iod ) return false;

    CcPieceEnum step_piece = cc_chessboard_get_piece( cb, step_pos.i, step_pos.j );

    bool has_promotion_sign = false;

    CcSideEffectEnum see =
        cc_parse_side_effect_type( side_effect_an, &has_promotion_sign );

    char const * se_an =
        side_effect_an + cc_side_effect_type_len( see, has_promotion_sign );

    switch ( see )
    {
        case CC_SEE_None :
        {

// TODO :: default interactions
//
//      -- if King is moving horizontally, for 2+ fields, it's castling
//      -- if Pawn is moving diagonally, onto empty field, it's en passant
//      -- if Pyramid is moving onto own Pawn, on a field on opponent's side
//         of a chessboard, it's tagging for promotion
//      -- Starchild moving onto empty field, in a syzygy, it's failed resurrection --> ignore (?)
//      -- otherwise, it's capture
//      -- if it's a capture made by Pawn, check if it's also a promotion

            if ( CC_PIECE_IS_NONE( step_piece ) )
            {
                *side_effect__o = cc_side_effect_none();
                return true;
            }
            else
            {
                return false; // TODO
            }
        }

        case CC_SEE_Capture :
        {
// TODO
//
//      -- moving promotion
//      -- if it's a capture made by Pawn, check if it's also a promotion

            char piece_symbol = ' ';

            if ( cc_fetch_piece_symbol( se_an, &piece_symbol, true, true ) )
            {
                if ( !cc_piece_has_congruent_type( piece_symbol, step_piece ) )
                {
                    char * step_an__a = cc_str_copy__new( step_start_an, step_end_an, CC_MAX_LEN_ZERO_TERMINATED );

                    cc_parse_msg_append_fmt_if( parse_msgs__iod,
                                                CC_PMTE_Error,
                                                CC_MAX_LEN_ZERO_TERMINATED,
                                                "Piece '%c' not found at step-field, in step '%s'.\n",
                                                piece_symbol,
                                                step_an__a );
                    CC_FREE( step_an__a );
                    return false;
                }

                ++se_an;
            }

            if ( !CC_PIECE_CAN_BE_CAPTURED( step_piece ) )
            {
                char * step_an__a = cc_str_copy__new( step_start_an, step_end_an, CC_MAX_LEN_ZERO_TERMINATED );
                char sp = cc_piece_symbol( step_piece );

                cc_parse_msg_append_fmt_if( parse_msgs__iod,
                                            CC_PMTE_Error,
                                            CC_MAX_LEN_ZERO_TERMINATED,
                                            "Piece '%c' at step-field cannot be captured, in step '%s'.\n",
                                            sp,
                                            step_an__a );
                CC_FREE( step_an__a );
                return false;
            }

            CcLosingTagEnum lte = cc_parse_losing_tag( se_an );

            char const * promo_an = se_an + cc_losing_tag_len( lte );
            CcPieceEnum promoted_to = CC_PE_None;

            if ( CC_PIECE_IS_PAWN( last_ply_destination.piece ) )
            {
                bool has_promo_sign = false;

                CcSideEffectEnum promo =
                    cc_parse_side_effect_type( promo_an, &has_promo_sign );

                if ( promo == CC_SEE_Promotion )
                {
                    promo_an += cc_side_effect_type_len( promo, has_promo_sign );

                    char promote_to_symbol = ' ';

                    if ( !cc_fetch_piece_symbol( promo_an, &promote_to_symbol, true, false ) )
                        return false;

                    if ( !cc_piece_symbol_is_valid( promote_to_symbol ) )
                    {
                        char * step_an__a = cc_str_copy__new( step_start_an, step_end_an, CC_MAX_LEN_ZERO_TERMINATED );

                        cc_parse_msg_append_fmt_if( parse_msgs__iod,
                                                    CC_PMTE_Error,
                                                    CC_MAX_LEN_ZERO_TERMINATED,
                                                    "Character '%c' is not valid piece symbol, in step '%s'.\n",
                                                    promote_to_symbol,
                                                    step_an__a );
                        CC_FREE( step_an__a );
                        return false;
                    }

                    bool is_light = cc_piece_is_light( step_piece );
                    CcPieceEnum promote_to = cc_piece_from_symbol( promote_to_symbol, is_light );

                    if ( !CC_PAWN_CAN_BE_PROMOTED_TO( promote_to ) )
                    {
                        char * step_an__a = cc_str_copy__new( step_start_an, step_end_an, CC_MAX_LEN_ZERO_TERMINATED );

                        cc_parse_msg_append_fmt_if( parse_msgs__iod,
                                                    CC_PMTE_Error,
                                                    CC_MAX_LEN_ZERO_TERMINATED,
                                                    "Pawn cannot be promoted to '%c', in step '%s'.\n",
                                                    promote_to_symbol,
                                                    step_an__a );
                        CC_FREE( step_an__a );
                        return false;
                    }

                    promoted_to = promote_to;
                }
                else if ( promo == CC_SEE_TagForPromotion )
                {
// TODO :: add flag
                }
            }

            *side_effect__o = cc_side_effect_capture( step_piece, lte, promoted_to );
            return true;
        }

        case CC_SEE_Displacement :
        {
            char piece_symbol = ' ';

            if ( cc_fetch_piece_symbol( se_an, &piece_symbol, true, true ) )
            {
                if ( !cc_piece_has_congruent_type( piece_symbol, step_piece ) )
                {
                    char * step_an__a = cc_str_copy__new( step_start_an, step_end_an, CC_MAX_LEN_ZERO_TERMINATED );

                    cc_parse_msg_append_fmt_if( parse_msgs__iod,
                                                CC_PMTE_Error,
                                                CC_MAX_LEN_ZERO_TERMINATED,
                                                "Piece '%c' not found at step-field, in step '%s'.\n",
                                                piece_symbol,
                                                step_an__a );
                    CC_FREE( step_an__a );
                    return false;
                }

                ++se_an;
            }

            if ( !CC_PIECE_CAN_BE_DISPLACED( step_piece ) )
            {
                char * step_an__a = cc_str_copy__new( step_start_an, step_end_an, CC_MAX_LEN_ZERO_TERMINATED );
                char sp = cc_piece_symbol( step_piece );

                cc_parse_msg_append_fmt_if( parse_msgs__iod,
                                            CC_PMTE_Error,
                                            CC_MAX_LEN_ZERO_TERMINATED,
                                            "Piece '%c' at step-field cannot be displaced, in step '%s'.\n",
                                            sp,
                                            step_an__a );
                CC_FREE( step_an__a );
                return false;
            }

            CcLosingTagEnum lte = cc_parse_losing_tag( se_an );
            char const * pos_an = se_an + cc_losing_tag_len( lte );

            CcPos pos = CC_POS_CAST_INVALID;
            char const * pos_end_an = NULL;

            if ( !cc_parse_pos( pos_an, &pos, &pos_end_an ) )
            {
                char * step_an__a = cc_str_copy__new( step_start_an, step_end_an, CC_MAX_LEN_ZERO_TERMINATED );

                cc_parse_msg_append_fmt_if( parse_msgs__iod,
                                            CC_PMTE_Error,
                                            CC_MAX_LEN_ZERO_TERMINATED,
                                            "Error parsing displacement destination, in step '%s'.\n",
                                            step_an__a );
                CC_FREE( step_an__a );
                return false;
            }

            if ( !cc_pos_is_valid( pos ) )
            {
                char * step_an__a = cc_str_copy__new( step_start_an, step_end_an, CC_MAX_LEN_ZERO_TERMINATED );

                cc_parse_msg_append_fmt_if( parse_msgs__iod,
                                            CC_PMTE_Error,
                                            CC_MAX_LEN_ZERO_TERMINATED,
                                            "Displacement destination has to be complete (not a disambiguation), in step '%s'.\n",
                                            step_an__a );
                CC_FREE( step_an__a );
                return false;
            }

            *side_effect__o = cc_side_effect_displacement( step_piece, lte, pos );
            return true;
        }

        case CC_SEE_EnPassant :
// TODO :: en passant

        case CC_SEE_Castle :
// TODO :: castling

        case CC_SEE_Promotion :
        {
// TODO -- static promotion
//      -- moving promotion

            if ( !CC_PIECE_IS_PAWN( step_piece ) )
            {
                char * step_an__a = cc_str_copy__new( step_start_an, step_end_an, CC_MAX_LEN_ZERO_TERMINATED );

                cc_parse_msg_append_fmt_if( parse_msgs__iod,
                                            CC_PMTE_Error,
                                            CC_MAX_LEN_ZERO_TERMINATED,
                                            "Only Pawn can be promoted, in step '%s'.\n",
                                            step_an__a );
                CC_FREE( step_an__a );
                return false;
            }

            char piece_symbol = ' ';

            if ( !cc_fetch_piece_symbol( se_an, &piece_symbol, true, false ) )
                return false;

            if ( !cc_piece_symbol_is_valid( piece_symbol ) )
            {
                char * step_an__a = cc_str_copy__new( step_start_an, step_end_an, CC_MAX_LEN_ZERO_TERMINATED );

                cc_parse_msg_append_fmt_if( parse_msgs__iod,
                                            CC_PMTE_Error,
                                            CC_MAX_LEN_ZERO_TERMINATED,
                                            "Character '%c' is not valid piece symbol, in step '%s'.\n",
                                            piece_symbol,
                                            step_an__a );
                CC_FREE( step_an__a );
                return false;
            }

            bool is_light = cc_piece_is_light( step_piece );
            CcPieceEnum promote_to = cc_piece_from_symbol( piece_symbol, is_light );

            if ( !CC_PAWN_CAN_BE_PROMOTED_TO( promote_to ) )
            {
                char * step_an__a = cc_str_copy__new( step_start_an, step_end_an, CC_MAX_LEN_ZERO_TERMINATED );

                cc_parse_msg_append_fmt_if( parse_msgs__iod,
                                            CC_PMTE_Error,
                                            CC_MAX_LEN_ZERO_TERMINATED,
                                            "Pawn cannot be promoted to '%c', in step '%s'.\n",
                                            piece_symbol,
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
                char * step_an__a = cc_str_copy__new( step_start_an, step_end_an, CC_MAX_LEN_ZERO_TERMINATED );

                cc_parse_msg_append_fmt_if( parse_msgs__iod,
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
        {
            char piece_symbol = ' ';

            if ( cc_fetch_piece_symbol( se_an, &piece_symbol, true, true ) )
            {
                if ( !cc_piece_has_congruent_type( piece_symbol, step_piece ) )
                {
                    char * step_an__a = cc_str_copy__new( step_start_an, step_end_an, CC_MAX_LEN_ZERO_TERMINATED );

                    cc_parse_msg_append_fmt_if( parse_msgs__iod,
                                                CC_PMTE_Error,
                                                CC_MAX_LEN_ZERO_TERMINATED,
                                                "Piece '%c' not found at step-field, in step '%s'.\n",
                                                piece_symbol,
                                                step_an__a );
                    CC_FREE( step_an__a );
                    return false;
                }

                ++se_an;
            }

            if ( !CC_PIECE_CAN_BE_CONVERTED( step_piece ) )
            {
                char * step_an__a = cc_str_copy__new( step_start_an, step_end_an, CC_MAX_LEN_ZERO_TERMINATED );

                cc_parse_msg_append_fmt_if( parse_msgs__iod,
                                            CC_PMTE_Error,
                                            CC_MAX_LEN_ZERO_TERMINATED,
                                            "Piece '%c' can't be converted, in step '%s'.\n",
                                            piece_symbol,
                                            step_an__a );
                CC_FREE( step_an__a );
                return false;
            }

            CcPieceEnum convert_to = cc_piece_opposite( step_piece );
            CcLosingTagEnum lte = cc_parse_losing_tag( se_an );

            *side_effect__o = cc_side_effect_convert( convert_to, lte );
            return true;
        }

        case CC_SEE_FailedConversion :
        {
            if ( !CC_PIECE_IS_STARCHILD( step_piece ) )
            {
                char * step_an__a = cc_str_copy__new( step_start_an, step_end_an, CC_MAX_LEN_ZERO_TERMINATED );
                char sp = cc_piece_symbol( step_piece );

                cc_parse_msg_append_fmt_if( parse_msgs__iod,
                                            CC_PMTE_Error,
                                            CC_MAX_LEN_ZERO_TERMINATED,
                                            "Only Starchild can't be converted, in step '%s'.\n",
                                            sp,
                                            step_an__a );
                CC_FREE( step_an__a );
                return false;
            }

            *side_effect__o = cc_side_effect_failed_conversion();
            return true;
        }

        case CC_SEE_DemoteToPawn :
// TODO :: demote to Pawn

        case CC_SEE_Resurrection :
        case CC_SEE_ResurrectingOpponent :
        {
            if ( !CC_PIECE_IS_NONE( step_piece ) )
            {
                char * step_an__a = cc_str_copy__new( step_start_an, step_end_an, CC_MAX_LEN_ZERO_TERMINATED );

                cc_parse_msg_append_fmt_if( parse_msgs__iod,
                                            CC_PMTE_Error,
                                            CC_MAX_LEN_ZERO_TERMINATED,
                                            "Resurrection can be initiated only on an empty field, in step '%s'.\n",
                                            step_an__a );
                CC_FREE( step_an__a );
                return false;
            }

            char piece_symbol = ' ';

            if ( !cc_fetch_piece_symbol( se_an, &piece_symbol, true, false ) )
                return false;

            if ( !cc_piece_symbol_is_valid( piece_symbol ) )
            {
                char * step_an__a = cc_str_copy__new( step_start_an, step_end_an, CC_MAX_LEN_ZERO_TERMINATED );

                cc_parse_msg_append_fmt_if( parse_msgs__iod,
                                            CC_PMTE_Error,
                                            CC_MAX_LEN_ZERO_TERMINATED,
                                            "Character '%c' is not valid piece symbol, in step '%s'.\n",
                                            piece_symbol,
                                            step_an__a );
                CC_FREE( step_an__a );
                return false;
            }

            bool is_light = true;

            if ( CC_GAME_STATUS_IS_LIGHT_TURN( game->status ) )
                is_light = ( see == CC_SEE_Resurrection );
            else if ( CC_GAME_STATUS_IS_DARK_TURN( game->status ) )
                is_light = ( see == CC_SEE_ResurrectingOpponent );
            else
                return false; // Should check status, within caller stack.

            CcPieceEnum resurrecting = cc_piece_from_symbol( piece_symbol, is_light );

            if ( !CC_PIECE_CAN_BE_RESURRECTED( resurrecting ) )
            {
                char * step_an__a = cc_str_copy__new( step_start_an, step_end_an, CC_MAX_LEN_ZERO_TERMINATED );
                char pt = cc_piece_symbol( resurrecting );

                cc_parse_msg_append_fmt_if( parse_msgs__iod,
                                            CC_PMTE_Error,
                                            CC_MAX_LEN_ZERO_TERMINATED,
                                            "Piece '%c' cannot be resurrected, in step '%s'.\n",
                                            pt,
                                            step_an__a );
                CC_FREE( step_an__a );
                return false;
            }

            CcPos pos = CC_POS_CAST_INVALID;

            if ( CC_PIECE_IS_WAVE( resurrecting ) || CC_PIECE_IS_STARCHILD( resurrecting ) )
            {
                char const * pos_an = se_an + 1;
                char const * pos_end_an = NULL;

                if ( cc_parse_pos( pos_an, &pos, &pos_end_an ) )
                {
                    if ( !cc_pos_is_valid( pos ) )
                    {
                        char * step_an__a = cc_str_copy__new( step_start_an, step_end_an, CC_MAX_LEN_ZERO_TERMINATED );

                        cc_parse_msg_append_fmt_if( parse_msgs__iod,
                                                    CC_PMTE_Error,
                                                    CC_MAX_LEN_ZERO_TERMINATED,
                                                    "Resurrection destination has to be complete (not a disambiguation), in step '%s'.\n",
                                                    step_an__a );
                        CC_FREE( step_an__a );
                        return false;
                    }
                }
            }

            *side_effect__o = cc_side_effect_resurrect( step_piece, pos );
            return true;
        }

        case CC_SEE_FailedResurrection :
        {
            *side_effect__o = cc_side_effect_failed_resurrection();
            return true;
        }

        default : return false;
    }
}