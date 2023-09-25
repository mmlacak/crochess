// Copyright (c) 2022 Mario Mlačak, mmlacak@gmail.com
// Licensed under GNU GPL v3+ license. See LICENSING, COPYING files for details.

#include "cc_parse_utils.h"
#include "cc_parse_side_effect.h"


// static bool cc_parse_side_effect_error_msg( char const * restrict side_effect_an,
//                                             char const * restrict step_end_an,
//                                             CcParseMsg ** restrict parse_msgs__iod,
//                                             char const * restrict fmt, ... ) {
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

static bool cc_check_piece_has_congruent_type( char piece_symbol,
                                               CcPieceEnum piece,
                                               char const * restrict step_start_an,
                                               char const * restrict step_end_an,
                                               CcParseMsg ** restrict parse_msgs__iod ) {
    if ( !cc_piece_has_congruent_type( piece_symbol, piece ) ) {
        char * step_an__a = cc_str_copy__new( step_start_an, step_end_an, CC_MAX_LEN_ZERO_TERMINATED );
        char * piece_str__a = cc_piece_as_string__new( piece, false );

        cc_parse_msg_append_fmt_if( parse_msgs__iod, CC_PMTE_Error, CC_MAX_LEN_ZERO_TERMINATED, "Piece '%c' not found at step-field, encountered %s, in step '%s'.\n", piece_symbol, piece_str__a, step_an__a );

        CC_FREE( piece_str__a );
        CC_FREE( step_an__a );
        return false;
    }

    return true;
}

static bool cc_check_piece_can_be_captured( CcPieceEnum piece,
                                            char const * restrict step_start_an,
                                            char const * restrict step_end_an,
                                            CcParseMsg ** restrict parse_msgs__iod ) {
    if ( !CC_PIECE_CAN_BE_CAPTURED( piece ) ) {
        char * step_an__a = cc_str_copy__new( step_start_an, step_end_an, CC_MAX_LEN_ZERO_TERMINATED );
        char * piece_str__a = cc_piece_as_string__new( piece, true );

        cc_parse_msg_append_fmt_if( parse_msgs__iod, CC_PMTE_Error, CC_MAX_LEN_ZERO_TERMINATED, "%s at step-field cannot be captured, in step '%s'.\n", piece_str__a, step_an__a );

        CC_FREE( piece_str__a );
        CC_FREE( step_an__a );
        return false;
    }

    return false;
}

static bool cc_check_piece_symbol_is_valid( char piece_symbol,
                                            char const * restrict step_start_an,
                                            char const * restrict step_end_an,
                                            CcParseMsg ** restrict parse_msgs__iod ) {
    if ( !cc_piece_symbol_is_valid( piece_symbol ) ) {
        char * step_an__a = cc_str_copy__new( step_start_an, step_end_an, CC_MAX_LEN_ZERO_TERMINATED );
        cc_parse_msg_append_fmt_if( parse_msgs__iod, CC_PMTE_Error, CC_MAX_LEN_ZERO_TERMINATED, "Character '%c' is not valid piece symbol, in step '%s'.\n", piece_symbol, step_an__a );
        CC_FREE( step_an__a );
        return false;
    }

    return false;
}

static bool cc_check_promote_to_piece_is_valid( CcPieceEnum promote_to_piece,
                                                char const * restrict step_start_an,
                                                char const * restrict step_end_an,
                                                CcParseMsg ** restrict parse_msgs__iod ) {
    if ( !CC_PAWN_CAN_BE_PROMOTED_TO( promote_to_piece ) ) {
        char * step_an__a = cc_str_copy__new( step_start_an, step_end_an, CC_MAX_LEN_ZERO_TERMINATED );
        char * piece_str__a = cc_piece_as_string__new( promote_to_piece, false );

        cc_parse_msg_append_fmt_if( parse_msgs__iod, CC_PMTE_Error, CC_MAX_LEN_ZERO_TERMINATED, "Pawn cannot be promoted to %s, in step '%s'.\n", piece_str__a, step_an__a );

        CC_FREE( piece_str__a );
        CC_FREE( step_an__a );
        return false;
    }

    return false;
}

static bool cc_check_piece_can_be_displaced( CcPieceEnum piece,
                                             char const * restrict step_start_an,
                                             char const * restrict step_end_an,
                                             CcParseMsg ** restrict parse_msgs__iod ) {
    if ( !CC_PIECE_CAN_BE_DISPLACED( piece ) ) {
        char * step_an__a = cc_str_copy__new( step_start_an, step_end_an, CC_MAX_LEN_ZERO_TERMINATED );
        char * piece_str__a = cc_piece_as_string__new( piece, true );

        cc_parse_msg_append_fmt_if( parse_msgs__iod, CC_PMTE_Error, CC_MAX_LEN_ZERO_TERMINATED, "%s at step-field cannot be displaced, in step '%s'.\n", piece_str__a, step_an__a );

        CC_FREE( piece_str__a );
        CC_FREE( step_an__a );
        return false;
    }

    return true;
}

static bool cc_parse_and_check_position( char const * pos_an,
                                         CcPos * pos__o,
                                         char const ** pos_end_an__o,
                                         char const * restrict step_start_an,
                                         char const * restrict step_end_an,
                                         CcParseMsg ** restrict parse_msgs__iod ) {
    if ( !cc_parse_pos( pos_an, pos__o, pos_end_an__o ) ) {
        char * step_an__a = cc_str_copy__new( step_start_an, step_end_an, CC_MAX_LEN_ZERO_TERMINATED );
        cc_parse_msg_append_fmt_if( parse_msgs__iod, CC_PMTE_Error, CC_MAX_LEN_ZERO_TERMINATED, "Error parsing displacement destination, in step '%s'.\n", step_an__a );
        CC_FREE( step_an__a );
        return false;
    }

    return true;
}

static bool cc_check_position_is_on_board( CcPos pos,
                                           CcChessboard * restrict cb,
                                           char const * msg_fmt,
                                           char const * restrict step_start_an,
                                           char const * restrict step_end_an,
                                           CcParseMsg ** restrict parse_msgs__iod ) {
    if ( !cc_chessboard_is_pos_on_board( cb, pos.i, pos.j ) ) {
        char * step_an__a = cc_str_copy__new( step_start_an, step_end_an, CC_MAX_LEN_ZERO_TERMINATED );
        cc_parse_msg_append_fmt_if( parse_msgs__iod, CC_PMTE_Error, CC_MAX_LEN_ZERO_TERMINATED, msg_fmt, step_an__a );
        CC_FREE( step_an__a );
        return false;
    }

    return false;
}

static bool cc_check_promoting_piece_is_pawn( CcPieceEnum piece,
                                              char const * restrict msg_fmt,
                                              char const * restrict step_start_an,
                                              char const * restrict step_end_an,
                                              CcParseMsg ** restrict parse_msgs__iod ) {
    if ( !CC_PIECE_IS_PAWN( piece ) ) {
        char * step_an__a = cc_str_copy__new( step_start_an, step_end_an, CC_MAX_LEN_ZERO_TERMINATED );
        char * piece_str__a = cc_piece_as_string__new( piece, false );

        cc_parse_msg_append_fmt_if( parse_msgs__iod, CC_PMTE_Error, CC_MAX_LEN_ZERO_TERMINATED, msg_fmt, piece_str__a, step_an__a );

        CC_FREE( piece_str__a );
        CC_FREE( step_an__a );
        return false;
    }

    return false;
}

static bool cc_check_piece_can_be_converted( CcPieceEnum piece,
                                             char const * restrict step_start_an,
                                             char const * restrict step_end_an,
                                             CcParseMsg ** restrict parse_msgs__iod ) {
    if ( !CC_PIECE_CAN_BE_CONVERTED( piece ) ) {
        char * step_an__a = cc_str_copy__new( step_start_an, step_end_an, CC_MAX_LEN_ZERO_TERMINATED );
        char * piece_str__a = cc_piece_as_string__new( piece, true );

        cc_parse_msg_append_fmt_if( parse_msgs__iod, CC_PMTE_Error, CC_MAX_LEN_ZERO_TERMINATED, "%s can't be converted, in step '%s'.\n", piece_str__a, step_an__a );

        CC_FREE( piece_str__a );
        CC_FREE( step_an__a );
        return false;
    }

    return false;
}

static bool cc_check_failed_conversion( CcPieceEnum piece,
                                        char const * restrict step_start_an,
                                        char const * restrict step_end_an,
                                        CcParseMsg ** restrict parse_msgs__iod ) {
    if ( !CC_PIECE_IS_STARCHILD( piece ) ) {
        char * step_an__a = cc_str_copy__new( step_start_an, step_end_an, CC_MAX_LEN_ZERO_TERMINATED );
        char * piece_str__a = cc_piece_as_string__new( piece, false );

        cc_parse_msg_append_fmt_if( parse_msgs__iod, CC_PMTE_Error, CC_MAX_LEN_ZERO_TERMINATED, "Conversion can fail only againt Starchild, encountered %s in step '%s'.\n", piece_str__a, step_an__a );

        CC_FREE( piece_str__a );
        CC_FREE( step_an__a );
        return false;
    }

    return false;
}


bool cc_parse_side_effect( char const * restrict side_effect_an,
                           char const * restrict step_start_an,
                           char const * restrict step_end_an,
                           CcGame * restrict game,
                           CcPosPieceTag before_ply_start,
                           CcChessboard * restrict cb,
                           CcStepLinkEnum sle,
                           CcPos step_pos,
                           CcSideEffect * restrict side_effect__o,
                           CcParseMsg ** restrict parse_msgs__iod ) {
    if ( !side_effect_an ) return false;
    if ( !step_start_an ) return false;
    if ( !step_end_an ) return false;
    if ( !game ) return false;
    if ( !cb ) return false;
    if ( !side_effect__o ) return false;
    if ( !parse_msgs__iod ) return false;

    if ( sle == CC_SLE_None ) return false; // Just sanity check; error msg is produced when parsing step, in cc_check_step_link().

    CcPieceEnum step_piece = cc_chessboard_get_piece( cb, step_pos.i, step_pos.j );
    bool has_promotion_sign = false;
    CcSideEffectEnum see = cc_parse_side_effect_type( side_effect_an, &has_promotion_sign );
    char const * se_an = side_effect_an + cc_side_effect_type_len( see, has_promotion_sign );

    switch ( see ) {
        case CC_SEE_None : {

            // TODO :: default interactions
            //
            //      -- if King is moving horizontally, for 2+ fields, it's castling
            //      -- if Pawn, Scout, or Grenadier is moving diagonally, onto empty field, it's en passant
            //      -- if Pyramid is moving onto own Pawn, on a field on opponent's side
            //         of a chessboard, it's tagging for promotion
            //      -- Starchild moving onto empty field, in a syzygy, it's failed resurrection --> ignore (?)
            //      -- otherwise, it's capture
            //      -- if it's a capture made by Pawn, check if it's also a tag for promotion;
            //         it can't be promotion, if promote-to piece is missing

            if ( CC_PIECE_IS_NONE( step_piece ) ) {
                *side_effect__o = cc_side_effect_none();
                return true;
            } else {
                if ( sle == CC_SLE_Start ) {
                    // Starting position, piece is the one found in destination of last ply, or the one starting a move.

                    // TODO :: too early for this :: UNCOMMENT when before_ply_start.piece is valid
                    //
                    // if ( step_piece != before_ply_start.piece ) // TODO :: piece starting a move
                    // {
                    //     char * step_an__a = cc_str_copy__new( step_start_an, step_end_an, CC_MAX_LEN_ZERO_TERMINATED );
                    //     char sp = cc_piece_as_char( step_piece );
                    //     char lpdp = cc_piece_as_char( before_ply_start.piece );

                    //     cc_parse_msg_append_fmt_if( parse_msgs__iod,
                    //                                 CC_PMTE_Error,
                    //                                 CC_MAX_LEN_ZERO_TERMINATED,
                    //                                 "Piece '%c' found at step-field, in step '%s'; expected '%c'.\n",
                    //                                 sp,
                    //                                 step_an__a,
                    //                                 lpdp );
                    //     CC_FREE( step_an__a );
                    //     return false;
                    // }
                    //
                    // TODO :: too early for this :: UNCOMMENT when before_ply_start.piece is valid

                    *side_effect__o = cc_side_effect_none();
                    return true;
                } else {
                    // TODO :: silent capture ::
                    //      -- if piece found on step-field has other owner
                    //      -- if ply piece (the one currently moving) can capture
                    //      -- if piece found on step-field can be captured
                    //      --> then it's silent capture (!)

                    // TODO ::  >>> TODO >>> Piece '%c' found at step-field, should be empty, in step '%s'.\n
                    char * step_an__a = cc_str_copy__new( step_start_an, step_end_an, CC_MAX_LEN_ZERO_TERMINATED );
                    char sp = cc_piece_as_char( step_piece );
                    cc_parse_msg_append_fmt_if( parse_msgs__iod, CC_PMTE_Error, CC_MAX_LEN_ZERO_TERMINATED, " >>> TODO >>> Piece '%c' found at step-field, should be empty, in step '%s'.\n", sp, step_an__a );
                    CC_FREE( step_an__a );
                    return false;
                }
            }
            // TODO
        } case CC_SEE_Capture : {
            // TODO
            //
            // -- moving promotion :: if it's a capture made by Pawn, check if it's also a promotion

            char piece_symbol = ' ';

            if ( cc_fetch_piece_symbol( se_an, &piece_symbol, true, true ) ) {
                if ( !cc_check_piece_has_congruent_type( piece_symbol, step_piece, step_start_an, step_end_an, parse_msgs__iod ) )
                    return false;

                ++se_an;
            }

            if ( !cc_check_piece_can_be_captured( step_piece, step_start_an, step_end_an, parse_msgs__iod ) )
                return false;

            CcLosingTagEnum lte = cc_parse_losing_tag( se_an );
            char const * promo_an = se_an + cc_losing_tag_len( lte );

            if ( CC_PIECE_IS_PAWN( before_ply_start.piece ) ) {
                bool has_promo_sign = false;
                CcSideEffectEnum promo = cc_parse_side_effect_type( promo_an, &has_promo_sign );

                if ( promo == CC_SEE_Promotion ) {
                    promo_an += cc_side_effect_type_len( promo, has_promo_sign );

                    char promote_to_symbol = ' ';

                    if ( !cc_fetch_piece_symbol( promo_an, &promote_to_symbol, true, false ) )
                        return false;

                    if ( !cc_check_piece_symbol_is_valid( promote_to_symbol, step_start_an, step_end_an, parse_msgs__iod ) )
                        return false;

                    bool is_light = cc_piece_is_light( step_piece );
                    CcPieceEnum promote_to = cc_piece_from_symbol( promote_to_symbol, is_light );

                    if ( !cc_check_promote_to_piece_is_valid( promote_to, step_start_an, step_end_an, parse_msgs__iod ) )
                        return false;

                    *side_effect__o = cc_side_effect_promote( step_piece, lte, promote_to );
                    return true;
                } else if ( promo == CC_SEE_TagForPromotion ) {
                    // TODO :: add flag
                }
            }

            *side_effect__o = cc_side_effect_capture( step_piece, lte );
            return true;
        } case CC_SEE_Displacement : {
            // TODO -- add Serpent
            //      -- check light entranced Shaman

            char piece_symbol = ' ';

            if ( cc_fetch_piece_symbol( se_an, &piece_symbol, true, true ) ) {
                if ( !cc_check_piece_has_congruent_type( piece_symbol, step_piece, step_start_an, step_end_an, parse_msgs__iod ) )
                    return false;

                ++se_an;
            }

            if ( !cc_check_piece_can_be_displaced( step_piece, step_start_an, step_end_an, parse_msgs__iod ) )
                return false;

            CcLosingTagEnum lte = cc_parse_losing_tag( se_an );
            char const * pos_an = se_an + cc_losing_tag_len( lte );

            CcPos pos = CC_POS_CAST_INVALID;
            char const * pos_end_an = NULL;

            if ( !cc_parse_and_check_position( pos_an, &pos, &pos_end_an, step_start_an, step_end_an, parse_msgs__iod ) )
                return false;

            if ( !cc_check_position_is_on_board( pos, cb, "Displacement destination has to be complete (not a disambiguation), in step '%s'.\n", step_start_an, step_end_an, parse_msgs__iod ) )
                return false;

            *side_effect__o = cc_side_effect_displacement( step_piece, lte, pos );
            return true;
        } case CC_SEE_EnPassant : {
            // TODO :: en passant
            return false;
        } case CC_SEE_Castle : {
            if ( !CC_PIECE_IS_KING( before_ply_start.piece ) ) return false;

            // TODO :: castling
            return false;
        } case CC_SEE_Promotion : {
            // TODO -- static promotion
            //      -- moving promotion
            //      -- silent capture before promotion

            if ( !cc_check_promoting_piece_is_pawn( step_piece, "Only Pawn can be promoted, encountered %s in step '%s'.\n", step_start_an, step_end_an, parse_msgs__iod ) )
                return false;

            char piece_symbol = ' ';

            if ( !cc_fetch_piece_symbol( se_an, &piece_symbol, true, false ) )
                return false;

            if ( !cc_check_piece_symbol_is_valid( piece_symbol, step_start_an, step_end_an, parse_msgs__iod ) )
                return false;

            bool is_light = cc_piece_is_light( step_piece );
            CcPieceEnum promote_to = cc_piece_from_symbol( piece_symbol, is_light );

            if ( !cc_check_promote_to_piece_is_valid( promote_to, step_start_an, step_end_an, parse_msgs__iod ) )
                return false;

            *side_effect__o = cc_side_effect_promote( CC_PE_None, CC_LTE_None, step_piece );
            return true;
        } case CC_SEE_TagForPromotion : {
            // TODO -- silent capture before promotion

            if ( !cc_check_promoting_piece_is_pawn( step_piece, "Only Pawn can be tagged for promotion, encountered %s in step '%s'.\n", step_start_an, step_end_an, parse_msgs__iod ) )
                return false;

            *side_effect__o = cc_side_effect_tag_for_promotion( CC_PE_None, CC_LTE_None );
            return true;
        } case CC_SEE_Conversion : {
            char piece_symbol = ' ';

            if ( cc_fetch_piece_symbol( se_an, &piece_symbol, true, true ) ) {
                if ( !cc_check_piece_has_congruent_type( piece_symbol, step_piece, step_start_an, step_end_an, parse_msgs__iod ) )
                    return false;

                ++se_an;
            }

            if ( !cc_check_piece_can_be_converted( step_piece, step_start_an, step_end_an, parse_msgs__iod ) )
                return false;

            CcPieceEnum convert_to = cc_piece_opposite( step_piece );
            CcLosingTagEnum lte = cc_parse_losing_tag( se_an );

            *side_effect__o = cc_side_effect_convert( convert_to, lte );
            return true;
        } case CC_SEE_FailedConversion : {
            if ( !cc_check_failed_conversion( step_piece, step_start_an, step_end_an, parse_msgs__iod ) )
                return false;

            *side_effect__o = cc_side_effect_failed_conversion();
            return true;
        } case CC_SEE_Transparency : { // Intentional fall-through ...
        } case CC_SEE_Divergence : {
            char piece_symbol = ' ';

            if ( cc_fetch_piece_symbol( se_an, &piece_symbol, false, true ) ) {
                if ( !cc_check_piece_has_congruent_type( piece_symbol, step_piece, step_start_an, step_end_an, parse_msgs__iod ) )
                    return false;

                ++se_an;
            }

            if ( see == CC_SEE_Transparency )
                *side_effect__o = cc_side_effect_transparency( step_piece );
            else if ( see == CC_SEE_Divergence )
                *side_effect__o = cc_side_effect_diversion( step_piece );
            else
                return false; // In case some other, unexpected side-effect gets here.

            return true;
        } case CC_SEE_DemoteToPawn : {
            // TODO :: demote to Pawn
            return false;
        } case CC_SEE_Resurrection : { // Intentional fall-through ...
        } case CC_SEE_ResurrectingOpponent : {
            if ( !CC_PIECE_IS_NONE( step_piece ) ) {
                char * step_an__a = cc_str_copy__new( step_start_an, step_end_an, CC_MAX_LEN_ZERO_TERMINATED );
                cc_parse_msg_append_fmt_if( parse_msgs__iod, CC_PMTE_Error, CC_MAX_LEN_ZERO_TERMINATED, "Resurrection can be initiated only on an empty field, in step '%s'.\n", step_an__a );
                CC_FREE( step_an__a );
                return false;
            }

            char piece_symbol = ' ';

            if ( !cc_fetch_piece_symbol( se_an, &piece_symbol, true, false ) )
                return false;

            if ( !cc_piece_symbol_is_valid( piece_symbol ) ) {
                char * step_an__a = cc_str_copy__new( step_start_an, step_end_an, CC_MAX_LEN_ZERO_TERMINATED );
                cc_parse_msg_append_fmt_if( parse_msgs__iod, CC_PMTE_Error, CC_MAX_LEN_ZERO_TERMINATED, "Character '%c' is not valid piece symbol, in step '%s'.\n", piece_symbol, step_an__a );
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

            if ( !CC_PIECE_CAN_BE_RESURRECTED( resurrecting ) ) {
                char * step_an__a = cc_str_copy__new( step_start_an, step_end_an, CC_MAX_LEN_ZERO_TERMINATED );
                char pt = cc_piece_as_char( resurrecting );
                cc_parse_msg_append_fmt_if( parse_msgs__iod, CC_PMTE_Error, CC_MAX_LEN_ZERO_TERMINATED, "Piece '%c' cannot be resurrected, in step '%s'.\n", pt, step_an__a );
                CC_FREE( step_an__a );
                return false;
            }

            CcPos pos = CC_POS_CAST_INVALID;

            if ( CC_PIECE_IS_WAVE( resurrecting ) || CC_PIECE_IS_STARCHILD( resurrecting ) ) {
                char const * pos_an = se_an + 1;
                char const * pos_end_an = NULL;

                if ( cc_parse_pos( pos_an, &pos, &pos_end_an ) ) {
                    if ( !cc_check_position_is_on_board( pos, cb, "Resurrection destination has to be complete (not a disambiguation), in step '%s'.\n", step_start_an, step_end_an, parse_msgs__iod ) )
                        return false;
                }
            }

            *side_effect__o = cc_side_effect_resurrect( step_piece, pos );
            return true;
        } case CC_SEE_FailedResurrection : {
            *side_effect__o = cc_side_effect_failed_resurrection();
            return true;
        }

        default : return false;
    }
}
