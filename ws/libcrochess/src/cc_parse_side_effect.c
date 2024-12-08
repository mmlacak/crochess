// Copyright (c) 2022 Mario Mlačak, mmlacak@gmail.com
// Licensed under GNU GPL v3+ license. See LICENSING, COPYING files for details.

#include "cc_setup_misc.h"

#include "cc_checks.h"
#include "cc_parse_utils.h"
#include "cc_parse_side_effect.h"


static bool _cc_fail_with_msg_unrecognized_piece_symbol( char piece_symbol,
                                                         char const * side_effect_an,
                                                         char const * step_end_an,
                                                         CcParseMsg ** parse_msgs__iod ) {
    char * se_an__a = cc_str_copy__new( side_effect_an, step_end_an, CC_MAX_LEN_ZERO_TERMINATED );
    cc_parse_msg_append_fmt( parse_msgs__iod, CC_PMTE_Error, CC_MAX_LEN_ZERO_TERMINATED, "Unrecognized piece symbol '%c' encountered, in side-effect '%s'.\n", piece_symbol, se_an__a );
    CC_FREE( se_an__a );
    return false;
}

static bool _cc_fail_with_msg_in_step( char const * msg_fmt,
                                       char const * step_start_an,
                                       char const * step_end_an,
                                       CcParseMsg ** parse_msgs__iod ) {
    char * step_an__a = cc_str_copy__new( step_start_an, step_end_an, CC_MAX_LEN_ZERO_TERMINATED );
    cc_parse_msg_append_fmt( parse_msgs__iod, CC_PMTE_Error, CC_MAX_LEN_ZERO_TERMINATED, msg_fmt, step_an__a );
    CC_FREE( step_an__a );
    return false;
}


bool cc_parse_side_effect( char const * side_effect_an,
                           char const * step_start_an,
                           char const * step_end_an,
                           bool is_turn_light,
                           cc_uint_t board_size,
                           CcSideEffect * side_effect__o,
                           CcParseMsg ** parse_msgs__iod ) {
    if ( !side_effect_an ) return false;
    if ( !step_start_an ) return false;
    if ( !step_end_an ) return false;
    if ( !side_effect__o ) return false;
    if ( !parse_msgs__iod ) return false;

    if ( !CC_IS_BOARD_SIZE_VALID( board_size ) ) return false;

    bool is_opponent_light = !is_turn_light;
    bool has_promotion_sign = false;
    CcSideEffectTypeEnum sete = cc_parse_side_effect_type( side_effect_an, &has_promotion_sign );
    char const * se_an = side_effect_an + cc_side_effect_type_len( sete, has_promotion_sign );

    switch ( sete ) {
        case CC_SETE_None : {
            *side_effect__o = cc_side_effect_none();
            return true;
        } case CC_SETE_Capture : {
            char piece_symbol = ' ';
            CcMaybeBoolEnum result = cc_fetch_piece_symbol( se_an, &piece_symbol, true );

            if ( result == CC_MBE_True )
                ++se_an;
            else if ( result == CC_MBE_False )
                return _cc_fail_with_msg_unrecognized_piece_symbol( piece_symbol, side_effect_an, step_end_an, parse_msgs__iod );

            CcPieceType piece = cc_piece_from_symbol( piece_symbol, is_opponent_light ); // If piece symbol was not found, piece is none.
            CcLosingTagType ltt = cc_parse_losing_tag( se_an );
            char const * promo_an = se_an + cc_losing_tag_len( ltt );

            // TODO :: check if followed by promotion

            *side_effect__o = cc_side_effect_capture( piece, ltt );
            return true;
        } case CC_SETE_Displacement : {
            char piece_symbol = ' ';
            CcMaybeBoolEnum result = cc_fetch_piece_symbol( se_an, &piece_symbol, true );

            if ( result == CC_MBE_True )
                ++se_an;
            else if ( result == CC_MBE_False )
                return _cc_fail_with_msg_unrecognized_piece_symbol( piece_symbol, side_effect_an, step_end_an, parse_msgs__iod );

            CcPieceType piece = cc_piece_from_symbol( piece_symbol, is_opponent_light ); // If piece symbol was not found, piece is none.
            CcLosingTagType ltt = cc_parse_losing_tag( se_an );
            char const * pos_an = se_an + cc_losing_tag_len( ltt );

            CcPos pos = CC_POS_CAST_INVALID;
            char const * pos_end_an = NULL;

            if ( !cc_parse_pos( pos_an, &pos, &pos_end_an ) )
                return _cc_fail_with_msg_in_step( "Error parsing displacement destination, in step '%s'.\n", step_start_an, step_end_an, parse_msgs__iod );

            if ( !CC_IS_POS_ON_BOARD( board_size, pos.i, pos.j ) )
                return _cc_fail_with_msg_in_step( "Displacement destination has to be complete (not a disambiguation), in step '%s'.\n", step_start_an, step_end_an, parse_msgs__iod );

            *side_effect__o = cc_side_effect_displacement( piece, ltt, pos );
            return true;
        } case CC_SETE_EnPassant : {
            // if ( !_cc_check_piece_en_passant( before_ply_start.piece, true, "Only Pawns, Scouts, Grenadiers can capture en passant, encountered %s in step '%s'.\n", step_start_an, step_end_an, parse_msgs__iod ) )
            //     return false;
            //
            // if ( !_cc_check_field_is_empty( piece, "Capturing by en passant can be performed only on an empty field, encountered %s in step '%s'.\n", step_start_an, step_end_an, parse_msgs__iod ) )
            //     return false;
            //
            // char piece_symbol = ' ';
            // CcPieceType maybe_captured = CC_PE_None;
            //
            // if ( cc_fetch_piece_symbol( se_an, &piece_symbol, false, true ) ) {
            //     bool is_light = !cc_piece_is_light( before_ply_start.piece ); // !light because capturing opponent's piece.
            //     CcPieceType maybe_private = cc_piece_from_symbol( piece_symbol, is_light );
            //
            //     if ( !_cc_check_piece_en_passant( maybe_private, false, "Only Pawns, Scouts, Grenadiers can be captured en passant, encountered %s in step '%s'.\n", step_start_an, step_end_an, parse_msgs__iod ) )
            //         return false;
            //
            //     maybe_captured = maybe_private;
            //     ++se_an;
            // }
            //
            // CcPos maybe_en_passant_location = CC_POS_CAST_INVALID;
            // char const * pos_end_an = NULL;
            //
            // if ( !_cc_parse_and_check_position( se_an, &maybe_en_passant_location, &pos_end_an, "Error parsing en passant location, in step '%s'.\n", step_start_an, step_end_an, parse_msgs__iod ) )
            //     return false;
            //
            // if ( CC_IS_COORD_VALID( maybe_en_passant_location.j ) ) { // If location is given, at least rank must be valid.
            //     if ( !_cc_check_en_passant_file( maybe_en_passant_location, cb, step_start_an, step_end_an, parse_msgs__iod ) )
            //         return false;
            // }
            //
            // CcPos captured_at = CC_POS_CAST_INVALID;
            //
            // if ( !_cc_check_captured_en_passant( before_ply_start.piece, *step_pos__io, maybe_en_passant_location, maybe_captured, &captured_at, cb, step_start_an, step_end_an, parse_msgs__iod ) )
            //     return false;
            //
            // // TODO :: check path of capturing piece
            //
            // *side_effect__o = cc_side_effect_en_passant( piece, captured_at );
            // return true;
            return false; // TODO
        } case CC_SETE_Castle : {
            // if ( !_cc_check_piece_is_castling_king( before_ply_start, step_start_an, step_end_an, parse_msgs__iod ) )
            //     return false;
            //
            // char piece_symbol = ' ';
            //
            // if ( cc_fetch_piece_symbol( se_an, &piece_symbol, false, true ) ) {
            //     bool is_light = cc_piece_is_light( before_ply_start.piece );
            //     CcPieceType maybe_rook = cc_piece_from_symbol( piece_symbol, is_light );
            //
            //     if ( !_cc_check_piece_is_rook_to_castle( maybe_rook, step_start_an, step_end_an, parse_msgs__iod ) )
            //         return false;
            //
            //     ++se_an;
            // }
            //
            // CcPos rook_dest = CC_POS_CAST_INVALID;
            // char const * end = NULL;
            //
            // if ( cc_parse_pos( se_an, &rook_dest, &end ) )
            //     se_an = end;
            //
            // CcPieceType rook = CC_PE_None;
            // CcPos rook_start = CC_POS_CAST_INVALID;
            //
            // if ( !_cc_check_king_and_rook_can_castle( before_ply_start, cb, step_pos__io, &rook_dest, &rook, &rook_start, step_start_an, step_end_an, parse_msgs__iod ) )
            //     return false;
            //
            // *side_effect__o = cc_side_effect_castle( rook, rook_start, rook_dest );
            // return true;
            return false; // TODO
        } case CC_SETE_Promotion : {
            // // TODO -- static promotion
            // //      -- moving promotion
            // //      -- silent capture before promotion
            //
            // if ( !_cc_check_promoting_piece_is_pawn( piece, "Only Pawn can be promoted, encountered %s in step '%s'.\n", step_start_an, step_end_an, parse_msgs__iod ) )
            //     return false;
            //
            // char piece_symbol = ' ';
            //
            // if ( !cc_fetch_piece_symbol( se_an, &piece_symbol, true, false ) )
            //     return false;
            //
            // if ( !_cc_check_piece_symbol_is_valid( piece_symbol, step_start_an, step_end_an, parse_msgs__iod ) )
            //     return false;
            //
            // bool is_light = cc_piece_is_light( piece );
            // CcPieceType promote_to = cc_piece_from_symbol( piece_symbol, is_light );
            //
            // if ( !_cc_check_promote_to_piece_is_valid( promote_to, step_start_an, step_end_an, parse_msgs__iod ) )
            //     return false;
            //
            // *side_effect__o = cc_side_effect_promote( CC_PE_None, CC_LTE_NoneLost, piece );
            // return true;
            return false; // TODO
        } case CC_SETE_TagForPromotion : {
            // // TODO -- silent capture before promotion
            //
            // if ( !_cc_check_promoting_piece_is_pawn( piece, "Only Pawn can be tagged for promotion, encountered %s in step '%s'.\n", step_start_an, step_end_an, parse_msgs__iod ) )
            //     return false;
            //
            // *side_effect__o = cc_side_effect_tag_for_promotion( CC_PE_None, CC_LTE_NoneLost );
            // return true;
            return false; // TODO
        } case CC_SETE_Conversion : {
            // char piece_symbol = ' ';
            //
            // if ( cc_fetch_piece_symbol( se_an, &piece_symbol, true, true ) ) {
            //     if ( !_cc_check_piece_has_congruent_type( piece_symbol, piece, step_start_an, step_end_an, parse_msgs__iod ) )
            //         return false;
            //
            //     ++se_an;
            // }
            //
            // if ( !_cc_check_piece_can_be_converted( piece, step_start_an, step_end_an, parse_msgs__iod ) )
            //     return false;
            //
            // CcPieceType convert_to = cc_piece_opposite( piece );
            // CcLosingTagType ltt = cc_parse_losing_tag( se_an );
            //
            // *side_effect__o = cc_side_effect_convert( convert_to, ltt );
            // return true;
            return false; // TODO
        } case CC_SETE_FailedConversion : {
            // if ( !_cc_check_failed_conversion( piece, step_start_an, step_end_an, parse_msgs__iod ) )
            //     return false;
            //
            // *side_effect__o = cc_side_effect_failed_conversion();
            // return true;
            return false; // TODO
        } case CC_SETE_Transparency : { // Intentional fall-through ...
        } case CC_SETE_Divergence : {
            // char piece_symbol = ' ';
            //
            // if ( cc_fetch_piece_symbol( se_an, &piece_symbol, false, true ) ) {
            //     if ( !_cc_check_piece_has_congruent_type( piece_symbol, piece, step_start_an, step_end_an, parse_msgs__iod ) )
            //         return false;
            //
            //     ++se_an;
            // }
            //
            // if ( sete == CC_SETE_Transparency )
            //     *side_effect__o = cc_side_effect_transparency( piece );
            // else if ( sete == CC_SETE_Divergence )
            //     *side_effect__o = cc_side_effect_diversion( piece );
            // else
            //     return false; // In case some other, unexpected side-effect gets here.
            //
            // return true;
            return false; // TODO
        } case CC_SETE_DemoteToPawn : {
            // TODO :: demote to Pawn
            return false;
        } case CC_SETE_Resurrection : { // Intentional fall-through ...
        } case CC_SETE_ResurrectingOpponent : {
            // if ( !_cc_check_field_is_empty( piece, "Resurrection can be performed only on an empty field, encountered %s in step '%s'.\n", step_start_an, step_end_an, parse_msgs__iod ) )
            //     return false;
            //
            // char piece_symbol = ' ';
            //
            // if ( !cc_fetch_piece_symbol( se_an, &piece_symbol, true, false ) )
            //     return false;
            //
            // if ( !_cc_check_piece_symbol_is_valid( piece_symbol, step_start_an, step_end_an, parse_msgs__iod ) )
            //     return false;
            //
            // bool is_light = true;
            //
            // if ( game->status == CC_GSE_Turn_Light )
            //     is_light = ( sete == CC_SETE_Resurrection );
            // else if ( game->status == CC_GSE_Turn_Dark )
            //     is_light = ( sete == CC_SETE_ResurrectingOpponent );
            // else
            //     return false; // Should check status, within caller stack.
            //
            // CcPieceType resurrecting = cc_piece_from_symbol( piece_symbol, is_light );
            //
            // if ( !_cc_check_piece_can_be_resurrected( resurrecting, step_start_an, step_end_an, parse_msgs__iod ) )
            //     return false;
            //
            // CcPos pos = CC_POS_CAST_INVALID;
            //
            // if ( CC_PIECE_IS_WAVE( resurrecting ) || CC_PIECE_IS_STARCHILD( resurrecting ) ) {
            //     char const * pos_an = se_an + 1;
            //     char const * pos_end_an = NULL;
            //
            //     if ( !_cc_parse_and_check_position( pos_an, &pos, &pos_end_an, "Error parsing resurrection destination, in step '%s'.\n", step_start_an, step_end_an, parse_msgs__iod ) )
            //         return false;
            //
            //     if ( !_cc_check_position_is_on_board( pos, cb, "Resurrection destination has to be complete (not a disambiguation), in step '%s'.\n", step_start_an, step_end_an, parse_msgs__iod ) )
            //         return false;
            // }
            //
            // *side_effect__o = cc_side_effect_resurrect( piece, pos );
            // return true;
            return false; // TODO
        } case CC_SETE_FailedResurrection : {
            *side_effect__o = cc_side_effect_failed_resurrection();
            return true;
        }

        default : return false;
    }
}
