// Copyright (c) 2022 Mario Mlačak, mmlacak@gmail.com
// Licensed under GNU GPL v3+ license. See LICENSING, COPYING files for details.

// #include <ctype.h>

#include "cc_setup_misc.h"

#include "cc_checks.h"
#include "cc_parse_utils.h"
#include "cc_parse_side_effect.h"


static bool _cc_fail_with_msg_unrecognized_piece_symbol( char piece_symbol,
                                                         char const * start_an,
                                                         char const * end_an,
                                                         CcParseMsg ** parse_msgs__iod ) {
    char * an__a = cc_str_copy__new( start_an, end_an, CC_MAX_LEN_BUFFER );
    cc_parse_msg_append_fmt( parse_msgs__iod, CC_PMTE_Error, CC_MAX_LEN_BUFFER, "Unrecognized piece symbol '%c' encountered, in '%s'.\n", piece_symbol, an__a );
    CC_FREE( an__a );
    return false;
}

static bool _cc_fail_with_msg_in_step( char const * msg_fmt,
                                       char const * start_an,
                                       char const * end_an,
                                       CcParseMsg ** parse_msgs__iod ) {
    char * an__a = cc_str_copy__new( start_an, end_an, CC_MAX_LEN_BUFFER );
    cc_parse_msg_append_fmt( parse_msgs__iod, CC_PMTE_Error, CC_MAX_LEN_BUFFER, msg_fmt, an__a );
    CC_FREE( an__a );
    return false;
}

static bool _cc_fail_with_msg_piece_in_side_effect( char const * msg_fmt,
                                                    CcPieceTagType piece,
                                                    bool capitalize,
                                                    bool empty_field,
                                                    char const * start_an,
                                                    char const * end_an,
                                                    CcParseMsg ** parse_msgs__iod ) {
    char const * piece_str = cc_piece_label( piece, capitalize, empty_field );
    char * an__a = cc_str_copy__new( start_an, end_an, CC_MAX_LEN_BUFFER );
    cc_parse_msg_append_fmt( parse_msgs__iod, CC_PMTE_Error, CC_MAX_LEN_BUFFER, msg_fmt, piece_str, an__a );
    CC_FREE( an__a );
    return false;
}


bool cc_parse_side_effect( char const * side_effect_an,
                           char const * step_start_an,
                           char const * step_end_an,
                           bool is_turn_light,
                           cc_uint_t board_size,
                           CcSideEffect * side_effect__o,
                           char const ** side_effect_end_an__o,
                           CcParseMsg ** parse_msgs__iod ) {
    if ( !side_effect_an ) return false;
    if ( !step_start_an ) return false;
    if ( !step_end_an ) return false;
    if ( !side_effect__o ) return false;
    if ( !side_effect_end_an__o || *side_effect_end_an__o ) return false;
    if ( !parse_msgs__iod ) return false;

    if ( !CC_IS_BOARD_SIZE_VALID( board_size ) ) return false;

    bool is_opponent_light = !is_turn_light;
    bool has_promotion_sign = false;
    CcSideEffectTypeEnum sete = cc_parse_side_effect_type( side_effect_an, &has_promotion_sign );
    char const * se_an = side_effect_an + cc_side_effect_type_len( sete, has_promotion_sign );

    switch ( sete ) {
        case CC_SETE_None : {
            *side_effect__o = cc_side_effect_none();
            *side_effect_end_an__o = side_effect_an;
            return true;
        } case CC_SETE_Capture : {
            char piece_symbol = ' ';
            CcMaybeBoolEnum result = cc_fetch_piece_symbol( se_an, CC_MBE_False, &piece_symbol );

            if ( result == CC_MBE_False )
                return _cc_fail_with_msg_unrecognized_piece_symbol( piece_symbol, step_start_an, step_end_an, parse_msgs__iod );
            else if ( result != CC_MBE_True ) // == CC_MBE_Void (or, some garbage)
                return false;

            CcPieceTagType piece = cc_piece_from_symbol( piece_symbol, is_opponent_light ); // If piece symbol was not found, piece is none.

            if ( !CC_PIECE_IS_NONE( piece ) ) // Piece is optional.
                if ( !CC_PIECE_CAN_BE_CAPTURED( piece ) )
                    return _cc_fail_with_msg_piece_in_side_effect( "%s cannot be captured, in step '%s'.\n", piece, true, true, step_start_an, step_end_an, parse_msgs__iod );

            if ( cc_piece_symbol_is_valid( *se_an ) ) ++se_an;

            CcLosingTagType ltt = cc_parse_losing_tag( se_an );
            se_an += cc_losing_tag_len( ltt );

            //
            // Check if followed by tag or promotion.

            bool is_tag_or_promo = false;
            if ( *se_an == '=' ) {
                is_tag_or_promo = true;
                ++se_an;
            }

            char promoted_to_symbol = ' ';
            CcMaybeBoolEnum result_pts = cc_fetch_piece_symbol( se_an, CC_MBE_False, &promoted_to_symbol );

            if ( result_pts == CC_MBE_False )
                return _cc_fail_with_msg_unrecognized_piece_symbol( promoted_to_symbol, step_start_an, step_end_an, parse_msgs__iod );
            else if ( result_pts != CC_MBE_True ) // == CC_MBE_Void (or, some garbage)
                return false;

            CcPieceTagType promoted_to = cc_piece_from_symbol( promoted_to_symbol, is_turn_light ); // If piece symbol was not found, piece is none.

            if ( !CC_PIECE_IS_NONE( promoted_to ) ) // Promoted-to piece is optional.
                if ( !CC_PAWN_CAN_BE_PROMOTED_TO( promoted_to ) )
                    return _cc_fail_with_msg_piece_in_side_effect( "Pawn cannot be promoted to %s, in step '%s'.\n", promoted_to, false, true, step_start_an, step_end_an, parse_msgs__iod );

            if ( cc_piece_symbol_is_valid( *se_an ) ) ++se_an;

            //
            // Capture turned into side-side-effect, if tag or promotion.

            if ( CC_PIECE_IS_VALID( promoted_to ) ) {
                *side_effect__o = cc_side_effect_promote( piece, promoted_to );
            } else if ( is_tag_or_promo ) {
                *side_effect__o = cc_side_effect_tag_for_promotion( piece );
            } else
                *side_effect__o = cc_side_effect_capture( piece );

            *side_effect_end_an__o = se_an;
            return true;
        } case CC_SETE_Displacement : {
            char piece_symbol = ' ';
            CcMaybeBoolEnum result = cc_fetch_piece_symbol( se_an, CC_MBE_False, &piece_symbol );

            if ( result == CC_MBE_False )
                return _cc_fail_with_msg_unrecognized_piece_symbol( piece_symbol, step_start_an, step_end_an, parse_msgs__iod );
            else if ( result != CC_MBE_True ) // == CC_MBE_Void (or, some garbage)
                return false;

            CcPieceTagType piece = cc_piece_from_symbol( piece_symbol, is_opponent_light ); // If piece symbol was not found, piece is none.

            if ( !CC_PIECE_IS_NONE( piece ) ) // Piece is optional.
                if ( !CC_PIECE_CAN_BE_DISPLACED_TRANCE_JOURNEY( piece ) )
                    return _cc_fail_with_msg_piece_in_side_effect( "%s cannot be displaced, in step '%s'.\n", piece, true, true, step_start_an, step_end_an, parse_msgs__iod );

            if ( cc_piece_symbol_is_valid( *se_an ) ) ++se_an;

            CcLosingTagType ltt = cc_parse_losing_tag( se_an );
            se_an += cc_losing_tag_len( ltt );

            CcPos pos = CC_POS_CAST_INVALID;
            char const * pos_end_an = NULL;

            if ( !cc_parse_pos( se_an, &pos, &pos_end_an ) ) // Displacement position is mandatory.
                return _cc_fail_with_msg_in_step( "Error parsing displacement destination, in step '%s'.\n", step_start_an, step_end_an, parse_msgs__iod );

            if ( !CC_IS_POS_ON_BOARD( board_size, pos.i, pos.j ) )
                return _cc_fail_with_msg_in_step( "Displacement destination has to be complete (not a disambiguation), in step '%s'.\n", step_start_an, step_end_an, parse_msgs__iod );

            *side_effect__o = cc_side_effect_displacement( piece, pos );
            *side_effect_end_an__o = pos_end_an;
            return true;
        } case CC_SETE_EnPassant : {
            char piece_symbol = ' ';
            CcMaybeBoolEnum result = cc_fetch_piece_symbol( se_an, CC_MBE_False, &piece_symbol ); // In addition to Pawns, in later variants Scouts and Grenadiers can be en passant-ed, too.

            if ( result == CC_MBE_False )
                return _cc_fail_with_msg_unrecognized_piece_symbol( piece_symbol, step_start_an, step_end_an, parse_msgs__iod );
            else if ( result != CC_MBE_True ) // == CC_MBE_Void (or, some garbage)
                return false;

            CcPieceTagType piece = cc_piece_from_symbol( piece_symbol, is_opponent_light ); // If piece symbol was not found, piece is none.

            if ( !CC_PIECE_IS_NONE( piece ) ) // Piece is optional.
                if ( !CC_PIECE_CAN_BE_CAPTURED_EN_PASSANT( piece ) )
                    return _cc_fail_with_msg_piece_in_side_effect( "Only privates can be captured en passant, %s encountered, in step '%s'.\n", piece, false, true, step_start_an, step_end_an, parse_msgs__iod );

            if ( cc_piece_symbol_is_valid( *se_an ) ) ++se_an;

            CcPos pos = CC_POS_CAST_INVALID;
            char const * pos_end_an = NULL;

            if ( CC_IS_CHAR_COORD( *se_an ) ) { // Position is optional.
                if ( !cc_parse_pos( se_an, &pos, &pos_end_an ) )
                    return _cc_fail_with_msg_in_step( "Error parsing en passant location, in step '%s'.\n", step_start_an, step_end_an, parse_msgs__iod );

                if ( !CC_IS_COORD_ON_BOARD( board_size, pos.j ) ) // If location is given, at least rank must be valid.
                    return _cc_fail_with_msg_in_step( "If en passant location is given, at least rank must be valid, in step '%s'.\n", step_start_an, step_end_an, parse_msgs__iod );

                se_an = pos_end_an;
            }

            *side_effect__o = cc_side_effect_en_passant( piece, pos );
            *side_effect_end_an__o = se_an;
            return true;
        } case CC_SETE_Castle : {
            char piece_symbol = ' ';
            CcMaybeBoolEnum result = cc_fetch_piece_symbol( se_an, CC_MBE_False, &piece_symbol );

            if ( result == CC_MBE_False )
                return _cc_fail_with_msg_unrecognized_piece_symbol( piece_symbol, step_start_an, step_end_an, parse_msgs__iod );
            else if ( result != CC_MBE_True ) // == CC_MBE_Void (or, some garbage)
                return false;

            CcPieceTagType piece = cc_piece_from_symbol( piece_symbol, is_turn_light );

            if ( !CC_PIECE_IS_NONE( piece ) ) // Piece is optional.
                if ( !CC_PIECE_IS_ROOK( piece ) )
                    return _cc_fail_with_msg_piece_in_side_effect( "Only Rooks can castle with their King, %s encountered, in step '%s'.\n", piece, false, true, step_start_an, step_end_an, parse_msgs__iod );

            if ( cc_piece_symbol_is_valid( *se_an ) ) ++se_an;

            CcPos pos = CC_POS_CAST_INVALID;
            char const * pos_end_an = NULL;

            if ( CC_IS_CHAR_COORD( *se_an ) ) { // Position is optional.
                if ( !cc_parse_pos( se_an, &pos, &pos_end_an ) )
                    return _cc_fail_with_msg_in_step( "Error parsing Rook castling destination, in step '%s'.\n", step_start_an, step_end_an, parse_msgs__iod );

                if ( !CC_IS_COORD_ON_BOARD( board_size, pos.i ) ) // If Rook castling destination is given, at least file must be valid.
                    return _cc_fail_with_msg_in_step( "If Rook castling destination is given, at least file must be valid, in step '%s'.\n", step_start_an, step_end_an, parse_msgs__iod );

                se_an = pos_end_an;
            }

            *side_effect__o = cc_side_effect_castle( piece, CC_POS_CAST_INVALID, pos );
            *side_effect_end_an__o = se_an;
            return true;
        } case CC_SETE_Promotion : {
            char piece_symbol = ' ';
            CcMaybeBoolEnum result = cc_fetch_piece_symbol( se_an, CC_MBE_Void, &piece_symbol );

            if ( result == CC_MBE_False )
                if ( isupper( piece_symbol ) )
                    return _cc_fail_with_msg_unrecognized_piece_symbol( piece_symbol, step_start_an, step_end_an, parse_msgs__iod );
                else
                    return _cc_fail_with_msg_in_step( "Figure to which Pawn has been promoted to is mandatory, in step '%s'.\n", step_start_an, step_end_an, parse_msgs__iod );
            else if ( result != CC_MBE_True ) // == CC_MBE_Void (or, some garbage)
                return false;

            CcPieceTagType piece = cc_piece_from_symbol( piece_symbol, is_turn_light ); // If piece symbol was not found, piece is none.

            if ( !CC_PAWN_CAN_BE_PROMOTED_TO( piece ) ) // Piece is not optional here, so no need to check if it's valid.
                return _cc_fail_with_msg_piece_in_side_effect( "Pawns can't be promoted to %s, in step '%s'.\n", piece, false, true, step_start_an, step_end_an, parse_msgs__iod );

            if ( cc_piece_symbol_is_valid( *se_an ) ) ++se_an;

            *side_effect__o = cc_side_effect_promote( CC_PTE_None, piece );
            *side_effect_end_an__o = se_an;
            return true;
        } case CC_SETE_TagForPromotion : {
            *side_effect__o = cc_side_effect_tag_for_promotion( CC_PTE_None );
            *side_effect_end_an__o = side_effect_an;
            return true;
        } case CC_SETE_Conversion : {
            char piece_symbol = ' ';
            CcMaybeBoolEnum result = cc_fetch_piece_symbol( se_an, CC_MBE_False, &piece_symbol );

            if ( result == CC_MBE_False )
                return _cc_fail_with_msg_unrecognized_piece_symbol( piece_symbol, step_start_an, step_end_an, parse_msgs__iod );
            else if ( result != CC_MBE_True ) // == CC_MBE_Void (or, some garbage)
                return false;

            CcPieceTagType piece = cc_piece_from_symbol( piece_symbol, is_opponent_light ); // If piece symbol was not found, piece is none.

            if ( !CC_PIECE_IS_NONE( piece ) ) // Piece is optional.
                if ( !CC_PIECE_CAN_BE_CONVERTED( piece ) )
                    return _cc_fail_with_msg_piece_in_side_effect( "Piece %s can't be converted, in step '%s'.\n", piece, true, true, step_start_an, step_end_an, parse_msgs__iod );

            if ( cc_piece_symbol_is_valid( *se_an ) ) ++se_an;

            CcLosingTagType ltt = cc_parse_losing_tag( se_an );
            se_an += cc_losing_tag_len( ltt );

            *side_effect__o = cc_side_effect_convert( piece );
            *side_effect_end_an__o = se_an;
            return true;
        } case CC_SETE_FailedConversion : {
            *side_effect__o = cc_side_effect_failed_conversion();
            *side_effect_end_an__o = side_effect_an;
            return true;
        } case CC_SETE_Transparency : {
            char piece_symbol = ' ';
            CcMaybeBoolEnum result = cc_fetch_piece_symbol( se_an, CC_MBE_False, &piece_symbol );

            if ( result == CC_MBE_False )
                return _cc_fail_with_msg_unrecognized_piece_symbol( piece_symbol, step_start_an, step_end_an, parse_msgs__iod );
            else if ( result != CC_MBE_True ) // == CC_MBE_Void (or, some garbage)
                return false;

            CcPieceTagType piece = cc_piece_from_symbol( piece_symbol, is_turn_light ); // If piece symbol was not found, piece is none.

            if ( !CC_PIECE_IS_NONE( piece ) ) // Piece is optional.
                if ( CC_PIECE_IS_OPAQUE( piece ) )
                    return _cc_fail_with_msg_piece_in_side_effect( "%s is opaque, expected transparent piece, in step '%s'.\n", piece, true, true, step_start_an, step_end_an, parse_msgs__iod );

            if ( cc_piece_symbol_is_valid( *se_an ) ) ++se_an;

            *side_effect__o = cc_side_effect_transparency( piece );
            *side_effect_end_an__o = se_an;
            return true;
        } case CC_SETE_Divergence : {
            char piece_symbol = ' ';
            CcMaybeBoolEnum result = cc_fetch_piece_symbol( se_an, CC_MBE_False, &piece_symbol );

            if ( result == CC_MBE_False )
                return _cc_fail_with_msg_unrecognized_piece_symbol( piece_symbol, step_start_an, step_end_an, parse_msgs__iod );
            else if ( result != CC_MBE_True ) // == CC_MBE_Void (or, some garbage)
                return false;

            CcPieceTagType piece = cc_piece_from_symbol( piece_symbol, is_turn_light ); // If piece symbol was not found, piece is none.

            if ( !CC_PIECE_IS_NONE( piece ) ) // Piece is optional.
                if ( !CC_PIECE_IS_DIVERGENT( piece ) )
                    return _cc_fail_with_msg_piece_in_side_effect( "%s is not divergent, in step '%s'.\n", piece, true, true, step_start_an, step_end_an, parse_msgs__iod );

            if ( cc_piece_symbol_is_valid( *se_an ) ) ++se_an;

            *side_effect__o = cc_side_effect_diversion( piece );
            *side_effect_end_an__o = se_an;
            return true;
        } case CC_SETE_DemoteToPawn : {
            char piece_symbol = ' ';
            CcMaybeBoolEnum result = cc_fetch_piece_symbol( se_an, CC_MBE_False, &piece_symbol );

            if ( result == CC_MBE_False )
                return _cc_fail_with_msg_unrecognized_piece_symbol( piece_symbol, step_start_an, step_end_an, parse_msgs__iod );
            else if ( result != CC_MBE_True ) // == CC_MBE_Void (or, some garbage)
                return false;

            CcPieceTagType piece = cc_piece_from_symbol( piece_symbol, is_turn_light ); // If piece symbol was not found, piece is none.

            if ( !CC_PIECE_IS_NONE( piece ) ) // Piece is optional.
                if ( !CC_PIECE_CAN_BE_DEMOTED( piece ) )
                    return _cc_fail_with_msg_piece_in_side_effect( "%s can't be demoted, in step '%s'.\n", piece, true, true, step_start_an, step_end_an, parse_msgs__iod );

            if ( cc_piece_symbol_is_valid( *se_an ) ) ++se_an;

            CcLosingTagType ltt = cc_parse_losing_tag( se_an );
            char const * pos_an = se_an + cc_losing_tag_len( ltt );

            CcPos pos = CC_POS_CAST_INVALID;
            char const * pos_end_an = NULL;

            if ( cc_parse_pos( pos_an, &pos, &pos_end_an ) ) { // Demoting position is also optional.
                if ( !CC_IS_DISAMBIGUATION_ON_BOARD( board_size, pos.i, pos.j ) ) // If demoting position is given, at least one coordinate must be valid.
                    return _cc_fail_with_msg_in_step( "If demoting position is given, it must be valid disambiguation, in step '%s'.\n", step_start_an, step_end_an, parse_msgs__iod );

                se_an = pos_end_an;
            }

            if ( !( CC_PIECE_IS_VALID( piece ) ||
                    CC_IS_DISAMBIGUATION_ON_BOARD( board_size, pos.i, pos.j ) ) ) // Either piece or demoting disambiguation has to be given.
                return _cc_fail_with_msg_in_step( "For demoting either piece or disambiguation has to be given, in step '%s'.\n", step_start_an, step_end_an, parse_msgs__iod );

            *side_effect__o = cc_side_effect_demote( piece, pos );
            *side_effect_end_an__o = se_an;
            return true;
        } case CC_SETE_Resurrection : // Intentional fall-through ...
        case CC_SETE_ResurrectingOpponent : {
            char piece_symbol = ' ';
            CcMaybeBoolEnum result = cc_fetch_piece_symbol( se_an, CC_MBE_Void, &piece_symbol );

            if ( result == CC_MBE_False )
                if ( isupper( piece_symbol ) )
                    return _cc_fail_with_msg_unrecognized_piece_symbol( piece_symbol, step_start_an, step_end_an, parse_msgs__iod );
                else
                    return _cc_fail_with_msg_in_step( "Resurrected piece is mandatory, in step '%s'.\n", step_start_an, step_end_an, parse_msgs__iod );
            else if ( result != CC_MBE_True ) // == CC_MBE_Void (or, some garbage)
                return false;

            bool is_light = ( sete == CC_SETE_ResurrectingOpponent ) ? is_opponent_light
                                                                     : is_turn_light;

            CcPieceTagType piece = cc_piece_from_symbol( piece_symbol, is_light ); // If piece symbol was not found, piece is none.

            if ( !CC_PIECE_CAN_BE_RESURRECTED( piece ) ) // Piece is not optional here, so no need to check if it's valid.
                return _cc_fail_with_msg_piece_in_side_effect( "%s can't be resurrected, in step '%s'.\n", piece, true, true, step_start_an, step_end_an, parse_msgs__iod );

            if ( cc_piece_symbol_is_valid( *se_an ) ) ++se_an;

            CcPos pos = CC_POS_CAST_INVALID;
            char const * pos_end_an = NULL;

            if ( CC_PIECE_IS_WAVE( piece ) || CC_PIECE_IS_STARCHILD( piece ) ) { // Resurrecting position is effectively optional, but if given, it has to be complete.
                if ( cc_parse_pos( se_an, &pos, &pos_end_an ) ) {
                    if ( !CC_IS_POS_ON_BOARD( board_size, pos.i, pos.j ) )
                        return _cc_fail_with_msg_in_step( "If given, resurrecting destination has to be complete (and not a disambiguation), in step '%s'.\n", step_start_an, step_end_an, parse_msgs__iod );

                    se_an = pos_end_an;
                }
            }

            *side_effect__o = cc_side_effect_resurrect( piece, pos );
            *side_effect_end_an__o = se_an;
            return true;
        } case CC_SETE_FailedResurrection : {
            *side_effect__o = cc_side_effect_failed_resurrection();
            *side_effect_end_an__o = side_effect_an;
            return true;
        }

        default : return false;
    }
}
