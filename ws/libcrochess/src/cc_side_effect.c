// Copyright (c) 2021, 2022, 2024 Mario Mlačak, mmlacak@gmail.com
// Licensed under GNU GPL v3+ license. See LICENSING, COPYING files for details.

#include <stdlib.h>

#include "cc_defines.h"
#include "cc_side_effect.h"


char const * cc_side_effect_type_symbol( CcSideEffectTypeEnum sete ) {
    switch ( sete ) {
        case CC_SETE_None : return ""; /* Side-effect not found, uninitialized, or error happened. */
        case CC_SETE_Capture : return "*"; /* Capturing, corresponds to * (asterisk). */
        case CC_SETE_Displacement : return "<"; /* Trance-journey displacement, corresponds to < (less-than). */
        case CC_SETE_EnPassant : return ":"; /* En passant, corresponds to : (colon). */
        case CC_SETE_Castle : return "&"; /* Castling, corresponds to & (ampersand). */
        case CC_SETE_Promotion : return "="; /* Promotion, corresponds to = (equal sign), sign is optional. */
        case CC_SETE_TagForPromotion : return "="; /* Tag for promotion, corresponds to = (equal sign). */
        case CC_SETE_Conversion : return "%"; /* Conversion, corresponds to % (percent sign). */
        case CC_SETE_FailedConversion : return "%%"; /* Failed conversion, corresponds to %% (double percent sign). */
        case CC_SETE_Transparency : return "^"; /* Transparency, corresponds to ^ (caret), optional. */
        case CC_SETE_Divergence : return "/"; /* Divergence, corresponds to / (slash), optional. */
        case CC_SETE_DemoteToPawn : return ">"; /* Syzygy, demoting to Pawn, corresponds to > (greater-than sign). */
        case CC_SETE_Resurrection : return "$"; /* Syzygy, resurrection, corresponds to $ (dollar-sign). */
        case CC_SETE_ResurrectingOpponent : return "$$"; /* Syzygy, resurrecting opponent's piece, corresponds to $$ (dual dollar-sign). */
        case CC_SETE_FailedResurrection : return "$$$"; /* Syzygy, failed resurrection, corresponds to $$$ (triple dollar-sign). */

        default : return CC_DEFAULT_VALUE_STRING;
    }
}

CcMaybeBoolEnum cc_side_effect_type_is_terminating( CcPieceTagType piece,
                                                    CcSideEffectTypeEnum sete ) {
    if ( sete == CC_SETE_None ) return CC_MBE_False;

    if ( CC_SIDE_EFFECT_TYPE_TERMINATES_PLY( sete ) ) return CC_MBE_True;

    if ( CC_SIDE_EFFECT_TYPE_DOES_NOT_TERMINATE_PLY( sete ) ) return CC_MBE_False;

    if ( !CC_SIDE_EFFECT_TYPE_IS_ENUMERATOR( sete ) ) return CC_MBE_Void; // Order is fine, above macros checks select SE types.
    if ( !CC_PIECE_IS_VALID( piece ) ) return CC_MBE_Void;

    if ( sete == CC_SETE_Capture )
        return CC_BOOL_TO_MAYBE( !CC_PIECE_IS_SHAMAN( piece ) );

    if ( sete == CC_SETE_Displacement )
        return CC_BOOL_TO_MAYBE( !CC_PIECE_IS_SHAMAN( piece ) && !CC_PIECE_IS_SERPENT( piece ) );

    return CC_MBE_Void; // All valid enum values has been handled above, return error for unrecognized (new?) value.
}


CcSideEffect cc_side_effect( CcSideEffectTypeEnum type,
                             CcPieceTagType piece,
                             CcPos start,
                             CcPos destination,
                             CcPieceTagType promoted_to ) {
    CcSideEffect sse = { .type = type, };

    // Nothing more to do if type == CC_SETE_None.
    if ( sse.type == CC_SETE_Capture ) {
        sse.capture.piece = piece;
    } else if ( sse.type == CC_SETE_Displacement ) {
        sse.displacement.piece = piece;
        sse.displacement.destination = destination;
    } else if ( sse.type == CC_SETE_EnPassant ) {
        sse.en_passant.pawn = piece;
        sse.en_passant.distant = destination;
    } else if ( sse.type == CC_SETE_Castle ) {
        sse.castle.rook = piece;
        sse.castle.start = start;
        sse.castle.destination = destination;
    } else if ( sse.type == CC_SETE_Promotion ) {
        sse.promote.captured = piece;
        sse.promote.promoted_to = promoted_to;
    } else if ( sse.type == CC_SETE_TagForPromotion ) {
        sse.tag_for_promotion.captured = piece;
    } else if ( sse.type == CC_SETE_Conversion ) {
        sse.convert.piece = piece;
    // Nothing more to do if type == CC_SETE_FailedConversion.
    } else if ( sse.type == CC_SETE_Transparency ) {
        sse.transparency.piece = piece;
    } else if ( sse.type == CC_SETE_Divergence ) {
        sse.diversion.piece = piece;
    } else if ( sse.type == CC_SETE_DemoteToPawn ) {
        sse.demote.piece = piece;
        sse.demote.distant = destination;
    } else if ( sse.type == CC_SETE_Resurrection ||
                sse.type == CC_SETE_ResurrectingOpponent ) {
        sse.resurrect.piece = piece;
        sse.resurrect.destination = destination;
    }
    // Nothing more to do if type == CC_SETE_FailedResurrection.

    return sse;
}

CcPieceTagType cc_side_effect_piece( CcSideEffect se ) {
    switch ( se.type ) {
        case CC_SETE_None : return CC_PTE_None;
        case CC_SETE_Capture : return se.capture.piece;
        case CC_SETE_Displacement : return se.displacement.piece;
        case CC_SETE_EnPassant : return se.en_passant.pawn;
        case CC_SETE_Castle : return se.castle.rook;
        case CC_SETE_Promotion : return se.promote.promoted_to;
        case CC_SETE_TagForPromotion : return CC_PTE_None;
        case CC_SETE_Conversion : return se.convert.piece;
        case CC_SETE_FailedConversion : return CC_PTE_None;
        case CC_SETE_Transparency : return se.transparency.piece;
        case CC_SETE_Divergence : return se.diversion.piece;
        case CC_SETE_DemoteToPawn : return se.demote.piece;

        case CC_SETE_Resurrection :
        case CC_SETE_ResurrectingOpponent :
            return se.resurrect.piece;

        case CC_SETE_FailedResurrection : return CC_PTE_None;

        default : return CC_PTE_None;
    }
}

CcPos cc_side_effect_destination( CcSideEffect se ) {
    switch ( se.type ) {
        case CC_SETE_None : return CC_POS_CAST_INVALID;
        case CC_SETE_Capture : return CC_POS_CAST_INVALID;
        case CC_SETE_Displacement : return se.displacement.destination;
        case CC_SETE_EnPassant : return se.en_passant.distant;
        case CC_SETE_Castle : return se.castle.destination;
        case CC_SETE_Promotion : return CC_POS_CAST_INVALID;
        case CC_SETE_TagForPromotion : return CC_POS_CAST_INVALID;
        case CC_SETE_Conversion : return CC_POS_CAST_INVALID;
        case CC_SETE_FailedConversion : return CC_POS_CAST_INVALID;
        case CC_SETE_Transparency : return CC_POS_CAST_INVALID;
        case CC_SETE_Divergence : return CC_POS_CAST_INVALID;
        case CC_SETE_DemoteToPawn : return se.demote.distant;

        case CC_SETE_Resurrection :
        case CC_SETE_ResurrectingOpponent :
            return se.resurrect.destination;

        case CC_SETE_FailedResurrection : return CC_POS_CAST_INVALID;

        default : return CC_POS_CAST_INVALID;
    }
}

bool cc_side_effect_has_destination( CcSideEffect se ) {
    switch ( se.type ) {
        case CC_SETE_Displacement : // Intentional fall-through.
        case CC_SETE_EnPassant :
        case CC_SETE_Castle :
        case CC_SETE_DemoteToPawn :
        case CC_SETE_Resurrection :
        case CC_SETE_ResurrectingOpponent :
            return true;

        default : return false;
    }
}


//
// User-readable representation of a side-effect.

bool cc_side_effect_to_str( CcSideEffect se,
                            cc_char_16 * se_str__o ) {
    if ( !se_str__o ) return false;

    if ( !cc_str_clear( *se_str__o, CC_SIZE_CHAR_16 ) )
        return false;

    if ( se.type == CC_SETE_None )
        return true;

    char const * se_end = (char *)(se_str__o) + CC_MAX_LEN_CHAR_16;
    char * se_p = (char *)(se_str__o);
    size_t copied = 0;

    CcPieceTagType captured = CC_PTE_None;
    CcLosingTagType ltt = CC_LTE_NoneLost;

    if ( se.type == CC_SETE_Promotion ) {
        captured = se.promote.captured;
        ltt = cc_losing_tag_from_piece( captured );
    } else if ( se.type == CC_SETE_TagForPromotion ) {
        captured = se.tag_for_promotion.captured;
        ltt = cc_losing_tag_from_piece( captured );
    }

    if ( !CC_PIECE_IS_NONE( captured ) ) {
        *se_p++ = '*';

        char captured_char = cc_piece_symbol( captured );
        *se_p++ = captured_char;
    }

    if ( ltt != CC_LTE_NoneLost ) {
        char const * lte_str = cc_losing_tag_symbol( ltt );
        size_t lte_str_len = cc_str_len( lte_str, NULL, CC_MAX_LEN_LOSING_TAG_SYMBOL );
        copied = cc_str_copy( lte_str, NULL, lte_str_len, *se_str__o, se_end, CC_MAX_LEN_CHAR_16 );
        if ( copied != lte_str_len ) return false;
        se_p += copied;
    }

    char const * see_str = cc_side_effect_type_symbol( se.type );
    size_t see_str_len = cc_str_len( see_str, NULL, CC_MAX_LEN_SIDE_EFFECT_TYPE_SYMBOL );
    copied = cc_str_copy( see_str, NULL, see_str_len, *se_str__o, se_end, CC_MAX_LEN_CHAR_16 );
    if ( copied != see_str_len ) return false;
    se_p += copied;

    CcPieceTagType pe = cc_side_effect_piece( se );
    char piece = cc_piece_symbol( pe );
    *se_p++ = piece;

    if ( cc_side_effect_has_destination( se ) ) {
        CcPos destination = cc_side_effect_destination( se );
        cc_char_8 pos_c8 = CC_CHAR_8_EMPTY;
        if ( !cc_pos_to_string( destination, &pos_c8 ) )
            return false;

        size_t pos_len = cc_str_len( pos_c8, NULL, CC_MAX_LEN_CHAR_8 );
        copied = cc_str_copy( pos_c8, NULL, pos_len, se_p, se_end, CC_MAX_LEN_CHAR_16 );
        if ( copied != pos_len ) return false;
        // se_p += copied;
    }

    return true;
}

//
// conveniences

CcSideEffect cc_side_effect_none( void ) {
    return cc_side_effect( CC_SETE_None, CC_PTE_None,
                           CC_POS_CAST_INVALID,
                           CC_POS_CAST_INVALID,
                           CC_PTE_None );
}

CcSideEffect cc_side_effect_capture( CcPieceTagType piece ) {
    return cc_side_effect( CC_SETE_Capture, piece,
                           CC_POS_CAST_INVALID,
                           CC_POS_CAST_INVALID,
                           CC_PTE_None );
}

CcSideEffect cc_side_effect_displacement( CcPieceTagType piece, CcPos destination ) {
    return cc_side_effect( CC_SETE_Displacement, piece,
                           CC_POS_CAST_INVALID,
                           destination,
                           CC_PTE_None );
}

CcSideEffect cc_side_effect_en_passant( CcPieceTagType pawn, CcPos distant ) {
    return cc_side_effect( CC_SETE_EnPassant, pawn,
                           CC_POS_CAST_INVALID,
                           distant,
                           CC_PTE_None );
}

CcSideEffect cc_side_effect_castle( CcPieceTagType rook, CcPos start, CcPos destination ) {
    return cc_side_effect( CC_SETE_Castle, rook,
                           start,
                           destination,
                           CC_PTE_None );
}

CcSideEffect cc_side_effect_promote( CcPieceTagType captured,  CcPieceTagType promoted_to ) {
    return cc_side_effect( CC_SETE_Promotion, captured,
                           CC_POS_CAST_INVALID,
                           CC_POS_CAST_INVALID,
                           promoted_to );
}

CcSideEffect cc_side_effect_tag_for_promotion( CcPieceTagType captured ) {
    return cc_side_effect( CC_SETE_TagForPromotion, captured,
                           CC_POS_CAST_INVALID,
                           CC_POS_CAST_INVALID,
                           CC_PTE_None );
}

CcSideEffect cc_side_effect_convert( CcPieceTagType piece ) {
    return cc_side_effect( CC_SETE_Conversion, piece,
                           CC_POS_CAST_INVALID,
                           CC_POS_CAST_INVALID,
                           CC_PTE_None );
}

CcSideEffect cc_side_effect_failed_conversion( void ) {
    return cc_side_effect( CC_SETE_FailedConversion, CC_PTE_None,
                           CC_POS_CAST_INVALID,
                           CC_POS_CAST_INVALID,
                           CC_PTE_None );
}

CcSideEffect cc_side_effect_transparency( CcPieceTagType piece ) {
    return cc_side_effect( CC_SETE_Transparency, piece,
                           CC_POS_CAST_INVALID,
                           CC_POS_CAST_INVALID,
                           CC_PTE_None );
}

CcSideEffect cc_side_effect_diversion( CcPieceTagType piece ) {
    return cc_side_effect( CC_SETE_Divergence, piece,
                           CC_POS_CAST_INVALID,
                           CC_POS_CAST_INVALID,
                           CC_PTE_None );
}

CcSideEffect cc_side_effect_demote( CcPieceTagType piece, CcPos distant ) {
    return cc_side_effect( CC_SETE_DemoteToPawn, piece,
                           CC_POS_CAST_INVALID,
                           distant,
                           CC_PTE_None );
}

CcSideEffect cc_side_effect_resurrect( CcPieceTagType piece, CcPos destination ) {
    return cc_side_effect( CC_SETE_Resurrection, piece,
                           CC_POS_CAST_INVALID,
                           destination,
                           CC_PTE_None );
}

CcSideEffect cc_side_effect_failed_resurrection( void ) {
    return cc_side_effect( CC_SETE_FailedResurrection, CC_PTE_None,
                           CC_POS_CAST_INVALID,
                           CC_POS_CAST_INVALID,
                           CC_PTE_None );
}
