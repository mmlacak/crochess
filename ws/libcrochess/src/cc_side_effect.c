// Copyright (c) 2021, 2022 Mario Mlačak, mmlacak@gmail.com
// Licensed under GNU GPL v3+ license. See LICENSING, COPYING files for details.

#include <stdlib.h>

#include "cc_defines.h"
#include "cc_side_effect.h"

/**
    @file cc_side_effect.c
    @brief Side-effects related functions.
*/


char const * cc_side_effect_symbol( CcSideEffectEnum see ) {
    switch ( see ) {
        case CC_SEE_None : return ""; /* Side-effect not found, uninitialized, or error happened. */
        case CC_SEE_Capture : return "*"; /* Capturing, corresponds to * (asterisk). */
        case CC_SEE_Displacement : return "<"; /* Trance-journey displacement, corresponds to < (less-than). */
        case CC_SEE_EnPassant : return ":"; /* En passant, corresponds to : (colon). */
        case CC_SEE_Castle : return "&"; /* Castling, corresponds to & (ampersand). */
        case CC_SEE_Promotion : return "="; /* Promotion, corresponds to = (equal sign), sign is optional. */
        case CC_SEE_TagForPromotion : return "="; /* Tag for promotion, corresponds to = (equal sign). */
        case CC_SEE_Conversion : return "%"; /* Conversion, corresponds to % (percent sign). */
        case CC_SEE_FailedConversion : return "%%"; /* Failed conversion, corresponds to %% (double percent sign). */
        case CC_SEE_Transparency : return "^"; /* Transparency, corresponds to ^ (caret), optional. */
        case CC_SEE_Divergence : return "/"; /* Divergence, corresponds to / (slash), optional. */
        case CC_SEE_DemoteToPawn : return ">"; /* Syzygy, demoting to Pawn, corresponds to > (greater-than sign). */
        case CC_SEE_Resurrection : return "$"; /* Syzygy, resurrection, corresponds to $ (dollar-sign). */
        case CC_SEE_ResurrectingOpponent : return "$$"; /* Syzygy, resurrecting opponent's piece, corresponds to $$ (dual dollar-sign). */
        case CC_SEE_FailedResurrection : return "$$$"; /* Syzygy, failed resurrection, corresponds to $$$ (triple dollar-sign). */

        default : return CC_DEFAULT_ENTITY_STRING;
    }
}


CcSideEffect cc_side_effect( CcSideEffectEnum type,
                             CcPieceEnum piece,
                             CcLosingTagEnum lost_tag,
                             CcPos start,
                             CcPos destination,
                             CcPieceEnum promoted_to ) {
    CcSideEffect sse = { .type = type, };

    // Nothing more to do if type == CC_SEE_None.
    if ( sse.type == CC_SEE_Capture ) {
        sse.capture.piece = piece;
        sse.capture.lost_tag = lost_tag;
    } else if ( sse.type == CC_SEE_Displacement ) {
        sse.displacement.piece = piece;
        sse.displacement.lost_tag = lost_tag;
        sse.displacement.destination = destination;
    } else if ( sse.type == CC_SEE_EnPassant ) {
        sse.en_passant.pawn = piece;
        sse.en_passant.distant = destination;
    } else if ( sse.type == CC_SEE_Castle ) {
        sse.castle.rook = piece;
        sse.castle.start = start;
        sse.castle.destination = destination;
    } else if ( sse.type == CC_SEE_Promotion ) {
        sse.promote.captured = piece;
        sse.promote.lost_tag = lost_tag;
        sse.promote.promoted_to = promoted_to;
    } else if ( sse.type == CC_SEE_TagForPromotion ) {
        sse.tag_for_promotion.captured = piece;
        sse.tag_for_promotion.lost_tag = lost_tag;
    } else if ( sse.type == CC_SEE_Conversion ) {
        sse.convert.piece = piece;
        sse.convert.lost_tag = lost_tag;
    // Nothing more to do if type == CC_SEE_FailedConversion.
    } else if ( sse.type == CC_SEE_Transparency ) {
        sse.transparency.piece = piece;
    } else if ( sse.type == CC_SEE_Divergence ) {
        sse.diversion.piece = piece;
    } else if ( sse.type == CC_SEE_DemoteToPawn ) {
        sse.demote.piece = piece;
        sse.demote.distant = destination;
    } else if ( sse.type == CC_SEE_Resurrection ||
                sse.type == CC_SEE_ResurrectingOpponent ) {
        sse.resurrect.piece = piece;
        sse.resurrect.destination = destination;
    }
    // Nothing more to do if type == CC_SEE_FailedResurrection.

    return sse;
}

CcPieceEnum cc_side_effect_piece( CcSideEffect se ) {
    switch ( se.type ) {
        case CC_SEE_None : return CC_PE_None;
        case CC_SEE_Capture : return se.capture.piece;
        case CC_SEE_Displacement : return se.displacement.piece;
        case CC_SEE_EnPassant : return se.en_passant.pawn;
        case CC_SEE_Castle : return se.castle.rook;
        case CC_SEE_Promotion : return se.promote.promoted_to;
        case CC_SEE_TagForPromotion : return CC_PE_None;
        case CC_SEE_Conversion : return se.convert.piece;
        case CC_SEE_FailedConversion : return CC_PE_None;
        case CC_SEE_Transparency : return se.transparency.piece;
        case CC_SEE_Divergence : return se.diversion.piece;
        case CC_SEE_DemoteToPawn : return se.demote.piece;

        case CC_SEE_Resurrection :
        case CC_SEE_ResurrectingOpponent :
            return se.resurrect.piece;

        case CC_SEE_FailedResurrection : return CC_PE_None;

        default : return CC_PE_None;
    }
}

CcPos cc_side_effect_destination( CcSideEffect se ) {
    switch ( se.type ) {
        case CC_SEE_None : return CC_POS_CAST_INVALID;
        case CC_SEE_Capture : return CC_POS_CAST_INVALID;
        case CC_SEE_Displacement : return se.displacement.destination;
        case CC_SEE_EnPassant : return se.en_passant.distant;
        case CC_SEE_Castle : return se.castle.destination;
        case CC_SEE_Promotion : return CC_POS_CAST_INVALID;
        case CC_SEE_TagForPromotion : return CC_POS_CAST_INVALID;
        case CC_SEE_Conversion : return CC_POS_CAST_INVALID;
        case CC_SEE_FailedConversion : return CC_POS_CAST_INVALID;
        case CC_SEE_Transparency : return CC_POS_CAST_INVALID;
        case CC_SEE_Divergence : return CC_POS_CAST_INVALID;
        case CC_SEE_DemoteToPawn : return se.demote.distant;

        case CC_SEE_Resurrection :
        case CC_SEE_ResurrectingOpponent :
            return se.resurrect.destination;

        case CC_SEE_FailedResurrection : return CC_POS_CAST_INVALID;

        default : return CC_POS_CAST_INVALID;
    }
}

//
// conveniences

CcSideEffect cc_side_effect_none( void ) {
    return cc_side_effect( CC_SEE_None, CC_PE_None, CC_LTE_None,
                           CC_POS_CAST_INVALID,
                           CC_POS_CAST_INVALID,
                           CC_PE_None );
}

CcSideEffect cc_side_effect_capture( CcPieceEnum piece, CcLosingTagEnum lost_tag ) {
    return cc_side_effect( CC_SEE_Capture, piece, lost_tag,
                           CC_POS_CAST_INVALID,
                           CC_POS_CAST_INVALID,
                           CC_PE_None );
}

CcSideEffect cc_side_effect_displacement( CcPieceEnum piece, CcLosingTagEnum lost_tag, CcPos destination ) {
    return cc_side_effect( CC_SEE_Displacement, piece, lost_tag,
                           CC_POS_CAST_INVALID,
                           destination,
                           CC_PE_None );
}

CcSideEffect cc_side_effect_en_passant( CcPieceEnum pawn, CcPos distant ) {
    return cc_side_effect( CC_SEE_EnPassant, pawn, CC_LTE_None,
                           CC_POS_CAST_INVALID,
                           distant,
                           CC_PE_None );
}

CcSideEffect cc_side_effect_castle( CcPieceEnum rook, CcPos start, CcPos destination ) {
    return cc_side_effect( CC_SEE_Castle, rook, CC_LTE_None,
                           start,
                           destination,
                           CC_PE_None );
}

CcSideEffect cc_side_effect_promote( CcPieceEnum captured, CcLosingTagEnum lost_tag, CcPieceEnum promoted_to ) {
    return cc_side_effect( CC_SEE_Promotion, captured, lost_tag,
                           CC_POS_CAST_INVALID,
                           CC_POS_CAST_INVALID,
                           promoted_to );
}

CcSideEffect cc_side_effect_tag_for_promotion( CcPieceEnum captured, CcLosingTagEnum lost_tag ) {
    return cc_side_effect( CC_SEE_TagForPromotion, captured, lost_tag,
                           CC_POS_CAST_INVALID,
                           CC_POS_CAST_INVALID,
                           CC_PE_None );
}

CcSideEffect cc_side_effect_convert( CcPieceEnum piece, CcLosingTagEnum lost_tag ) {
    return cc_side_effect( CC_SEE_Conversion, piece, lost_tag,
                           CC_POS_CAST_INVALID,
                           CC_POS_CAST_INVALID,
                           CC_PE_None );
}

CcSideEffect cc_side_effect_failed_conversion( void ) {
    return cc_side_effect( CC_SEE_FailedConversion, CC_PE_None, CC_LTE_None,
                           CC_POS_CAST_INVALID,
                           CC_POS_CAST_INVALID,
                           CC_PE_None );
}

CcSideEffect cc_side_effect_transparency( CcPieceEnum piece ) {
    return cc_side_effect( CC_SEE_Transparency, piece, CC_LTE_None,
                           CC_POS_CAST_INVALID,
                           CC_POS_CAST_INVALID,
                           CC_PE_None );
}

CcSideEffect cc_side_effect_diversion( CcPieceEnum piece ) {
    return cc_side_effect( CC_SEE_Divergence, piece, CC_LTE_None,
                           CC_POS_CAST_INVALID,
                           CC_POS_CAST_INVALID,
                           CC_PE_None );
}

CcSideEffect cc_side_effect_demote( CcPieceEnum piece, CcLosingTagEnum lost_tag, CcPos distant ) {
    return cc_side_effect( CC_SEE_DemoteToPawn, piece, lost_tag,
                           CC_POS_CAST_INVALID,
                           distant,
                           CC_PE_None );
}

CcSideEffect cc_side_effect_resurrect( CcPieceEnum piece, CcPos destination ) {
    return cc_side_effect( CC_SEE_Resurrection, piece, CC_LTE_None,
                           CC_POS_CAST_INVALID,
                           destination,
                           CC_PE_None );
}

CcSideEffect cc_side_effect_failed_resurrection( void ) {
    return cc_side_effect( CC_SEE_FailedResurrection, CC_PE_None, CC_LTE_None,
                           CC_POS_CAST_INVALID,
                           CC_POS_CAST_INVALID,
                           CC_PE_None );
}


bool cc_side_effect_to_short_str( CcSideEffect se,
                                  cc_char_16 * restrict se_str__o ) {
    if ( !cc_str_clear( *se_str__o, CC_MAX_LEN_CHAR_16 ) )
        return false;

    if ( se.type == CC_SEE_None )
        return true;

    char const * se_end = (char *)(se_str__o) + CC_MAX_LEN_CHAR_16;
    char * se_p = (char *)(se_str__o);
    size_t copied = 0;

    CcPieceEnum captured = CC_PE_None;
    CcLosingTagEnum lte = CC_LTE_None;

    if ( se.type == CC_SEE_Promotion ) {
        captured = se.promote.captured;
        lte = se.promote.lost_tag;
    } else if ( se.type == CC_SEE_TagForPromotion ) {
        captured = se.tag_for_promotion.captured;
        lte = se.tag_for_promotion.lost_tag;
    }

    if ( !CC_PIECE_IS_NONE( captured ) ) {
        *se_p++ = '*';

        char captured_char = cc_piece_symbol( captured );
        *se_p++ = captured_char;
    }

    if ( lte != CC_LTE_None ) {
        char const * lte_str = cc_losing_tag_as_string( lte );
        size_t lte_str_len = cc_str_len( lte_str, NULL, CC_MAX_LEN_LOSING_TAG );
        copied = cc_str_copy( lte_str, NULL, lte_str_len, *se_str__o, se_end, CC_MAX_LEN_CHAR_16 );
        if ( copied != lte_str_len ) return false;
        se_p += copied;
    }

    char const * see_str = cc_side_effect_symbol( se.type );
    size_t see_str_len = cc_str_len( see_str, NULL, CC_MAX_LEN_SIDE_EFFECT_SYMBOL );
    copied = cc_str_copy( see_str, NULL, see_str_len, *se_str__o, se_end, CC_MAX_LEN_CHAR_16 );
    if ( copied != see_str_len ) return false;
    se_p += copied;

    CcPieceEnum pe = cc_side_effect_piece( se );
    char piece = cc_piece_symbol( pe );
    *se_p++ = piece;

    CcPos destination = cc_side_effect_destination( se );
    cc_char_8 pos_c8 = CC_CHAR_8_EMPTY;
    if ( !cc_pos_to_short_string( destination, &pos_c8 ) )
        return false;

    size_t pos_len = cc_str_len( pos_c8, NULL, CC_MAX_LEN_CHAR_8 );
    copied = cc_str_copy( pos_c8, NULL, pos_len, se_p, se_end, CC_MAX_LEN_CHAR_16 );
    if ( copied != pos_len ) return false;
    // se_p += copied;

    return true;
}
