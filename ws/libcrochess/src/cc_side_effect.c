// Copyright (c) 2021, 2022, 2024 Mario Mlačak, mmlacak@gmail.com
// Licensed under GNU GPL v3+ license. See LICENSING, COPYING files for details.

#include <stdlib.h>

#include "cc_defines.h"
#include "cc_side_effect.h"

//
// Side-effect.

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
        sse.en_passant.private = piece;
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
        case CC_SETE_EnPassant : return se.en_passant.private;
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

bool cc_side_effect_is_valid( CcSideEffect se, bool include_none ) {
    if ( include_none ) {
        if ( !CC_SIDE_EFFECT_TYPE_IS_ENUMERATOR( se.type ) ) return false;
    } else {
        if ( !CC_SIDE_EFFECT_TYPE_IS_VALID( se.type ) ) return false;
    }

    switch ( se.type ) {
        case CC_SETE_None : return true;

        case CC_SETE_Capture : {
            if ( !CC_PIECE_IS_ENUMERATOR( se.capture.piece ) ) return false;
            if ( !CC_PIECE_CAN_BE_CAPTURED( se.capture.piece ) ) return false;
            return true;
        }

        case CC_SETE_Displacement : {
            if ( !CC_PIECE_IS_ENUMERATOR( se.displacement.piece ) ) return false;
            if ( !( CC_PIECE_CAN_BE_DISPLACED( se.displacement.piece )
                    || CC_PIECE_CAN_BE_DISPLACED_TRANCE_JOURNEY( se.displacement.piece ) ) )
                        return false;
            if ( !CC_POS_IS_VALID( se.displacement.destination ) ) return false;
            return true;
        }

        case CC_SETE_EnPassant : {
            // if ( !CC_PIECE_IS_ENUMERATOR( se.en_passant.private ) ) return false; // Not needed, CC_PIECE_CAN_BE_CAPTURED_EN_PASSANT() enumerates privates.
            if ( !CC_PIECE_CAN_BE_CAPTURED_EN_PASSANT( se.en_passant.private ) ) return false;
            if ( !CC_POS_IS_VALID( se.en_passant.distant ) ) return false;
            return true;
        }

        case CC_SETE_Castle : {
            // if ( !CC_PIECE_IS_ENUMERATOR( se.castle.rook ) ) return false; // Not needed, CC_PIECE_IS_ROOK() enumerates Rooks.
            if ( !CC_PIECE_IS_ROOK( se.castle.rook ) ) return false;
            if ( !CC_POS_IS_VALID( se.castle.start ) ) return false;
            if ( !CC_POS_IS_VALID( se.castle.destination ) ) return false;
            return true;
        }

        case CC_SETE_Promotion : {
            if ( !CC_PIECE_IS_ENUMERATOR( se.promote.captured ) ) return false;
            if ( !( ( se.promote.captured == CC_PTE_None )
                      || CC_PIECE_CAN_BE_CAPTURED( se.promote.captured ) ) )
                        return false;
            if ( !CC_PIECE_IS_VALID( se.promote.promoted_to ) ) return false;
            return true;
        }

        case CC_SETE_TagForPromotion : {
            if ( !CC_PIECE_IS_ENUMERATOR( se.tag_for_promotion.captured ) ) return false;
            if ( !( ( se.tag_for_promotion.captured == CC_PTE_None )
                      || CC_PIECE_CAN_BE_CAPTURED( se.tag_for_promotion.captured ) ) )
                        return false;
            return true;
        }

        case CC_SETE_Conversion : {
            if ( !CC_PIECE_IS_ENUMERATOR( se.convert.piece ) ) return false;
            if ( !CC_PIECE_CAN_BE_CONVERTED( se.convert.piece ) ) return false;
            return true;
        }

        case CC_SETE_FailedConversion : return true;

        case CC_SETE_Transparency : {
            // if ( !CC_PIECE_IS_ENUMERATOR( se.transparency.piece ) ) return false; // Not needed, CC_PIECE_IS_TRANSPARENT() enumerates transparent pieces.
            if ( !CC_PIECE_IS_TRANSPARENT( se.transparency.piece ) ) return false;
            return true;
        }

        case CC_SETE_Divergence : {
            // if ( !CC_PIECE_IS_ENUMERATOR( se.diversion.piece ) ) return false; // Not needed, CC_PIECE_IS_DIVERGENT() enumerates divergent pieces.
            if ( !CC_PIECE_IS_DIVERGENT( se.diversion.piece ) ) return false;
            return true;
        }

        case CC_SETE_DemoteToPawn : {
            if ( !CC_PIECE_IS_ENUMERATOR( se.demote.piece ) ) return false;
            if ( !CC_PIECE_CAN_BE_DEMOTED( se.demote.piece ) ) return false;
            return true;
        }

        case CC_SETE_Resurrection :
        case CC_SETE_ResurrectingOpponent : {
            if ( !CC_PIECE_IS_ENUMERATOR( se.resurrect.piece ) ) return false;
            if ( !CC_PIECE_CAN_BE_RESURRECTED( se.resurrect.piece ) ) return false;
            return true;
        }

        case CC_SETE_FailedResurrection : return true;

        default : return false;
    }
}

//
// User-readable representation of a side-effect.

bool cc_side_effect_to_str( CcSideEffect se,
                            cc_char_16 * se_str__o ) {
    if ( !se_str__o ) return false;

    if ( !cc_str_pad( *se_str__o, '\0', CC_SIZE_CHAR_16 ) )
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

CcSideEffect cc_side_effect_en_passant( CcPieceTagType private, CcPos distant ) {
    return cc_side_effect( CC_SETE_EnPassant, private,
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

//
// Side-effect linked list.

CcSideEffectLink * cc_side_effect_link__new( CcSideEffect side_effect ) {
    CcSideEffectLink * sel__t = CC_MALLOC( sizeof( CcSideEffectLink ) );
    if ( !sel__t ) return NULL;

    sel__t->side_effect = side_effect;
    sel__t->next = NULL;

    return sel__t;
}

CcSideEffectLink * cc_side_effect_link_append( CcSideEffectLink ** se_link__iod_a,
                                               CcSideEffect side_effect ) {
    if ( !se_link__iod_a ) return NULL;

    CcSideEffectLink * sel__t = cc_side_effect_link__new( side_effect );
    if ( !sel__t ) return NULL;

    if ( !*se_link__iod_a ) {
        *se_link__iod_a = sel__t; // Ownership transfer.
    } else {
        CcSideEffectLink * sel = *se_link__iod_a;
        CC_FASTFORWARD( sel );
        sel->next = sel__t; // Append + ownership transfer.
    }

    return sel__t; // Weak pointer.
}

CcSideEffectLink * cc_side_effect_link_duplicate_all__new( CcSideEffectLink * se_link ) {
    if ( !se_link ) return NULL;

    CcSideEffectLink * se_link__a = NULL;
    CcSideEffectLink * from = se_link;

    while ( from ) {
        CcSideEffectLink * sel__w = cc_side_effect_link_append( &se_link__a, from->side_effect );
        if ( !sel__w ) { // Failed append --> ownership not transferred ...
            cc_side_effect_link_free_all( &se_link__a );
            return NULL;
        }

        from = from->next;
    }

    return se_link__a;
}

CcSideEffectLink * cc_side_effect_link_extend( CcSideEffectLink ** se_link__iod_a,
                                               CcSideEffectLink ** se_link__n ) {
    if ( !se_link__iod_a ) return NULL;
    if ( !se_link__n ) return NULL;

    if ( !*se_link__n ) return *se_link__iod_a;

    if ( !*se_link__iod_a ) {
        // Ownership transfer.
        *se_link__iod_a = *se_link__n;
        *se_link__n = NULL;

        return *se_link__iod_a;
    }

    CcSideEffectLink * last = *se_link__iod_a;
    CC_FASTFORWARD( last );

    // Ownership transfer.
    last->next = *se_link__n;
    *se_link__n = NULL;

    return last->next;
}

bool cc_side_effect_link_free_all( CcSideEffectLink ** se_link__f ) {
    if ( !se_link__f ) return false;
    if ( !*se_link__f ) return true;

    CcSideEffectLink * sel = *se_link__f;
    CcSideEffectLink * tmp = NULL;

    while ( sel ) {
        tmp = sel->next;
        CC_FREE( sel );
        sel = tmp;
    }

    *se_link__f = NULL;
    return true;
}

size_t cc_side_effect_link_len( CcSideEffectLink * se_link ) {
    if ( !se_link ) return 0;

    size_t len = 0;
    CcSideEffectLink * sel = se_link;

    while ( sel ) {
        ++len;
        sel = sel->next;
    }

    return len;
}

char * cc_side_effect_link_to_string__new( CcSideEffectLink * se_link ) {
    if ( !se_link ) return NULL;

    size_t len = cc_side_effect_link_len( se_link );
    size_t size = len * ( CC_SIZE_CHAR_16 + 1 ); // +1 == ',' between each 2 side-effects

    char * se_str__a = calloc( 1, size );
    if ( !se_str__a ) return NULL;

    char * se = se_str__a;
    CcSideEffectLink * sel = se_link;
    cc_char_16 se_str = CC_CHAR_16_EMPTY;

    while ( sel ) {
        if ( !cc_side_effect_to_str( sel->side_effect, &se_str ) ) {
            CC_FREE( se_str__a );
            return NULL;
        }

        se = cc_str_append_into( se, se_str__a + size, CC_SIZE_IGNORE, se_str, NULL, CC_SIZE_CHAR_16 );
        if ( !se ) {
            CC_FREE( se_str__a );
            return NULL;
        }

        if ( sel->next ) *se++ = ',';

        if ( !cc_str_pad( se_str, '\0', CC_SIZE_CHAR_16 ) ) {
            CC_FREE( se_str__a );
            return NULL;
        }

        sel = sel->next;
    }

    return se_str__a;
}
