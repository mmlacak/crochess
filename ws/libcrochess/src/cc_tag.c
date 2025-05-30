// Copyright (c) 2021 Mario Mlačak, mmlacak@gmail.com
// Licensed under GNU GPL v3+ license. See LICENSING, COPYING files for details.

#include "cc_defines.h"
#include "cc_tag.h"

//
// Tag enum

char cc_tag_as_char( CcPieceTagType ptt ) {
    if ( ptt == CC_PTE_None ) return CC_TAG_CHAR_NONE;
    else if ( CC_TAG_IS_CAN_RUSH( ptt ) ) return CC_TAG_CHAR_CAN_RUSH;
    else if ( CC_TAG_IS_CAN_CASTLE( ptt ) ) return CC_TAG_CHAR_CAN_CASTLE;
    else if ( CC_TAG_IS_DELAYED_PROMOTION( ptt ) ) return CC_TAG_CHAR_DELAYED_PROMOTION;
    else if ( CC_TAG_IS_RUSHED_PREVIOUS( ptt ) ) return CC_TAG_CHAR_RUSHED_PREVIOUS;
    else if ( CC_TAG_IS_RUSHED_CURRENT( ptt ) ) return CC_TAG_CHAR_RUSHED_CURRENT;
    else return CC_TAG_CHAR_INVALID;
}

// TODO :: DELETE : DOCS
// CcPieceTagType cc_tag_from_char( char c ) { // TODO :: FIX cc_piece_from_char()
//     return CC_PTE_None; // TODO :: FIX
// }

bool cc_tag_is_congruent( CcPieceTagType ptt_1, CcPieceTagType ptt_2 ) { // TODO :: FIX
    if ( !CC_TAG_IS_ENUMERATOR( ptt_1 ) ) return false;
    if ( !CC_TAG_IS_ENUMERATOR( ptt_2 ) ) return false;

    if ( ptt_1 != ptt_2 )
        return ( ( ptt_1 == CC_TE_None ) || ( ptt_2 == CC_TE_None ) );

    return true;
}

//
// Losing tag enum

char const * cc_losing_tag_symbol( CcLosingTagType ltt ) {
    switch ( ltt ) {
        case CC_LTE_NoneLost : return "";

        case CC_LTE_RushingTagLost : return "::";
        case CC_LTE_CastlingTagLost : return "&&";
        case CC_LTE_DelayedPromotionLost : return "==";

        default : return CC_DEFAULT_VALUE_STRING;
    }
}

char const * cc_losing_tag_as_string( CcLosingTagType ltt,
                                      bool capitalize,
                                      bool no_tag ) {
    switch ( ltt ) {
        case CC_LTE_NoneLost :
            return no_tag ? ( capitalize ? "None"
                                         : "none" )
                          : "";

        case CC_LTE_RushingTagLost : return capitalize ? "En passant" : "en passant";
        case CC_LTE_CastlingTagLost : return capitalize ? "Castling" : "castling";
        case CC_LTE_DelayedPromotionLost : return capitalize ? "Delayed promotion" : "delayed promotion";

        default : return CC_DEFAULT_VALUE_STRING;
    }
}

CcLosingTagType cc_convert_tag_to_losing( CcTagType te ) {
    switch ( te ) {
        case CC_TE_DelayedPromotion : return CC_LTE_DelayedPromotionLost;
        case CC_TE_CanRush : return CC_LTE_RushingTagLost;
        case CC_TE_CanCastle : return CC_LTE_CastlingTagLost;

        default : return CC_LTE_NoneLost;
    }
}

CcTagType cc_convert_tag_from_losing( CcLosingTagType ltt ) {
    switch ( ltt ) {
        case CC_LTE_DelayedPromotionLost : return CC_TE_DelayedPromotion;
        case CC_LTE_RushingTagLost : return CC_TE_CanRush;
        case CC_LTE_CastlingTagLost : return CC_TE_CanCastle;

        default : return CC_TE_None;
    }
}

bool cc_losing_tag_is_congruent( CcLosingTagType ltt_1, CcLosingTagType ltt_2 ) {
    if ( !CC_LOSING_TAG_IS_ENUMERATOR( ltt_1 ) ) return false;
    if ( !CC_LOSING_TAG_IS_ENUMERATOR( ltt_2 ) ) return false;

    if ( ( ltt_1 != CC_TE_None ) && ( ltt_2 != CC_TE_None ) && ( ltt_1 != ltt_2 ) )
        // Both losing tags are valid, but different ...
        return false;

    return true;
}
