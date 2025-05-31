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
    else
        return ( CC_PIECE_IS_VALID( ptt ) ) ? CC_TAG_CHAR_PIECE
                                            : CC_TAG_CHAR_INVALID;
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

CcLosingTagType cc_convert_tag_to_losing( CcPieceTagType ptt ) {
    if ( CC_TAG_IS_CAN_RUSH( ptt ) ) return CC_LTE_RushingTagLost;
    else if ( CC_TAG_IS_DELAYED_PROMOTION( ptt ) ) return CC_LTE_DelayedPromotionLost;
    else if ( CC_TAG_IS_CAN_CASTLE( ptt ) ) return CC_LTE_CastlingTagLost;
    else return CC_LTE_NoneLost;
}
