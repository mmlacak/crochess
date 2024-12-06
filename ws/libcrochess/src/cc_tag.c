// Copyright (c) 2021 Mario Mlačak, mmlacak@gmail.com
// Licensed under GNU GPL v3+ license. See LICENSING, COPYING files for details.

#include "cc_defines.h"
#include "cc_tag.h"

//
// Tag enum

char cc_tag_as_char( CcTagType ct ) {
    switch ( ct ) {
        case CC_TE_None : return CC_TAG_CHAR_NONE;

        case CC_TE_CanRush : return CC_TAG_CHAR_CAN_RUSH;
        case CC_TE_CanCastle : return CC_TAG_CHAR_CAN_CASTLE;
        case CC_TE_DelayedPromotion : return CC_TAG_CHAR_DELAYED_PROMOTION;

        default : return CC_TAG_CHAR_INVALID;
    }
}

CcTagType cc_tag_from_char( char c ) {
    switch ( c ) {
        case CC_TAG_CHAR_NONE : return CC_TE_None;

        case CC_TAG_CHAR_CAN_RUSH : return CC_TE_CanRush;
        case CC_TAG_CHAR_CAN_CASTLE : return CC_TE_CanCastle;
        case CC_TAG_CHAR_DELAYED_PROMOTION : return CC_TE_DelayedPromotion;

        default : return CC_TE_None;
    }
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

CcLosingTagType cc_tag_to_losing( CcTagType te ) {
    switch ( te ) {
        case CC_TE_DelayedPromotion : return CC_LTE_DelayedPromotionLost;
        case CC_TE_CanRush : return CC_LTE_RushingTagLost;
        case CC_TE_CanCastle : return CC_LTE_CastlingTagLost;

        default : return CC_LTE_NoneLost;
    }
}

CcTagType cc_tag_from_losing( CcLosingTagType ltt ) {
    switch ( ltt ) {
        case CC_LTE_DelayedPromotionLost : return CC_TE_DelayedPromotion;
        case CC_LTE_RushingTagLost : return CC_TE_CanRush;
        case CC_LTE_CastlingTagLost : return CC_TE_CanCastle;

        default : return CC_TE_None;
    }
}
