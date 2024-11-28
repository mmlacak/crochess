// Copyright (c) 2021 Mario Mlačak, mmlacak@gmail.com
// Licensed under GNU GPL v3+ license. See LICENSING, COPYING files for details.

#include "cc_defines.h"
#include "cc_tag.h"


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

char const * cc_tag_as_string( CcTagType tt, bool capitalize, bool no_tag ) {
    switch ( tt ) {
        case CC_TE_None :
            return no_tag ? ( capitalize ? "None"
                                         : "none" )
                          : "";

        case CC_TE_CanRush : return capitalize ? "En passant" : "en passant";
        case CC_TE_CanCastle : return capitalize ? "Castling" : "castling";
        case CC_TE_DelayedPromotion : return capitalize ? "Delayed promotion" : "delayed promotion";

        default : return CC_DEFAULT_VALUE_STRING;
    }
}
