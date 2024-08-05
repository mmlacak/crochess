// Copyright (c) 2021 Mario Mlačak, mmlacak@gmail.com
// Licensed under GNU GPL v3+ license. See LICENSING, COPYING files for details.

#include "cc_defines.h"
#include "cc_tag.h"


char cc_tag_as_char( cc_tag_t ct ) {
    switch ( CC_TAG_VALUE( ct ) ) {
        case CC_TE_None : return CC_TAG_CHAR_NONE;

        case CC_TE_CanRush : return CC_MOVE_STARTER_FLAG( ct ) ? CC_TAG_CHAR_MOVE_STARTER_CAN_RUSH : CC_TAG_CHAR_CAN_RUSH;
        case CC_TE_CanCastle : return CC_MOVE_STARTER_FLAG( ct ) ? CC_TAG_CHAR_MOVE_STARTER_CAN_CASTLE : CC_TAG_CHAR_CAN_CASTLE;
        case CC_TE_DelayedPromotion : return CC_MOVE_STARTER_FLAG( ct ) ? CC_TAG_CHAR_MOVE_STARTER_DELAYED_PROMOTION : CC_TAG_CHAR_DELAYED_PROMOTION;
        case CC_TE_EnPassant : return CC_MOVE_STARTER_FLAG( ct ) ? CC_TAG_CHAR_MOVE_STARTER_EN_PASSANT : CC_TAG_CHAR_EN_PASSANT;
        case CC_TE_PawnSacrifice : return CC_MOVE_STARTER_FLAG( ct ) ? CC_TAG_CHAR_MOVE_STARTER_PAWN_SACRIFICE : CC_TAG_CHAR_PAWN_SACRIFICE;

        default : return CC_TAG_CHAR_INVALID;
    }
}

cc_tag_t cc_tag_from_char( char c ) {
    switch ( c ) {
        case CC_TAG_CHAR_NONE : return CC_TE_None;

        case CC_TAG_CHAR_CAN_RUSH : return CC_TE_CanRush;
        case CC_TAG_CHAR_CAN_CASTLE : return CC_TE_CanCastle;
        case CC_TAG_CHAR_DELAYED_PROMOTION : return CC_TE_DelayedPromotion;
        case CC_TAG_CHAR_EN_PASSANT : return CC_TE_EnPassant;
        case CC_TAG_CHAR_PAWN_SACRIFICE : return CC_TE_PawnSacrifice;

        case CC_TAG_CHAR_MOVE_STARTER_CAN_RUSH : return CC_SET_MOVE_STARTER_FLAG( CC_TE_CanRush );
        case CC_TAG_CHAR_MOVE_STARTER_CAN_CASTLE : return CC_SET_MOVE_STARTER_FLAG( CC_TE_CanCastle );
        case CC_TAG_CHAR_MOVE_STARTER_DELAYED_PROMOTION : return CC_SET_MOVE_STARTER_FLAG( CC_TE_DelayedPromotion );
        case CC_TAG_CHAR_MOVE_STARTER_EN_PASSANT : return CC_SET_MOVE_STARTER_FLAG( CC_TE_EnPassant );
        case CC_TAG_CHAR_MOVE_STARTER_PAWN_SACRIFICE : return CC_SET_MOVE_STARTER_FLAG( CC_TE_PawnSacrifice );

        default : return CC_TE_None;
    }
}


char const * cc_losing_tag_symbol( CcLosingTagEnum lte ) {
    switch ( CC_TAG_VALUE( lte ) ) {
        case CC_LTE_NoneLost : return "";

        case CC_LTE_RushingTagLost : return "::";
        case CC_LTE_CastlingTagLost : return "&&";
        case CC_LTE_DelayedPromotionLost : return "==";

        default : return CC_DEFAULT_VALUE_STRING;
    }
}

char const * cc_losing_tag_as_string( CcLosingTagEnum lte,
                                      bool capitalize,
                                      bool no_tag ) {
    switch ( CC_TAG_VALUE( lte ) ) {
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

CcLosingTagEnum cc_tag_to_losing( cc_tag_t te ) {
    switch ( CC_TAG_VALUE( te ) ) {
        case CC_TE_DelayedPromotion : return CC_LTE_DelayedPromotionLost;
        case CC_TE_CanRush : return CC_LTE_RushingTagLost;
        case CC_TE_CanCastle : return CC_LTE_CastlingTagLost;

        default : return CC_LTE_NoneLost;
    }
}

cc_tag_t cc_tag_from_losing( CcLosingTagEnum lte ) {
    switch ( lte ) {
        case CC_LTE_DelayedPromotionLost : return CC_TE_DelayedPromotion;
        case CC_LTE_RushingTagLost : return CC_TE_CanRush;
        case CC_LTE_CastlingTagLost : return CC_TE_CanCastle;

        default : return CC_TE_None;
    }
}
