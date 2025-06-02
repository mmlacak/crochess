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

CcPieceTagType cc_set_piece_tag_from_losing( CcPieceTagType ptt,
                                             CcLosingTagType ltt,
                                             bool override_conflicting_tag ) {
    switch ( ltt ) {
        case CC_LTE_RushingTagLost : {
            switch ( ptt ) {
                case CC_PTE_DarkGrenadier_RushedCurrent :
                case CC_PTE_DarkGrenadier_RushedPrevious :
                    return override_conflicting_tag ? CC_PTE_DarkGrenadier_CanRush
                                                    : ptt;

                case CC_PTE_DarkGrenadier_CanRush :
                case CC_PTE_DarkGrenadier : return CC_PTE_DarkGrenadier_CanRush;

                case CC_PTE_DarkScout_RushedCurrent :
                case CC_PTE_DarkScout_RushedPrevious :
                    return override_conflicting_tag ? CC_PTE_DarkScout_CanRush
                                                    : ptt;

                case CC_PTE_DarkScout_CanRush :
                case CC_PTE_DarkScout : return CC_PTE_DarkScout_CanRush;

                case CC_PTE_DarkPawn_DelayedPromotion :
                case CC_PTE_DarkPawn_RushedCurrent :
                case CC_PTE_DarkPawn_RushedPrevious :
                    return override_conflicting_tag ? CC_PTE_DarkPawn_CanRush
                                                    : ptt;

                case CC_PTE_DarkPawn_CanRush :
                case CC_PTE_DarkPawn : return CC_PTE_DarkPawn_CanRush;

                case CC_PTE_LightPawn :
                case CC_PTE_LightPawn_CanRush : return CC_PTE_LightPawn_CanRush;

                case CC_PTE_LightPawn_RushedPrevious :
                case CC_PTE_LightPawn_RushedCurrent :
                case CC_PTE_LightPawn_DelayedPromotion :
                    return override_conflicting_tag ? CC_PTE_LightPawn_CanRush
                                                    : ptt;

                case CC_PTE_LightScout :
                case CC_PTE_LightScout_CanRush : return CC_PTE_LightScout_CanRush;

                case CC_PTE_LightScout_RushedPrevious :
                case CC_PTE_LightScout_RushedCurrent :
                    return override_conflicting_tag ? CC_PTE_LightScout_CanRush
                                                    : ptt;

                case CC_PTE_LightGrenadier :
                case CC_PTE_LightGrenadier_CanRush : return CC_PTE_LightGrenadier_CanRush;

                case CC_PTE_LightGrenadier_RushedPrevious :
                case CC_PTE_LightGrenadier_RushedCurrent :
                    return override_conflicting_tag ? CC_PTE_LightGrenadier_CanRush
                                                    : ptt;

                default : return ptt;
            }
        }

        case CC_LTE_CastlingTagLost : {
            switch ( ptt ) {
                case CC_PTE_DarkKing_CanCastle :
                case CC_PTE_DarkKing : return CC_PTE_DarkKing_CanCastle;

                case CC_PTE_DarkRook_CanCastle :
                case CC_PTE_DarkRook : return CC_PTE_DarkRook_CanCastle;

                case CC_PTE_LightRook :
                case CC_PTE_LightRook_CanCastle : return CC_PTE_LightRook_CanCastle;

                case CC_PTE_LightKing :
                case CC_PTE_LightKing_CanCastle : return CC_PTE_LightKing;

                default : return ptt;
            }
        }

        case CC_LTE_DelayedPromotionLost : {
            switch ( ptt ) {
                case CC_PTE_DarkPawn_DelayedPromotion :
                case CC_PTE_DarkPawn : return CC_PTE_DarkPawn_DelayedPromotion;

                case CC_PTE_DarkPawn_RushedCurrent :
                case CC_PTE_DarkPawn_RushedPrevious :
                case CC_PTE_DarkPawn_CanRush :
                    return override_conflicting_tag ? CC_PTE_DarkPawn_DelayedPromotion
                                                    : ptt;

                case CC_PTE_LightPawn_CanRush :
                case CC_PTE_LightPawn_RushedPrevious :
                case CC_PTE_LightPawn_RushedCurrent :
                    return override_conflicting_tag ? CC_PTE_LightPawn_DelayedPromotion
                                                    : ptt;

                case CC_PTE_LightPawn_DelayedPromotion :
                case CC_PTE_LightPawn : return CC_PTE_LightPawn_DelayedPromotion;

                default : return ptt;
            }
        }

        default : return ptt;
    }
}
