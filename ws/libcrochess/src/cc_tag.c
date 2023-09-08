// Copyright (c) 2021 Mario Mlačak, mmlacak@gmail.com
// Licensed under GNU GPL v3+ license. See LICENSING, COPYING files for details.

#include "cc_tag.h"

/**
    @file cc_tag.c
    @brief Tag related functions.
*/


char cc_tag_as_char( CcTagEnum ct ) {
    switch ( ct ) {
        case CC_TE_None : return CC_TAG_CHAR_NONE;
        case CC_TE_CanRush : return CC_TAG_CHAR_CAN_RUSH;
        case CC_TE_CanCastle : return CC_TAG_CHAR_CAN_CASTLE;
        case CC_TE_DelayedPromotion : return CC_TAG_CHAR_DELAYED_PROMOTION;
        case CC_TE_EnPassant : return CC_TAG_CHAR_EN_PASSANT;
        case CC_TE_PawnSacrifice : return CC_TAG_CHAR_PAWN_SACRIFICE;

        default : return CC_TAG_CHAR_INVALID; } }

CcTagEnum cc_tag_from_char( char c ) {
    switch ( c ) {
        case CC_TAG_CHAR_NONE : return CC_TE_None;
        case CC_TAG_CHAR_CAN_RUSH : return CC_TE_CanRush;
        case CC_TAG_CHAR_CAN_CASTLE : return CC_TE_CanCastle;
        case CC_TAG_CHAR_DELAYED_PROMOTION : return CC_TE_DelayedPromotion;
        case CC_TAG_CHAR_EN_PASSANT : return CC_TE_EnPassant;
        case CC_TAG_CHAR_PAWN_SACRIFICE : return CC_TE_PawnSacrifice;

        default : return CC_TE_None; } }


char const * cc_losing_tag_as_string( CcLosingTagEnum te ) {
    switch ( te ) {
        case CC_LTE_DelayedPromotion : return "==";
        case CC_LTE_CanRush : return "::";
        case CC_LTE_CanCastle : return "&&";

        default : return ""; } }

CcLosingTagEnum cc_tag_to_losing( CcTagEnum te ) {
    switch ( te ) {
        case CC_TE_DelayedPromotion : return CC_LTE_DelayedPromotion;
        case CC_TE_CanRush : return CC_LTE_CanRush;
        case CC_TE_CanCastle : return CC_LTE_CanCastle;

        default : return CC_LTE_None; } }

CcTagEnum cc_tag_from_losing( CcLosingTagEnum lte ) {
    switch ( lte ) {
        case CC_LTE_DelayedPromotion : return CC_TE_DelayedPromotion;
        case CC_LTE_CanRush : return CC_TE_CanRush;
        case CC_LTE_CanCastle : return CC_TE_CanCastle;

        default : return CC_TE_None; } }
