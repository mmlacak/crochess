// Copyright (c) 2024 Mario Mlačak, mmlacak@gmail.com
// Licensed under GNU GPL v3+ license. See LICENSING, COPYING files for details.

#include <stddef.h>

#include "cc_defines.h"

#include "cc_parse_defs.h"

char const * cc_ply_link_symbol( CcPlyLinkEnum ple ) {
    switch ( ple ) {
        case CC_PLE_None : return NULL;
        case CC_PLE_StartingPly : return "";
        case CC_PLE_CascadingPly : return "~";
        case CC_PLE_Teleportation : return "|";
        case CC_PLE_TeleportationReemergence : return "||";
        case CC_PLE_TeleportationOblation : return "|||";
        case CC_PLE_TranceJourney : return "@";
        case CC_PLE_DualTranceJourney : return "@@";
        case CC_PLE_FailedTranceJourney : return "@@@";
        case CC_PLE_PawnSacrifice : return ";;";
        case CC_PLE_SenseJourney : return "\"";
        case CC_PLE_FailedSenseJourney : return "'";

        default : return NULL;
    }
}


char const * cc_step_link_symbol( CcStepLinkEnum sle ) {
    switch ( sle ) {
        case CC_SLE_None : return NULL;
        case CC_SLE_Start : return "";
        case CC_SLE_Reposition : return ",";
        case CC_SLE_Next : return ".";
        case CC_SLE_Distant : return "..";
        case CC_SLE_Destination : return "-";
        case CC_SLE_JustDestination : return "";

        default : return NULL;
    }
}


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

        default : return CC_DEFAULT_VALUE_STRING;
    }
}
