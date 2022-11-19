// Copyright (c) 2021, 2022 Mario Mlačak, mmlacak@gmail.com
// Licensed under GNU GPL v3+ license. See LICENSING, COPYING files for details.

#include <stdlib.h>

#include "cc_defines.h"
#include "cc_side_effect.h"

/**
    @file cc_side_effect.c
    @brief Side-effects related functions.
*/


char const * cc_side_effect_symbol( CcSideEffectEnum see )
{
    switch ( see )
    {
        case CC_SEE_None : return ""; /**< Side-effect not found, uninitialized, or error happened. */
        case CC_SEE_Capture : return "*"; /* Capturing, corresponds to * (asterisk). */
        case CC_SEE_Displacement : return "<"; /* Trance-journey displacement, correspondes to < (less-than). */
        case CC_SEE_EnPassant : return ":"; /* En passant, corresponds to : (colon). */
        case CC_SEE_Castle : return "&"; /* Castling, corresponds to & (ampersand). */
        case CC_SEE_Promotion : return "="; /* Promotion, corresponds to = (equal sign), optional. */
        case CC_SEE_TagForPromotion : return "="; /* Tag for promotion, corresponds to = (equal sign). */
        case CC_SEE_Conversion : return "%"; /* Conversion, corresponds to % (percent sign). */
        case CC_SEE_FailedConversion : return "%%"; /* Failed conversion, corresponds to %% (double percent sign). */
        case CC_SEE_DemoteToPawn : return ">"; /* Syzygy, demoting to Pawn, corresponds to > (greater-than sign). */
        case CC_SEE_Resurrection : return "$"; /* Syzygy, resurrection, corresponds to $ (dollar-sign). */
        case CC_SEE_FailedResurrection : return "$$"; /* Syzygy, failed resurrection, corresponds to $$ (dual dollar-sign). */

        default : return "?";
    }
}


CcSideEffect cc_side_effect( CcSideEffectEnum type,
                             CcPieceEnum piece,
                             CcTagEnum lost_tag,
                             int start_i, int start_j,
                             int dest_i, int dest_j )
{
    CcSideEffect sse = { .type = type, };

    // Nothing more to do if type == CC_SEE_None.
    if ( sse.type == CC_SEE_Capture )
    {
        sse.capture.piece = piece;
        sse.capture.lost_tag = lost_tag;
    }
    else if ( sse.type == CC_SEE_Displacement )
    {
        sse.displacement.piece = piece;
        sse.displacement.lost_tag = lost_tag;
        sse.displacement.dest_i = dest_i;
        sse.displacement.dest_j = dest_j;
    }
    else if ( sse.type == CC_SEE_EnPassant )
    {
        sse.en_passant.pawn = piece;
        sse.en_passant.dest_i = dest_i;
        sse.en_passant.dest_j = dest_j;
    }
    else if ( sse.type == CC_SEE_Castle )
    {
        sse.castle.rook = piece;
        sse.castle.start_i = start_i;
        sse.castle.start_j = start_j;
        sse.castle.dest_i = dest_i;
        sse.castle.dest_j = dest_j;
    }
    else if ( sse.type == CC_SEE_Promotion )
    {
        sse.promote.piece = piece;
    }
    // Nothing more to do if type == CC_SEE_TagForPromotion.
    else if ( sse.type == CC_SEE_Conversion )
    {
        sse.convert.piece = piece;
        sse.convert.lost_tag = lost_tag;
    }
    // Nothing more to do if type == CC_SEE_FailedConversion.
    else if ( sse.type == CC_SEE_DemoteToPawn )
    {
        sse.demote.piece = piece;
        sse.demote.dest_i = dest_i;
        sse.demote.dest_j = dest_j;
    }
    else if ( sse.type == CC_SEE_Resurrection )
    {
        sse.resurrect.piece = piece;
        sse.resurrect.dest_i = dest_i;
        sse.resurrect.dest_j = dest_j;
    }
    // Nothing more to do if type == CC_SEE_FailedResurrection.

    return sse;
}

bool cc_side_effect_is_valid( CcSideEffect see, unsigned int board_size )
{
    switch ( see.type )
    {
        case CC_SEE_None :
        case CC_SEE_TagForPromotion :
        case CC_SEE_FailedConversion :
        case CC_SEE_FailedResurrection :
            return true;

        case CC_SEE_Capture :
            return CC_PIECE_CAN_BE_CAPTURED( see.capture.piece ) &&
                   CC_TAG_CAN_BE_LOST( see.capture.lost_tag );

        case CC_SEE_Displacement :
            return CC_PIECE_CAN_BE_DISPLACED( see.displacement.piece ) &&
                   CC_IS_COORD_2_ON_BOARD( board_size,
                                           see.displacement.dest_i,
                                           see.displacement.dest_j ) &&
                   CC_TAG_CAN_BE_LOST( see.displacement.lost_tag );

        case CC_SEE_EnPassant :
            return CC_PIECE_IS_PAWN( see.en_passant.pawn ) &&
                   CC_IS_COORD_2_ON_BOARD( board_size,
                                           see.en_passant.dest_i,
                                           see.en_passant.dest_j );

        case CC_SEE_Castle :
            return CC_PIECE_IS_ROOK( see.castle.rook ) &&
                   CC_IS_COORD_2_ON_BOARD( board_size,
                                           see.castle.start_i,
                                           see.castle.start_j ) &&
                   CC_IS_COORD_2_ON_BOARD( board_size,
                                           see.castle.dest_i,
                                           see.castle.dest_j );

        case CC_SEE_Promotion :
            return CC_PAWN_CAN_BE_PROMOTED_TO( see.promote.piece );

        case CC_SEE_Conversion :
            return CC_PIECE_CAN_BE_CONVERTED( see.convert.piece ) &&
                   CC_TAG_CAN_BE_LOST( see.convert.lost_tag );

        case CC_SEE_DemoteToPawn :
            return CC_PIECE_CAN_BE_DEMOTED( see.demote.piece ) &&
                   CC_TAG_CAN_BE_LOST( see.demote.lost_tag ) &&
                   CC_IS_COORD_2_ON_BOARD( board_size,
                                           see.demote.dest_i,
                                           see.demote.dest_j );

        case CC_SEE_Resurrection :
            return CC_PIECE_CAN_BE_RESURRECTED( see.resurrect.piece ) &&
                   CC_IS_COORD_2_ON_BOARD( board_size,
                                           see.resurrect.dest_i,
                                           see.resurrect.dest_j );

        default :
            return false;
    }
}

CcPieceEnum cc_side_effect_piece( CcSideEffect se )
{
    switch ( se.type )
    {
        case CC_SEE_None : return CC_PE_None;
        case CC_SEE_Capture : return se.capture.piece;
        case CC_SEE_Displacement : return se.displacement.piece;
        case CC_SEE_EnPassant : return se.en_passant.pawn;
        case CC_SEE_Castle : return se.castle.rook;
        case CC_SEE_Promotion : return se.promote.piece;
        case CC_SEE_TagForPromotion : return CC_PE_None;
        case CC_SEE_Conversion : return se.convert.piece;
        case CC_SEE_FailedConversion : return CC_PE_None;
        case CC_SEE_DemoteToPawn : return se.demote.piece;
        case CC_SEE_Resurrection : return se.resurrect.piece;
        case CC_SEE_FailedResurrection : return CC_PE_None;

        default : return CC_PE_None;
    }
}

CcPos cc_side_effect_destination( CcSideEffect se )
{
    switch ( se.type )
    {
        case CC_SEE_None : return CC_POS_CAST_INVALID;
        case CC_SEE_Capture : return CC_POS_CAST_INVALID;
        case CC_SEE_Displacement : return cc_pos( se.displacement.dest_i, se.displacement.dest_j );
        case CC_SEE_EnPassant : return cc_pos( se.en_passant.dest_i, se.en_passant.dest_j );
        case CC_SEE_Castle : return cc_pos( se.castle.dest_i, se.castle.dest_j );
        case CC_SEE_Promotion : return CC_POS_CAST_INVALID;
        case CC_SEE_TagForPromotion : return CC_POS_CAST_INVALID;
        case CC_SEE_Conversion : return CC_POS_CAST_INVALID;
        case CC_SEE_FailedConversion : return CC_POS_CAST_INVALID;
        case CC_SEE_DemoteToPawn : return cc_pos( se.demote.dest_i, se.demote.dest_j );
        case CC_SEE_Resurrection : return cc_pos( se.resurrect.dest_i, se.resurrect.dest_j );
        case CC_SEE_FailedResurrection : return CC_POS_CAST_INVALID;

        default : return CC_POS_CAST_INVALID;
    }
}

//
// conveniances

CcSideEffect cc_side_effect_none()
{
    return cc_side_effect( CC_SEE_None, CC_PE_None, CC_TE_None,
                           CC_INVALID_COORD,
                           CC_INVALID_COORD,
                           CC_INVALID_COORD,
                           CC_INVALID_COORD );
}

CcSideEffect cc_side_effect_capture( CcPieceEnum piece, CcTagEnum lost_tag )
{
    return cc_side_effect( CC_SEE_Capture, piece, lost_tag,
                           CC_INVALID_COORD,
                           CC_INVALID_COORD,
                           CC_INVALID_COORD,
                           CC_INVALID_COORD );
}

CcSideEffect cc_side_effect_displacement( CcPieceEnum piece, CcTagEnum lost_tag, int dest_i, int dest_j )
{
    return cc_side_effect( CC_SEE_Displacement, piece, lost_tag,
                           CC_INVALID_COORD,
                           CC_INVALID_COORD,
                           dest_i,
                           dest_j );
}

CcSideEffect cc_side_effect_en_passant( CcPieceEnum piece, int dest_i, int dest_j )
{
    return cc_side_effect( CC_SEE_EnPassant, piece, CC_TE_None,
                           CC_INVALID_COORD,
                           CC_INVALID_COORD,
                           dest_i,
                           dest_j );
}

CcSideEffect cc_side_effect_castle( CcPieceEnum rook, int start_i, int start_j, int dest_i, int dest_j )
{
    return cc_side_effect( CC_SEE_Castle, rook, CC_TE_None, start_i, start_j, dest_i, dest_j );
}

CcSideEffect cc_side_effect_promote( CcPieceEnum piece )
{
    return cc_side_effect( CC_SEE_Promotion, piece, CC_TE_None,
                           CC_INVALID_COORD,
                           CC_INVALID_COORD,
                           CC_INVALID_COORD,
                           CC_INVALID_COORD );
}

CcSideEffect cc_side_effect_tag_for_promotion()
{
    return cc_side_effect( CC_SEE_TagForPromotion, CC_PE_None, CC_TE_None,
                           CC_INVALID_COORD,
                           CC_INVALID_COORD,
                           CC_INVALID_COORD,
                           CC_INVALID_COORD );
}

CcSideEffect cc_side_effect_convert( CcPieceEnum piece, CcTagEnum lost_tag )
{
    return cc_side_effect( CC_SEE_Conversion, piece, lost_tag,
                           CC_INVALID_COORD,
                           CC_INVALID_COORD,
                           CC_INVALID_COORD,
                           CC_INVALID_COORD );
}

CcSideEffect cc_side_effect_failed_conversion()
{
    return cc_side_effect( CC_SEE_FailedConversion, CC_PE_None, CC_TE_None,
                           CC_INVALID_COORD,
                           CC_INVALID_COORD,
                           CC_INVALID_COORD,
                           CC_INVALID_COORD );
}

CcSideEffect cc_side_effect_demote( CcPieceEnum piece, CcTagEnum lost_tag, int dest_i, int dest_j )
{
    return cc_side_effect( CC_SEE_DemoteToPawn, piece, lost_tag,
                           CC_INVALID_COORD,
                           CC_INVALID_COORD,
                           dest_i,
                           dest_j );
}

CcSideEffect cc_side_effect_resurrect( CcPieceEnum piece, int dest_i, int dest_j )
{
    return cc_side_effect( CC_SEE_Resurrection, piece, CC_TE_None,
                           CC_INVALID_COORD,
                           CC_INVALID_COORD,
                           dest_i,
                           dest_j );
}

CcSideEffect cc_side_effect_failed_resurrection()
{
    return cc_side_effect( CC_SEE_FailedResurrection, CC_PE_None, CC_TE_None,
                           CC_INVALID_COORD,
                           CC_INVALID_COORD,
                           CC_INVALID_COORD,
                           CC_INVALID_COORD );
}


bool cc_side_effect_to_short_str( CcSideEffect se,
                                  cc_char_16 * restrict se_str__o )
{
    if ( !cc_str_clear( *se_str__o, CC_MAX_LEN_CHAR_16 ) )
        return false;

    if ( se.type == CC_SEE_None )
        return true;

    char * se_end = (char *)(se_str__o);

    char const * see_str = cc_side_effect_symbol( se.type );
    se_end += cc_str_copy( see_str, NULL, 2, *se_str__o, CC_MAX_LEN_CHAR_16 );

    CcPieceEnum pe = cc_side_effect_piece( se );
    char piece = cc_piece_symbol( pe );
    *se_end++ = piece;

    CcPos destination = cc_side_effect_destination( se );
    cc_char_8 pos_c8 = CC_CHAR_8_EMPTY;
    if ( !cc_pos_to_short_string( destination, &pos_c8 ) )
        return false;

    size_t unused = CC_MAX_LEN_CHAR_16 - ( se_end - (char *)(se_str__o) );
    /* se_end += */ cc_str_copy( pos_c8, NULL, CC_MAX_LEN_CHAR_8, se_end, unused );

    return true;
}
