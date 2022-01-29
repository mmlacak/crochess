// Copyright (c) 2021 Mario Mlačak, mmlacak@gmail.com
// Licensed under GNU GPL v3+ license. See LICENSING, COPYING files for details.

#include <stdlib.h>

#include "cc_defines.h"
#include "cc_step.h"

/**
    @file cc_side_effect.c
    @brief Side-effects related functions.
*/


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
        sse.en_passant.piece = piece;
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
    else if ( sse.type == CC_SEE_Demotion )
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

CcSideEffect cc_side_effect_none()
{
    return cc_side_effect( CC_SEE_None, CC_PE_None, CC_TE_None,
                           CC_INVALID_OFF_BOARD_COORD_MIN,
                           CC_INVALID_OFF_BOARD_COORD_MIN,
                           CC_INVALID_OFF_BOARD_COORD_MIN,
                           CC_INVALID_OFF_BOARD_COORD_MIN );
}

CcSideEffect cc_side_effect_capture( CcPieceEnum piece, CcTagEnum lost_tag )
{
    return cc_side_effect( CC_SEE_Capture, piece, lost_tag,
                           CC_INVALID_OFF_BOARD_COORD_MIN,
                           CC_INVALID_OFF_BOARD_COORD_MIN,
                           CC_INVALID_OFF_BOARD_COORD_MIN,
                           CC_INVALID_OFF_BOARD_COORD_MIN );
}

CcSideEffect cc_side_effect_displacement( CcPieceEnum piece, CcTagEnum lost_tag, int dest_i, int dest_j )
{
    return cc_side_effect( CC_SEE_Displacement, piece, lost_tag,
                           CC_INVALID_OFF_BOARD_COORD_MIN,
                           CC_INVALID_OFF_BOARD_COORD_MIN,
                           dest_i,
                           dest_j );
}

CcSideEffect cc_side_effect_en_passant( CcPieceEnum piece, int dest_i, int dest_j )
{
    return cc_side_effect( CC_SEE_EnPassant, piece, CC_TE_None,
                           CC_INVALID_OFF_BOARD_COORD_MIN,
                           CC_INVALID_OFF_BOARD_COORD_MIN,
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
                           CC_INVALID_OFF_BOARD_COORD_MIN,
                           CC_INVALID_OFF_BOARD_COORD_MIN,
                           CC_INVALID_OFF_BOARD_COORD_MIN,
                           CC_INVALID_OFF_BOARD_COORD_MIN );
}

CcSideEffect cc_side_effect_tag_for_promotion()
{
    return cc_side_effect( CC_SEE_TagForPromotion, CC_PE_None, CC_TE_None,
                           CC_INVALID_OFF_BOARD_COORD_MIN,
                           CC_INVALID_OFF_BOARD_COORD_MIN,
                           CC_INVALID_OFF_BOARD_COORD_MIN,
                           CC_INVALID_OFF_BOARD_COORD_MIN );
}

CcSideEffect cc_side_effect_convert( CcPieceEnum piece, CcTagEnum lost_tag )
{
    return cc_side_effect( CC_SEE_Conversion, piece, lost_tag,
                           CC_INVALID_OFF_BOARD_COORD_MIN,
                           CC_INVALID_OFF_BOARD_COORD_MIN,
                           CC_INVALID_OFF_BOARD_COORD_MIN,
                           CC_INVALID_OFF_BOARD_COORD_MIN );
}

CcSideEffect cc_side_effect_failed_conversion()
{
    return cc_side_effect( CC_SEE_FailedConversion, CC_PE_None, CC_TE_None,
                           CC_INVALID_OFF_BOARD_COORD_MIN,
                           CC_INVALID_OFF_BOARD_COORD_MIN,
                           CC_INVALID_OFF_BOARD_COORD_MIN,
                           CC_INVALID_OFF_BOARD_COORD_MIN );
}

CcSideEffect cc_side_effect_demote( CcPieceEnum piece, CcTagEnum lost_tag, int dest_i, int dest_j )
{
    return cc_side_effect( CC_SEE_Demotion, piece, lost_tag,
                           CC_INVALID_OFF_BOARD_COORD_MIN,
                           CC_INVALID_OFF_BOARD_COORD_MIN,
                           dest_i,
                           dest_j );
}

CcSideEffect cc_side_effect_resurrect( CcPieceEnum piece, int dest_i, int dest_j )
{
    return cc_side_effect( CC_SEE_Resurrection, piece, CC_TE_None,
                           CC_INVALID_OFF_BOARD_COORD_MIN,
                           CC_INVALID_OFF_BOARD_COORD_MIN,
                           dest_i,
                           dest_j );
}

CcSideEffect cc_side_effect_failed_resurrection()
{
    return cc_side_effect( CC_SEE_FailedResurrection, CC_PE_None, CC_TE_None,
                           CC_INVALID_OFF_BOARD_COORD_MIN,
                           CC_INVALID_OFF_BOARD_COORD_MIN,
                           CC_INVALID_OFF_BOARD_COORD_MIN,
                           CC_INVALID_OFF_BOARD_COORD_MIN );
}
