// Copyright (c) 2021 Mario Mlačak, mmlacak@gmail.com
// Licensed under GNU GPL v3+ license. See LICENSING, COPYING files for details.

#include <stdlib.h>

#include "cc_defines.h"
#include "cc_step.h"

/**
    @file cc_step.c
    @brief Step related functions.
*/


char * cc_step_link_symbol( CcStepLinkEnum sle )
{
    switch ( sle )
    {
        case CC_SLE_Start : return "";
        case CC_SLE_Reposition : return ",";
        case CC_SLE_Next : return ".";
        case CC_SLE_Distant : return "..";
        case CC_SLE_Destination : return "-";

        default : return NULL; // gcc complains without default.
    }
}


bool cc_side_effect_enum_is_castling( CcSideEffectEnum see )
{
    return ( see == CC_SEE_Castle );
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


CcStep * cc_step_new( CcStepLinkEnum link,
                      int i, int j, CcSideEffect side_effect,
                      CcFormatStepUsageEnum usage )
{
    CcStep * step__t = malloc( sizeof( CcStep ) );
    if ( !step__t ) return NULL;

    step__t->link = link;
    step__t->i = i;
    step__t->j = j;
    step__t->side_effect = side_effect;
    step__t->usage = usage;
    step__t->next = NULL;

    return step__t;
}

CcStep * cc_step_append( CcStep * restrict steps__io,
                         CcStepLinkEnum link, int i, int j, CcSideEffect side_effect,
                         CcFormatStepUsageEnum usage )
{
    if ( !steps__io ) return NULL;

    CcStep * new = cc_step_new( link, i, j, side_effect, usage );
    if ( !new ) return NULL;

    CcStep * s = steps__io;
    while ( s->next ) s = s->next; // rewind
    s->next = new; // append

    return new;
}

CcStep * cc_step_append_or_init( CcStep ** restrict steps__io,
                                 CcStepLinkEnum link, int i, int j, CcSideEffect side_effect,
                                 CcFormatStepUsageEnum usage )
{
    if ( !steps__io ) return NULL;

    CcStep * new = NULL;

    if ( !*steps__io )
        *steps__io = new = cc_step_new( link, i, j, side_effect, usage );
    else
        new = cc_step_append( *steps__io, link, i, j, side_effect, usage );

    return new;
}

CcStep * cc_step_duplicate_all_new( CcStep * restrict steps__io )
{
    if ( !steps__io ) return NULL;

    CcStep * new__o = cc_step_new( steps__io->link, steps__io->i, steps__io->j, steps__io->side_effect, steps__io->usage );
    if ( !new__o ) return NULL;

    CcStep * from = steps__io->next;

    while ( from )
    {
        CcStep * n__w = cc_step_append( new__o, from->link, from->i, from->j, from->side_effect, from->usage );
        if ( !n__w )
        {
            cc_step_free_all_steps( &new__o );
            return NULL;
        }

        from = from->next;
    }

    return new__o;
}

bool cc_step_free_all_steps( CcStep ** restrict steps__f )
{
    if ( !steps__f ) return false;
    if ( !*steps__f ) return true;

    CcStep * s = *steps__f;

    while ( s )
    {
        CcStep * tmp = s->next;
        CC_FREE( s );
        s = tmp;
    }

    *steps__f = NULL;
    return true;
}


// new

CcStep * cc_step_none_new( CcStepLinkEnum link, int i, int j,
                           CcFormatStepUsageEnum usage )
{
    CcSideEffect se = cc_side_effect_none();
    return cc_step_new( link, i, j, se, usage );
}

CcStep * cc_step_capture_new( CcStepLinkEnum link, int i, int j,
                              CcPieceEnum piece, CcTagEnum lost_tag,
                              CcFormatStepUsageEnum usage )
{
    CcSideEffect se = cc_side_effect_capture( piece, lost_tag );
    return cc_step_new( link, i, j, se, usage );
}

CcStep * cc_step_displacement_new( CcStepLinkEnum link, int i, int j,
                                   CcPieceEnum piece, CcTagEnum lost_tag, int dest_i, int dest_j,
                                   CcFormatStepUsageEnum usage )
{
    CcSideEffect se = cc_side_effect_displacement( piece, lost_tag, dest_i, dest_j );
    return cc_step_new( link, i, j, se, usage );
}

CcStep * cc_step_en_passant_new( CcStepLinkEnum link, int i, int j,
                                 CcPieceEnum piece, int dest_i, int dest_j,
                                 CcFormatStepUsageEnum usage )
{
    CcSideEffect se = cc_side_effect_en_passant( piece, dest_i, dest_j );
    return cc_step_new( link, i, j, se, usage );
}

CcStep * cc_step_castle_new( CcStepLinkEnum link, int i, int j,
                             CcPieceEnum rook, int start_i, int start_j, int dest_i, int dest_j,
                             CcFormatStepUsageEnum usage )
{
    CcSideEffect se = cc_side_effect_castle( rook, start_i, start_j, dest_i, dest_j );
    return cc_step_new( link, i, j, se, usage );
}

CcStep * cc_step_promote_new( CcStepLinkEnum link, int i, int j,
                              CcPieceEnum piece,
                              CcFormatStepUsageEnum usage )
{
    CcSideEffect se = cc_side_effect_promote( piece );
    return cc_step_new( link, i, j, se, usage );
}

CcStep * cc_step_tag_for_promotion_new( CcStepLinkEnum link, int i, int j,
                                        CcFormatStepUsageEnum usage )
{
    CcSideEffect se = cc_side_effect_tag_for_promotion();
    return cc_step_new( link, i, j, se, usage );
}

CcStep * cc_step_convert_new( CcStepLinkEnum link, int i, int j,
                              CcPieceEnum piece, CcTagEnum lost_tag,
                              CcFormatStepUsageEnum usage )
{
    CcSideEffect se = cc_side_effect_convert( piece, lost_tag );
    return cc_step_new( link, i, j, se, usage );
}

CcStep * cc_step_failed_conversion_new( CcStepLinkEnum link, int i, int j,
                                        CcFormatStepUsageEnum usage )
{
    CcSideEffect se = cc_side_effect_failed_conversion();
    return cc_step_new( link, i, j, se, usage );
}

CcStep * cc_step_demote_new( CcStepLinkEnum link, int i, int j,
                             CcPieceEnum piece, CcTagEnum lost_tag, int dest_i, int dest_j,
                             CcFormatStepUsageEnum usage )
{
    CcSideEffect se = cc_side_effect_demote( piece, lost_tag, dest_i, dest_j );
    return cc_step_new( link, i, j, se, usage );
}

CcStep * cc_step_resurrect_new( CcStepLinkEnum link, int i, int j,
                                CcPieceEnum piece, int dest_i, int dest_j,
                                CcFormatStepUsageEnum usage )
{
    CcSideEffect se = cc_side_effect_resurrect( piece, dest_i, dest_j );
    return cc_step_new( link, i, j, se, usage );
}

CcStep * cc_step_failed_resurrection_new( CcStepLinkEnum link, int i, int j,
                                          CcFormatStepUsageEnum usage )
{
    CcSideEffect se = cc_side_effect_failed_resurrection();
    return cc_step_new( link, i, j, se, usage );
}


// append

CcStep * cc_step_none_append( CcStep * restrict steps__io,
                              CcStepLinkEnum link, int i, int j,
                              CcFormatStepUsageEnum usage )
{
    CcSideEffect se = cc_side_effect_none();
    return cc_step_append( steps__io, link, i, j, se, usage );
}

CcStep * cc_step_capture_append( CcStep * restrict steps__io,
                                 CcStepLinkEnum link, int i, int j,
                                 CcPieceEnum piece, CcTagEnum lost_tag,
                                 CcFormatStepUsageEnum usage )
{
    CcSideEffect se = cc_side_effect_capture( piece, lost_tag );
    return cc_step_append( steps__io, link, i, j, se, usage );
}

CcStep * cc_step_displacement_append( CcStep * restrict steps__io,
                                      CcStepLinkEnum link, int i, int j,
                                      CcPieceEnum piece, CcTagEnum lost_tag, int dest_i, int dest_j,
                                      CcFormatStepUsageEnum usage )
{
    CcSideEffect se = cc_side_effect_displacement( piece, lost_tag, dest_i, dest_j );
    return cc_step_append( steps__io, link, i, j, se, usage );
}

CcStep * cc_step_en_passant_append( CcStep * restrict steps__io,
                                    CcStepLinkEnum link, int i, int j,
                                    CcPieceEnum piece, int dest_i, int dest_j,
                                    CcFormatStepUsageEnum usage )
{
    CcSideEffect se = cc_side_effect_en_passant( piece, dest_i, dest_j );
    return cc_step_append( steps__io, link, i, j, se, usage );
}

CcStep * cc_step_castle_append( CcStep * restrict steps__io,
                                CcStepLinkEnum link, int i, int j,
                                CcPieceEnum rook, int start_i, int start_j, int dest_i, int dest_j,
                                CcFormatStepUsageEnum usage )
{
    CcSideEffect se = cc_side_effect_castle( rook, start_i, start_j, dest_i, dest_j );
    return cc_step_append( steps__io, link, i, j, se, usage );
}

CcStep * cc_step_promote_append( CcStep * restrict steps__io,
                                 CcStepLinkEnum link, int i, int j,
                                 CcPieceEnum piece,
                                 CcFormatStepUsageEnum usage )
{
    CcSideEffect se = cc_side_effect_promote( piece );
    return cc_step_append( steps__io, link, i, j, se, usage );
}

CcStep * cc_step_tag_for_promotion_append( CcStep * restrict steps__io,
                                           CcStepLinkEnum link, int i, int j,
                                           CcFormatStepUsageEnum usage )
{
    CcSideEffect se = cc_side_effect_tag_for_promotion();
    return cc_step_append( steps__io, link, i, j, se, usage );
}

CcStep * cc_step_convert_append( CcStep * restrict steps__io,
                                 CcStepLinkEnum link, int i, int j,
                                 CcPieceEnum piece, CcTagEnum lost_tag,
                                 CcFormatStepUsageEnum usage )
{
    CcSideEffect se = cc_side_effect_convert( piece, lost_tag );
    return cc_step_append( steps__io, link, i, j, se, usage );
}

CcStep * cc_step_failed_conversion_append( CcStep * restrict steps__io,
                                           CcStepLinkEnum link, int i, int j,
                                           CcFormatStepUsageEnum usage )
{
    CcSideEffect se = cc_side_effect_failed_conversion();
    return cc_step_append( steps__io, link, i, j, se, usage );
}

CcStep * cc_step_demote_append( CcStep * restrict steps__io,
                                CcStepLinkEnum link, int i, int j,
                                CcPieceEnum piece, CcTagEnum lost_tag, int dest_i, int dest_j,
                                CcFormatStepUsageEnum usage )
{
    CcSideEffect se = cc_side_effect_demote( piece, lost_tag, dest_i, dest_j );
    return cc_step_append( steps__io, link, i, j, se, usage );
}

CcStep * cc_step_resurrect_append( CcStep * restrict steps__io,
                                   CcStepLinkEnum link, int i, int j,
                                   CcPieceEnum piece, int dest_i, int dest_j,
                                   CcFormatStepUsageEnum usage )
{
    CcSideEffect se = cc_side_effect_resurrect( piece, dest_i, dest_j );
    return cc_step_append( steps__io, link, i, j, se, usage );
}

CcStep * cc_step_failed_resurrection_append( CcStep * restrict steps__io,
                                             CcStepLinkEnum link, int i, int j,
                                             CcFormatStepUsageEnum usage )
{
    CcSideEffect se = cc_side_effect_failed_resurrection();
    return cc_step_append( steps__io, link, i, j, se, usage );
}


size_t cc_step_count_usage( CcStep * restrict steps,
                            CcFormatStepUsageEnum usage )
{
    if ( !steps ) return 0;

    size_t count = 0;
    CcStep * s = steps;

    while ( s )
    {
        if ( s->usage <= usage ) ++count;
        s = s->next;
    }

    return count;
}
