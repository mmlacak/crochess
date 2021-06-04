// Copyright (c) 2021 Mario Mlačak, mmlacak@gmail.com
// Licensed under 3-clause (modified) BSD license. See LICENSE for details.

#include <stdlib.h>

#include "cc_defines.h"
#include "cc_ply.h"


CcPlySideEffect ply_side_effect( CcPlySideEffectEnum type, CcPieceEnum piece, bool is_promo_tag_lost, int start_i, int start_j, int dest_i, int dest_j )
{
    CcPlySideEffect pse = { .type = type, };

    // Nothing more to do if type == CC_PSEE_None.
    if ( pse.type == CC_PSEE_Capture )
    {
        pse.capture.piece = piece;
        pse.capture.is_promo_tag_lost = is_promo_tag_lost;
    }
    else if ( pse.type == CC_PSEE_EnPassant )
    {
        pse.en_passant.dest_i = dest_i;
        pse.en_passant.dest_j = dest_j;
    }
    else if ( pse.type == CC_PSEE_Castle )
    {
        pse.castle.rook = piece;
        pse.castle.start_i = start_i;
        pse.castle.start_j = start_j;
        pse.castle.dest_i = dest_i;
        pse.castle.dest_j = dest_j;
    }
    else if ( pse.type == CC_PSEE_Promotion )
    {
        pse.promote.piece = piece;
    }
    // Nothing more to do if type == CC_PSEE_TagForPromotion.
    else if ( pse.type == CC_PSEE_Conversion )
    {
        pse.convert.piece = piece;
        pse.convert.is_promo_tag_lost = is_promo_tag_lost;
    }
    // Nothing more to do if type == CC_PSEE_FailedConversion.
    else if ( pse.type == CC_PSEE_Demotion )
    {
        pse.demote.piece = piece;
        pse.demote.dest_i = dest_i;
        pse.demote.dest_j = dest_j;
    }
    else if ( pse.type == CC_PSEE_Resurrection )
    {
        pse.resurrect.piece = piece;
        pse.resurrect.dest_i = dest_i;
        pse.resurrect.dest_j = dest_j;
    }
    // Nothing more to do if type == CC_PSEE_FailedResurrection.

    return pse;
}

CcPlySideEffect cc_ply_side_effect_none()
{
    return ply_side_effect( CC_PSEE_None, CC_PE_None, false, CC_OFF_BOARD_COORD, CC_OFF_BOARD_COORD, CC_OFF_BOARD_COORD, CC_OFF_BOARD_COORD );
}

CcPlySideEffect cc_ply_side_effect_capture( CcPieceEnum piece, bool is_promo_tag_lost )
{
    return ply_side_effect( CC_PSEE_Capture, piece, is_promo_tag_lost, CC_OFF_BOARD_COORD, CC_OFF_BOARD_COORD, CC_OFF_BOARD_COORD, CC_OFF_BOARD_COORD );
}

CcPlySideEffect cc_ply_side_effect_en_passant( int dest_i, int dest_j )
{
    return ply_side_effect( CC_PSEE_EnPassant, CC_PE_None, false, CC_OFF_BOARD_COORD, CC_OFF_BOARD_COORD, dest_i, dest_j );
}

CcPlySideEffect cc_ply_side_effect_castle( CcPieceEnum rook, int start_i, int start_j, int dest_i, int dest_j )
{
    return ply_side_effect( CC_PSEE_Castle, rook, false, start_i, start_j, dest_i, dest_j );
}

CcPlySideEffect cc_ply_side_effect_promote( CcPieceEnum piece )
{
    return ply_side_effect( CC_PSEE_Promotion, piece, false, CC_OFF_BOARD_COORD, CC_OFF_BOARD_COORD, CC_OFF_BOARD_COORD, CC_OFF_BOARD_COORD );
}

CcPlySideEffect cc_ply_side_effect_tag_for_promotion()
{
    return ply_side_effect( CC_PSEE_TagForPromotion, CC_PE_None, false, CC_OFF_BOARD_COORD, CC_OFF_BOARD_COORD, CC_OFF_BOARD_COORD, CC_OFF_BOARD_COORD );
}

CcPlySideEffect cc_ply_side_effect_convert( CcPieceEnum piece, bool is_promo_tag_lost )
{
    return ply_side_effect( CC_PSEE_Conversion, piece, is_promo_tag_lost, CC_OFF_BOARD_COORD, CC_OFF_BOARD_COORD, CC_OFF_BOARD_COORD, CC_OFF_BOARD_COORD );
}

CcPlySideEffect cc_ply_side_effect_failed_conversion()
{
    return ply_side_effect( CC_PSEE_FailedConversion, CC_PE_None, false, CC_OFF_BOARD_COORD, CC_OFF_BOARD_COORD, CC_OFF_BOARD_COORD, CC_OFF_BOARD_COORD );
}

CcPlySideEffect cc_ply_side_effect_demote( CcPieceEnum piece, int dest_i, int dest_j )
{
    return ply_side_effect( CC_PSEE_Demotion, piece, false, CC_OFF_BOARD_COORD, CC_OFF_BOARD_COORD, dest_i, dest_j );
}

CcPlySideEffect cc_ply_side_effect_resurrect( CcPieceEnum piece, int dest_i, int dest_j )
{
    return ply_side_effect( CC_PSEE_Resurrection, piece, false, CC_OFF_BOARD_COORD, CC_OFF_BOARD_COORD, dest_i, dest_j );
}

CcPlySideEffect cc_ply_side_effect_failed_resurrection()
{
    return ply_side_effect( CC_PSEE_FailedResurrection, CC_PE_None, false, CC_OFF_BOARD_COORD, CC_OFF_BOARD_COORD, CC_OFF_BOARD_COORD, CC_OFF_BOARD_COORD );
}


CcPieceField * cc_ply_piece_field_new( CcPieceEnum piece, int i, int j )
{
    CcPieceField * pf = malloc( sizeof( CcPieceField ) );
    if ( !pf ) return NULL;

    pf->piece = piece;
    pf->i = i;
    pf->j = j;
    pf->next = NULL;

    return pf;
}

CcPieceField * cc_ply_append_piece_field_new( CcPieceField * const restrict piece_fields, CcPieceEnum piece, int i, int j )
{
    CcPieceField * new = cc_ply_piece_field_new( piece, i, j );
    if ( !new ) return NULL;
    if ( !piece_fields ) return new;

    CcPieceField * pf = piece_fields;
    while ( pf->next ) pf = pf->next; // rewind
    pf->next = new; // append

    return new;
}

bool cc_ply_free_all_piece_fields( CcPieceField ** const piece_fields )
{
    if ( !piece_fields ) return true;
    if ( !*piece_fields ) return false;

    CcPieceField * pf = *piece_fields;

    while ( pf )
    {
        CcPieceField * tmp = pf->next;
        free( pf );
        pf = tmp;
    }

    *piece_fields = NULL;
    return true;
}


CcPly * cc_ply_new(  CcPlyLinkEnum link, CcPieceEnum piece,
                    CcStep * const restrict steps, int i, int j,
                    CcSideEffectStep * const restrict side_effect_steps,
                    CcPieceField * const restrict captured,
                    CcPlySideEffect side_effect )
{
    CcPly * ply = calloc( 1, sizeof( CcPly ) );
    if ( !ply ) return NULL;

    ply->link = link;
    ply->piece = piece;

    if ( ply->link == CC_PLE_Ply )
    {
        ply->ply.steps = steps;
    }
    else if ( ply->link == CC_PLE_Teleportation )
    {
        ply->teleport.i = i;
        ply->teleport.j = j;
    }
    else if ( ply->link == CC_PLE_TeleportationWave )
    {
        ply->teleport_wave.steps = steps;
    }
    // Nothing additional to do if link == CC_PLE_FailedTeleportationOblation.
    else if ( ply->link == CC_PLE_FailedTeleportation )
    {
        ply->failed_teleport.i = i;
        ply->failed_teleport.j = j;
    }
    else if ( ply->link == CC_PLE_TranceJourney )
    {
        ply->trance_journey.i = i;
        ply->trance_journey.j = j;
        ply->trance_journey.steps = side_effect_steps;
    }
    else if ( ply->link == CC_PLE_DualTranceJourney )
    {
        ply->dual_trance_journey.captured = captured;
    }
    // Nothing additional to do if link == CC_PLE_FailedTranceJourney.
    else if ( ply->link == CC_PLE_PawnSacrifice )
    {
        ply->pawn_sacrifice.steps = side_effect_steps;
    }

    ply->side_effect = side_effect;
    ply->next = NULL;

    return ply;
}

CcPly * cc_ply_append_new(   CcPly * const restrict plies,
                        CcPlyLinkEnum link, CcPieceEnum piece,
                        CcStep * const restrict steps, int i, int j,
                        CcSideEffectStep * const restrict side_effect_steps,
                        CcPieceField * const restrict captured,
                        CcPlySideEffect side_effect )
{
    CcPly * new = cc_ply_new( link, piece, steps, i, j, side_effect_steps, captured, side_effect );
    if ( !new ) return NULL;
    if ( !plies ) return new;

    CcPly * p = plies;
    while ( p->next ) p = p->next; // rewind
    p->next = new; // append

    return new;
}

bool cc_ply_free_all_plies( CcPly ** const plies )
{
    if ( !plies ) return true;
    if ( !*plies ) return false;

    bool result = true;
    CcPly * ply = *plies;

    while ( ply )
    {
        switch ( ply->link )
        {
            case CC_PLE_Ply :
            {
                CcStep ** steps = &( ply->ply.steps );
                result = result && cc_step_free_all_steps( steps );
                break;
            }

            case CC_PLE_TeleportationWave :
            {
                CcStep ** steps = &( ply->teleport_wave.steps );
                result = result && cc_step_free_all_steps( steps );
                break;
            }

            case CC_PLE_TranceJourney :
            {
                CcSideEffectStep ** steps = &( ply->trance_journey.steps );
                result = result && cc_step_free_all_side_effect_steps( steps );
                break;
            }

            case CC_PLE_DualTranceJourney :
            {
                CcPieceField ** captured = &( ply->dual_trance_journey.captured );
                result = result && cc_ply_free_all_piece_fields( captured );
                break;
            }

            case CC_PLE_PawnSacrifice :
            {
                CcSideEffectStep ** steps = &( ply->pawn_sacrifice.steps );
                result = result && cc_step_free_all_side_effect_steps( steps );
                break;
            }

            case CC_PLE_Teleportation :
            case CC_PLE_FailedTeleportationOblation :
            case CC_PLE_FailedTeleportation :
            case CC_PLE_FailedTranceJourney :
                break;
        }

        CcPly * tmp = ply->next;
        free( ply );
        ply = tmp;
    }

    *plies = NULL;
    return result;
}


CcPly * cc_ply_new_ply_new( CcPieceEnum piece, CcStep * const restrict steps, CcPlySideEffect side_effect )
{
    return cc_ply_new( CC_PLE_Ply, piece, steps, CC_OFF_BOARD_COORD, CC_OFF_BOARD_COORD, NULL, NULL, side_effect );
}

CcPly * cc_ply_new_teleport_new( CcPieceEnum piece, int i, int j, CcPlySideEffect side_effect )
{
    return cc_ply_new( CC_PLE_Teleportation, piece, NULL, i, j, NULL, NULL, side_effect );
}

CcPly * cc_ply_new_teleport_wave_new( CcPieceEnum piece, CcStep * const restrict steps, CcPlySideEffect side_effect )
{
    return cc_ply_new( CC_PLE_TeleportationWave, piece, steps, CC_OFF_BOARD_COORD, CC_OFF_BOARD_COORD, NULL, NULL, side_effect );
}

CcPly * cc_ply_new_failed_teleport_oblation_new( CcPieceEnum piece, CcPlySideEffect side_effect )
{
    return cc_ply_new( CC_PLE_FailedTeleportationOblation, piece, NULL, CC_OFF_BOARD_COORD, CC_OFF_BOARD_COORD, NULL, NULL, side_effect );
}

CcPly * cc_ply_new_failed_teleport_new( CcPieceEnum piece, int i, int j, CcPlySideEffect side_effect )
{
    return cc_ply_new( CC_PLE_FailedTeleportation, piece, NULL, i, j, NULL, NULL, side_effect );
}

CcPly * cc_ply_new_trance_journey_new( CcPieceEnum piece, int i, int j, CcSideEffectStep * const restrict steps, CcPlySideEffect side_effect )
{
    return cc_ply_new( CC_PLE_TranceJourney, piece, NULL, i, j, steps, NULL, side_effect );
}

CcPly * cc_ply_new_dual_trance_journey_new( CcPieceField * const restrict captured, CcPlySideEffect side_effect )
{
    return cc_ply_new( CC_PLE_DualTranceJourney, CC_PE_None, NULL, CC_OFF_BOARD_COORD, CC_OFF_BOARD_COORD, NULL, captured, side_effect );
}

CcPly * cc_ply_new_failed_trance_journey_new( CcPieceEnum piece, CcPlySideEffect side_effect )
{
    return cc_ply_new( CC_PLE_FailedTranceJourney, piece, NULL, CC_OFF_BOARD_COORD, CC_OFF_BOARD_COORD, NULL, NULL, side_effect );
}

CcPly * cc_ply_new_pawn_sacrifice_new( CcPieceEnum piece, CcSideEffectStep * const restrict steps, CcPlySideEffect side_effect )
{
    return cc_ply_new( CC_PLE_PawnSacrifice, piece, NULL, CC_OFF_BOARD_COORD, CC_OFF_BOARD_COORD, steps, NULL, side_effect );
}


CcPly * cc_ply_append_ply_new( CcPly * const restrict plies, CcPieceEnum piece, CcStep * const restrict steps, CcPlySideEffect side_effect )
{
    return cc_ply_append_new( plies, CC_PLE_Ply, piece, steps, CC_OFF_BOARD_COORD, CC_OFF_BOARD_COORD, NULL, NULL, side_effect );
}

CcPly * cc_ply_append_teleport_new( CcPly * const restrict plies, CcPieceEnum piece, int i, int j, CcPlySideEffect side_effect )
{
    return cc_ply_append_new( plies, CC_PLE_Teleportation, piece, NULL, i, j, NULL, NULL, side_effect );
}

CcPly * cc_ply_append_teleport_wave_new( CcPly * const restrict plies, CcPieceEnum piece, CcStep * const restrict steps, CcPlySideEffect side_effect )
{
    return cc_ply_append_new( plies, CC_PLE_TeleportationWave, piece, steps, CC_OFF_BOARD_COORD, CC_OFF_BOARD_COORD, NULL, NULL, side_effect );
}

CcPly * cc_ply_append_failed_teleport_oblation_new( CcPly * const restrict plies, CcPieceEnum piece, CcPlySideEffect side_effect )
{
    return cc_ply_append_new( plies, CC_PLE_FailedTeleportationOblation, piece, NULL, CC_OFF_BOARD_COORD, CC_OFF_BOARD_COORD, NULL, NULL, side_effect );
}

CcPly * cc_ply_append_failed_teleport_new( CcPly * const restrict plies, CcPieceEnum piece, int i, int j, CcPlySideEffect side_effect )
{
    return cc_ply_append_new( plies, CC_PLE_FailedTeleportation, piece, NULL, i, j, NULL, NULL, side_effect );
}

CcPly * cc_ply_append_trance_journey_new( CcPly * const restrict plies, CcPieceEnum piece, int i, int j, CcSideEffectStep * const restrict steps, CcPlySideEffect side_effect )
{
    return cc_ply_append_new( plies, CC_PLE_TranceJourney, piece, NULL, i, j, steps, NULL, side_effect );
}

CcPly * cc_ply_append_dual_trance_journey_new( CcPly * const restrict plies, CcPieceField * const restrict captured, CcPlySideEffect side_effect )
{
    return cc_ply_append_new( plies, CC_PLE_DualTranceJourney, CC_PE_None, NULL, CC_OFF_BOARD_COORD, CC_OFF_BOARD_COORD, NULL, captured, side_effect );
}

CcPly * cc_ply_append_failed_trance_journey_new( CcPly * const restrict plies, CcPieceEnum piece, CcPlySideEffect side_effect )
{
    return cc_ply_append_new( plies, CC_PLE_FailedTranceJourney, piece, NULL, CC_OFF_BOARD_COORD, CC_OFF_BOARD_COORD, NULL, NULL, side_effect );
}

CcPly * cc_ply_append_pawn_sacrifice_new( CcPly * const restrict plies, CcPieceEnum piece, CcSideEffectStep * const restrict steps, CcPlySideEffect side_effect )
{
    return cc_ply_append_new( plies, CC_PLE_PawnSacrifice, piece, NULL, CC_OFF_BOARD_COORD, CC_OFF_BOARD_COORD, steps, NULL, side_effect );
}
