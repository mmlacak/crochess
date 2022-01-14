// Copyright (c) 2021 Mario Mlačak, mmlacak@gmail.com
// Licensed under GNU GPL v3+ license. See LICENSING, COPYING files for details.

#include <stddef.h>
#include <math.h>

#include "cc_defines.h"
#include "cc_pos.h"
#include "cc_gen_pos.h"
#include "cc_gen_steps.h"
#include "cc_rule_steps.h"


bool cc_rule_steps_find_unique_path( CcGame * restrict game,
                                     CcPieceEnum last_active,
                                     CcPieceEnum teleport_in,
                                     CcPieceEnum piece,
                                     bool include_opposite,
                                     int disamb_i__d,
                                     int disamb_j__d,
                                     CcPos dest,
                                     CcPosLink ** restrict path__o )
{
    if ( !game ) return false;
    if ( !game->chessboard ) return false;
    if ( !path__o ) return false;
    if ( *path__o ) return false;

    CcChessboard * cb = game->chessboard;

    bool is_disamb_i = cc_chessboard_is_coord_on_board( cb, disamb_i__d );
    bool is_disamb_j = cc_chessboard_is_coord_on_board( cb, disamb_j__d );

    if ( ( CC_COORD_IS_VALID( disamb_i__d ) ) && ( !is_disamb_i ) ) return false;
    if ( ( CC_COORD_IS_VALID( disamb_j__d ) ) && ( !is_disamb_j ) ) return false;

    if ( !cc_chessboard_is_pos_on_board( cb, dest.i, dest.j ) )
        return false;

    if ( is_disamb_i && is_disamb_j )
    {
        CcPieceEnum pe = cc_chessboard_get_piece( cb, disamb_i__d, disamb_j__d );
        CcPos start = cc_pos( disamb_i__d, disamb_j__d );

        if ( CC_PIECE_IS_THE_SAME( pe, piece ) ||
             ( include_opposite && cc_piece_has_same_type( pe, piece ) ) )
        {
            if ( CC_PIECE_IS_TELEPORTER( teleport_in ) ) return false;

            if (  cc_rule_steps_check_movement( game, last_active, teleport_in, pe,
                                                start,
                                                dest,
                                                path__o ) )
                return true;
        }
        else if ( CC_PIECE_IS_TELEPORTER( pe ) )
        {
            // pe == teleporter_out
            if ( !CC_PIECE_IS_TELEPORTER( teleport_in ) ) return false;

            if ( cc_rule_steps_check_teleportation( teleport_in, start, piece, dest, pe ) )
            {
                if ( CC_PIECE_IS_WAVE( piece ) )
                {
                    if (  cc_rule_steps_check_movement( game, last_active, teleport_in, piece,
                                                        start,
                                                        dest,
                                                        path__o ) )
                        return true;
                }
                else
                    return true;
            }

// TODO :: teleporting
        }

        return false;
    }

    CcPos start = cc_pos_invalid();
    CcPosLink * pls__t = NULL;

// TODO :: teleporting
    while ( cc_gen_steps_piece_pos_iter( cb, piece, include_opposite, &start ) )
// TODO :: teleporting
    {
        if ( is_disamb_i && ( disamb_i__d != start.i ) ) continue;
        if ( is_disamb_j && ( disamb_j__d != start.j ) ) continue;

        CcPieceEnum pe = cc_chessboard_get_piece( cb, start.i, start.j );

        if ( CC_PIECE_IS_THE_SAME( pe, piece ) ||
             ( include_opposite && cc_piece_has_same_type( pe, piece ) ) )
        {
            if ( CC_PIECE_IS_TELEPORTER( teleport_in ) ) return false;

            if ( cc_rule_steps_check_movement( game, last_active, teleport_in, pe,
                                               start,
                                               dest,
                                               &pls__t ) )
            {
                if ( *path__o ) // Previous path found? --> Not unique!
                {
                    cc_pos_link_free_all( &pls__t );
                    cc_pos_link_free_all( path__o );
                    return false;
                }

                *path__o = pls__t;
                pls__t = NULL;
            }
        }
        else if ( CC_PIECE_IS_TELEPORTER( pe ) )
        {
            if ( !CC_PIECE_IS_TELEPORTER( teleport_in ) ) return false;

// TODO :: teleporting
        }
    }

    return (bool)( *path__o );
}

bool cc_rule_steps_check_teleportation( CcPieceEnum teleport_in,
                                        CcPos start,
                                        CcPieceEnum piece,
                                        CcPos dest,
                                        CcPieceEnum teleport_out )
{
    if ( !CC_PIECE_IS_TELEPORTABLE( piece ) ) return false;
    if ( !CC_PIECE_IS_TELEPORTER( teleport_in ) ) return false;
    if ( !CC_PIECE_IS_TELEPORTER( teleport_out ) ) return false;

    if ( CC_PIECE_IS_WAVE( piece ) )
    {
        if ( CC_PIECE_IS_THE_SAME( teleport_in, teleport_out ) )
            return true; // Movement is checked normally.
    }
    else
    {
        if ( ( teleport_in == CC_PE_DimStar ) &&
             ( teleport_out == CC_PE_DimStar ) )
                return false;

        if ( ( teleport_in == CC_PE_BrightStar ) &&
             ( teleport_out == CC_PE_BrightStar ) )
                return false;

        // Teleporting into Monolith does not limit
        // from which teleporter piece can emerge.

        CcPos offset = cc_pos_subtract( dest, start );
        if ( CC_GEN_POS_QUEEN_STEP_IS_VALID( offset ) )
            return true; // Normal movement of a piece should not be checked.
    }

    return false;
}

// TODO :: REDESIGN
bool cc_rule_steps_is_ply_allowed( CcGame * restrict game,
                                   CcPieceEnum piece,
                                   CcPos start,
                                   CcPos dest )
{
    if ( !game ) return false;
    if ( !game->chessboard ) return false;

    CcChessboard * cb = game->chessboard;
    CcPieceEnum target = cc_chessboard_get_piece( cb, dest.i, dest.j );

    // An empty field, always targetable.
    if ( CC_PIECE_IS_NONE( target ) ) return true;

    // Kings can't be ever captured, activated.
    if ( CC_PIECE_IS_KING( target ) ) return false;

    bool is_same_owner = cc_piece_has_same_owner( piece, target );

    // Own Wave, Pyramid can be activated by any own piece.
    if ( ( CC_PIECE_IS_WAVE( target ) || CC_PIECE_IS_PYRAMID( target ) )
        && is_same_owner )
            return true;

    // Wave can activate other Wave, or any other own piece.
    if ( CC_PIECE_IS_WAVE( piece ) )
        if ( CC_PIECE_IS_WAVE( target ) || is_same_owner )
            return true;

    // Starchild can activate own Starchild, Wave; on its step-fields.
    if ( CC_PIECE_IS_STARCHILD( piece ) )
        if ( ( CC_PIECE_IS_STARCHILD( target ) || CC_PIECE_IS_WAVE( target ) )
            && is_same_owner )
                return true;

    // Special case, not handled:
    // Starchild can activate any own piece (except King), opponent’s
    // Starchild and any Star on its neighboring-fields.

    // Monolith, Star can only move to an empty field.
    if ( CC_PIECE_IS_MONOLITH( piece ) || CC_PIECE_IS_STAR( piece ) )
        return ( CC_PIECE_IS_NONE( target ) );

    // Any piece, own or opponent's, can teleport, except Kings.
    if ( !CC_PIECE_IS_KING( piece ) )
    {
        if ( CC_PIECE_IS_MONOLITH( target ) ) return true;
        if ( CC_PIECE_IS_STAR( target ) ) return true;
    }

    // Any piece can capture opponent's piece.
    if ( cc_piece_has_different_owner( piece, target ) ) return true;

    return false;
}
// TODO :: REDESIGN

bool cc_rule_steps_check_bishop( CcGame * restrict game,
                                 CcPieceEnum last_active,
                                 CcPieceEnum teleport_in,
                                 CcPieceEnum piece,
                                 CcPos start,
                                 CcPos dest,
                                 CcPosLink ** restrict pls__o )
{
    if ( !game ) return false;
    if ( !game->chessboard ) return false;
    if ( !pls__o ) return false;
    if ( *pls__o ) return false;

    if ( !CC_PIECE_IS_BISHOP( piece ) ) return false;

// TODO :: teleportation

    CcPos offset = cc_pos_subtract( dest, start );
    if ( ( offset.i == 0 ) || ( offset.j == 0 ) ) return false;
    if ( abs( offset.i ) != abs( offset.j ) ) return false;

    CcPos step = cc_pos( CC_SIGN( offset.i ), CC_SIGN( offset.j ) );
    if ( !CC_GEN_POS_BISHOP_STEP_IS_VALID( step ) ) return false;

    CcChessboard * cb = game->chessboard;
    CcPosLink * pl__t = cc_pos_link_new( start );

    for ( CcPos p = cc_pos_add( start, step ); !cc_pos_is_equal( p, dest ); p = cc_pos_add( p, step ) )
    {
        CcPieceEnum pe = cc_chessboard_get_piece( cb, p.i, p.j );
        if ( !CC_PIECE_IS_NONE( pe ) ) return false;

        if ( !cc_pos_link_append( pl__t, p ) )
        {
            cc_pos_link_free_all( &pl__t );
            return false;
        }
    }

    if ( !cc_pos_link_append( pl__t, dest ) )
    {
        cc_pos_link_free_all( &pl__t );
        return false;
    }

    if ( cc_rule_steps_is_ply_allowed( game, piece, start, dest ) )
    {
        *pls__o = pl__t; // Ownership transfer --> pl__t is now weak pointer --> no free().
        return true;
    }

    cc_pos_link_free_all( &pl__t );
    return false;
}

bool cc_rule_steps_check_movement( CcGame * restrict game,
                                   CcPieceEnum last_active,
                                   CcPieceEnum teleport_in,
                                   CcPieceEnum piece,
                                   CcPos start,
                                   CcPos dest,
                                   CcPosLink ** restrict pls__o )
{
    // if ( !cb ) return false;
    // if ( !pls__o ) return false;
    // if ( *pls__o ) return false;

    if ( CC_PIECE_IS_BISHOP( piece ) )
        return cc_rule_steps_check_bishop( game, last_active, teleport_in, piece, start, dest, pls__o );
    else
        return false;
}
