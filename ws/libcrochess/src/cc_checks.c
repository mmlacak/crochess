// Copyright (c) 2023 Mario Mlačak, mmlacak@gmail.com
// Licensed under GNU GPL v3+ license. See LICENSING, COPYING files for details.

#include "cc_pos_utils.h"

#include "cc_checks.h"


bool cc_check_valid_draw_offer_exists( CcMove * moves,
                                       CcGameStatusEnum gse ) {
    if ( !moves ) return false;
    if ( !CC_GAME_STATUS_IS_TURN( gse ) ) return false;

    CcMove * m = moves;
    CC_FASTFORWARD( m );

    while ( m ) {
        if ( CC_MOVE_STATUS_IS_DRAW_OFFER_REVOKED( m->status ) )
            return false;
        else if ( CC_MOVE_STATUS_IS_DRAW_OFFER( m->status ) )
            return true;

        // Skip two moves, because draw offer is made by one player.
        m = m->prev__w;
        if ( m )
            m = m->prev__w;
        else
            break;
    }

    return false;
}

bool cc_check_piece_can_lose_tag( CcPieceType piece, CcLosingTagType ltt ) {
    if ( ltt == CC_LTE_NoneLost ) {
        return true;
    } else if ( CC_PIECE_IS_PAWN( piece ) ) {
        return ( ( ltt == CC_LTE_RushingTagLost ) || ( ltt == CC_LTE_DelayedPromotionLost ) );
    } else if ( CC_PIECE_IS_ROOK( piece ) || CC_PIECE_IS_KING( piece ) ) {
        return ( ltt == CC_LTE_CastlingTagLost );
    } else
        return false;
}

CcMaybeBoolEnum cc_check_piece_is_blocked_at( CcChessboard * cb,
                                              CcPieceType piece,
                                              CcPos pos ) {
    if ( !CC_PIECE_IS_VALID( piece ) ) return CC_MBE_Void;
    if ( !cb ) return CC_MBE_Void;

    CcPieceType pe = cc_chessboard_get_piece( cb, pos.i, pos.j );
    if ( CC_PIECE_IS_NONE( pe ) ) return CC_MBE_False;

    if ( CC_PIECE_IS_WAVE( piece ) ) {
        return CC_BOOL_TO_MAYBE( CC_PIECE_IS_OPAQUE( pe ) );
    }

    if ( CC_PIECE_IS_OPAQUE( pe ) )
        if ( !CC_PIECE_IS_COMPLETELY_TRANSPARENT( piece ) )
            return CC_MBE_True;

    if ( CC_PIECE_IS_OPAQUE( piece ) )
        if ( !CC_PIECE_IS_COMPLETELY_TRANSPARENT( pe ) )
            return CC_MBE_True;

    if ( CC_PIECE_IS_SEMI_OPAQUE( piece ) )
        if ( CC_PIECE_IS_SEMI_OPAQUE( pe ) )
            return CC_MBE_True;

    return CC_MBE_False;
}

CcMaybeBoolEnum cc_check_piece_can_capture_at( CcChessboard * cb,
                                               CcPieceType piece,
                                               CcPos pos ) {
    if ( !CC_PIECE_IS_VALID( piece ) ) return CC_MBE_Void;

    if ( !CC_PIECE_CAN_CAPTURE( piece ) ) return CC_MBE_False; // This weeds out pieces without owner.

    if ( !cb ) return CC_MBE_Void;

    CcPieceType pe = cc_chessboard_get_piece( cb, pos.i, pos.j );
    if ( !CC_PIECE_CAN_BE_CAPTURED( pe ) ) return CC_MBE_False; // Also weeds out other pieces without owner.

    return CC_BOOL_TO_MAYBE( cc_piece_has_different_owner( piece, pe ) );
}

CcMaybeBoolEnum cc_check_piece_can_diverge_at( CcChessboard * cb,
                                               CcPieceType piece,
                                               cc_uint_t momentum,
                                               CcPieceType activator,
                                               CcPos pos ) {
    if ( !CC_PIECE_IS_VALID( piece ) ) return CC_MBE_Void;

    if ( momentum == 0 ) return CC_MBE_False;

    if ( CC_PIECE_IS_WAVE( piece ) ) {
        // Not needed, checked within CC_WAVE_CAN_BE_DIVERGED() below.
        // if ( !CC_PIECE_IS_ACTIVATOR( activator ) ) return CC_MBE_False;

        if ( !CC_WAVE_CAN_BE_DIVERGED( activator ) ) return CC_MBE_False;
    } else {
        if ( !CC_PIECE_CAN_BE_DIVERGED( piece ) ) return CC_MBE_False;
    }

    if ( !cb ) return CC_MBE_Void;

    CcPieceType pe = cc_chessboard_get_piece( cb, pos.i, pos.j );
    if ( CC_PIECE_IS_STARCHILD( pe ) ) return CC_MBE_True;

    if ( CC_PIECE_IS_SHAMAN( pe ) ) {
        if ( CC_PIECE_IS_SHAMAN( piece ) )
            return CC_MBE_True;
        else
            return CC_BOOL_TO_MAYBE( cc_piece_has_same_owner( piece, pe ) );
    } else
        return CC_MBE_False;
}

CcMaybeBoolEnum cc_check_castling_step_fields( CcChessboard * cb,
                                               CcPos king_start,
                                               CcPos king_dest,
                                               CcPos rook_start,
                                               CcPos rook_dest ) {
    if ( !cb ) return CC_MBE_Void;

    if ( king_start.j != rook_start.j ) return CC_MBE_False;

    CcPieceType king = cc_chessboard_get_piece( cb, king_start.i, king_start.j );
    if ( !CC_PIECE_IS_KING( king ) ) return CC_MBE_False;

    CcPieceType rook = cc_chessboard_get_piece( cb, rook_start.i, rook_start.j );
    if ( !CC_PIECE_IS_ROOK( rook ) ) return CC_MBE_False;

    if ( !cc_piece_has_same_color( king, rook ) ) return CC_MBE_False;

    CcTagType king_tag = cc_chessboard_get_tag( cb, king_start.i, king_start.j );
    if ( king_tag != CC_TE_CanCastle ) return CC_MBE_False;

    CcTagType rook_tag = cc_chessboard_get_tag( cb, rook_start.i, rook_start.j );
    if ( rook_tag != CC_TE_CanCastle ) return CC_MBE_False;

    CcPieceType empty_for_king = cc_chessboard_get_piece( cb, king_dest.i, king_dest.j );
    if ( !CC_PIECE_IS_NONE( empty_for_king ) ) return CC_MBE_False;

    CcPieceType empty_for_rook = cc_chessboard_get_piece( cb, rook_dest.i, rook_dest.j );
    if ( !CC_PIECE_IS_NONE( empty_for_rook ) ) return CC_MBE_False;

    bool is_queen_side = ( king_start.i > rook_start.i );
    CcPos king_step = is_queen_side ? CC_POS_CAST( -1, 0 ) : CC_POS_CAST( 1, 0 );

    CcPos current = cc_pos_add_steps( king_start, king_step, 1 );
    cc_uint_t momentum = 1;

    do {
        // Rook is semi-opaque just like King, so it's enough to check only King
        //     against all fields in-between the two.
        if ( cc_check_piece_is_blocked_at( cb, king, current ) == CC_MBE_True )
            return CC_MBE_False;

        current = cc_pos_add_steps( current, king_step, 1 );

        ++momentum;
    } while ( !CC_POS_IS_EQUAL( rook_dest, current ) );

    return CC_MBE_True;
}

CcMaybeBoolEnum cc_find_en_passant_target( CcChessboard * cb,
                                           CcPieceType private,
                                           CcPos destination,
                                           CcPosDesc * target__o ) {
    if ( !cb ) return CC_MBE_Void;
    if ( !target__o ) return CC_MBE_Void;

    if ( !CC_PIECE_CAN_CAPTURE_EN_PASSANT( private ) ) return CC_MBE_Void;

    // Do not remove, cc_chessboard_get_piece() returns empty field if position is outside chessboard.
    if ( !cc_chessboard_is_pos_on_board( cb, destination.i, destination.j ) ) return CC_MBE_Void;

    bool is_piece_light = cc_piece_is_light( private );

    if ( is_piece_light ) { // En passant can only be done on opposite side of a chessboard.
        if ( cc_chessboard_is_field_on_light_side( cb, destination.j ) ) return CC_MBE_Void;
    } else {
        if ( cc_chessboard_is_field_on_dark_side( cb, destination.j ) ) return CC_MBE_Void;
    }

    // Checking target, it might be not blocking en passant (if activatable piece), or blocking (if not activatable).
    CcPieceType target = cc_chessboard_get_piece( cb, destination.i, destination.j );
    if ( !( ( target == CC_PE_None ) ||
            ( CC_PIECE_CAN_BE_ACTIVATED( target ) &&
              cc_piece_has_same_owner( private, target ) ) ) ) // No need to check for different owners, as it only applies to Wave --> Wave, Starchild --> Starchild activations.
        return CC_MBE_False;

    CcPos pos = destination;
    int diff = is_piece_light ? -1 : 1;
    CcTagType tag = CC_TE_None;
    bool found = false;

    do {
        pos = cc_pos_add( pos, 0, diff );
        if ( !cc_chessboard_is_pos_on_board( cb, pos.i, pos.j ) ) break;

        target = cc_chessboard_get_piece( cb, pos.i, pos.j );
        if ( CC_PIECE_CAN_BE_CAPTURED_EN_PASSANT( target ) ) {
            tag = cc_chessboard_get_tag( cb, pos.i, pos.j );
            if ( CC_TAG_IS_EN_PASSANT( tag ) )
                found = true;
        }
    } while ( !found );

    if ( found && cc_piece_has_different_owner( private, target ) ) {
        *target__o = (CcPosDesc){ .pos = pos, .piece = target, .tag = tag };
        return CC_MBE_True;
    }

    return CC_MBE_Void;
}
