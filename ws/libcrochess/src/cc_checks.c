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

bool cc_check_piece_can_lose_tag( CcPieceTagType ptt,
                                  CcLosingTagType ltt,
                                  bool compare_tag_and_losing_tag ) {
    if ( ltt == CC_LTE_NoneLost ) return true;

    if ( compare_tag_and_losing_tag ) {
        switch ( ltt ) {
            case CC_LTE_RushingTagLost : return CC_PIECE_CAN_RUSH( ptt );
            case CC_LTE_CastlingTagLost : return CC_PIECE_CAN_CASTLE( ptt );
            case CC_LTE_DelayedPromotionLost : return CC_PIECE_IS_TAGGED_FOR_PROMOTION( ptt );
        }
    } else {
        switch ( ltt ) {
            case CC_LTE_RushingTagLost : return CC_PIECE_IS_PAWN( ptt ) || CC_PIECE_IS_SCOUT( ptt ) || CC_PIECE_IS_GRENADIER( ptt );
            case CC_LTE_CastlingTagLost : return CC_PIECE_IS_ROOK( ptt ) || CC_PIECE_IS_KING( ptt );
            case CC_LTE_DelayedPromotionLost : return CC_PIECE_IS_PAWN( ptt );
        }
    }

    return false;
}

bool cc_check_piece_can_capture_other( CcPieceTagType moving, CcPieceTagType still ) {
    if ( !CC_PIECE_CAN_CAPTURE( moving ) ) return false;
    if ( !CC_PIECE_CAN_BE_CAPTURED( still ) ) return false;
    if ( !cc_piece_has_different_owner( moving, still ) ) return false;
    return true;
}


bool cc_check_piece_is_blocked_at( CcChessboard * cb,
                                   CcPieceTagType piece,
                                   CcPos pos ) {
    if ( !CC_PIECE_IS_VALID( piece ) ) return false;
    if ( !cb ) return false;

    CcPieceTagType still = cc_chessboard_get_piece( cb, pos.i, pos.j );
    if ( !CC_PIECE_IS_VALID( still ) ) return false;

    if ( CC_PIECE_IS_WAVE( piece ) )
        return CC_PIECE_IS_OPAQUE( still );

    if ( CC_PIECE_IS_OPAQUE( still ) )
        if ( !CC_PIECE_IS_COMPLETELY_TRANSPARENT( piece ) )
            return true;

    if ( CC_PIECE_IS_OPAQUE( piece ) )
        if ( !CC_PIECE_IS_COMPLETELY_TRANSPARENT( still ) )
            return true;

    if ( CC_PIECE_IS_SEMI_OPAQUE( piece ) )
        if ( CC_PIECE_IS_SEMI_OPAQUE( still ) )
            return true;

    return false;
}

CcMaybeBoolEnum cc_check_piece_can_capture_at( CcChessboard * cb,
                                               CcPieceTagType piece,
                                               CcPos pos ) {
    if ( !CC_PIECE_IS_VALID( piece ) ) return CC_MBE_Void;

    if ( !CC_PIECE_CAN_CAPTURE( piece ) ) return CC_MBE_False; // This weeds out pieces without owner.

    if ( !cb ) return CC_MBE_Void;

    CcPieceTagType still = cc_chessboard_get_piece( cb, pos.i, pos.j );
    if ( !CC_PIECE_CAN_BE_CAPTURED( still ) ) return CC_MBE_False; // Also weeds out other pieces without owner.

    return CC_BOOL_TO_MAYBE( cc_piece_has_different_owner( piece, still ) );
}

CcMaybeBoolEnum cc_check_piece_can_diverge_at( CcChessboard * cb,
                                               CcPieceTagType piece,
                                               cc_uint_t momentum,
                                               CcPieceTagType activator,
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

    CcPieceTagType still = cc_chessboard_get_piece( cb, pos.i, pos.j );
    if ( CC_PIECE_IS_STARCHILD( still ) ) return CC_MBE_True;

    if ( CC_PIECE_IS_SHAMAN( still ) ) {
        if ( CC_PIECE_IS_SHAMAN( piece ) )
            return CC_MBE_True;
        else
            return CC_BOOL_TO_MAYBE( cc_piece_has_same_owner( piece, still ) );
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

    CcPieceTagType king = cc_chessboard_get_piece( cb, king_start.i, king_start.j );
    if ( !CC_PIECE_IS_KING( king ) ) return CC_MBE_False;
    if ( !CC_PIECE_CAN_CASTLE( king ) ) return CC_MBE_False;

    CcPieceTagType rook = cc_chessboard_get_piece( cb, rook_start.i, rook_start.j );
    if ( !CC_PIECE_IS_ROOK( rook ) ) return CC_MBE_False;
    if ( !CC_PIECE_CAN_CASTLE( rook ) ) return CC_MBE_False;

    if ( !cc_piece_has_same_color( king, rook ) ) return CC_MBE_False;

    CcPieceTagType empty_for_king = cc_chessboard_get_piece( cb, king_dest.i, king_dest.j );
    if ( !CC_PIECE_IS_NONE( empty_for_king ) ) return CC_MBE_False;

    CcPieceTagType empty_for_rook = cc_chessboard_get_piece( cb, rook_dest.i, rook_dest.j );
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

CcMaybeBoolEnum cc_check_piece_can_activate( CcPieceTagType moving,
                                             CcPieceTagType encounter,
                                             cc_uint_t momentum,
                                             CcStepTypeEnum step_type ) {
    if ( !CC_STEP_TYPE_IS_VALID( step_type ) ) return CC_MBE_Void;
    if ( !CC_STEP_TYPE_IS_ENUMERATOR( step_type ) ) return CC_MBE_Void;

    if ( !CC_PIECE_CAN_ACTIVATE( moving ) ) return CC_MBE_False;
    if ( !CC_PIECE_CAN_BE_ACTIVATED( encounter ) ) return CC_MBE_False; // [1]
    if ( step_type == CC_STE_None ) return CC_MBE_False;

    bool wave_moving = CC_PIECE_IS_WAVE( moving );
    bool wave_encounter = CC_PIECE_IS_WAVE( encounter );

    if ( wave_moving && wave_encounter ) return CC_MBE_True;

    bool starchild_moving = CC_PIECE_IS_STARCHILD( moving );
    bool starchild_encounter = CC_PIECE_IS_STARCHILD( encounter );
    bool positive_momentum = ( momentum > 0 );

    if ( starchild_moving ) {
        if ( step_type == CC_STE_Miracle ) {
            return ( CC_PIECE_IS_STAR( encounter ) && positive_momentum ) ? CC_MBE_True
                                                                          : CC_MBE_False;
        } else if ( step_type == CC_STE_Uplifting ) {
            // Kings and Monoliths can't be activated at all, already filtered-out at [1].
            return ( !CC_PIECE_IS_WAVE( encounter ) && !CC_PIECE_IS_STAR( encounter ) ) ? CC_MBE_True
                                                                                        : CC_MBE_False;
        }
    }

    if ( CC_PIECE_IS_SHAMAN( moving ) ) {
        if ( step_type == CC_STE_Entrancement ) {
            return ( CC_PIECE_IS_SHAMAN( encounter ) || starchild_encounter ) ? CC_MBE_True
                                                                              : CC_MBE_False;
        }
    }

    if ( !cc_piece_has_same_owner( moving, encounter ) ) return CC_MBE_False;

    if ( CC_PIECE_IS_PYRAMID( encounter ) ) {
        return ( CC_STEP_TYPE_IS_CAPTURE( step_type ) && positive_momentum ) ? CC_MBE_True
                                                                             : CC_MBE_False;
    }

    if ( wave_moving || wave_encounter ) return CC_MBE_True; // King encounter already filtered-out at [1].

    if ( starchild_moving && starchild_encounter ) return CC_MBE_True;

    return CC_MBE_False;
}

CcMaybeBoolEnum cc_check_piece_can_activate_at( CcChessboard * cb,
                                                CcPieceTagType moving,
                                                CcActivationDesc act_desc,
                                                CcPos destination,
                                                CcStepTypeEnum step_type ) {
    if ( !cb ) return CC_MBE_Void;
    if ( !CC_POS_IS_LEGAL( destination, cc_chessboard_get_size( cb ) ) ) return CC_MBE_Void;

    CcPieceTagType encounter = cc_chessboard_get_piece( cb, destination.i, destination.j );

    // Function checks its arguments, and -by extension- ours moving, step_type.
    CcMaybeBoolEnum can_activate = cc_check_piece_can_activate( moving, encounter, act_desc.momentum, step_type );
    if ( can_activate != CC_MBE_True ) return can_activate;

    CcMaybeBoolEnum is_act_desc_valid = cc_activation_desc_is_valid( act_desc, true ); // true --> ignore if activator is none.
    if ( is_act_desc_valid != CC_MBE_True ) return is_act_desc_valid;

    if ( CC_PIECE_IS_WEIGHTLESS( encounter ) )
        return CC_MBE_True;
    else
        return ( act_desc.momentum > 0 ) ? CC_MBE_True
                                         : CC_MBE_False;
}

CcMaybeBoolEnum cc_find_en_passant_target( CcChessboard * cb,
                                           CcPieceTagType private,
                                           CcActivationDesc act_desc,
                                           CcPos destination,
                                           CcPosDesc * target__o ) {
    if ( !cb ) return CC_MBE_Void;
    if ( !target__o ) return CC_MBE_Void;

    if ( !CC_PIECE_CAN_CAPTURE_EN_PASSANT( private ) ) return CC_MBE_Void;

    // Do not remove, cc_chessboard_get_piece() returns empty field if position is outside chessboard.
    if ( !cc_chessboard_is_pos_on_board( cb, destination.i, destination.j ) ) return CC_MBE_Void;

    bool is_piece_light = CC_PIECE_IS_LIGHT( private );

    if ( is_piece_light ) { // En passant can only be done on opposite side of a chessboard.
        if ( cc_chessboard_is_field_on_light_side( cb, destination.j ) ) return CC_MBE_Void;
    } else {
        if ( cc_chessboard_is_field_on_dark_side( cb, destination.j ) ) return CC_MBE_Void;
    }

    // Checking encountered piece, it might be not blocking en passant (if it can be activated), or blocking (if it can't).
    CcPieceTagType encounter = cc_chessboard_get_piece( cb, destination.i, destination.j );
    if ( encounter != CC_PTE_None ) {
        // Function checks its arguments, and -by extension- ours act_desc.
        CcMaybeBoolEnum can_activate = cc_check_piece_can_activate_at( cb, private, act_desc, destination, CC_STE_CaptureOnly );
        if ( can_activate != CC_MBE_True ) return can_activate;
    }

    int diff = is_piece_light ? -1 : 1;
    CcPos pos = destination;
    CcPieceTagType target = CC_PTE_None;
    bool found = false;

    do {
        pos = cc_pos_add( pos, 0, diff );
        if ( !cc_chessboard_is_pos_on_board( cb, pos.i, pos.j ) ) break;

        target = cc_chessboard_get_piece( cb, pos.i, pos.j );
        if ( CC_PIECE_CAN_BE_CAPTURED_EN_PASSANT( target ) ) {
            if ( CC_PIECE_RUSHED( target ) )
                found = true;
        }
    } while ( !found );

    if ( found && cc_piece_has_different_owner( private, target ) ) {
        *target__o = (CcPosDesc){ .pos = pos, .piece = target };
        return CC_MBE_True;
    }

    return CC_MBE_Void;
}
