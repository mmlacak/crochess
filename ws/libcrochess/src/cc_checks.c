// Copyright (c) 2023 Mario Mlačak, mmlacak@gmail.com
// Licensed under GNU GPL v3+ license. See LICENSING, COPYING files for details.

#include "cc_pos_utils.h"

#include "cc_checks.h"


bool cc_check_valid_draw_offer_exists( CcMove * moves,
                                       CcGameStatusEnum gse ) {
    if ( !moves ) return false;
    if ( !CC_GAME_STATUS_IS_TURN( gse ) ) return false; // No need to check validity here, everything except turns is filtered out.

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

//
// Piece checks

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

bool cc_check_piece_is_blocked( CcPieceTagType moving,
                                CcPieceTagType encounter,
                                cc_uint_t momentum ) {
    if ( !CC_PIECE_IS_VALID( moving ) ) return false;
    if ( !CC_PIECE_IS_VALID( encounter ) ) return false;

    if ( CC_PIECE_IS_WAVE( moving ) )
        return CC_PIECE_IS_OPAQUE( encounter );

    if ( CC_PIECE_IS_OPAQUE( encounter ) )
        if ( !CC_PIECE_IS_COMPLETELY_TRANSPARENT( moving ) )
            return true;

    if ( CC_PIECE_IS_OPAQUE( moving ) )
        if ( !CC_PIECE_IS_COMPLETELY_TRANSPARENT( encounter ) )
            return true;

    if ( CC_PIECE_IS_SEMI_OPAQUE( moving ) )
        if ( CC_PIECE_IS_SEMI_OPAQUE( encounter ) )
            return true;

    if ( CC_PIECE_IS_TRANSPARENT( encounter ) )
        if ( !CC_PIECE_IS_WEIGHTLESS( moving ) )
            return ( momentum < 1 );

    return false;
}

bool cc_check_piece_can_step_over( CcPieceTagType moving,
                                   CcPieceTagType encounter,
                                   cc_uint_t momentum ) {
    if ( !CC_PIECE_IS_VALID( moving ) ) return false;
    if ( !CC_PIECE_IS_VALID( encounter ) ) return false;

    if ( CC_PIECE_IS_WAVE( moving ) )
        return CC_PIECE_IS_SEMI_TRANSPARENT( encounter );

    bool has_enough_momentum = ( momentum > 0 );

    if ( CC_PIECE_IS_WAVE( encounter ) )
        if ( CC_PIECE_IS_SEMI_TRANSPARENT( moving ) )
            return has_enough_momentum || CC_PIECE_IS_WEIGHTLESS( moving );

    if ( CC_PIECE_IS_OPAQUE( encounter ) )
        if ( CC_PIECE_IS_COMPLETELY_TRANSPARENT( moving ) )
            return true;

    if ( CC_PIECE_IS_COMPLETELY_TRANSPARENT( moving ) )
        return true;

    if ( CC_PIECE_IS_COMPLETELY_TRANSPARENT( encounter ) )
        return has_enough_momentum || CC_PIECE_IS_WEIGHTLESS( moving );

    return false;
}

bool cc_check_piece_can_capture( CcPieceTagType moving,
                                 CcPieceTagType encounter ) {
    if ( !CC_PIECE_IS_VALID( moving ) ) return false;
    if ( !CC_PIECE_IS_VALID( encounter ) ) return false;
    if ( !CC_PIECE_CAN_CAPTURE( moving ) ) return false; // This weeds out invalid pieces, and those without owner.
    if ( !CC_PIECE_CAN_BE_CAPTURED( encounter ) ) return false; // Also weeds out invalid pieces, and those without owner.
    // All pieces that can capture also use momentum for movement, capture can be done with no momentum.
    if ( !cc_piece_has_different_owner( moving, encounter ) ) return false;
    return true;
}

bool cc_check_piece_can_activate( CcPieceTagType moving,
                                  CcPieceTagType encounter,
                                  cc_uint_t momentum,
                                  CcStepTypeEnum step_type ) {
    if ( !CC_PIECE_IS_VALID( moving ) ) return false;
    if ( !CC_PIECE_IS_VALID( encounter ) ) return false;
    if ( !CC_STEP_TYPE_IS_VALID( step_type ) ) return false;

    if ( !CC_PIECE_CAN_ACTIVATE( moving ) ) return false; // [1] Stars and Monolith can't activate anything.
    if ( !CC_PIECE_CAN_BE_ACTIVATED( encounter ) ) return false; // [2] Kings and Monoliths can't be activated.

    bool wave_moving = CC_PIECE_IS_WAVE( moving );
    bool wave_encounter = CC_PIECE_IS_WAVE( encounter );

    if ( wave_moving && wave_encounter ) return true;

    bool starchild_moving = CC_PIECE_IS_STARCHILD( moving );
    bool starchild_encounter = CC_PIECE_IS_STARCHILD( encounter );
    bool positive_momentum = ( momentum > 0 );

    if ( starchild_moving ) {
        if ( step_type == CC_STE_Miracle ) {
            return ( CC_PIECE_IS_STAR( encounter ) && positive_momentum );
        } else if ( step_type == CC_STE_Uplifting ) {
            return ( !wave_encounter && !CC_PIECE_IS_STAR( encounter ) ); // Kings and Monoliths already filtered-out at [2].
        }
    }

    if ( CC_PIECE_IS_SHAMAN( moving ) ) {
        if ( step_type == CC_STE_Entrancement ) {
            return ( CC_PIECE_IS_SHAMAN( encounter ) || starchild_encounter );
        }
    }

    if ( !cc_piece_has_same_owner( moving, encounter ) ) return false;

    if ( CC_PIECE_IS_PYRAMID( encounter ) ) {
        return ( positive_momentum && CC_PIECE_CAN_ACTIVATE_PYRAMID( moving ) && CC_STEP_TYPE_IS_CAPTURE( step_type ) ); // Wave, Starchild cannot activate Pyramid, only material pieces can.
    }

    if ( wave_moving || wave_encounter ) // King encounter already filtered-out at [2].
        return CC_PIECE_IS_WEIGHTLESS( encounter ) || positive_momentum;

    if ( starchild_moving && starchild_encounter ) return true;

    return false;
}

//
// Positional checks

bool cc_check_piece_is_blocked_at( CcChessboard * cb,
                                   CcPieceTagType moving,
                                   CcActivationDesc act_desc,
                                   bool is_first_ply,
                                   CcPos pos ) {
    if ( !cb ) return false;

    if ( !cc_activation_desc_is_legal( act_desc, moving, is_first_ply ) ) return false;

    CcPieceTagType encounter = cc_chessboard_get_piece( cb, pos.i, pos.j );

    return cc_check_piece_is_blocked( moving, encounter, act_desc.momentum );
}

bool cc_check_piece_can_capture_at( CcChessboard * cb,
                                    CcPieceTagType moving,
                                    CcPos pos ) {
    if ( !cb ) return false;
    CcPieceTagType encounter = cc_chessboard_get_piece( cb, pos.i, pos.j );
    return cc_check_piece_can_capture( moving, encounter );
}

bool cc_check_piece_can_activate_at( CcChessboard * cb,
                                     CcPieceTagType moving,
                                     CcActivationDesc act_desc,
                                     bool is_first_ply,
                                     CcPos destination,
                                     CcStepTypeEnum step_type ) {
    if ( !cb ) return false;
    if ( !CC_POS_IS_LEGAL( destination, cc_chessboard_get_size( cb ) ) ) return false;

    if ( !cc_activation_desc_is_legal( act_desc, moving, is_first_ply ) ) return false;

    CcPieceTagType encounter = cc_chessboard_get_piece( cb, destination.i, destination.j );

    // Function checks its arguments, and -by extension- ours moving, step_type.
    if ( !cc_check_piece_can_activate( moving, encounter, act_desc.momentum, step_type ) ) return false;

    if ( CC_PIECE_IS_WEIGHTLESS( encounter ) )
        return true;
    else
        return ( act_desc.momentum > 0 );
}

bool cc_check_piece_can_diverge_at( CcChessboard * cb,
                                    CcPieceTagType moving,
                                    cc_uint_t momentum, // TODO :: use CcActivationDesc act_desc, bool is_first_ply, instead momentum, activator
                                    CcPieceTagType activator,
                                    CcPos pos ) {
    if ( !CC_PIECE_IS_VALID( moving ) ) return false;
    if ( !CC_PIECE_IS_VALID( activator ) ) return false;

    // TODO :: check act_desc is valid

    if ( momentum == 0 ) return false;

    if ( CC_PIECE_IS_WAVE( moving ) ) {
        if ( !CC_PIECE_IS_ACTIVATOR( activator ) ) return false;
        if ( !CC_WAVE_CAN_BE_DIVERGED( activator ) ) return false;
    } else {
        if ( !CC_PIECE_CAN_BE_DIVERGED( moving ) ) return false;
    }

    if ( !cb ) return false;

    CcPieceTagType encounter = cc_chessboard_get_piece( cb, pos.i, pos.j );
    if ( CC_PIECE_IS_STARCHILD( encounter ) ) return true;

    if ( CC_PIECE_IS_SHAMAN( encounter ) ) {
        if ( CC_PIECE_IS_SHAMAN( moving ) )
            return true;
        else
            return cc_piece_has_same_owner( moving, encounter );
    } else
        return false;
}

bool cc_check_castling_step_fields( CcChessboard * cb,
                                    CcPos king_start,
                                    CcPos king_dest,
                                    CcPos rook_start,
                                    CcPos rook_dest ) {
    if ( !cb ) return false;

    if ( king_start.j != rook_start.j ) return false;

    CcPieceTagType king = cc_chessboard_get_piece( cb, king_start.i, king_start.j );
    if ( !CC_PIECE_IS_KING( king ) ) return false;
    if ( !CC_PIECE_CAN_CASTLE( king ) ) return false;

    CcPieceTagType rook = cc_chessboard_get_piece( cb, rook_start.i, rook_start.j );
    if ( !CC_PIECE_IS_ROOK( rook ) ) return false;
    if ( !CC_PIECE_CAN_CASTLE( rook ) ) return false;

    if ( !cc_piece_has_same_color( king, rook ) ) return false;

    CcPieceTagType empty_for_king = cc_chessboard_get_piece( cb, king_dest.i, king_dest.j );
    if ( !CC_PIECE_IS_NONE( empty_for_king ) ) return false;

    CcPieceTagType empty_for_rook = cc_chessboard_get_piece( cb, rook_dest.i, rook_dest.j );
    if ( !CC_PIECE_IS_NONE( empty_for_rook ) ) return false;

    bool is_queen_side = ( king_start.i > rook_start.i );
    CcPos king_step = is_queen_side ? CC_POS_CAST( -1, 0 ) : CC_POS_CAST( 1, 0 );

    CcActivationDesc ad = CC_ACTIVATION_DESC_INITIAL;
    CcPos current = cc_pos_add_steps( king_start, king_step, 1 );
    if ( cc_activation_desc_calc_momentum( &ad, 1 ) != CC_MBE_True ) return false;

    do {
        // Rook is semi-opaque just like King, so it's enough to check only King
        //     against all fields in-between the two.
        if ( cc_check_piece_is_blocked_at( cb, king, ad, true, current ) ) // true --> King cannot be activated, all its movement is strictly in the first ply.
            return false;

        current = cc_pos_add_steps( current, king_step, 1 );

        if ( cc_activation_desc_calc_momentum( &ad, 1 ) != CC_MBE_True ) return false;
    } while ( !CC_POS_IS_EQUAL( rook_dest, current ) );

    return true;
}

//
// Finders

bool cc_find_en_passant_target( CcChessboard * cb,
                                CcPieceTagType capturing,
                                CcActivationDesc act_desc,
                                bool is_first_ply,
                                CcPos destination,
                                CcPosDesc * target__o ) {
    if ( !cb ) return false;
    if ( !target__o ) return false;

    if ( !CC_PIECE_IS_VALID( capturing ) ) return false;
    if ( !CC_PIECE_CAN_CAPTURE_EN_PASSANT( capturing ) ) return false;

    // Do not remove, cc_chessboard_get_piece() returns empty field if position is outside chessboard.
    if ( !cc_chessboard_is_pos_on_board( cb, destination.i, destination.j ) ) return false;

    bool is_capturing_piece_light = CC_PIECE_IS_LIGHT( capturing );

    if ( is_capturing_piece_light ) { // En passant can only be done on opposite side of a chessboard.
        if ( cc_chessboard_is_field_on_light_side( cb, destination.j ) ) return false;
    } else {
        if ( cc_chessboard_is_field_on_dark_side( cb, destination.j ) ) return false;
    }

    // Checking encountered piece, it might be not blocking en passant (if it can be activated), or blocking (if it can't).
    CcPieceTagType encounter = cc_chessboard_get_piece( cb, destination.i, destination.j );
    if ( encounter != CC_PTE_None ) {
        // Function checks its arguments, and -by extension- ours act_desc.
        if ( !cc_check_piece_can_activate_at( cb, capturing, act_desc, is_first_ply, destination, CC_STE_CaptureOnly ) ) return false;
    }

    int j_diff = is_capturing_piece_light ? -1 : 1;
    CcPos pos = destination;
    CcPieceTagType target = CC_PTE_None;
    bool found = false;

    do {
        pos = cc_pos_add( pos, 0, j_diff );
        if ( !cc_chessboard_is_pos_on_board( cb, pos.i, pos.j ) ) break;

        target = cc_chessboard_get_piece( cb, pos.i, pos.j );
        if ( CC_PIECE_CAN_BE_CAPTURED_EN_PASSANT( target ) ) {
            if ( CC_PIECE_RUSHED( target ) )
                found = true;
        }
    } while ( !found );

    if ( found && cc_piece_has_different_owner( capturing, target ) ) {
        *target__o = (CcPosDesc){ .pos = pos, .piece = target };
        return true;
    }

    return false;
}

bool cc_find_first_piece( CcChessboard * cb,
                          CcPieceTagType piece,
                          CcPos start,
                          CcPos step,
                          bool check_start_pos,
                          bool compare_tags,
                          CcPosDesc * found__o ) {
    if ( !cb ) return false;
    if ( !found__o ) return false;

    if ( !CC_PIECE_IS_VALID( piece ) ) return false;
    if ( !cc_chessboard_is_pos_on_board( cb, start.i, start.j ) ) return false;

    CcPieceTagType p = compare_tags ? piece
                                    : cc_piece_strip_tag( piece );
    CcPieceTagType encounter = CC_PTE_None;
    CcPieceTagType ptt = CC_PTE_None;

    CcPos pos = check_start_pos ? start
                                : cc_pos_add_steps( start, step, 1 );

    do {
        if ( !cc_chessboard_is_pos_on_board( cb, pos.i, pos.j ) ) break;

        ptt = encounter = cc_chessboard_get_piece( cb, pos.i, pos.j );
        if ( !compare_tags )
            ptt = cc_piece_strip_tag( encounter );

        if ( ptt == p ) {
            found__o->piece = encounter;
            found__o->pos = pos;
            return true;
        }

        pos = cc_pos_add_steps( pos, step, 1 );
    } while ( true );

    return false;
}
