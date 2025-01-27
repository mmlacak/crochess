// Copyright (c) 2024, 2025 Mario Mlačak, mmlacak@gmail.com
// Licensed under GNU GPL v3+ license. See LICENSING, COPYING files for details.

// #include "cc_defines.h"
// #include "cc_math.h"

#include "cc_path_utils.h"


static CcPathLink * _cc_path_one_step__new( CcChessboard * cb,
                                            CcPosDesc pd,
                                            CcTypedStep const * step,
                                            CcSideEffect side_effect,
                                            CcMomentum momentum ) {
    CcPos field = cc_pos_add( pd.pos, step->step, 1 );
    CcPosLink * fields__t = NULL;

    CcPieceEnum piece = CC_PE_None;
    CcTagEnum tag = CC_TE_None;
    CcMomentum m = momentum;

    while ( cc_chessboard_is_pos_on_board( cb, field.i, field.j ) ) {
        CcMaybeBoolEnum result = cc_momentum_calc_next( &m, 1 );

        if ( !CC_MAYBE_BOOL_IS_VALID( result ) ) {
            cc_pos_link_free_all( &fields__t );
            return NULL;
        }

        if ( result == CC_MBE_False ) break; // TODO :: CHECK :: There is not enough momentum to move any further.

        CcPosLink * field__w = cc_pos_link_append( &fields__t, field );
        if ( !field__w ) {
            cc_pos_link_free_all( &fields__t );
            return NULL;
        }

        piece = cc_chessboard_get_piece( cb, field.i, field.j );
        tag = cc_chessboard_get_tag( cb, field.i, field.j );

        if ( piece == CC_PE_None ) {
            field = cc_pos_add( field, step->step, 1 );
        } else {
            CcSideEffect se = side_effect;

            // TODO :: determine se, m(.usage) --> recursive calls


            break;
        }
    }

    CcPathLink * pl__a = cc_path_link__new( side_effect, fields__t, piece, tag, m );
    if ( !pl__a ) {
        cc_pos_link_free_all( &fields__t );
        return NULL;
    }

    return pl__a;
}

CcPathLink * cc_path_tree_one_step__new( CcChessboard * cb,
                                            CcPosDesc pd ) {
    if ( !cb ) return NULL;

    if ( !CC_PIECE_IS_ONE_STEP( pd.piece ) ) return NULL; // TODO :: add Wave
    if ( !cc_chessboard_is_pos_on_board( cb, pd.pos.i, pd.pos.j ) ) return NULL;
    if ( !CC_TAG_IS_ENUMERATOR( pd.tag ) ) return NULL;

    // [!] Piece, and its tag, might not be at pd.pos position on chessboard,
    //     e.g. if already activated (transitioning problem); for everything
    //     else chessboard should be correct.

    CcPos field = pd.pos;

    CcPosLink * fields__t = cc_pos_link__new( field );
    if ( !fields__t ) return NULL;

    CcSideEffect se = cc_side_effect_none();
    CcMomentum m = CC_MOMENTUM_CAST_INITIAL;

    CcPathLink * pl__a = cc_path_link__new( se, fields__t, pd.piece, pd.tag, m );
    if ( !pl__a ) {
        cc_pos_link_free_all( &fields__t );
        return NULL;
    }

    bool sideways_pawns = CC_VARIANT_HAS_SIDEWAYS_PAWNS( cb->type );
    bool is_same_color = cc_pos_piece_are_same_color( field, pd.piece );
    CcTypedStep const * step = NULL;

    while ( cc_iter_piece_steps( pd.piece,
                                 sideways_pawns,
                                 is_same_color, // Only for Unicorn, Centaur is not single-step piece.
                                 CC_SDE_BothDiagonals, // Serpent is not single-step piece, although this is correct for initial step.
                                 CC_STE_None, // No filtering by step types.
                                 &step ) ) {

    }



    return pl__a;
}
