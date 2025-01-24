// Copyright (c) 2024, 2025 Mario Mlačak, mmlacak@gmail.com
// Licensed under GNU GPL v3+ license. See LICENSING, COPYING files for details.

// #include "cc_defines.h"
// #include "cc_math.h"

#include "cc_path_utils.h"


CcPathLink * cc_path_tree_single_step__new( CcChessboard * cb,
                                            CcPosDesc pd ) {
    if ( !cb ) return NULL;

    if ( !CC_PIECE_IS_SINGLE_STEP( pd.piece ) ) return NULL; // TODO :: add Wave
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
                                 is_same_color,
                                 CC_SDE_BothDiagonals,
                                 CC_STE_None,
                                 &step ) ) {

    }



    return pl__a;
}
