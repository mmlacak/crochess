// Copyright (c) 2024, 2025 Mario Mlačak, mmlacak@gmail.com
// Licensed under GNU GPL v3+ license. See LICENSING, COPYING files for details.

// #include "cc_defines.h"
// #include "cc_math.h"

#include "cc_path_utils.h"


CcPathLink * cc_path_single_step__new( CcChessboard * cb,
                                       CcPosDesc pd,
                                       CcTypedStep step ) {
    if ( !cb ) return NULL;

    if ( !CC_PIECE_IS_VALID( pd.piece ) ) return NULL;
    if ( !cc_chessboard_is_pos_on_board( cb, pd.pos.i, pd.pos.j ) ) return NULL;
    if ( !CC_TAG_IS_ENUMERATOR( pd.tag ) ) return NULL;
    if ( !CC_TYPED_STEP_IS_VALID( step ) ) return NULL;

    CcPos field = pd.pos;
    CcPosLink * fields__t = NULL;
    CcSideEffect se = cc_side_effect_none();

    // TODO :: REDO
    //
    // while ( cc_chessboard_is_pos_on_board( cb, field.i, field.j ) ) {
    //     // TODO :: check if empty field,
    //     //      :: check step type, e.g. movement only -->
    //     //      :: interactions with encoutered piece --> set se
    //
    //     if ( !cc_pos_link_append( &fields__t, field ) ) {
    //         cc_pos_link_free_all( &fields__t );
    //         return NULL;
    //     }
    //
    //     field = cc_pos_add( field, step.step, 1 );
    // }
    //
    // CcPathLink * segment__a = cc_path_link__new( fields__t, se );
    // if ( !segment__a ) {
    //     cc_pos_link_free_all( &fields__t );
    //     return NULL;
    // }
    //
    // return segment__a;
    //
    // TODO :: REDO

    return NULL; // TODO :: FIX
}
