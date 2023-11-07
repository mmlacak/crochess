// Copyright (c) 2021, 2022 Mario Mlačak, mmlacak@gmail.com
// Licensed under GNU GPL v3+ license. See LICENSING, COPYING files for details.

#include "cc_path_defs.h"

/**
    @file cc_path_defs.c
    @brief Path checkers.
*/


CcPos const CC_STEPS_LIGHT_PAWN[ CC_STEPS_PAWN_SIZE ] = {
    { .i =  0, .j =  1 },

    CC_POS_INVALID,
};

CcPos const CC_STEPS_DARK_PAWN[ CC_STEPS_PAWN_SIZE ] = {
    { .i =  0, .j = -1 },

    CC_POS_INVALID,
};

CcPos const CC_STEPS_CAPTURE_LIGHT_PAWN[ CC_STEPS_CAPTURE_PAWN_SIZE ] = {
    { .i = -1, .j =  1 },
    { .i =  1, .j =  1 },

    CC_POS_INVALID,
};

CcPos const CC_STEPS_CAPTURE_DARK_PAWN[ CC_STEPS_CAPTURE_PAWN_SIZE ] = {
    { .i = -1, .j = -1 },
    { .i =  1, .j = -1 },

    CC_POS_INVALID,
};

CcPos const CC_STEPS_LIGHT_SIDEWAYS_PAWN[ CC_STEPS_SIDEWAYS_PAWN_SIZE ] = {
    { .i =  0, .j =  1 },
    { .i = -1, .j =  0 },
    { .i =  1, .j =  0 },

    CC_POS_INVALID,
};

CcPos const CC_STEPS_DARK_SIDEWAYS_PAWN[ CC_STEPS_SIDEWAYS_PAWN_SIZE ] = {
    { .i =  0, .j = -1 },
    { .i = -1, .j =  0 },
    { .i =  1, .j =  0 },

    CC_POS_INVALID,
};

CcPos const CC_STEPS_KNIGHT[ CC_STEPS_KNIGHT_SIZE ] = {
    { .i =  2, .j =  1 },
    { .i =  1, .j =  2 },

    { .i = -1, .j =  2 },
    { .i = -2, .j =  1 },

    { .i = -2, .j = -1 },
    { .i = -1, .j = -2 },

    { .i =  1, .j = -2 },
    { .i =  2, .j = -1 },

    CC_POS_INVALID,
};

CcPos const CC_STEPS_BISHOP[ CC_STEPS_BISHOP_SIZE ] = {
    { .i =  1, .j =  1 },
    { .i = -1, .j =  1 },
    { .i = -1, .j = -1 },
    { .i =  1, .j = -1 },

    CC_POS_INVALID,
};

CcPos const CC_STEPS_ROOK[ CC_STEPS_ROOK_SIZE ] = {
    { .i =  1, .j =  0 },
    { .i =  0, .j =  1 },
    { .i = -1, .j =  0 },
    { .i =  0, .j = -1 },

    CC_POS_INVALID,
};

CcPos const CC_STEPS_QUEEN[ CC_STEPS_QUEEN_SIZE ] = {
    { .i =  1, .j =  0 },
    { .i =  1, .j =  1 },
    { .i =  0, .j =  1 },
    { .i = -1, .j =  1 },
    { .i = -1, .j =  0 },
    { .i = -1, .j = -1 },
    { .i =  0, .j = -1 },
    { .i =  1, .j = -1 },

    CC_POS_INVALID,
};

CcPos const CC_STEPS_LONG_UNICORN[ CC_STEPS_LONG_UNICORN_SIZE ] = {
    { .i =  4, .j =  1 },
    { .i =  3, .j =  2 },
    { .i =  2, .j =  3 },
    { .i =  1, .j =  4 },

    { .i = -1, .j =  4 },
    { .i = -2, .j =  3 },
    { .i = -3, .j =  2 },
    { .i = -4, .j =  1 },

    { .i = -4, .j = -1 },
    { .i = -3, .j = -2 },
    { .i = -2, .j = -3 },
    { .i = -1, .j = -4 },

    { .i =  1, .j = -4 },
    { .i =  2, .j = -3 },
    { .i =  3, .j = -2 },
    { .i =  4, .j = -1 },

    CC_POS_INVALID,
};

CcPos const CC_STEPS_SERPENT_LEFT[ CC_STEPS_SERPENT_SIZE ] = {
    { .i = -1, .j =  1 },
    { .i =  1, .j = -1 },

    CC_POS_INVALID,
};

CcPos const CC_STEPS_SERPENT_RIGHT[ CC_STEPS_SERPENT_SIZE ] = {
    { .i = -1, .j = -1 },
    { .i =  1, .j =  1 },

    CC_POS_INVALID,
};

CcPos const CC_STEPS_MONOLITH_LEFT[ CC_STEPS_MONOLITH_SIZE ] = {
    { .i =  2, .j =  1 },
    { .i = -1, .j =  2 },
    { .i = -2, .j = -1 },
    { .i =  1, .j = -2 },

    CC_POS_INVALID,
};

CcPos const CC_STEPS_MONOLITH_RIGHT[ CC_STEPS_MONOLITH_SIZE ] = {
    { .i =  1, .j =  2 },
    { .i = -2, .j =  1 },
    { .i = -1, .j = -2 },
    { .i =  2, .j = -1 },

    CC_POS_INVALID,
};

bool cc_is_step_valid( CcPos step, CcPos const array[  ], size_t array_len ) {
    for ( int k = 0; (size_t)k < array_len; ++k ) {
        CcPos p = array[ k ];

        if ( cc_pos_is_equal( step, p ) )
            return true;
    }

    return false;
}


//
// Linked list of linked list of positions.

CcPathLink * cc_path_link__new( CcPosLink ** restrict pos__n ) {
    if ( !pos__n ) return NULL;
    if ( !*pos__n ) return NULL;

    CcPathLink * pl__a = malloc( sizeof( CcPathLink ) );
    if ( !pl__a ) return NULL;

    pl__a->pos_ll = *pos__n; // Transfering ownership.
    *pos__n = NULL; // Preventing usage from old pointer holding ownership.

    pl__a->next = NULL;

    return pl__a;
}

CcPathLink * cc_path_link_append( CcPathLink * restrict path_link__io,
                                  CcPosLink ** restrict pos__n ) {
    if ( !path_link__io ) return NULL;

    CcPathLink * pl__t = cc_path_link__new( pos__n );
    if ( !pl__t ) return NULL;

    CcPathLink * pl = path_link__io;

    CC_FASTFORWARD( pl );
    pl->next = pl__t; // append // Ownership transfer --> pl__t is now weak pointer.

    return pl__t;
}

CcPathLink * cc_path_link_expand( CcPathLink ** restrict path_link__io,
                                  CcPosLink ** restrict pos__n ) {

    if ( !path_link__io ) return NULL;

    CcPathLink * pl__w = NULL;

    if ( !*path_link__io )
        *path_link__io = pl__w = cc_path_link__new( pos__n );
    else
        pl__w = cc_path_link_append( *path_link__io, pos__n );

    return pl__w;
}

bool cc_path_link_free_all( CcPathLink ** restrict path_link__f ) {
    if ( !path_link__f ) return false;
    if ( !*path_link__f ) return true;

    bool result = true;
    CcPathLink * pl = *path_link__f;
    CcPathLink * tmp = NULL;

    while ( pl ) {
        tmp = pl->next;

        result = cc_pos_link_free_all( &(pl->pos_ll) ) && result;

        CC_FREE( pl );
        pl = tmp;
    }

    *path_link__f = NULL;
    return result;
}

size_t cc_path_link_len( CcPathLink * restrict path_link ) {
    if ( !path_link ) return 0;

    size_t len = 0;
    CcPathLink * pl = path_link;

    while ( pl ) {
        ++len;
        pl = pl->next;
    }

    return len;
}


bool cc_is_pawn_step( CcVariantEnum type, CcPieceEnum piece, CcPos step ) {
    if ( cc_variant_has_sideways_pawns( type ) ) {
        if ( cc_piece_is_light( piece ) )
            return CC_LIGHT_SIDEWAYS_PAWN_STEP_IS_VALID( step );
        else
            return CC_DARK_SIDEWAYS_PAWN_STEP_IS_VALID( step );
    } else {
        if ( cc_piece_is_light( piece ) )
            return CC_LIGHT_PAWN_STEP_IS_VALID( step );
        else
            return CC_DARK_PAWN_STEP_IS_VALID( step );
    }
}

bool cc_is_pawn_capture_step( CcVariantEnum type, CcPieceEnum piece, CcPos step ) {
    if ( cc_piece_is_light( piece ) )
        return CC_LIGHT_PAWN_CAPTURE_STEP_IS_VALID( step );
    else
        return CC_DARK_PAWN_CAPTURE_STEP_IS_VALID( step );
}
