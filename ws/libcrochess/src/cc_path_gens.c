// Copyright (c) 2021, 2022 Mario Mlačak, mmlacak@gmail.com
// Licensed under GNU GPL v3+ license. See LICENSING, COPYING files for details.

#include "cc_path_defs.h"
#include "cc_path_gens.h"

/**
    @file cc_path_gens.c
    @brief Path generators.
*/


bool cc_iter_piece_pos( CcChessboard * restrict cb_before_activation,
                        CcPos expected,
                        CcPieceEnum piece,
                        bool include_opponent,
                        CcPos * restrict pos__io ) {
    if ( !cb_before_activation ) return false;
    if ( !pos__io ) return false;

    int size = (int)cb_before_activation->size;
    CcPos pos = *pos__io;

    // Next position to check.
    if ( !cc_chessboard_is_pos_on_board( cb_before_activation, pos.i, pos.j ) )
        pos = CC_POS_CAST_ORIGIN_FIELD;
    else if ( pos.j < size - 1 )
        pos = CC_POS_CAST( pos.i, pos.j + 1 );
    else
        pos = CC_POS_CAST( pos.i + 1, 0 );

    bool is_comparable = cc_pos_is_valid( expected ) ||
                         cc_pos_is_disambiguation( expected );

    for ( int i = pos.i; i < size; ++i ) {
        for ( int j = pos.j; j < size; ++j ) {
            CcPieceEnum pe = cc_chessboard_get_piece( cb_before_activation, i, j );

            if ( CC_PIECE_IS_THE_SAME( pe, piece ) ||
                    ( include_opponent && cc_piece_is_opposite( pe, piece ) ) ) {
                CcPos current = CC_POS_CAST( i, j );

                if ( ( !is_comparable ) ||
                       cc_pos_is_congruent( expected, current ) ) {
                    *pos__io = current;
                    return true;
                }
            }
        }

        pos.j = 0;
    }

    *pos__io = CC_POS_CAST_INVALID;
    return false;
}


bool cc_is_step_capture( CcPieceEnum activator,
                         CcPieceEnum piece,
                         CcPos step,
                         CcPos step_2 ) {
    if ( !CC_PIECE_IS_VALID( piece ) ) return false;

    if ( !cc_pos_is_valid( step ) ) return false;
    if ( cc_pos_is_static_step( step ) ) return false;

    if ( CC_PIECE_IS_PAWN( piece ) ) {
        if ( cc_piece_is_light( piece ) )
            return CC_LIGHT_PAWN_CAPTURE_STEP_IS_VALID( step );
        else
            return CC_DARK_PAWN_CAPTURE_STEP_IS_VALID( step ); }
    else if ( CC_PIECE_IS_SHAMAN( piece ) ) {
        if ( cc_piece_is_light( piece ) )
            return CC_LIGHT_SHAMAN_CAPTURE_STEP_IS_VALID( step );
        else
            return CC_DARK_SHAMAN_CAPTURE_STEP_IS_VALID( step ); }
    else if ( CC_PIECE_IS_WAVE( piece ) )
        return cc_is_step_capture( CC_PE_None, activator, step, step_2 );
    else if ( CC_PIECE_IS_MONOLITH( piece ) )
        return false;
    else if ( CC_PIECE_IS_STAR( piece ) )
        return false;
    else if ( CC_PIECE_IS_STARCHILD( piece ) )
        return false;

    return true;
}

bool cc_is_step_miracle( CcPieceEnum piece, CcPos step ) {
    if ( CC_PIECE_IS_STARCHILD( piece ) )
        return CC_STARCHILD_MIRACLE_STEP_IS_VALID( step );

    return false;
}

bool cc_is_step_shamans_capture( CcPieceEnum piece, CcPos step ) {
    return ( ( ( piece == CC_PE_LightShaman ) &&
               CC_LIGHT_SHAMAN_CAPTURE_STEP_IS_VALID( step ) ) ||
             ( ( piece == CC_PE_DarkShaman ) &&
               CC_DARK_SHAMAN_CAPTURE_STEP_IS_VALID( step ) ) );
}


bool cc_is_the_same_color( CcPieceEnum piece, CcPos pos ) {
    if ( cc_piece_is_light( piece ) && CC_IS_FIELD_LIGHT( pos.i, pos.j ) )
        return true;

    if ( cc_piece_is_dark( piece ) && CC_IS_FIELD_DARK( pos.i, pos.j ) )
        return true;

    return false;
}
