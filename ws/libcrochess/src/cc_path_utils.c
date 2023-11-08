// Copyright (c) 2021, 2022, 2023 Mario Mlačak, mmlacak@gmail.com
// Licensed under GNU GPL v3+ license. See LICENSING, COPYING files for details.

#include "cc_path_defs.h"
#include "cc_path_utils.h"

/**
    @file cc_path_utils.c
    @brief Path utils.
*/


// TODO :: create new pos link, extended with another
//         check merge point is the same (last pos in 1st list, first pos in 2nd list)

// TODO :: in a ppt list, check piece, tag for all pos on-board

// TODO :: in a ppt list, fill-in piece, tag for all pos on-board

// TODO :: create new ppt link, extended with another
//         check merge point is the same (last ppt in 1st list, first ppt in 2nd list)
//         check piece, tag for all pos on-board

CcPptLink * cc_convert_pos_link_to_ppt_link__new( CcChessboard * restrict cb,
                                                  CcPosLink * restrict pos_link )
{
    if ( !cb ) return NULL;
    if ( !pos_link ) return NULL;

    CcPptLink * ppt_link__a = NULL;
    CcPosLink * p = pos_link;

    while ( p ) {
        CcPieceEnum pe = cc_chessboard_get_piece( cb, p->pos.i, p->pos.j );
        CcTagEnum te = cc_chessboard_get_tag( cb, p->pos.i, p->pos.j );
        CcPosPieceTag ppt = cc_pos_piece_tag( p->pos, pe, te );

        if ( !cc_ppt_link_append( &ppt_link__a, ppt ) ) {
            cc_ppt_link_free_all( &ppt_link__a );
            return NULL;
        }

        p = p->next;
    }

    return ppt_link__a;
}


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


// TODO ::
//
// bool cc_steps_are_congruent( CcSteps * restrict steps,
//                              CcPptLink * restrict positions )
// {
//     if ( !steps ) return false;
//     if ( !positions ) return false;

//     CcPptLink * p = positions;
//     CcSteps * prev_s = NULL;
//     CcSteps * s = steps;

//     CC_REWIND( s );

//     while ( s )
//     {
//         switch ( s->step_link )
//         {
//             case CC_SLE_Start :
//             {
//                 if ( !s->prev && ( p == positions ) ) // First step, and position?
//                 {
//                     if ( !cc_pos_is_congruent( s->pos, p->pos ) )
//                         return false;
//                 }
//                 else
//                     return false;

//                 break;
//             }

//             case CC_SLE_Reposition :
//             case CC_SLE_Next :
//             {
//                 if ( p && p->next )
//                 {
//                     // If reposition, position before must be the first one.
//                     if ( ( s->step_link == CC_SLE_Reposition ) &&
//                          ( p != positions ) )
//                             return false;

//                     p = p->next;

//                     if ( !cc_pos_is_equal( s->pos, p->pos ) )
//                         return false;
//                 }
//                 else
//                     return false;

//                 break;
//             }

//             case CC_SLE_Distant :
//             {
//                 if ( p )
//                     p = p->next;
//                 else
//                     return false;

//                 bool found = false;

//                 while ( p )
//                 {
//                     if ( cc_pos_is_equal( s->pos, p->pos ) )
//                     {
//                         found = true;
//                         break;
//                     }

//                     p = p->next;
//                 }

//                 if ( !found )
//                     return false;

//                 break;
//             }

//             case CC_SLE_Destination :
//             {
//                 if ( s->next ) return false; // Not the last one?

//                 CC_FASTFORWARD( p );

//                 if ( !cc_pos_is_equal( s->pos, p->pos ) )
//                     return false;

//                 break;
//             }

//             case CC_SLE_None :
//             default :
//                 return false;
//         }

//         prev_s = s;
//         s = s->next;
//     }

//     return ( prev_s && !prev_s->next && p && !p->next );
// }
//
// TODO ::
