// Copyright (c) 2024, 2025 Mario Mlačak, mmlacak@gmail.com
// Licensed under GNU GPL v3+ license. See LICENSING, COPYING files for details.

// // #include "cc_defines.h"
// // #include "cc_math.h"

// #include "cc_pos_utils.h"
// #include "cc_path_utils.h"
// #include "cc_checks.h"


// TODO :: DELETE
//
// CcMaybeBoolEnum cc_path_side_effect( CcChessboard * cb,
//                                      CcPosDesc moving,
//                                      CcPosDesc encounter,
//                                      CcSideEffect * side_effect__io ) {
//     if ( !side_effect__io ) return CC_MBE_Void;
//     if ( !CC_SIDE_EFFECT_TYPE_IS_ENUMERATOR( *side_effect__io ) ) return CC_MBE_Void;
//
//     if ( !CC_PIECE_IS_VALID( moving.piece ) ) return CC_MBE_Void;
//     if ( !CC_PIECE_IS_ENUMERATOR( encounter.piece ) ) return CC_MBE_Void;
//
//     if ( !CC_TAG_IS_ENUMERATOR( moving.tag ) ) return CC_MBE_Void;
//     if ( !CC_TAG_IS_ENUMERATOR( encounter.tag ) ) return CC_MBE_Void;
//
//     if ( !cc_chessboard_is_pos_on_board( cb, moving.pos.i, moving.pos.j ) ) return CC_MBE_Void;
//     if ( !cc_chessboard_is_pos_on_board( cb, encounter.pos.i, encounter.pos.j ) ) return CC_MBE_Void;
//
//     if ( *side_effect__io == CC_SETE_None ) {
//         *side_effect__io = CC_SETE_Capture;
//
//         if ( CC_PIECE_CAN_CAPTURE( moving ) &&
//              CC_PIECE_CAN_BE_CAPTURED( encounter ) ) return CC_MBE_True;
//     }
//
//     if ( *side_effect__io == CC_SETE_Capture ) {
//         *side_effect__io = CC_SETE_Displacement;
//
//         if ( CC_PIECE_CAN_DISPLACE( moving ) &&
//              CC_PIECE_CAN_BE_DISPLACED( encounter ) ) return CC_MBE_True;
//
//         // Trance-journey has to be handled separately.
//     }
//
//     if ( *side_effect__io == CC_SETE_Displacement ) {
//         *side_effect__io = CC_SETE_EnPassant;
//
//         if ( CC_PIECE_CAN_CAPTURE_EN_PASSANT( moving ) &&
//              CC_PIECE_CAN_BE_CAPTURED_EN_PASSANT( encounter ) ) return CC_MBE_True;
//     }
//
//     if ( *side_effect__io == CC_SETE_EnPassant ) {
//         *side_effect__io = CC_SETE_Castle;
//
//         if ( CC_TAG_IS_CAN_CASTLE( moving ) &&
//              CC_TAG_IS_CAN_CASTLE( encounter ) ) return CC_MBE_True;
//     }
//
//     if ( *side_effect__io == CC_SETE_Castle ) {
//         *side_effect__io = CC_SETE_Promotion;
//
//         if ( CC_PAWN_CAN_BE_PROMOTED( moving ) ) return CC_MBE_True;
//         else if ( CC_PIECE_CAN_PROMOTE( moving ) &&
//                   CC_PAWN_CAN_BE_PROMOTED( encounter ) ) return CC_MBE_True;
//     }
//
//
//     // TODO
//
//
//     return CC_MBE_Void; // TODO :: FIX
// }
//
// TODO :: DELETE
