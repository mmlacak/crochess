// Copyright (c) 2024 Mario Mlačak, mmlacak@gmail.com
// Licensed under GNU GPL v3+ license. See LICENSING, COPYING files for details.

#include "cc_checks.h"

#include "cc_path_single_step.h"


// bool cc_path_single_step( CcChessboard * cb,
//                           CcPieceType piece,
//                           CcTagType tag,
//                           CcPieceType activator,
//                           CcPos from_pos,
//                           cc_uint_t momentum,
//                           bool is_accumulating_momentum /* ,
//                           CcPathLink ** path__o */ ) {
//     if ( !cb ) return false;
//     // if ( !path__o ) return false;
//     // if ( *path__o ) return false;
//
//     if ( !cc_chessboard_is_pos_on_board( cb, from_pos.i, from_pos.j ) ) return false;
//
//     CcPieceType from_piece = cc_chessboard_get_piece( cb, from_pos.i, from_pos.j );
//     // CcPathLink * path__t = NULL;
//
//     if ( CC_PIECE_IS_TELEPORTER( from_piece ) ) {
//         // TODO
//     } else {
//         if ( CC_PIECE_IS_PAWN( piece ) ) {
//             // if ( !cc_path_pawn( cb, piece, tag, from_pos, momentum, is_accumulating_momentum, NULL, &path__t ) ) return false;
//             //
//             // *path__o = path__t;
//             // path__t = NULL;
//             return true;
//         } else if ( CC_PIECE_IS_WAVE( piece ) ) {
//             if ( !CC_PIECE_IS_ACTIVATOR( activator ) ) return false;
//
//             // TODO
//         } else
//             return false;
//     }
//
//     // TODO
//
//     return false;
// }
