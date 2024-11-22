// Copyright (c) 2024 Mario Mlačak, mmlacak@gmail.com
// Licensed under GNU GPL v3+ license. See LICENSING, COPYING files for details.

#ifndef __CC_POS_UTILS_H__
#define __CC_POS_UTILS_H__

#include "cc_defines.h"
#include "cc_piece.h"
#include "cc_chessboard.h"
#include "cc_pos.h"


CcPosDesc cc_convert_pos_to_pos_desc( CcChessboard * cb, CcPos pos );

bool cc_calc_momentum_for_next_step( cc_uint_t * momentum__io, CcMaybeBoolEnum accumulating );

bool cc_calc_if_accumulating_momentum( CcPieceType piece,
                                       CcTagType tag,
                                       CcMaybeBoolEnum * accumulating__o );

// // TODO :: REDO
// CcPathLink * cc_build_path_segment__new( CcChessboard * cb,
//                                          CcPos pos,
//                                          cc_uint_t momentum,
//                                          CcMaybeBoolEnum accumulating,
//                                          CcTypedStepLink * steps );


// TODO :: cc_iter_typed_steps() --> iter by piece, tag, step type filter, chessboard, ...
//         similar to cc_calc_momentum_for_next_step() --> cc_check_momentum_for_next_step()
//
// bool cc_iter_piece_steps( CcChessboard * cb,
//                           CcPieceType piece,
//                           CcTagType tag,
//                           cc_uint_t momentum,
//                           CcStepTypeEnum filter__d,
//                           CcTypedStep const ** step__iod );

bool cc_iter_piece_pos( CcChessboard * cb,
                        CcPos expected__d,
                        CcPieceType piece,
                        bool include_opponent,
                        CcPos * pos__io );


#endif /* __CC_POS_UTILS_H__ */
