// Copyright (c) 2024 Mario Mlačak, mmlacak@gmail.com
// Licensed under GNU GPL v3+ license. See LICENSING, COPYING files for details.

#ifndef __CC_POS_UTILS_H__
#define __CC_POS_UTILS_H__

#include "cc_defines.h"
#include "cc_piece.h"
#include "cc_chessboard.h"
#include "cc_pos.h"


CcPosDesc cc_convert_pos_to_pos_desc( CcChessboard * cb, CcPos pos );

// TODO :: accumulating --> usage :: true --> +, false --> -, void --> ~
//
// bool cc_calc_momentum_for_next_step( cc_uint_t * momentum__io, CcMaybeBoolEnum accumulating );
//
// bool cc_calc_if_accumulating_momentum( CcPieceType piece,
//                                        CcTagType tag,
//                                        CcMaybeBoolEnum * accumulating__o );
//
// TODO :: accumulating --> usage :: true --> +, false --> -, void --> ~


bool cc_iter_piece_pos( CcChessboard * cb,
                        CcPos expected__d,
                        CcPieceType piece,
                        bool include_opponent,
                        CcPos * pos__io );


#endif /* __CC_POS_UTILS_H__ */
