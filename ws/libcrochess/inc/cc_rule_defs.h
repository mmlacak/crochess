// Copyright (c) 2021 Mario Mlačak, mmlacak@gmail.com
// Licensed under GNU GPL v3+ license. See LICENSING, COPYING files for details.

#ifndef __CC_RULE_DEFS_H__
#define __CC_RULE_DEFS_H__

#include "cc_piece.h"


typedef struct CcPos {
    int i;
    int j;
} CcPos;

CcPos cc_pos( int const i, int const j );


typedef struct CcPiecePos {
    CcPieceEnum piece;
    int i;
    int j;
} CcPiecePos;

CcPiecePos cc_piece_pos( CcPieceEnum const piece, int const i, int const j );

CcPiecePos cc_piece_pos_from_pos( CcPieceEnum const piece, CcPos const pos );
CcPos cc_pos_from_piece_pos( CcPiecePos const piece_pos );


#endif /* __CC_RULE_DEFS_H__ */
