// Copyright (c) 2021 Mario Mlačak, mmlacak@gmail.com
// Licensed under GNU GPL v3+ license. See LICENSING, COPYING files for details.

#include "cc_defines.h"
#include "cc_rule_defs.h"


CcPos cc_pos( int const i, int const j )
{
    CcPos pos = { .i = i, .j = j };
    return pos;
}

CcPos cc_pos_empty()
{
    return cc_pos( CC_OFF_BOARD_COORD, CC_OFF_BOARD_COORD );
}


CcPiecePos cc_piece_pos( CcPieceEnum const piece, int const i, int const j )
{
    CcPiecePos pp = { .piece = piece, .i = i, .j = j };
    return pp;
}

CcPiecePos cc_piece_pos_empty()
{
    return cc_piece_pos( CC_PE_None, CC_OFF_BOARD_COORD, CC_OFF_BOARD_COORD );
}

CcPiecePos cc_piece_pos_from_pos( CcPieceEnum const piece, CcPos const pos )
{
    return cc_piece_pos( piece, pos.i, pos.j );
}

CcPos cc_pos_from_piece_pos( CcPiecePos const piece_pos )
{
    return cc_pos( piece_pos.i, piece_pos.j );
}
