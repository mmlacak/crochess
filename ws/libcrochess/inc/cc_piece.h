// Copyright (c) 2021 Mario Mlačak, mmlacak@gmail.com
// Licensed under 3-clause (modified) BSD license. See LICENSE for details.

#ifndef __CC_PIECE_H__
#define __CC_PIECE_H__

#include <stdbool.h>


typedef enum CcPieceEnum
{
    CC_PE_DimStar = -15,

    CC_PE_DarkStarchild,
    CC_PE_DarkShaman,
    CC_PE_DarkSerpent,
    CC_PE_DarkCentaur,
    CC_PE_DarkWave,
    CC_PE_DarkUnicorn,
    CC_PE_DarkPyramid,
    CC_PE_DarkPegasus,
    CC_PE_DarkKing,
    CC_PE_DarkQueen,
    CC_PE_DarkRook,
    CC_PE_DarkBishop,
    CC_PE_DarkKnight,
    CC_PE_DarkPawn,

    CC_PE_None = 0,

    CC_PE_LightPawn,
    CC_PE_LightKnight,
    CC_PE_LightBishop,
    CC_PE_LightRook,
    CC_PE_LightQueen,
    CC_PE_LightKing,
    CC_PE_LightPegasus,
    CC_PE_LightPyramid,
    CC_PE_LightUnicorn,
    CC_PE_LightWave,
    CC_PE_LightCentaur,
    CC_PE_LightSerpent,
    CC_PE_LightShaman,
    CC_PE_LightStarchild,

    CC_PE_BrightStar,

    CC_PE_Monolith,
} CcPieceEnum;


CcPieceEnum cc_piece_from_symbol(char const c, bool const is_light);
CcPieceEnum cc_piece_opposite(CcPieceEnum const pe);

char cc_piece_as_char(CcPieceEnum const pe);
char cc_piece_symbol(CcPieceEnum const pe);
char const * cc_piece_label(CcPieceEnum const pe);

bool cc_piece_is_dark(CcPieceEnum const pe);
bool cc_piece_is_light(CcPieceEnum const pe);
bool cc_piece_is_pawn(CcPieceEnum const pe);
bool cc_piece_is_figure(CcPieceEnum const pe);
bool cc_piece_is_none(CcPieceEnum const pe);

bool cc_piece_is_opposite_color(CcPieceEnum const pe1, CcPieceEnum const pe2);
bool cc_piece_is_opposite_shade(CcPieceEnum const pe1, CcPieceEnum const pe2);
bool cc_piece_is_teleporter(CcPieceEnum const pe);


#endif /* __CC_PIECE_H__ */
