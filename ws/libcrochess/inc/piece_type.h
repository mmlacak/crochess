// Copyright (c) 2021 Mario Mlačak, mmlacak@gmail.com
// Licensed under 3-clause (modified) BSD license. See LICENSE for details.

#ifndef __PIECE_TYPE_H__
#define __PIECE_TYPE_H__


typedef enum PieceType
{
    PT_DimStar = -15,

    PT_DarkStarchild,
    PT_DarkShaman,
    PT_DarkSerpent,
    PT_DarkCentaur,
    PT_DarkWave,
    PT_DarkUnicorn,
    PT_DarkPyramid,
    PT_DarkPegasus,
    PT_DarkKing,
    PT_DarkQueen,
    PT_DarkRook,
    PT_DarkBishop,
    PT_DarkKnight,
    PT_DarkPawn,

    PT_None = 0,

    PT_LightPawn,
    PT_LightKnight,
    PT_LightBishop,
    PT_LightRook,
    PT_LightQueen,
    PT_LightKing,
    PT_LightPegasus,
    PT_LightPyramid,
    PT_LightUnicorn,
    PT_LightWave,
    PT_LightCentaur,
    PT_LightSerpent,
    PT_LightShaman,
    PT_LightStarchild,

    PT_BrightStar,

    PT_Monolith,
} PieceType;


PieceType piece_from_symbol(char const c, bool const is_light);
PieceType opposite_piece(PieceType const pt);

char piece_as_char(PieceType const pt);
char piece_symbol(PieceType const pt);
char const * const piece_label(PieceType const pt);

bool is_piece_dark(PieceType const pt);
bool is_piece_light(PieceType const pt);
bool is_piece_pawn(PieceType const pt);
bool is_piece_figure(PieceType const pt);
bool is_piece_none(PieceType const pt);


#endif /* __PIECE_TYPE_H__ */
