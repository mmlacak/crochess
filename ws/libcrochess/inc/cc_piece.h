// Copyright (c) 2021 Mario Mlačak, mmlacak@gmail.com
// Licensed under 3-clause (modified) BSD license. See LICENSE for details.

#ifndef __CC_PIECE_H__
#define __CC_PIECE_H__

#include <stdbool.h>

/**
    @file cc_piece.h
    @brief Piece enumeration, and related functions.

    In this text, piece symbol is uppercase char, representing piece type, e.g. `N`.

    Piece symbol on its own does not contain information if a particular piece is
    light/bright or dark/dim.

    Piece char is representation of a piece type, it is lowercase if piece is dark/dim
    pieces, otherwise it's uppercase.

    For example, piece char for dark Knight is `n`, while for light Knight it is `N`;
    piece symbol for both is the same, `N`.
*/

/**
    Enumerates all pieces, used in all variants.

    Piece `CC_PE_None` is used for e.g. empty on-board fields, any off-board field.
*/
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

    CC_PE_BrightStar = 15,

    CC_PE_Monolith,
} CcPieceEnum;


/**
    Function interface, i.e. function pointer type.

    @param pe Piece enum item.

    @return Char, either a piece symbol, or a piece char.
*/
typedef char (*cc_piece_fp_char_value_t)( CcPieceEnum const pe );

/**
    Function returning piece enum, based on a piece symbol, and a flag.

    @param symbol Piece symbol, uppercase char. It is taken verbatim, i.e. not converted to uppercase char.
    @param is_light Whether piece is light/bright (`true`), or dark/dim (`false`).

    @return Piece enum if valid piece symbol passed, otherwise `CC_PE_None`.
*/
CcPieceEnum cc_piece_from_symbol( char const symbol, bool const is_light );

/**
    Function returns whether given character is a valid chess piece symbol.

    @param c A character.

    @return `true` if given character is a valid chess piece symbol, `false` otherwise.
*/
bool cc_piece_is_symbol( char const c );

/**
    Function returns whether given piece enum value is a valid chess piece.

    @param pe A piece enum value.

    @return `true` if given enum is a valid chess piece, `false` otherwise.
*/
bool cc_piece_is_valid( CcPieceEnum const pe );

/**
    Function returning piece enum in opposite color (shade) to argument.

    @param pe Piece enum argument.

    @return Piece enum, dark (dim) piece converted to light (bright) piece, and vice versa.
            Monolith, None pieces are returned unchanged.
*/
CcPieceEnum cc_piece_opposite( CcPieceEnum const pe );

/**
    Function returning piece char, based on piece enum.

    @param pe Piece enum.

    @return Piece char, lowercase if piece is dark (dim), uppercase if piece is light (bright),
            space otherwise.
*/
char cc_piece_as_char( CcPieceEnum const pe );

/**
    Function returning piece label.

    @param pe Piece enum.

    @return Piece label, capitalized name of a piece. Piece label is the same for dark (dim)
            and light (bright) pieces. For None piece, label is empty string.
*/
char const * cc_piece_label( CcPieceEnum const pe );

/**
    Function returning piece symbol, based on piece enum.

    @param pe Piece enum.

    @return Piece symbol, uppercase char if valid piece, space otherwise (if piece is None).
*/
char cc_piece_symbol( CcPieceEnum const pe );

/**
    Function returning Pawn to which piece can be demoted, or None if piece can't be demoted.

    @param pe Piece enum.

    @return Dark Pawn if dark piece, light Pawn if piece is light, otherwise `CC_PE_None`.
*/
CcPieceEnum cc_piece_demoting_to( CcPieceEnum const pe );

/**
    Function returning whether piece is dark.

    @param pe Piece enum.
    @param include_stars Flag, whether to include dim Star.

    @return `true` if piece is dark, `false` otherwise.
*/
bool cc_piece_is_dark( CcPieceEnum const pe, bool include_stars );

/**
    Function returning whether piece is light.

    @param pe Piece enum.
    @param include_stars Flag, whether to include bright Star.

    @return `true` if piece is light, `false` otherwise.
*/
bool cc_piece_is_light( CcPieceEnum const pe, bool include_stars );

/**
    Function returning whether piece is a Pawn.

    @param pe Piece enum.

    @return `true` if piece is a Pawn, regardless if light or dark; `false` otherwise.
*/
bool cc_piece_is_pawn( CcPieceEnum const pe );

/**
    Function returning whether piece is None.

    @param pe Piece enum.

    @return `true` if piece is None, `false` otherwise.
*/
bool cc_piece_is_none( CcPieceEnum const pe );

/**
    Function returning whether piece is a Star.

    @param pe Piece enum.

    @return `true` if piece is a Star, regardless if bright or dim; `false` otherwise.
*/
bool cc_piece_is_star( CcPieceEnum const pe );

/**
    Function returning whether piece is a Monolith.

    @param pe Piece enum.

    @return `true` if piece is a Monolith; `false` otherwise.
*/
bool cc_piece_is_monolith( CcPieceEnum const pe );

/**
    Function checks if two given pieces are the same type, or are the same.

    @param pe_1 A piece.
    @param pe_2 The other piece.
    @param strict Flag, whether to check pieces are the same (if `true`),
                  or have the same type (if `false`).

    @note
    Type of a piece is what remains after it has been stripped of color (or shade).
    For instance, light and dark Rook are both Rooks, that is their type.

    @return `true` if the same (type), `false` otherwise.
*/
bool cc_piece_is_the_same_type( CcPieceEnum const pe_1, CcPieceEnum const pe_2, bool const strict );

/**
    Function returning whether pieces are of opposite color (dark, light),
    or shade (dim, bright).

    @param pe_1 A piece.
    @param pe_2 Another piece.
    @param strict Flag, is comparison strict.

    @note
    Strict comparison means types of pieces are the same, and only their colors (or shades) are different.
    For instance, light Bishop and dark Bishop are strict opposites, whereas light Bishop and dark Rook
    are only opposites, but not strict opposites.

    @note
    Under no circumstances are shades and colors opposing each other, they are not comparable.
    Bright Star and dark Bishop are one such an example.

    @return `true` if one piece is dark (dim) and the other is light (bright), `false` otherwise.
*/
bool cc_piece_is_opposite( CcPieceEnum const pe_1, CcPieceEnum const pe_2, bool const strict );

/**
    Function returning whether piece is teleporting piece.

    @param pe Piece enum.

    @return `true` if piece is Monolith, or a star; `false` otherwise.
*/
bool cc_piece_is_teleporter( CcPieceEnum const pe );

/**
    Function returning whether piece is lightweight.

    @param pe Piece enum.

    @return `true` if piece is lightweight; `false` otherwise.
*/
bool cc_piece_is_lightweight( CcPieceEnum const pe );

/**
    Function returning whether piece is a figure.

    By the book, figure is any piece, except Pawn. For practical reasons,
    additional flags to filter out Monolith and stars are included.

    @param pe Piece enum.
    @param include_monolith Whether to include Monolith.
    @param include_stars Whether to include stars.

    @return `true` if piece is a figure, `false` otherwise.
*/
bool cc_piece_is_figure( CcPieceEnum const pe,
                         bool const include_monolith,
                         bool const include_stars );

/**
    Function returns piece coerced to a given color (or shade, if it's a Star).

    @param pe A piece.
    @param to_light A flag, coerce piece either to light (or bright) if `true`,
                    or to dark (or dim) if `false`.

    @return A piece coerced to a given color (or shade).
*/
CcPieceEnum cc_piece_coerce( CcPieceEnum const pe, bool const to_light );

#endif /* __CC_PIECE_H__ */
