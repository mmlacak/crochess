// Copyright (c) 2021 Mario Mlačak, mmlacak@gmail.com
// Licensed under GNU GPL v3+ license. See LICENSING, COPYING files for details.

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
    Macro expression to evaluate whether given piece is a valid chess piece, and not None.

    @param pe Piece enum.

    @return `true` if valid chess piece, `false` otherwise.
*/
#define CC_PIECE_IS_VALID(pe) ( (pe) != CC_PE_None )

/**
    Macro expression to evaluate whether given pieces are the same.

    @param pe1 A piece enum.
    @param pe2 Other piece enum.

    @return `true` if pieces are the same, `false` otherwise.
*/
#define CC_PIECE_IS_THE_SAME(pe1,pe2) ( (pe2) == (pe2) )

/**
    Macro expression to evaluate whether piece is a Pawn.

    @param pe Piece enum.

    @return `true` if piece is a Pawn, `false` otherwise.
*/
#define CC_PIECE_IS_PAWN(pe) ( ( (pe) == CC_PE_LightPawn ) || ( (pe) == CC_PE_DarkPawn ) )

/**
    Macro expression to evaluate whether piece is a Bishop.

    @param pe Piece enum.

    @return `true` if piece is a Bishop, `false` otherwise.
*/
#define CC_PIECE_IS_BISHOP(pe) ( ( (pe) == CC_PE_LightBishop ) || ( (pe) == CC_PE_DarkBishop ) )

/**
    Macro expression to evaluate whether piece is a Rook.

    @param pe Piece enum.

    @return `true` if piece is a Rook, `false` otherwise.
*/
#define CC_PIECE_IS_ROOK(pe) ( ( (pe) == CC_PE_LightRook ) || ( (pe) == CC_PE_DarkRook ) )

/**
    Macro expression to evaluate whether piece is a King.

    @param pe Piece enum.

    @return `true` if piece is a King, `false` otherwise.
*/
#define CC_PIECE_IS_KING(pe) ( ( (pe) == CC_PE_LightKing ) || ( (pe) == CC_PE_DarkKing ) )

/**
    Macro expression to evaluate whether piece is a Pyramid.

    @param pe Piece enum.

    @return `true` if piece is a Pyramid, `false` otherwise.
*/
#define CC_PIECE_IS_PYRAMID(pe) ( ( (pe) == CC_PE_LightPyramid ) || ( (pe) == CC_PE_DarkPyramid ) )

/**
    Macro expression to evaluate whether piece is a Wave.

    @param pe Piece enum.

    @return `true` if piece is a Wave, `false` otherwise.
*/
#define CC_PIECE_IS_WAVE(pe) ( ( (pe) == CC_PE_LightWave ) || ( (pe) == CC_PE_DarkWave ) )

/**
    Macro expression to evaluate whether piece is None.

    @param pe Piece enum.

    @return `true` if piece is None, `false` otherwise.
*/
#define CC_PIECE_IS_NONE(pe) ( (pe) == CC_PE_None )

/**
    Macro expression to evaluate whether piece is a Star.

    @param pe Piece enum.

    @return `true` if piece is a Star, `false` otherwise.
*/
#define CC_PIECE_IS_STAR(pe) ( ( (pe) == CC_PE_BrightStar ) || ( (pe) == CC_PE_DimStar ) )

/**
    Macro expression to evaluate whether piece is a Monolith.

    @param pe Piece enum.

    @return `true` if piece is a Monolith, `false` otherwise.
*/
#define CC_PIECE_IS_MONOLITH(pe) ( (pe) == CC_PE_Monolith )

/**
    Macro expression to evaluate whether piece is a Starchild.

    @param pe Piece enum.

    @return `true` if piece is a Starchild, `false` otherwise.
*/
#define CC_PIECE_IS_STARCHILD(pe) ( ( (pe) == CC_PE_LightStarchild ) || ( (pe) == CC_PE_DarkStarchild ) )

/**
    Macro expression to evaluate whether piece is weightless.

    @param pe Piece enum.

    @return `true` if piece is weightless, `false` otherwise.
*/
#define CC_PIECE_IS_WEIGHTLESS(pe) ( ( (pe) == CC_PE_LightStarchild ) \
                                  || ( (pe) == CC_PE_DarkStarchild )  \
                                  || ( (pe) == CC_PE_LightWave )      \
                                  || ( (pe) == CC_PE_DarkWave ) )

/**
    Macro expression to evaluate whether piece can be captured.

    @param pe Piece enum.

    @return `true` if piece is disposable, `false` otherwise.
*/
#define CC_PIECE_IS_DISPOSABLE(pe) ( ( (pe) != CC_PE_DimStar )      \
                                  && ( (pe) != CC_PE_DarkKing )     \
                                  && ( (pe) != CC_PE_None )         \
                                  && ( (pe) != CC_PE_LightKing )    \
                                  && ( (pe) != CC_PE_BrightStar )   \
                                  && ( (pe) != CC_PE_Monolith ) )

/**
    Macro expression to evaluate whether Pawn can be promoted to a given piece.

    @param pe Piece enum.

    @return `true` if piece is promote-to, `false` otherwise.
*/
#define CC_PIECE_IS_PROMOTE_TO(pe) ( ( (pe) != CC_PE_DimStar )      \
                                  && ( (pe) != CC_PE_DarkKing )     \
                                  && ( (pe) != CC_PE_DarkPawn )     \
                                  && ( (pe) != CC_PE_None )         \
                                  && ( (pe) != CC_PE_LightPawn )    \
                                  && ( (pe) != CC_PE_LightKing )    \
                                  && ( (pe) != CC_PE_BrightStar )   \
                                  && ( (pe) != CC_PE_Monolith ) )

/**
    Macro expression to evaluate whether piece can be displaced.

    @param pe Piece enum.

    @return `true` if piece is displaceable, `false` otherwise.
*/
#define CC_PIECE_IS_DISPLACEABLE(pe) ( ( (pe) != CC_PE_DimStar )      \
                                    && ( (pe) != CC_PE_DarkKing )     \
                                    && ( (pe) != CC_PE_None )         \
                                    && ( (pe) != CC_PE_LightKing )    \
                                    && ( (pe) != CC_PE_BrightStar )   \
                                    && ( (pe) != CC_PE_Monolith ) )


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
    Function checks if two given pieces are the same type.

    @param pe_1 A piece.
    @param pe_2 The other piece.

    @note
    Type of a piece is what remains after it has been stripped of color (or shade).
    For instance, light and dark Rook are both Rooks, that is their type.

    @return `true` if the same type, `false` otherwise.
*/
bool cc_piece_has_same_type( CcPieceEnum const pe_1, CcPieceEnum const pe_2 );

/**
    Function checks if two given pieces are the same color.

    @param pe_1 A piece.
    @param pe_2 The other piece.

    @note
    Stars have shade (bright, dim), not color, so this function returns `false`
    if any given piece is a Star.

    @return `true` if the same color, `false` otherwise.
*/
bool cc_piece_has_same_color( CcPieceEnum const pe_1, CcPieceEnum const pe_2 );

/**
    Function checks if two given Stars are the same shade.

    @param pe_1 A piece.
    @param pe_2 The other piece.

    @return `true` if Stars are the same shade, `false` otherwise.
*/
bool cc_piece_has_same_shade( CcPieceEnum const pe_1, CcPieceEnum const pe_2 );

/**
    Function returning whether pieces are of opposite color (dark, light),
    or shade (dim, bright), but otherwise the same type.

    @param pe_1 A piece.
    @param pe_2 Another piece.

    @warning
    Pieces with no color (or shade) always return `false`; these are None, Monolith pieces.

    @note
    Under no circumstances are shades and colors opposing each other, they are not comparable.
    Bright Star and dark Bishop are one such an example.

    @return
    `true` if two pieces of the same type are dark and light (or, dim and bright), `false` otherwise.
*/
bool cc_piece_is_opposite( CcPieceEnum const pe_1, CcPieceEnum const pe_2 );

/**
    Function checks whether two pieces has the same owner, i.e.
    if pieces are of the same color (dark, light), not neccessarily the same type.

    @param pe_1 A piece.
    @param pe_2 Another piece.

    @warning
    Pieces with no ownership (i.e. color) always return `false`;
    these are None, Monolith, Stars.

    @return `true` if both pieces are dark or light, `false` otherwise.
*/
bool cc_piece_has_same_owner( CcPieceEnum const pe_1, CcPieceEnum const pe_2 );

/**
    Function checks whether two pieces belongs to different players, i.e.
    if pieces are of opposite color (dark, light), not neccessarily the same type.

    @param pe_1 A piece.
    @param pe_2 Another piece.

    @warning
    Pieces with no ownership (i.e. color) always return `false`;
    these are None, Monolith, Stars.

    @return `true` if one piece is dark and the other is light, `false` otherwise.
*/
bool cc_piece_has_different_owner( CcPieceEnum const pe_1, CcPieceEnum const pe_2 );

/**
    Function tests whether second piece is targetable.

    @param piece A piece, moving about.
    @param target Target piece, one located at destination field of first piece.

    @note
    None piece is always targetable, a piece would always be able to end its
    movement on an empty field.

    @note
    This function handles only capturing, activating target piece,
    and teleporting first piece.

    @warning
    Special cases (like own Pyramid capturing own Pawn for Pawn-sacrifice) has to be
    handled in a separate function(s).

    @return `true` if targetable, `false` otherwise.
*/
// TODO :: DELETE :: MOVE :: REDESIGN
bool cc_piece_is_targetable( CcPieceEnum const piece, CcPieceEnum const target );
// TODO :: DELETE :: MOVE :: REDESIGN

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


#endif /* __CC_PIECE_H__ */
