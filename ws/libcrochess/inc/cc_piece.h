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
    Macro expression to evaluate whether given piece is a valid chess piece.

    @param pe Piece enum, one of `CcPieceEnum` values.

    @see CcPieceEnum

    @return `true` if valid chess piece, `false` otherwise.
*/
#define CC_PIECE_IS_VALID(pe) ( ( CC_PE_DimStar <= (pe) ) && ( (pe) <= CC_PE_Monolith ) )

/**
    Macro expression to evaluate whether given pieces are the same.

    @param pe1 A piece enum.
    @param pe2 Other piece enum.

    @return `true` if pieces are the same, `false` otherwise.
*/
#define CC_PIECE_IS_EQUAL(pe1,pe2) ( (pe1) == (pe2) )

/**
    Macro expression to evaluate whether piece is None.

    @param pe Piece enum, one of `CcPieceEnum` values.

    @see CcPieceEnum

    @return `true` if piece is None, `false` otherwise.
*/
#define CC_PIECE_IS_NONE(pe) ( (pe) == CC_PE_None )

/**
    Macro expression to evaluate whether piece is a Pawn.

    @param pe Piece enum, one of `CcPieceEnum` values.

    @see CcPieceEnum

    @return `true` if piece is a Pawn, `false` otherwise.
*/
#define CC_PIECE_IS_PAWN(pe) ( ( (pe) == CC_PE_LightPawn ) || ( (pe) == CC_PE_DarkPawn ) )

/**
    Macro expression to evaluate whether piece is a Knight.

    @param pe Piece enum, one of `CcPieceEnum` values.

    @see CcPieceEnum

    @return `true` if piece is a Knight, `false` otherwise.
*/
#define CC_PIECE_IS_KNIGHT(pe) ( ( (pe) == CC_PE_LightKnight ) || ( (pe) == CC_PE_DarkKnight ) )

/**
    Macro expression to evaluate whether piece is a Bishop.

    @param pe Piece enum, one of `CcPieceEnum` values.

    @see CcPieceEnum

    @return `true` if piece is a Bishop, `false` otherwise.
*/
#define CC_PIECE_IS_BISHOP(pe) ( ( (pe) == CC_PE_LightBishop ) || ( (pe) == CC_PE_DarkBishop ) )

/**
    Macro expression to evaluate whether piece is a Rook.

    @param pe Piece enum, one of `CcPieceEnum` values.

    @see CcPieceEnum

    @return `true` if piece is a Rook, `false` otherwise.
*/
#define CC_PIECE_IS_ROOK(pe) ( ( (pe) == CC_PE_LightRook ) || ( (pe) == CC_PE_DarkRook ) )

/**
    Macro expression to evaluate whether piece is a Queen.

    @param pe Piece enum, one of `CcPieceEnum` values.

    @see CcPieceEnum

    @return `true` if piece is a Queen, `false` otherwise.
*/
#define CC_PIECE_IS_QUEEN(pe) ( ( (pe) == CC_PE_LightQueen ) || ( (pe) == CC_PE_DarkQueen ) )

/**
    Macro expression to evaluate whether piece is a King.

    @param pe Piece enum, one of `CcPieceEnum` values.

    @see CcPieceEnum

    @return `true` if piece is a King, `false` otherwise.
*/
#define CC_PIECE_IS_KING(pe) ( ( (pe) == CC_PE_LightKing ) || ( (pe) == CC_PE_DarkKing ) )

/**
    Macro expression to evaluate whether piece is a Pegasus.

    @param pe Piece enum, one of `CcPieceEnum` values.

    @see CcPieceEnum

    @return `true` if piece is a Pegasus, `false` otherwise.
*/
#define CC_PIECE_IS_PEGASUS(pe) ( ( (pe) == CC_PE_LightPegasus ) || ( (pe) == CC_PE_DarkPegasus ) )

/**
    Macro expression to evaluate whether piece is a Pyramid.

    @param pe Piece enum, one of `CcPieceEnum` values.

    @see CcPieceEnum

    @return `true` if piece is a Pyramid, `false` otherwise.
*/
#define CC_PIECE_IS_PYRAMID(pe) ( ( (pe) == CC_PE_LightPyramid ) || ( (pe) == CC_PE_DarkPyramid ) )

/**
    Macro expression to evaluate whether piece is a Unicorn.

    @param pe Piece enum, one of `CcPieceEnum` values.

    @see CcPieceEnum

    @return `true` if piece is a Unicorn, `false` otherwise.
*/
#define CC_PIECE_IS_UNICORN(pe) ( ( (pe) == CC_PE_LightUnicorn ) || ( (pe) == CC_PE_DarkUnicorn ) )

/**
    Macro expression to evaluate whether piece is a Wave.

    @param pe Piece enum, one of `CcPieceEnum` values.

    @see CcPieceEnum

    @return `true` if piece is a Wave, `false` otherwise.
*/
#define CC_PIECE_IS_WAVE(pe) ( ( (pe) == CC_PE_LightWave ) || ( (pe) == CC_PE_DarkWave ) )

/**
    Macro expression to evaluate whether piece is a Star.

    @param pe Piece enum, one of `CcPieceEnum` values.

    @see CcPieceEnum

    @return `true` if piece is a Star, `false` otherwise.
*/
#define CC_PIECE_IS_STAR(pe) ( ( (pe) == CC_PE_BrightStar ) || ( (pe) == CC_PE_DimStar ) )

/**
    Macro expression to evaluate whether piece is a Centaur.

    @param pe Piece enum, one of `CcPieceEnum` values.

    @see CcPieceEnum

    @return `true` if piece is a Centaur, `false` otherwise.
*/
#define CC_PIECE_IS_CENTAUR(pe) ( ( (pe) == CC_PE_LightCentaur ) || ( (pe) == CC_PE_DarkCentaur ) )

/**
    Macro expression to evaluate whether piece is a Scout.

    @param pe Piece enum, one of `CcPieceEnum` values.

    @see CcPieceEnum

    @return `true` if piece is a Scout, `false` otherwise.
*/
#define CC_PIECE_IS_SCOUT(pe) ( ( (pe) == CC_PE_LightScout ) || ( (pe) == CC_PE_DarkScout ) )

/**
    Macro expression to evaluate whether piece is a Grenadier.

    @param pe Piece enum, one of `CcPieceEnum` values.

    @see CcPieceEnum

    @return `true` if piece is a Grenadier, `false` otherwise.
*/
#define CC_PIECE_IS_GRENADIER(pe) ( ( (pe) == CC_PE_LightGrenadier ) || ( (pe) == CC_PE_DarkGrenadier ) )

/**
    Macro expression to evaluate whether piece is a Serpent.

    @param pe Piece enum, one of `CcPieceEnum` values.

    @see CcPieceEnum

    @return `true` if piece is a Serpent, `false` otherwise.
*/
#define CC_PIECE_IS_SERPENT(pe) ( ( (pe) == CC_PE_LightSerpent ) || ( (pe) == CC_PE_DarkSerpent ) )

/**
    Macro expression to evaluate whether piece is a Shaman.

    @param pe Piece enum, one of `CcPieceEnum` values.

    @see CcPieceEnum

    @return `true` if piece is a Shaman, `false` otherwise.
*/
#define CC_PIECE_IS_SHAMAN(pe) ( ( (pe) == CC_PE_LightShaman ) || ( (pe) == CC_PE_DarkShaman ) )

/**
    Macro expression to evaluate whether piece is a Monolith.

    @param pe Piece enum, one of `CcPieceEnum` values.

    @see CcPieceEnum

    @return `true` if piece is a Monolith, `false` otherwise.
*/
#define CC_PIECE_IS_MONOLITH(pe) ( (pe) == CC_PE_Monolith )

/**
    Macro expression to evaluate whether piece is a Starchild.

    @param pe Piece enum, one of `CcPieceEnum` values.

    @see CcPieceEnum

    @return `true` if piece is a Starchild, `false` otherwise.
*/
#define CC_PIECE_IS_STARCHILD(pe) ( ( (pe) == CC_PE_LightStarchild ) || ( (pe) == CC_PE_DarkStarchild ) )


/**
    Macro expression to evaluate whether piece has owner, i.e. if it's light or dark piece.

    @param pe Piece enum, one of `CcPieceEnum` values.

    @see CcPieceEnum

    @return `true` if piece has owner, `false` otherwise.
*/
#define CC_PIECE_HAS_OWNER(pe) ( ( (pe) != CC_PE_DimStar )              \
                              && ( (pe) != CC_PE_None )                 \
                              && ( (pe) != CC_PE_BrightStar )           \
                              && ( (pe) != CC_PE_Monolith ) )

/**
    Macro expression to evaluate whether piece is activator.

    @param pe Piece enum, one of `CcPieceEnum` values.

    @see CcPieceEnum

    @return `true` if piece is activator, `false` otherwise.
*/
#define CC_PIECE_IS_ACTIVATOR(pe) ( ( (pe) != CC_PE_DimStar )           \
                                 && ( (pe) != CC_PE_DarkWave )          \
                                 && ( (pe) != CC_PE_None )              \
                                 && ( (pe) != CC_PE_LightWave )         \
                                 && ( (pe) != CC_PE_BrightStar )        \
                                 && ( (pe) != CC_PE_Monolith ) )

/**
    Macro expression to evaluate whether piece can activate other piece.

    @param pe Piece enum, one of `CcPieceEnum` values.

    @see CcPieceEnum

    @return `true` if piece is activator, `false` otherwise.
*/
#define CC_PIECE_CAN_ACTIVATE(pe) ( ( (pe) != CC_PE_DimStar )           \
                                 && ( (pe) != CC_PE_None )              \
                                 && ( (pe) != CC_PE_BrightStar )        \
                                 && ( (pe) != CC_PE_Monolith ) )

/**
    Macro expression to evaluate whether piece can capture other piece.

    @param pe Piece enum, one of `CcPieceEnum` values.

    @see CcPieceEnum

    @return `true` if piece can capture, `false` otherwise.
*/
#define CC_PIECE_CAN_CAPTURE(pe) ( ( (pe) != CC_PE_DarkStarchild )      \
                                && ( (pe) != CC_PE_DimStar )            \
                                && ( (pe) != CC_PE_DarkWave )           \
                                && ( (pe) != CC_PE_None )               \
                                && ( (pe) != CC_PE_LightWave )          \
                                && ( (pe) != CC_PE_BrightStar )         \
                                && ( (pe) != CC_PE_LightStarchild )     \
                                && ( (pe) != CC_PE_Monolith ) )

/**
    Macro expression to evaluate whether piece can capture other piece en passant.

    @param pe Piece enum, one of `CcPieceEnum` values.

    @see CcPieceEnum

    @return `true` if piece can capture, `false` otherwise.
*/
#define CC_PIECE_CAN_CAPTURE_EN_PASSANT(pe) ( ( (pe) == CC_PE_DarkGrenadier )       \
                                           || ( (pe) == CC_PE_DarkScout )           \
                                           || ( (pe) == CC_PE_DarkPawn )            \
                                           || ( (pe) == CC_PE_LightPawn )           \
                                           || ( (pe) == CC_PE_LightScout )          \
                                           || ( (pe) == CC_PE_LightGrenadier ) )

/**
    Macro expression to evaluate whether piece can be captured en passant.

    @param pe Piece enum, one of `CcPieceEnum` values.

    @see CcPieceEnum

    @return `true` if piece can capture, `false` otherwise.
*/
#define CC_PIECE_CAN_BE_CAPTURED_EN_PASSANT(pe) ( ( (pe) == CC_PE_DarkGrenadier )       \
                                               || ( (pe) == CC_PE_DarkScout )           \
                                               || ( (pe) == CC_PE_DarkPawn )            \
                                               || ( (pe) == CC_PE_LightPawn )           \
                                               || ( (pe) == CC_PE_LightScout )          \
                                               || ( (pe) == CC_PE_LightGrenadier ) )

/**
    Macro expression to evaluate whether piece is passive.

    @param pe Piece enum, one of `CcPieceEnum` values.

    @see CcPieceEnum

    @return `true` if piece is passive, `false` otherwise.
*/
#define CC_PIECE_IS_PASIVE(pe) ( ( (pe) == CC_PE_DimStar )              \
                              || ( (pe) == CC_PE_DarkWave )             \
                              || ( (pe) == CC_PE_DarkPyramid )          \
                              || ( (pe) == CC_PE_LightPyramid )         \
                              || ( (pe) == CC_PE_LightWave )            \
                              || ( (pe) == CC_PE_BrightStar ) )

/**
    Macro expression to evaluate whether piece is active.

    @param pe Piece enum, one of `CcPieceEnum` values.

    @see CcPieceEnum

    @return `true` if piece is active, `false` otherwise.
*/
#define CC_PIECE_IS_ACTIVE(pe) ( ( (pe) != CC_PE_DimStar )              \
                              && ( (pe) != CC_PE_DarkWave )             \
                              && ( (pe) != CC_PE_DarkPyramid )          \
                              && ( (pe) != CC_PE_None )                 \
                              && ( (pe) != CC_PE_LightPyramid )         \
                              && ( (pe) != CC_PE_LightWave )            \
                              && ( (pe) != CC_PE_BrightStar ) )

/**
    Macro expression to evaluate whether piece is weightless.

    @param pe Piece enum, one of `CcPieceEnum` values.

    @see CcPieceEnum

    @return `true` if piece is weightless, `false` otherwise.
*/
#define CC_PIECE_IS_WEIGHTLESS(pe) ( ( (pe) == CC_PE_LightStarchild )   \
                                  || ( (pe) == CC_PE_DarkStarchild )    \
                                  || ( (pe) == CC_PE_LightWave )        \
                                  || ( (pe) == CC_PE_DarkWave ) )

/**
    Macro expression to evaluate whether piece can be activated.

    @param pe Piece enum, one of `CcPieceEnum` values.

    @see CcPieceEnum

    @return `true` if piece can be activated, `false` otherwise.
*/
#define CC_PIECE_CAN_BE_ACTIVATED(pe) ( ( (pe) != CC_PE_DarkKing )      \
                                     && ( (pe) != CC_PE_None )          \
                                     && ( (pe) != CC_PE_LightKing )     \
                                     && ( (pe) != CC_PE_Monolith ) )

/**
    Macro expression to evaluate whether piece can be captured.

    @param pe Piece enum, one of `CcPieceEnum` values.

    @see CcPieceEnum

    @return `true` if piece is disposable, `false` otherwise.
*/
#define CC_PIECE_CAN_BE_CAPTURED(pe) ( ( (pe) != CC_PE_DimStar )        \
                                    && ( (pe) != CC_PE_DarkKing )       \
                                    && ( (pe) != CC_PE_None )           \
                                    && ( (pe) != CC_PE_LightKing )      \
                                    && ( (pe) != CC_PE_BrightStar )     \
                                    && ( (pe) != CC_PE_Monolith ) )

/**
    Macro expression to evaluate whether Pawn can be promoted to a given piece.

    @param pe Piece enum, one of `CcPieceEnum` values.

    @see CcPieceEnum

    @return `true` if piece is promote-to, `false` otherwise.
*/
#define CC_PAWN_CAN_BE_PROMOTED_TO(pe) ( ( (pe) != CC_PE_DimStar )      \
                                      && ( (pe) != CC_PE_DarkKing )     \
                                      && ( (pe) != CC_PE_DarkPawn )     \
                                      && ( (pe) != CC_PE_None )         \
                                      && ( (pe) != CC_PE_LightPawn )    \
                                      && ( (pe) != CC_PE_LightKing )    \
                                      && ( (pe) != CC_PE_BrightStar )   \
                                      && ( (pe) != CC_PE_Monolith ) )

/**
    Macro expression to evaluate whether piece can be displaced.

    @param pe Piece enum, one of `CcPieceEnum` values.

    @see CcPieceEnum

    @return `true` if piece is displaceable, `false` otherwise.
*/
#define CC_PIECE_CAN_BE_DISPLACED(pe) ( ( (pe) != CC_PE_DimStar )       \
                                     && ( (pe) != CC_PE_DarkKing )      \
                                     && ( (pe) != CC_PE_None )          \
                                     && ( (pe) != CC_PE_LightKing )     \
                                     && ( (pe) != CC_PE_BrightStar )    \
                                     && ( (pe) != CC_PE_Monolith ) )

/**
    Macro expression to evaluate whether piece can be converted.

    @param pe Piece enum, one of `CcPieceEnum` values.

    @see CcPieceEnum

    @return `true` if piece is convertable, `false` otherwise.
*/
#define CC_PIECE_CAN_BE_CONVERTED(pe) ( ( (pe) != CC_PE_DimStar )       \
                                     && ( (pe) != CC_PE_DarkKing )      \
                                     && ( (pe) != CC_PE_None )          \
                                     && ( (pe) != CC_PE_LightKing )     \
                                     && ( (pe) != CC_PE_BrightStar )    \
                                     && ( (pe) != CC_PE_Monolith ) )

/**
    Macro expression to evaluate whether piece can be demoted.

    @param pe Piece enum, one of `CcPieceEnum` values.

    @see CcPieceEnum

    @return `true` if piece is demoteable, `false` otherwise.
*/
#define CC_PIECE_CAN_BE_DEMOTED(pe) ( ( (pe) != CC_PE_DimStar )         \
                                   && ( (pe) != CC_PE_DarkKing )        \
                                   && ( (pe) != CC_PE_None )            \
                                   && ( (pe) != CC_PE_LightKing )       \
                                   && ( (pe) != CC_PE_BrightStar )      \
                                   && ( (pe) != CC_PE_Monolith ) )

/**
    Macro expression to evaluate whether piece can be resurrected.

    @param pe Piece enum, one of `CcPieceEnum` values.

    @see CcPieceEnum

    @return `true` if piece is resurrectable, `false` otherwise.
*/
#define CC_PIECE_CAN_BE_RESURRECTED(pe) ( ( (pe) != CC_PE_DimStar )      \
                                       && ( (pe) != CC_PE_DarkKing )     \
                                       && ( (pe) != CC_PE_None )         \
                                       && ( (pe) != CC_PE_LightKing )    \
                                       && ( (pe) != CC_PE_BrightStar )   \
                                       && ( (pe) != CC_PE_Monolith ) )

/**
    Macro expression to evaluate whether piece can teleport other pieces.

    @param pe Piece enum, one of `CcPieceEnum` values.

    @see CcPieceEnum

    @return `true` if piece is teleporter, `false` otherwise.
*/
#define CC_PIECE_IS_TELEPORTER(pe) ( ( (pe) == CC_PE_DimStar )      \
                                  || ( (pe) == CC_PE_BrightStar )   \
                                  || ( (pe) == CC_PE_Monolith ) )

/**
    Macro expression to evaluate whether piece can teleport.

    @param pe Piece enum, one of `CcPieceEnum` values.

    @see CcPieceEnum

    @return `true` if piece is teleportable, `false` otherwise.
*/
#define CC_PIECE_CAN_BE_TELEPORTED(pe) ( ( (pe) != CC_PE_DimStar )      \
                                      && ( (pe) != CC_PE_DarkKing )     \
                                      && ( (pe) != CC_PE_None )         \
                                      && ( (pe) != CC_PE_LightKing )    \
                                      && ( (pe) != CC_PE_BrightStar )   \
                                      && ( (pe) != CC_PE_Monolith ) )

/**
    Macro expression to evaluate whether piece is opaque.

    @param pe Piece enum, one of `CcPieceEnum` values.

    @see CcPieceEnum

    @return `true` if piece is opaque, `false` otherwise.
*/
#define CC_PIECE_IS_OPAQUE(pe) ( (pe) == CC_PE_Monolith )

/**
    Macro expression to evaluate whether piece is transparent to Wave.

    @param pe Piece enum, one of `CcPieceEnum` values.

    @see CcPieceEnum

    @return `true` if piece is semi-transparent, `false` otherwise.
*/
#define CC_PIECE_IS_SEMI_TRANSPARENT(pe) ( (pe) != CC_PE_Monolith )

/**
    Macro expression to evaluate whether piece is transparent to Wave.

    @param pe Piece enum, one of `CcPieceEnum` values.

    @see CcPieceEnum

    @return `true` if piece is transparent, `false` otherwise.
*/
#define CC_PIECE_IS_TRANSPARENT(pe)  ( ( (pe) == CC_PE_DarkWave )   \
                                    || ( (pe) == CC_PE_LightWave ) )

/**
    Macro expression to evaluate whether piece is can be diverged from.

    @param pe Piece enum, one of `CcPieceEnum` values.

    @see CcPieceEnum

    @return `true` if piece is divergent, `false` otherwise.
*/
#define CC_PIECE_CAN_BE_DIVERGED_FROM(pe) ( ( (pe) == CC_PE_DarkStarchild )     \
                                         || ( (pe) == CC_PE_DarkShaman )        \
                                         || ( (pe) == CC_PE_LightShaman )       \
                                         || ( (pe) == CC_PE_LightStarchild ) )

/**
    Enumerates all pieces, used in all variants.

    Piece `CC_PE_None` is used for e.g. empty on-board fields, any off-board field.
*/
typedef enum CcPieceEnum {
    CC_PE_DimStar = -17,

    CC_PE_DarkStarchild,
    CC_PE_DarkShaman,
    CC_PE_DarkSerpent,
    CC_PE_DarkGrenadier,
    CC_PE_DarkScout,
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
    CC_PE_LightScout,
    CC_PE_LightGrenadier,
    CC_PE_LightSerpent,
    CC_PE_LightShaman,
    CC_PE_LightStarchild,

    CC_PE_BrightStar = 17,

    CC_PE_Monolith,
} CcPieceEnum;


/**
    Function interface, i.e. function pointer type.

    @param pe Piece enum item.

    @return Char, either a piece symbol, or a piece char.
*/
typedef char (*cc_piece_fp_char_value_t)( CcPieceEnum pe );

/**
    Function returning piece enum, based on a piece symbol, and a flag.

    @param symbol Piece symbol, uppercase char. It is taken verbatim, i.e. not converted to uppercase char.
    @param is_light Whether piece is light/bright (`true`), or dark/dim (`false`).

    @return Piece enum if valid piece symbol passed, otherwise `CC_PE_None`.
*/
CcPieceEnum cc_piece_from_symbol( char symbol, bool is_light );

/**
    Function returns whether given character is a valid chess piece symbol.

    @param c A character.

    @return `true` if given character is a valid chess piece symbol, `false` otherwise.
*/
bool cc_piece_symbol_is_valid( char c );

/**
    Function returning piece enum in opposite color (shade) to argument.

    @param pe Piece enum argument.

    @return Piece enum, dark (dim) piece converted to light (bright) piece, and vice versa.
            Monolith, None pieces are returned unchanged.
*/
CcPieceEnum cc_piece_opposite( CcPieceEnum pe );

/**
    Function returning piece char, based on piece enum.

    @param pe Piece enum, one of `CcPieceEnum` values.

    @see CcPieceEnum

    @return Piece char, lowercase if piece is dark (dim), uppercase if piece is light (bright),
            space otherwise.
*/
char cc_piece_as_char( CcPieceEnum pe );

/**
    Function returning piece enum, based on a piece character.

    @param piece Piece character.

    Whether piece returned is dark/dim, or light/bright is determined by the case of a given `char`.

    For lowercase piece `char` dark/dim piece is returned, otherwise light/bright piece is returned.

    @note
    Monolith is returned only for uppercase M `char`, i.e. only for ``'M'``.

    @return Piece enum if valid piece char passed, otherwise `CC_PE_None`.
*/
CcPieceEnum cc_piece_from_char( char piece );

/**
    Function returning piece label.

    @param pe Piece enum, one of `CcPieceEnum` values.

    @see CcPieceEnum

    @note
    Piece label is capitalized name of a piece. Piece label is the same for
    dark (dim) and light (bright) pieces. For None piece, label is empty string.

    @warning
    Returned string is not allocated, so do not try to `free()` it.

    @return Pointer to string if successful, `CC_DEFAULT_ENTITY_STRING` otherwise.
*/
char const * cc_piece_label( CcPieceEnum pe );

/**
    Function returning piece symbol, based on piece enum.

    @param pe Piece enum, one of `CcPieceEnum` values.

    @see CcPieceEnum

    @return Piece symbol, uppercase char if valid piece, space otherwise (if piece is None).
*/
char cc_piece_symbol( CcPieceEnum pe );

/**
    Function returning Pawn to which piece can be demoted, or None if piece can't be demoted.

    @param pe Piece enum, one of `CcPieceEnum` values.

    @see CcPieceEnum

    @return Dark Pawn if dark piece, light Pawn if piece is light, otherwise `CC_PE_None`.
*/
CcPieceEnum cc_piece_demoting_to( CcPieceEnum pe );

/**
    Function returning whether piece is dark.

    @param pe Piece enum, one of `CcPieceEnum` values.

    @see CcPieceEnum

    @return `true` if piece is dark, `false` otherwise.
*/
bool cc_piece_is_dark( CcPieceEnum pe );

/**
    Function returning whether piece is light.

    @param pe Piece enum, one of `CcPieceEnum` values.

    @see CcPieceEnum

    @return `true` if piece is light, `false` otherwise.
*/
bool cc_piece_is_light( CcPieceEnum pe );

/**
    Function returning whether piece has color, i.e. if it's either light, or dark.

    @param pe Piece enum, one of `CcPieceEnum` values.

    @see CcPieceEnum

    @return `true` if piece has color, `false` otherwise.
*/
bool cc_piece_has_color( CcPieceEnum pe );

/**
    Function returning whether piece has shade, i.e. if it's either bright, or dim.

    @param pe Piece enum, one of `CcPieceEnum` values.

    @see CcPieceEnum

    @return `true` if piece has shade, `false` otherwise.
*/
bool cc_piece_has_shade( CcPieceEnum pe );

/**
    Function returning whether piece has prefix, i.e. if it has either a color, or a shade.

    @param pe Piece enum, one of `CcPieceEnum` values.

    @see CcPieceEnum

    @return `true` if piece has prefix, `false` otherwise.
*/
bool cc_piece_has_prefix( CcPieceEnum pe );

/**
    Function returning piece prefix.

    @param pe Piece enum, one of `CcPieceEnum` values.
    @param capitalize Flag, whether to return capitalized prefix.

    @see CcPieceEnum

    @note
    Piece prefix is either a color, or a shade of a given piece, depending what it has.
    For pieces without neither color, nor shade (None, and Monolith), prefix is empty string.

    @warning
    Returned string is not allocated, so do not try to `free()` it.

    @return Pointer to string if successful, `CC_DEFAULT_ENTITY_STRING` otherwise.
*/
char const * cc_piece_prefix( CcPieceEnum pe, bool capitalize );

/**
    Function checks if given piece has the same type as a piece symbol.

    @param symbol Piece symbol, uppercase char. It is taken verbatim, i.e. not converted to uppercase char.
    @param pe A piece.

    @note
    Type of a piece is what remains after it has been stripped of color (or shade).
    For instance, light and dark Rook are both Rooks, that is their type.

    @return `true` if the same type, `false` otherwise.
*/
bool cc_piece_has_congruent_type( char symbol, CcPieceEnum pe );

/**
    Function checks if given piece is equal to one produce by a piece symbol, and a flag.

    @param symbol Piece symbol, uppercase char. It is taken verbatim, i.e. not converted to uppercase char.
    @param is_light Whether piece is light/bright (`true`), or dark/dim (`false`).
    @param pe A piece.

    @see cc_piece_from_symbol()

    @return `true` if piece is the same, `false` otherwise.
*/
bool cc_piece_is_equal( char symbol, bool is_light, CcPieceEnum pe );

/**
    Function checks if two given pieces are the same type.

    @param pe_1 A piece.
    @param pe_2 The other piece.

    @note
    Type of a piece is what remains after it has been stripped of color (or shade).
    For instance, light and dark Rook are both Rooks, that is their type.

    @return `true` if the same type, `false` otherwise.
*/
bool cc_piece_has_same_type( CcPieceEnum pe_1, CcPieceEnum pe_2 );

/**
    Function checks if two given pieces are the same color.

    @param pe_1 A piece.
    @param pe_2 The other piece.

    @note
    Stars have shade (bright, dim), not color, so this function returns `false`
    if any given piece is a Star.

    @return `true` if the same color, `false` otherwise.
*/
bool cc_piece_has_same_color( CcPieceEnum pe_1, CcPieceEnum pe_2 );

/**
    Function checks if two given Stars are the same shade.

    @param pe_1 A piece.
    @param pe_2 The other piece.

    @return `true` if Stars are the same shade, `false` otherwise.
*/
bool cc_piece_has_same_shade( CcPieceEnum pe_1, CcPieceEnum pe_2 );

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
bool cc_piece_is_opposite( CcPieceEnum pe_1, CcPieceEnum pe_2 );

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
bool cc_piece_has_same_owner( CcPieceEnum pe_1, CcPieceEnum pe_2 );

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
bool cc_piece_has_different_owner( CcPieceEnum pe_1, CcPieceEnum pe_2 );

/**
    Function returning whether piece is an owned figure.

    By the book, owned figure is any light, or dark piece, except Pawn.

    @param pe Piece enum.

    @return `true` if piece is an owned figure, `false` otherwise.
*/
bool cc_piece_is_owned_figure( CcPieceEnum pe );

/**
    Function returning whether piece is a figure.

    By the book, figure is any piece, except Pawn.

    @param pe Piece enum.

    @return `true` if piece is a figure, `false` otherwise.
*/
bool cc_piece_is_figure( CcPieceEnum pe );

/**
    Function returns string, containing piece prefix and label.

    @param pe Piece enum, one of `CcPieceEnum` values.
    @param capitalize Flag, whether to return capitalized string.
    @param empty_field Flag, whether to return "empty field", or empty string.

    @see CcPieceEnum, cc_piece_prefix(), cc_piece_label(), CC_DEFAULT_ENTITY_STRING

    @warning
    Returned string is not allocated, so do not try to `free()` it.

    @return Pointer to string if successful, `CC_DEFAULT_ENTITY_STRING` otherwise.
*/
char const * cc_piece_as_string( CcPieceEnum pe, bool capitalize, bool empty_field );


#endif /* __CC_PIECE_H__ */
