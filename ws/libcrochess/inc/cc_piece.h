// Copyright (c) 2021 Mario Mlačak, mmlacak@gmail.com
// Licensed under GNU GPL v3+ license. See LICENSING, COPYING files for details.

#ifndef __CC_PIECE_H__
#define __CC_PIECE_H__

#include <stdbool.h>


// TODO :: revise VALID vs. IN_DOMAIN, for all macros

#define CC_PIECE_IS_ENUMERATOR(pe) ( ( CC_PE_DimStar <= (pe) ) && ( (pe) <= CC_PE_Monolith ) )

#define CC_PIECE_IS_VALID(pe) ( ( CC_PE_DimStar <= (pe) ) && ( (pe) <= CC_PE_Monolith ) && ( (pe) != CC_PE_None ) )

#define CC_PIECE_IS_EQUAL(pe1,pe2) ( (pe1) == (pe2) )

#define CC_PIECE_IS_NONE(pe) ( (pe) == CC_PE_None )

#define CC_PIECE_IS_PAWN(pe) ( ( (pe) == CC_PE_LightPawn ) || ( (pe) == CC_PE_DarkPawn ) )

#define CC_PIECE_IS_KNIGHT(pe) ( ( (pe) == CC_PE_LightKnight ) || ( (pe) == CC_PE_DarkKnight ) )

#define CC_PIECE_IS_BISHOP(pe) ( ( (pe) == CC_PE_LightBishop ) || ( (pe) == CC_PE_DarkBishop ) )

#define CC_PIECE_IS_ROOK(pe) ( ( (pe) == CC_PE_LightRook ) || ( (pe) == CC_PE_DarkRook ) )

#define CC_PIECE_IS_QUEEN(pe) ( ( (pe) == CC_PE_LightQueen ) || ( (pe) == CC_PE_DarkQueen ) )

#define CC_PIECE_IS_KING(pe) ( ( (pe) == CC_PE_LightKing ) || ( (pe) == CC_PE_DarkKing ) )

#define CC_PIECE_IS_PEGASUS(pe) ( ( (pe) == CC_PE_LightPegasus ) || ( (pe) == CC_PE_DarkPegasus ) )

#define CC_PIECE_IS_PYRAMID(pe) ( ( (pe) == CC_PE_LightPyramid ) || ( (pe) == CC_PE_DarkPyramid ) )

#define CC_PIECE_IS_UNICORN(pe) ( ( (pe) == CC_PE_LightUnicorn ) || ( (pe) == CC_PE_DarkUnicorn ) )

#define CC_PIECE_IS_WAVE(pe) ( ( (pe) == CC_PE_LightWave ) || ( (pe) == CC_PE_DarkWave ) )

#define CC_PIECE_IS_STAR(pe) ( ( (pe) == CC_PE_BrightStar ) || ( (pe) == CC_PE_DimStar ) )

#define CC_PIECE_IS_CENTAUR(pe) ( ( (pe) == CC_PE_LightCentaur ) || ( (pe) == CC_PE_DarkCentaur ) )

#define CC_PIECE_IS_SCOUT(pe) ( ( (pe) == CC_PE_LightScout ) || ( (pe) == CC_PE_DarkScout ) )

#define CC_PIECE_IS_GRENADIER(pe) ( ( (pe) == CC_PE_LightGrenadier ) || ( (pe) == CC_PE_DarkGrenadier ) )

#define CC_PIECE_IS_SERPENT(pe) ( ( (pe) == CC_PE_LightSerpent ) || ( (pe) == CC_PE_DarkSerpent ) )

#define CC_PIECE_IS_SHAMAN(pe) ( ( (pe) == CC_PE_LightShaman ) || ( (pe) == CC_PE_DarkShaman ) )

#define CC_PIECE_IS_MONOLITH(pe) ( (pe) == CC_PE_Monolith )

#define CC_PIECE_IS_STARCHILD(pe) ( ( (pe) == CC_PE_LightStarchild ) || ( (pe) == CC_PE_DarkStarchild ) )


#define CC_PIECE_HAS_OWNER(pe) ( ( (pe) != CC_PE_DimStar )              \
                              && ( (pe) != CC_PE_None )                 \
                              && ( (pe) != CC_PE_BrightStar )           \
                              && ( (pe) != CC_PE_Monolith ) )

#define CC_PIECE_IS_ACTIVATOR(pe) ( ( (pe) != CC_PE_DimStar )           \
                                 && ( (pe) != CC_PE_DarkWave )          \
                                 && ( (pe) != CC_PE_None )              \
                                 && ( (pe) != CC_PE_LightWave )         \
                                 && ( (pe) != CC_PE_BrightStar )        \
                                 && ( (pe) != CC_PE_Monolith ) )

#define CC_PIECE_CAN_ACTIVATE(pe) ( ( (pe) != CC_PE_DimStar )           \
                                 && ( (pe) != CC_PE_None )              \
                                 && ( (pe) != CC_PE_BrightStar )        \
                                 && ( (pe) != CC_PE_Monolith ) )

#define CC_PIECE_CAN_ACTIVATE_STAR(pe) ( ( (pe) == CC_PE_DarkStarchild )        \
                                      || ( (pe) == CC_PE_LightStarchild ) )

#define CC_PIECE_CAN_CAPTURE(pe) ( ( (pe) != CC_PE_DarkStarchild )      \
                                && ( (pe) != CC_PE_DimStar )            \
                                && ( (pe) != CC_PE_DarkWave )           \
                                && ( (pe) != CC_PE_None )               \
                                && ( (pe) != CC_PE_LightWave )          \
                                && ( (pe) != CC_PE_BrightStar )         \
                                && ( (pe) != CC_PE_LightStarchild )     \
                                && ( (pe) != CC_PE_Monolith ) )

#define CC_PIECE_CAN_CAPTURE_EN_PASSANT(pe) ( ( (pe) == CC_PE_DarkGrenadier )       \
                                           || ( (pe) == CC_PE_DarkScout )           \
                                           || ( (pe) == CC_PE_DarkPawn )            \
                                           || ( (pe) == CC_PE_LightPawn )           \
                                           || ( (pe) == CC_PE_LightScout )          \
                                           || ( (pe) == CC_PE_LightGrenadier ) )

#define CC_PIECE_CAN_BE_CAPTURED_EN_PASSANT(pe) ( ( (pe) == CC_PE_DarkGrenadier )       \
                                               || ( (pe) == CC_PE_DarkScout )           \
                                               || ( (pe) == CC_PE_DarkPawn )            \
                                               || ( (pe) == CC_PE_LightPawn )           \
                                               || ( (pe) == CC_PE_LightScout )          \
                                               || ( (pe) == CC_PE_LightGrenadier ) )

#define CC_PIECE_IS_PASIVE(pe) ( ( (pe) == CC_PE_DimStar )              \
                              || ( (pe) == CC_PE_DarkWave )             \
                              || ( (pe) == CC_PE_DarkPyramid )          \
                              || ( (pe) == CC_PE_LightPyramid )         \
                              || ( (pe) == CC_PE_LightWave )            \
                              || ( (pe) == CC_PE_BrightStar ) )

#define CC_PIECE_IS_ACTIVE(pe) ( ( (pe) != CC_PE_DimStar )              \
                              && ( (pe) != CC_PE_DarkWave )             \
                              && ( (pe) != CC_PE_DarkPyramid )          \
                              && ( (pe) != CC_PE_None )                 \
                              && ( (pe) != CC_PE_LightPyramid )         \
                              && ( (pe) != CC_PE_LightWave )            \
                              && ( (pe) != CC_PE_BrightStar ) )

#define CC_PIECE_IS_WEIGHTLESS(pe) ( ( (pe) == CC_PE_DarkStarchild )   \
                                  || ( (pe) == CC_PE_DarkWave )    \
                                  || ( (pe) == CC_PE_LightWave )        \
                                  || ( (pe) == CC_PE_LightStarchild ) )

#define CC_PIECE_CAN_BE_ACTIVATED(pe) ( ( (pe) != CC_PE_DarkKing )      \
                                     && ( (pe) != CC_PE_None )          \
                                     && ( (pe) != CC_PE_LightKing )     \
                                     && ( (pe) != CC_PE_Monolith ) )

#define CC_PIECE_CAN_BE_CAPTURED(pe) ( ( (pe) != CC_PE_DimStar )        \
                                    && ( (pe) != CC_PE_DarkKing )       \
                                    && ( (pe) != CC_PE_None )           \
                                    && ( (pe) != CC_PE_LightKing )      \
                                    && ( (pe) != CC_PE_BrightStar )     \
                                    && ( (pe) != CC_PE_Monolith ) )

#define CC_PAWN_CAN_BE_PROMOTED_TO(pe) ( ( (pe) != CC_PE_DimStar )      \
                                      && ( (pe) != CC_PE_DarkKing )     \
                                      && ( (pe) != CC_PE_DarkPawn )     \
                                      && ( (pe) != CC_PE_None )         \
                                      && ( (pe) != CC_PE_LightPawn )    \
                                      && ( (pe) != CC_PE_LightKing )    \
                                      && ( (pe) != CC_PE_BrightStar )   \
                                      && ( (pe) != CC_PE_Monolith ) )

#define CC_PIECE_CAN_BE_DISPLACED(pe) ( ( (pe) != CC_PE_DimStar )       \
                                     && ( (pe) != CC_PE_DarkKing )      \
                                     && ( (pe) != CC_PE_None )          \
                                     && ( (pe) != CC_PE_LightKing )     \
                                     && ( (pe) != CC_PE_BrightStar )    \
                                     && ( (pe) != CC_PE_Monolith ) )

#define CC_PIECE_CAN_BE_CONVERTED(pe) ( ( (pe) != CC_PE_DimStar )       \
                                     && ( (pe) != CC_PE_DarkKing )      \
                                     && ( (pe) != CC_PE_None )          \
                                     && ( (pe) != CC_PE_LightKing )     \
                                     && ( (pe) != CC_PE_BrightStar )    \
                                     && ( (pe) != CC_PE_Monolith ) )

#define CC_PIECE_CAN_BE_DEMOTED(pe) ( ( (pe) != CC_PE_DimStar )         \
                                   && ( (pe) != CC_PE_DarkKing )        \
                                   && ( (pe) != CC_PE_None )            \
                                   && ( (pe) != CC_PE_LightKing )       \
                                   && ( (pe) != CC_PE_BrightStar )      \
                                   && ( (pe) != CC_PE_Monolith ) )

#define CC_PIECE_CAN_BE_RESURRECTED(pe) ( ( (pe) != CC_PE_DimStar )      \
                                       && ( (pe) != CC_PE_DarkKing )     \
                                       && ( (pe) != CC_PE_None )         \
                                       && ( (pe) != CC_PE_LightKing )    \
                                       && ( (pe) != CC_PE_BrightStar )   \
                                       && ( (pe) != CC_PE_Monolith ) )

#define CC_PIECE_IS_TELEPORTER(pe) ( ( (pe) == CC_PE_DimStar )      \
                                  || ( (pe) == CC_PE_BrightStar )   \
                                  || ( (pe) == CC_PE_Monolith ) )

#define CC_PIECE_CAN_BE_TELEPORTED(pe) ( ( (pe) != CC_PE_DimStar )      \
                                      && ( (pe) != CC_PE_DarkKing )     \
                                      && ( (pe) != CC_PE_None )         \
                                      && ( (pe) != CC_PE_LightKing )    \
                                      && ( (pe) != CC_PE_BrightStar )   \
                                      && ( (pe) != CC_PE_Monolith ) )

#define CC_PIECE_IS_COMPLETELY_TRANSPARENT(pe)  ( ( (pe) == CC_PE_DarkStarchild )   \
                                               || ( (pe) == CC_PE_LightStarchild ) )

#define CC_PIECE_IS_TRANSPARENT(pe)  ( ( (pe) == CC_PE_DarkWave )           \
                                    || ( (pe) == CC_PE_LightWave )          \
                                    || ( (pe) == CC_PE_DarkStarchild )      \
                                    || ( (pe) == CC_PE_LightStarchild ) )

#define CC_PIECE_IS_SEMI_TRANSPARENT(pe) ( ( (pe) != CC_PE_Monolith ) )

#define CC_PIECE_IS_SEMI_OPAQUE(pe) ( ( (pe) != CC_PE_Monolith )          \
                                   && ( (pe) != CC_PE_DarkWave )          \
                                   && ( (pe) != CC_PE_LightWave )         \
                                   && ( (pe) != CC_PE_DarkStarchild )     \
                                   && ( (pe) != CC_PE_LightStarchild ) )

#define CC_PIECE_IS_OPAQUE(pe) ( (pe) == CC_PE_Monolith )

#define CC_PIECE_IS_DIVERGENT(pe) ( ( (pe) == CC_PE_DarkStarchild )     \
                                 || ( (pe) == CC_PE_DarkShaman )        \
                                 || ( (pe) == CC_PE_LightShaman )       \
                                 || ( (pe) == CC_PE_LightStarchild ) )

#define CC_PIECE_CAN_BE_DIVERGED(pe) ( ( (pe) != CC_PE_DimStar )        \
                                    && ( (pe) != CC_PE_DarkStarchild )  \
                                    && ( (pe) != CC_PE_DarkCentaur )    \
                                    && ( (pe) != CC_PE_DarkSerpent )    \
                                    && ( (pe) != CC_PE_DarkKing )       \
                                    && ( (pe) != CC_PE_None )           \
                                    && ( (pe) != CC_PE_LightKing )      \
                                    && ( (pe) != CC_PE_LightSerpent )   \
                                    && ( (pe) != CC_PE_LightCentaur )   \
                                    && ( (pe) != CC_PE_LightStarchild ) \
                                    && ( (pe) != CC_PE_BrightStar )     \
                                    && ( (pe) != CC_PE_Monolith ) )

#define CC_WAVE_CAN_BE_DIVERGED(activator) ( ( (activator) != CC_PE_DarkCentaur )       \
                                          && ( (activator) != CC_PE_DarkSerpent )       \
                                          && ( (activator) != CC_PE_DarkUnicorn )       \
                                          && ( (activator) != CC_PE_None )              \
                                          && ( (activator) != CC_PE_LightUnicorn )      \
                                          && ( (activator) != CC_PE_LightSerpent )      \
                                          && ( (activator) != CC_PE_LightCentaur )      \
                                          && ( CC_PIECE_IS_ACTIVATOR( (activator) ) ) )

// TODO :: move into function, then fix
//
// /**
//     Macro expression to evaluate whether piece has two alternating steps.

//     @param pe Piece enum, one of `cc_piece` values.
//     @param activator Piece enum. Last material (i.e. non-Wave) piece which activates Wave in a cascade.

//     Two step pieces are Centaur, and Wave activated by Unicorn, Centaur, or Serpent.
//     Unicorn itself is not two-step piece, because it makes only one step in a ply,
//     and so it can choose direction independently to any previous choice.

//     @see cc_piece

//     @return `true` if piece has two alternating steps, `false` otherwise.
// */
// #define CC_PIECE_IS_TWO_STEP(pe,activator) \
//     ( ( ( (pe) == CC_PE_DarkCentaur ) || ( (pe) == CC_PE_LightCentaur ) )   \
//    || ( ( ( (pe) == CC_PE_DarkWave ) || ( (pe) == CC_PE_LightWave ) )       \
//         && ( ( ( (activator) == CC_PE_DarkUnicorn ) || ( (activator) == CC_PE_LightUnicorn ) ) \
//           || ( ( (activator) == CC_PE_DarkCentaur ) || ( (activator) == CC_PE_LightCentaur ) ) \
//           || ( ( (activator) == CC_PE_DarkSerpent ) || ( (activator) == CC_PE_LightSerpent ) ) ) ) )
//
// TODO :: move into function, then fix

#define CC_PIECE_HAS_NEW_STEP_AFTER_EACH(pe)  ( ( (pe) == CC_PE_DarkSerpent )   \
                                             || ( (pe) == CC_PE_LightSerpent )  \
                                             || ( (pe) == CC_PE_Monolith ) )

// TODO :: DOCS
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

// TODO :: DOCS
#define CC_PIECE_VALUE_MASK (0x9F) // <!> Keep in sync with CcPieceEnum enumerators.

// TODO :: DOCS
#define CC_PIECE_VALUE(pe) ( (pe) & CC_PIECE_VALUE_MASK )

// TODO :: DOCS
typedef signed char cc_piece;


typedef char (*cc_piece_fp_char_value_t)( cc_piece pe );

cc_piece cc_piece_from_symbol( char symbol, bool is_light );

bool cc_piece_symbol_is_valid( char c );

cc_piece cc_piece_opposite( cc_piece pe );

char cc_piece_as_char( cc_piece pe );

cc_piece cc_piece_from_char( char piece );

char const * cc_piece_label( cc_piece pe );

char cc_piece_symbol( cc_piece pe );

cc_piece cc_piece_demoting_to( cc_piece pe );

bool cc_piece_is_dark( cc_piece pe );

bool cc_piece_is_light( cc_piece pe );

bool cc_piece_has_color( cc_piece pe );

bool cc_piece_has_shade( cc_piece pe );

bool cc_piece_has_prefix( cc_piece pe );

char const * cc_piece_prefix( cc_piece pe, bool capitalize );

bool cc_piece_has_congruent_type( char symbol, cc_piece pe );

bool cc_piece_is_equal( char symbol, bool is_light, cc_piece pe );

bool cc_piece_has_same_type( cc_piece pe_1, cc_piece pe_2 );

bool cc_piece_has_same_color( cc_piece pe_1, cc_piece pe_2 );

bool cc_piece_has_same_shade( cc_piece pe_1, cc_piece pe_2 );

bool cc_piece_is_opposite( cc_piece pe_1, cc_piece pe_2 );

bool cc_piece_has_same_owner( cc_piece pe_1, cc_piece pe_2 );

bool cc_piece_has_different_owner( cc_piece pe_1, cc_piece pe_2 );

bool cc_piece_is_owned_figure( cc_piece pe );

bool cc_piece_is_figure( cc_piece pe );

char const * cc_piece_as_string( cc_piece pe, bool capitalize, bool empty_field );


#endif /* __CC_PIECE_H__ */
