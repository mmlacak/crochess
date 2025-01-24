// Copyright (c) 2021 Mario Mlačak, mmlacak@gmail.com
// Licensed under GNU GPL v3+ license. See LICENSING, COPYING files for details.

#ifndef __CC_PIECE_H__
#define __CC_PIECE_H__

#include <stdbool.h>


#define CC_PIECE_IS_ENUMERATOR(pe) ( ( CC_PE_DimStar <= (pe) ) && ( (pe) <= CC_PE_Monolith ) )

#define CC_PIECE_IS_VALID(pe) ( ( (pe) != CC_PE_None ) && ( CC_PE_DimStar <= (pe) ) && ( (pe) <= CC_PE_Monolith ) )

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
                              && ( (pe) != CC_PE_BrightStar )           \
                              && ( (pe) != CC_PE_Monolith )             \
                              && ( CC_PIECE_IS_VALID( (pe) ) ) )

#define CC_PIECE_IS_ACTIVATOR(pe) ( ( (pe) != CC_PE_DimStar )           \
                                 && ( (pe) != CC_PE_DarkWave )          \
                                 && ( (pe) != CC_PE_LightWave )         \
                                 && ( (pe) != CC_PE_BrightStar )        \
                                 && ( (pe) != CC_PE_Monolith )          \
                                 && ( CC_PIECE_IS_VALID( (pe) ) ) )

#define CC_PIECE_CAN_ACTIVATE(pe) ( ( (pe) != CC_PE_DimStar )           \
                                 && ( (pe) != CC_PE_BrightStar )        \
                                 && ( (pe) != CC_PE_Monolith )          \
                                 && ( CC_PIECE_IS_VALID( (pe) ) )

#define CC_PIECE_CAN_ACTIVATE_STAR(pe) ( ( (pe) == CC_PE_DarkStarchild )        \
                                      || ( (pe) == CC_PE_LightStarchild ) )

#define CC_PIECE_CAN_CAPTURE(pe) ( ( (pe) != CC_PE_DarkStarchild )      \
                                && ( (pe) != CC_PE_DimStar )            \
                                && ( (pe) != CC_PE_DarkWave )           \
                                && ( (pe) != CC_PE_LightWave )          \
                                && ( (pe) != CC_PE_BrightStar )         \
                                && ( (pe) != CC_PE_LightStarchild )     \
                                && ( (pe) != CC_PE_Monolith )           \
                                && ( CC_PIECE_IS_VALID( (pe) ) ) )

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
                              && ( (pe) != CC_PE_LightPyramid )         \
                              && ( (pe) != CC_PE_LightWave )            \
                              && ( (pe) != CC_PE_BrightStar )           \
                              && ( CC_PIECE_IS_VALID( (pe) ) ) )

#define CC_PIECE_IS_WEIGHTLESS(pe) ( ( (pe) == CC_PE_DarkStarchild )   \
                                  || ( (pe) == CC_PE_DarkWave )    \
                                  || ( (pe) == CC_PE_LightWave )        \
                                  || ( (pe) == CC_PE_LightStarchild ) )

#define CC_PIECE_CAN_BE_ACTIVATED(pe) ( ( (pe) != CC_PE_DarkKing )      \
                                     && ( (pe) != CC_PE_None )          \
                                     && ( (pe) != CC_PE_LightKing )     \
                                     && ( (pe) != CC_PE_Monolith )      \
                                     && ( CC_PIECE_IS_VALID( (pe) ) ) )

#define CC_PIECE_CAN_BE_CAPTURED(pe) ( ( (pe) != CC_PE_DimStar )        \
                                    && ( (pe) != CC_PE_DarkKing )       \
                                    && ( (pe) != CC_PE_LightKing )      \
                                    && ( (pe) != CC_PE_BrightStar )     \
                                    && ( (pe) != CC_PE_Monolith )       \
                                    && ( CC_PIECE_IS_VALID( (pe) ) ) )

#define CC_PAWN_CAN_BE_PROMOTED_TO(pe) ( ( (pe) != CC_PE_DimStar )      \
                                      && ( (pe) != CC_PE_DarkKing )     \
                                      && ( (pe) != CC_PE_DarkPawn )     \
                                      && ( (pe) != CC_PE_LightPawn )    \
                                      && ( (pe) != CC_PE_LightKing )    \
                                      && ( (pe) != CC_PE_BrightStar )   \
                                      && ( (pe) != CC_PE_Monolith )     \
                                      && ( CC_PIECE_IS_VALID( (pe) ) ) )

#define CC_PIECE_CAN_BE_DISPLACED(pe) ( ( (pe) != CC_PE_DimStar )       \
                                     && ( (pe) != CC_PE_DarkKing )      \
                                     && ( (pe) != CC_PE_LightKing )     \
                                     && ( (pe) != CC_PE_BrightStar )    \
                                     && ( (pe) != CC_PE_Monolith )      \
                                     && ( CC_PIECE_IS_VALID( (pe) ) ) )

#define CC_PIECE_CAN_BE_CONVERTED(pe) ( ( (pe) != CC_PE_DimStar )       \
                                     && ( (pe) != CC_PE_DarkKing )      \
                                     && ( (pe) != CC_PE_LightKing )     \
                                     && ( (pe) != CC_PE_BrightStar )    \
                                     && ( (pe) != CC_PE_Monolith )      \
                                     && ( CC_PIECE_IS_VALID( (pe) ) ) )

#define CC_PIECE_CAN_BE_DEMOTED(pe) ( ( (pe) != CC_PE_DimStar )         \
                                   && ( (pe) != CC_PE_DarkKing )        \
                                   && ( (pe) != CC_PE_LightKing )       \
                                   && ( (pe) != CC_PE_BrightStar )      \
                                   && ( (pe) != CC_PE_Monolith )        \
                                   && ( CC_PIECE_IS_VALID( (pe) ) ) )

#define CC_PIECE_CAN_BE_RESURRECTED(pe) ( ( (pe) != CC_PE_DimStar )      \
                                       && ( (pe) != CC_PE_DarkKing )     \
                                       && ( (pe) != CC_PE_LightKing )    \
                                       && ( (pe) != CC_PE_BrightStar )   \
                                       && ( (pe) != CC_PE_Monolith )     \
                                       && ( CC_PIECE_IS_VALID( (pe) ) ) )

#define CC_PIECE_IS_TELEPORTER(pe) ( ( (pe) == CC_PE_DimStar )      \
                                  || ( (pe) == CC_PE_BrightStar )   \
                                  || ( (pe) == CC_PE_Monolith ) )

#define CC_PIECE_CAN_BE_TELEPORTED(pe) ( ( (pe) != CC_PE_DimStar )      \
                                      && ( (pe) != CC_PE_DarkKing )     \
                                      && ( (pe) != CC_PE_LightKing )    \
                                      && ( (pe) != CC_PE_BrightStar )   \
                                      && ( (pe) != CC_PE_Monolith )     \
                                      && ( CC_PIECE_IS_VALID( (pe) ) ) )

#define CC_PIECE_IS_COMPLETELY_TRANSPARENT(pe)  ( ( (pe) == CC_PE_DarkStarchild )   \
                                               || ( (pe) == CC_PE_LightStarchild ) )

#define CC_PIECE_IS_TRANSPARENT(pe)  ( ( (pe) == CC_PE_DarkWave )           \
                                    || ( (pe) == CC_PE_LightWave )          \
                                    || ( (pe) == CC_PE_DarkStarchild )      \
                                    || ( (pe) == CC_PE_LightStarchild ) )

#define CC_PIECE_IS_SEMI_TRANSPARENT(pe) ( ( (pe) != CC_PE_Monolith )      \
                                        && ( CC_PIECE_IS_VALID( (pe) ) ) )

#define CC_PIECE_IS_SEMI_OPAQUE(pe) ( ( (pe) != CC_PE_Monolith )          \
                                   && ( (pe) != CC_PE_DarkWave )          \
                                   && ( (pe) != CC_PE_LightWave )         \
                                   && ( (pe) != CC_PE_DarkStarchild )     \
                                   && ( (pe) != CC_PE_LightStarchild )    \
                                   && ( CC_PIECE_IS_VALID( (pe) ) ) )

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
                                    && ( (pe) != CC_PE_Monolith )       \
                                    && ( CC_PIECE_IS_VALID( (pe) ) ) )

#define CC_WAVE_CAN_BE_DIVERGED(activator) ( ( (activator) != CC_PE_DarkCentaur )       \
                                          && ( (activator) != CC_PE_DarkSerpent )       \
                                          && ( (activator) != CC_PE_DarkUnicorn )       \
                                          && ( (activator) != CC_PE_None )              \
                                          && ( (activator) != CC_PE_LightUnicorn )      \
                                          && ( (activator) != CC_PE_LightSerpent )      \
                                          && ( (activator) != CC_PE_LightCentaur )      \
                                          && ( CC_PIECE_IS_ACTIVATOR( (activator) ) ) )

#define CC_PIECE_IS_SINGLE_STEP(pe) ( CC_PIECE_IS_PAWN(pe)          \
                                   || CC_PIECE_IS_KNIGHT(pe)        \
                                   || CC_PIECE_IS_BISHOP(pe)        \
                                   || CC_PIECE_IS_ROOK(pe)          \
                                   || CC_PIECE_IS_QUEEN(pe)         \
                                   || CC_PIECE_IS_KING(pe)          \
                                   || CC_PIECE_IS_PEGASUS(pe)       \
                                   || CC_PIECE_IS_PYRAMID(pe)       \
                                   || CC_PIECE_IS_UNICORN(pe)       \
                                   || CC_PIECE_IS_GRENADIER(pe)     \
                                   || CC_PIECE_IS_SHAMAN(pe)        \
                                   || CC_PIECE_IS_STARCHILD(pe)     \
                                   || CC_PIECE_IS_STAR(pe) )

#define CC_PIECE_IS_TWO_STEP(pe) ( CC_PIECE_IS_CENTAUR(pe) )

#define CC_WAVE_IS_TWO_STEP(activator) ( CC_PIECE_IS_UNICORN(activator)   \
                                      || CC_PIECE_IS_CENTAUR(activator)   \
                                      || CC_PIECE_IS_SERPENT(activator) )

#define CC_PIECE_HAS_NEW_STEP_AFTER_EACH(pe)  ( ( (pe) == CC_PE_DarkSerpent )   \
                                             || ( (pe) == CC_PE_LightSerpent )  \
                                             || ( (pe) == CC_PE_Monolith ) )

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

typedef signed char CcPieceType;


typedef char (*cc_piece_fp_char_value_t)( CcPieceType pe );

CcPieceType cc_piece_from_symbol( char symbol, bool is_light );

bool cc_piece_symbol_is_valid( char c );

CcPieceType cc_piece_opposite( CcPieceType pe );

char cc_piece_as_char( CcPieceType pe );

CcPieceType cc_piece_from_char( char piece );

char const * cc_piece_label( CcPieceType pe, bool capitalize, bool empty_field );

char cc_piece_symbol( CcPieceType pe );

CcPieceType cc_piece_demoting_to( CcPieceType pe );

bool cc_piece_is_dark( CcPieceType pe );

bool cc_piece_is_light( CcPieceType pe );

bool cc_piece_has_color( CcPieceType pe );

bool cc_piece_has_shade( CcPieceType pe );

bool cc_piece_has_prefix( CcPieceType pe );

char const * cc_piece_prefix( CcPieceType pe, bool capitalize );

bool cc_piece_has_congruent_type( char symbol, CcPieceType pe );

bool cc_piece_is_equal( char symbol, bool is_light, CcPieceType pe );

bool cc_piece_has_same_type( CcPieceType pe_1, CcPieceType pe_2 );

bool cc_piece_has_same_color( CcPieceType pe_1, CcPieceType pe_2 );

bool cc_piece_has_same_shade( CcPieceType pe_1, CcPieceType pe_2 );

bool cc_piece_is_opposite( CcPieceType pe_1, CcPieceType pe_2 );

bool cc_piece_has_same_owner( CcPieceType pe_1, CcPieceType pe_2 );

bool cc_piece_has_different_owner( CcPieceType pe_1, CcPieceType pe_2 );

bool cc_piece_is_owned_figure( CcPieceType pe );

bool cc_piece_is_figure( CcPieceType pe );

char const * cc_piece_as_string( CcPieceType pe, bool capitalize, bool empty_field );


#endif /* __CC_PIECE_H__ */
