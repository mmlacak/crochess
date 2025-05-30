// Copyright (c) 2021 Mario Mlačak, mmlacak@gmail.com
// Licensed under GNU GPL v3+ license. See LICENSING, COPYING files for details.

#ifndef __CC_PIECE_H__
#define __CC_PIECE_H__

#include <stdbool.h>


#define CC_PIECE_IS_ENUMERATOR(pe) ( ( CC_PTE_DimStar <= (pe) ) && ( (pe) <= CC_PTE_Monolith ) )

#define CC_PIECE_IS_VALID(pe) ( ( (pe) != CC_PTE_None ) && ( CC_PTE_DimStar <= (pe) ) && ( (pe) <= CC_PTE_Monolith ) )

#define CC_PIECE_IS_NONE(pe) ( (pe) == CC_PTE_None )

#define CC_PIECE_IS_PAWN(pe) ( ( (pe) == CC_PTE_LightPawn ) || ( (pe) == CC_PTE_DarkPawn ) )

#define CC_PIECE_IS_KNIGHT(pe) ( ( (pe) == CC_PTE_LightKnight ) || ( (pe) == CC_PTE_DarkKnight ) )

#define CC_PIECE_IS_BISHOP(pe) ( ( (pe) == CC_PTE_LightBishop ) || ( (pe) == CC_PTE_DarkBishop ) )

#define CC_PIECE_IS_ROOK(pe) ( ( (pe) == CC_PTE_LightRook ) || ( (pe) == CC_PTE_DarkRook ) )

#define CC_PIECE_IS_QUEEN(pe) ( ( (pe) == CC_PTE_LightQueen ) || ( (pe) == CC_PTE_DarkQueen ) )

#define CC_PIECE_IS_KING(pe) ( ( (pe) == CC_PTE_LightKing ) || ( (pe) == CC_PTE_DarkKing ) )

#define CC_PIECE_IS_PEGASUS(pe) ( ( (pe) == CC_PTE_LightPegasus ) || ( (pe) == CC_PTE_DarkPegasus ) )

#define CC_PIECE_IS_PYRAMID(pe) ( ( (pe) == CC_PTE_LightPyramid ) || ( (pe) == CC_PTE_DarkPyramid ) )

#define CC_PIECE_IS_UNICORN(pe) ( ( (pe) == CC_PTE_LightUnicorn ) || ( (pe) == CC_PTE_DarkUnicorn ) )

#define CC_PIECE_IS_WAVE(pe) ( ( (pe) == CC_PTE_LightWave ) || ( (pe) == CC_PTE_DarkWave ) )

#define CC_PIECE_IS_STAR(pe) ( ( (pe) == CC_PTE_BrightStar ) || ( (pe) == CC_PTE_DimStar ) )

#define CC_PIECE_IS_CENTAUR(pe) ( ( (pe) == CC_PTE_LightCentaur ) || ( (pe) == CC_PTE_DarkCentaur ) )

#define CC_PIECE_IS_SCOUT(pe) ( ( (pe) == CC_PTE_LightScout ) || ( (pe) == CC_PTE_DarkScout ) )

#define CC_PIECE_IS_GRENADIER(pe) ( ( (pe) == CC_PTE_LightGrenadier ) || ( (pe) == CC_PTE_DarkGrenadier ) )

#define CC_PIECE_IS_SERPENT(pe) ( ( (pe) == CC_PTE_LightSerpent ) || ( (pe) == CC_PTE_DarkSerpent ) )

#define CC_PIECE_IS_SHAMAN(pe) ( ( (pe) == CC_PTE_LightShaman ) || ( (pe) == CC_PTE_DarkShaman ) )

#define CC_PIECE_IS_MONOLITH(pe) ( (pe) == CC_PTE_Monolith )

#define CC_PIECE_IS_STARCHILD(pe) ( ( (pe) == CC_PTE_LightStarchild ) || ( (pe) == CC_PTE_DarkStarchild ) )

#define CC_PIECE_IS_TROOPER(pe) ( ( (pe) == CC_PTE_DarkGrenadier )       \
                               || ( (pe) == CC_PTE_DarkScout )           \
                               || ( (pe) == CC_PTE_LightScout )          \
                               || ( (pe) == CC_PTE_LightGrenadier ) )

#define CC_PIECE_IS_PRIVATE(pe) ( ( (pe) == CC_PTE_DarkGrenadier )       \
                               || ( (pe) == CC_PTE_DarkScout )           \
                               || ( (pe) == CC_PTE_DarkPawn )            \
                               || ( (pe) == CC_PTE_LightPawn )           \
                               || ( (pe) == CC_PTE_LightScout )          \
                               || ( (pe) == CC_PTE_LightGrenadier ) )

#define CC_PIECE_IS_MATERIAL(pe) ( ( (pe) != CC_PTE_DarkWave )           \
                                && ( (pe) != CC_PTE_LightWave )          \
                                && ( CC_PIECE_IS_VALID( (pe) ) ) )

#define CC_PIECE_IS_CELESTIAL(pe) ( ( (pe) == CC_PTE_DarkStarchild )     \
                                 || ( (pe) == CC_PTE_DarkWave )          \
                                 || ( (pe) == CC_PTE_LightWave )         \
                                 || ( (pe) == CC_PTE_LightStarchild )    \
                                 || ( (pe) == CC_PTE_Monolith ) )


#define CC_PIECE_HAS_OWNER(pe) ( ( (pe) != CC_PTE_DimStar )              \
                              && ( (pe) != CC_PTE_BrightStar )           \
                              && ( (pe) != CC_PTE_Monolith )             \
                              && ( CC_PIECE_IS_VALID( (pe) ) ) )

#define CC_PIECE_IS_ACTIVATOR(pe) ( ( (pe) != CC_PTE_DimStar )           \
                                 && ( (pe) != CC_PTE_DarkWave )          \
                                 && ( (pe) != CC_PTE_LightWave )         \
                                 && ( (pe) != CC_PTE_BrightStar )        \
                                 && ( (pe) != CC_PTE_Monolith )          \
                                 && ( CC_PIECE_IS_VALID( (pe) ) ) )

#define CC_PIECE_CAN_ACTIVATE(pe) ( ( (pe) != CC_PTE_DimStar )           \
                                 && ( (pe) != CC_PTE_BrightStar )        \
                                 && ( (pe) != CC_PTE_Monolith )          \
                                 && ( CC_PIECE_IS_VALID( (pe) ) ) )

#define CC_PIECE_CAN_ACTIVATE_STAR(pe) ( ( (pe) == CC_PTE_DarkStarchild )        \
                                      || ( (pe) == CC_PTE_LightStarchild ) )

#define CC_PIECE_CAN_CAPTURE(pe) ( ( (pe) != CC_PTE_DarkStarchild )      \
                                && ( (pe) != CC_PTE_DimStar )            \
                                && ( (pe) != CC_PTE_DarkWave )           \
                                && ( (pe) != CC_PTE_LightWave )          \
                                && ( (pe) != CC_PTE_BrightStar )         \
                                && ( (pe) != CC_PTE_LightStarchild )     \
                                && ( (pe) != CC_PTE_Monolith )           \
                                && ( CC_PIECE_IS_VALID( (pe) ) ) )

#define CC_PIECE_CAN_CAPTURE_EN_PASSANT(pe) ( CC_PIECE_IS_PRIVATE( (pe) ) )

#define CC_PIECE_CAN_BE_CAPTURED_EN_PASSANT(pe) ( CC_PIECE_IS_PRIVATE( (pe) ) )

#define CC_PIECE_IS_PASIVE(pe) ( ( (pe) == CC_PTE_DimStar )              \
                              || ( (pe) == CC_PTE_DarkWave )             \
                              || ( (pe) == CC_PTE_DarkPyramid )          \
                              || ( (pe) == CC_PTE_LightPyramid )         \
                              || ( (pe) == CC_PTE_LightWave )            \
                              || ( (pe) == CC_PTE_BrightStar ) )

#define CC_PIECE_IS_ACTIVE(pe) ( ( (pe) != CC_PTE_DimStar )              \
                              && ( (pe) != CC_PTE_DarkWave )             \
                              && ( (pe) != CC_PTE_DarkPyramid )          \
                              && ( (pe) != CC_PTE_LightPyramid )         \
                              && ( (pe) != CC_PTE_LightWave )            \
                              && ( (pe) != CC_PTE_BrightStar )           \
                              && ( CC_PIECE_IS_VALID( (pe) ) ) )

#define CC_PIECE_IS_WEIGHTLESS(pe) ( ( (pe) == CC_PTE_DarkStarchild )    \
                                  || ( (pe) == CC_PTE_DarkWave )         \
                                  || ( (pe) == CC_PTE_LightWave )        \
                                  || ( (pe) == CC_PTE_LightStarchild ) )

#define CC_PIECE_CAN_BE_ACTIVATED(pe) ( ( (pe) != CC_PTE_DarkKing )      \
                                     && ( (pe) != CC_PTE_None )          \
                                     && ( (pe) != CC_PTE_LightKing )     \
                                     && ( (pe) != CC_PTE_Monolith )      \
                                     && ( CC_PIECE_IS_VALID( (pe) ) ) )

#define CC_PIECE_CAN_BE_CAPTURED(pe) ( ( (pe) != CC_PTE_DimStar )        \
                                    && ( (pe) != CC_PTE_DarkKing )       \
                                    && ( (pe) != CC_PTE_LightKing )      \
                                    && ( (pe) != CC_PTE_BrightStar )     \
                                    && ( (pe) != CC_PTE_Monolith )       \
                                    && ( CC_PIECE_IS_VALID( (pe) ) ) )

#define CC_PAWN_CAN_BE_PROMOTED_TO(pe) ( ( (pe) != CC_PTE_DimStar )      \
                                      && ( (pe) != CC_PTE_DarkKing )     \
                                      && ( (pe) != CC_PTE_DarkPawn )     \
                                      && ( (pe) != CC_PTE_LightPawn )    \
                                      && ( (pe) != CC_PTE_LightKing )    \
                                      && ( (pe) != CC_PTE_BrightStar )   \
                                      && ( (pe) != CC_PTE_Monolith )     \
                                      && ( CC_PIECE_IS_VALID( (pe) ) ) )

#define CC_PIECE_CAN_CASTLE(pe) ( ( (pe) == CC_PTE_DarkKing )        \
                               || ( (pe) == CC_PTE_DarkRook )        \
                               || ( (pe) == CC_PTE_LightRook )       \
                               || ( (pe) == CC_PTE_LightKing ) )

#define CC_PIECE_CAN_DISPLACE(pe) ( ( (pe) == CC_PTE_DarkSerpent )       \
                                 || ( (pe) == CC_PTE_LightSerpent ) )

#define CC_PIECE_CAN_BE_DISPLACED(pe) ( ( (pe) == CC_PTE_DarkPawn )      \
                                     || ( (pe) == CC_PTE_LightPawn ) )

#define CC_PIECE_CAN_BE_DISPLACED_TRANCE_JOURNEY(pe) ( ( (pe) != CC_PTE_DimStar )       \
                                                    && ( (pe) != CC_PTE_DarkKing )      \
                                                    && ( (pe) != CC_PTE_LightKing )     \
                                                    && ( (pe) != CC_PTE_BrightStar )    \
                                                    && ( (pe) != CC_PTE_Monolith )      \
                                                    && ( CC_PIECE_IS_VALID( (pe) ) ) )

#define CC_PIECE_CAN_BE_CONVERTED(pe) ( ( (pe) != CC_PTE_DimStar )       \
                                     && ( (pe) != CC_PTE_DarkKing )      \
                                     && ( (pe) != CC_PTE_LightKing )     \
                                     && ( (pe) != CC_PTE_BrightStar )    \
                                     && ( (pe) != CC_PTE_Monolith )      \
                                     && ( CC_PIECE_IS_VALID( (pe) ) ) )

#define CC_PIECE_CAN_BE_DEMOTED(pe) ( ( (pe) != CC_PTE_DimStar )         \
                                   && ( (pe) != CC_PTE_DarkKing )        \
                                   && ( (pe) != CC_PTE_LightKing )       \
                                   && ( (pe) != CC_PTE_BrightStar )      \
                                   && ( (pe) != CC_PTE_Monolith )        \
                                   && ( CC_PIECE_IS_VALID( (pe) ) ) )

#define CC_PIECE_CAN_BE_RESURRECTED(pe) ( ( (pe) != CC_PTE_DimStar )      \
                                       && ( (pe) != CC_PTE_DarkKing )     \
                                       && ( (pe) != CC_PTE_LightKing )    \
                                       && ( (pe) != CC_PTE_BrightStar )   \
                                       && ( (pe) != CC_PTE_Monolith )     \
                                       && ( CC_PIECE_IS_VALID( (pe) ) ) )

#define CC_PIECE_IS_TELEPORTER(pe) ( ( (pe) == CC_PTE_DimStar )      \
                                  || ( (pe) == CC_PTE_BrightStar )   \
                                  || ( (pe) == CC_PTE_Monolith ) )

#define CC_PIECE_CAN_BE_TELEPORTED(pe) ( ( (pe) != CC_PTE_DimStar )      \
                                      && ( (pe) != CC_PTE_DarkKing )     \
                                      && ( (pe) != CC_PTE_LightKing )    \
                                      && ( (pe) != CC_PTE_BrightStar )   \
                                      && ( (pe) != CC_PTE_Monolith )     \
                                      && ( CC_PIECE_IS_VALID( (pe) ) ) )

#define CC_PIECE_IS_COMPLETELY_TRANSPARENT(pe)  ( ( (pe) == CC_PTE_DarkStarchild )   \
                                               || ( (pe) == CC_PTE_LightStarchild ) )

#define CC_PIECE_IS_TRANSPARENT(pe)  ( ( (pe) == CC_PTE_DarkWave )           \
                                    || ( (pe) == CC_PTE_LightWave )          \
                                    || ( (pe) == CC_PTE_DarkStarchild )      \
                                    || ( (pe) == CC_PTE_LightStarchild ) )

#define CC_PIECE_IS_SEMI_TRANSPARENT(pe) ( ( (pe) != CC_PTE_Monolith )      \
                                        && ( CC_PIECE_IS_VALID( (pe) ) ) )

#define CC_PIECE_IS_SEMI_OPAQUE(pe) ( ( (pe) != CC_PTE_Monolith )          \
                                   && ( (pe) != CC_PTE_DarkWave )          \
                                   && ( (pe) != CC_PTE_LightWave )         \
                                   && ( (pe) != CC_PTE_DarkStarchild )     \
                                   && ( (pe) != CC_PTE_LightStarchild )    \
                                   && ( CC_PIECE_IS_VALID( (pe) ) ) )

#define CC_PIECE_IS_OPAQUE(pe) ( (pe) == CC_PTE_Monolith )

#define CC_PIECE_IS_DIVERGENT(pe) ( ( (pe) == CC_PTE_DarkStarchild )     \
                                 || ( (pe) == CC_PTE_DarkShaman )        \
                                 || ( (pe) == CC_PTE_LightShaman )       \
                                 || ( (pe) == CC_PTE_LightStarchild ) )

#define CC_PIECE_CAN_BE_DIVERGED(pe) ( ( (pe) != CC_PTE_DimStar )        \
                                    && ( (pe) != CC_PTE_DarkStarchild )  \
                                    && ( (pe) != CC_PTE_DarkCentaur )    \
                                    && ( (pe) != CC_PTE_DarkSerpent )    \
                                    && ( (pe) != CC_PTE_DarkKing )       \
                                    && ( (pe) != CC_PTE_None )           \
                                    && ( (pe) != CC_PTE_LightKing )      \
                                    && ( (pe) != CC_PTE_LightSerpent )   \
                                    && ( (pe) != CC_PTE_LightCentaur )   \
                                    && ( (pe) != CC_PTE_LightStarchild ) \
                                    && ( (pe) != CC_PTE_BrightStar )     \
                                    && ( (pe) != CC_PTE_Monolith )       \
                                    && ( CC_PIECE_IS_VALID( (pe) ) ) )

#define CC_WAVE_CAN_BE_DIVERGED(activator) ( ( (activator) != CC_PTE_DarkCentaur )       \
                                          && ( (activator) != CC_PTE_DarkSerpent )       \
                                          && ( (activator) != CC_PTE_DarkUnicorn )       \
                                          && ( (activator) != CC_PTE_None )              \
                                          && ( (activator) != CC_PTE_LightUnicorn )      \
                                          && ( (activator) != CC_PTE_LightSerpent )      \
                                          && ( (activator) != CC_PTE_LightCentaur )      \
                                          && ( CC_PIECE_IS_ACTIVATOR( (activator) ) ) )

#define CC_PIECE_IS_SINGLE_STEP (pe) ( CC_PIECE_IS_PAWN(pe)             \
                                    || CC_PIECE_IS_KNIGHT(pe)           \
                                    || CC_PIECE_IS_KING(pe)             \
                                    || CC_PIECE_IS_UNICORN(pe)          \
                                    || CC_PIECE_IS_STARCHILD(pe)        \
                                    || CC_PIECE_IS_STAR(pe) )

#define CC_PIECE_IS_ONE_STEP(pe) ( CC_PIECE_IS_BISHOP(pe)        \
                                || CC_PIECE_IS_ROOK(pe)          \
                                || CC_PIECE_IS_QUEEN(pe)         \
                                || CC_PIECE_IS_PEGASUS(pe)       \
                                || CC_PIECE_IS_PYRAMID(pe)       \
                                || CC_PIECE_IS_GRENADIER(pe)     \
                                || CC_PIECE_IS_SHAMAN(pe) )

#define CC_PIECE_IS_TWO_STEP(pe) ( CC_PIECE_IS_CENTAUR(pe) )

#define CC_WAVE_IS_TWO_STEP(activator) ( CC_PIECE_IS_UNICORN(activator)   \
                                      || CC_PIECE_IS_CENTAUR(activator)   \
                                      || CC_PIECE_IS_SERPENT(activator) )

#define CC_PIECE_HAS_NEW_STEP_AFTER_EACH(pe)  ( CC_PIECE_IS_SERPENT(pe)         \
                                             || CC_PIECE_IS_SCOUT(pe)           \
                                             || CC_PIECE_IS_MONOLITH(pe) )

typedef enum CcPieceTagEnum {
    CC_PTE_DimStar = -29,

    CC_PTE_DarkStarchild,
    CC_PTE_DarkShaman,
    CC_PTE_DarkSerpent,

    CC_PTE_DarkGrenadier_RushedCurrent,
    CC_PTE_DarkGrenadier_RushedPrevious,
    CC_PTE_DarkGrenadier_CanRush,
    CC_PTE_DarkGrenadier,

    CC_PTE_DarkScout_RushedCurrent,
    CC_PTE_DarkScout_RushedPrevious,
    CC_PTE_DarkScout_CanRush,
    CC_PTE_DarkScout,

    CC_PTE_DarkCentaur,
    CC_PTE_DarkWave,
    CC_PTE_DarkUnicorn,
    CC_PTE_DarkPyramid,
    CC_PTE_DarkPegasus,

    CC_PTE_DarkKing_CanCastle,
    CC_PTE_DarkKing,

    CC_PTE_DarkQueen,

    CC_PTE_DarkRook_CanCastle,
    CC_PTE_DarkRook,

    CC_PTE_DarkBishop,
    CC_PTE_DarkKnight,

    CC_PTE_DarkPawn_DelayedPromotion,
    CC_PTE_DarkPawn_RushedCurrent,
    CC_PTE_DarkPawn_RushedPrevious,
    CC_PTE_DarkPawn_CanRush,
    CC_PTE_DarkPawn,

    CC_PTE_None = 0,

    CC_PTE_LightPawn,
    CC_PTE_LightPawn_CanRush,
    CC_PTE_LightPawn_RushedPrevious,
    CC_PTE_LightPawn_RushedCurrent,
    CC_PTE_LightPawn_DelayedPromotion,

    CC_PTE_LightKnight,
    CC_PTE_LightBishop,

    CC_PTE_LightRook,
    CC_PTE_LightRook_CanCastle,

    CC_PTE_LightQueen,

    CC_PTE_LightKing,
    CC_PTE_LightKing_CanCastle,

    CC_PTE_LightPegasus,
    CC_PTE_LightPyramid,
    CC_PTE_LightUnicorn,
    CC_PTE_LightWave,
    CC_PTE_LightCentaur,

    CC_PTE_LightScout,
    CC_PTE_LightScout_CanRush,
    CC_PTE_LightScout_RushedPrevious,
    CC_PTE_LightScout_RushedCurrent,

    CC_PTE_LightGrenadier,
    CC_PTE_LightGrenadier_CanRush,
    CC_PTE_LightGrenadier_RushedPrevious,
    CC_PTE_LightGrenadier_RushedCurrent,

    CC_PTE_LightSerpent,
    CC_PTE_LightShaman,
    CC_PTE_LightStarchild,

    CC_PTE_BrightStar = 29,

    CC_PTE_Monolith,
} CcPieceTagEnum;

// CC_TE_CanRush, /* Pawn can rush, persistent tag. */
// CC_TE_CanCastle, /* Rooks, Kings can castle, persistent tag. */
// CC_TE_DelayedPromotion, /* Pawn delayed promotion, persistent tag. */
//
// CC_TE_EnPassant_Previous, /* A private rushed in previous turn, this is en passant opportunity tag. */
// CC_TE_EnPassant_Current, /* A private rushed in current turn (in a previous ply), this will become en passant opportunity for opponent in the very next turn. */

typedef signed char CcPieceTagType;


typedef char (*cc_piece_fp_char_value_t)( CcPieceTagType pe );

CcPieceTagType cc_piece_from_symbol( char symbol, bool is_light );

bool cc_piece_symbol_is_valid( char c );

CcPieceTagType cc_piece_opposite( CcPieceTagType pe );

char cc_piece_as_char( CcPieceTagType pe );

// TODO :: DOCS
CcPieceTagType cc_piece_from_char( char piece, char tag );

char const * cc_piece_label( CcPieceTagType pe, bool capitalize, bool empty_field );

char cc_piece_symbol( CcPieceTagType pe );

CcPieceTagType cc_piece_demoting_to( CcPieceTagType pe );

bool cc_piece_is_dark( CcPieceTagType pe );

bool cc_piece_is_light( CcPieceTagType pe );

bool cc_piece_has_color( CcPieceTagType pe );

bool cc_piece_has_shade( CcPieceTagType pe );

bool cc_piece_has_prefix( CcPieceTagType pe );

char const * cc_piece_prefix( CcPieceTagType pe, bool capitalize );

bool cc_piece_has_congruent_type( char symbol, CcPieceTagType pe );

bool cc_piece_is_equal( char symbol, bool is_light, CcPieceTagType pe );

bool cc_piece_has_same_type( CcPieceTagType pe_1, CcPieceTagType pe_2 );

bool cc_piece_has_same_color( CcPieceTagType pe_1, CcPieceTagType pe_2 );

bool cc_piece_has_same_shade( CcPieceTagType pe_1, CcPieceTagType pe_2 );

bool cc_piece_is_opposite( CcPieceTagType pe_1, CcPieceTagType pe_2 );

bool cc_piece_has_same_owner( CcPieceTagType pe_1, CcPieceTagType pe_2 );

bool cc_piece_has_different_owner( CcPieceTagType pe_1, CcPieceTagType pe_2 );

bool cc_piece_is_owned_figure( CcPieceTagType pe );

bool cc_piece_is_figure( CcPieceTagType pe );

char const * cc_piece_as_string( CcPieceTagType pe, bool capitalize, bool empty_field );


#endif /* __CC_PIECE_H__ */
