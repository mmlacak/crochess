// Copyright (c) 2021 Mario Mlačak, mmlacak@gmail.com
// Licensed under GNU GPL v3+ license. See LICENSING, COPYING files for details.

#ifndef __CC_PIECE_H__
#define __CC_PIECE_H__

#include <stdbool.h>


// TODO :: DOCS :: param: pe --> ptt

#define CC_PIECE_IS_ENUMERATOR(ptt) ( ( CC_PTE_DimStar <= (ptt) ) && ( (ptt) <= CC_PTE_Monolith ) )

#define CC_PIECE_IS_VALID(ptt) ( ( (ptt) != CC_PTE_None ) && ( CC_PTE_DimStar <= (ptt) ) && ( (ptt) <= CC_PTE_Monolith ) )

#define CC_PIECE_IS_NONE(ptt) ( (ptt) == CC_PTE_None )

#define CC_PIECE_IS_PAWN(ptt) ( ( (ptt) == CC_PTE_LightPawn ) || ( (ptt) == CC_PTE_DarkPawn ) )

#define CC_PIECE_IS_KNIGHT(ptt) ( ( (ptt) == CC_PTE_LightKnight ) || ( (ptt) == CC_PTE_DarkKnight ) )

#define CC_PIECE_IS_BISHOP(ptt) ( ( (ptt) == CC_PTE_LightBishop ) || ( (ptt) == CC_PTE_DarkBishop ) )

#define CC_PIECE_IS_ROOK(ptt) ( ( (ptt) == CC_PTE_LightRook ) || ( (ptt) == CC_PTE_DarkRook ) )

#define CC_PIECE_IS_QUEEN(ptt) ( ( (ptt) == CC_PTE_LightQueen ) || ( (ptt) == CC_PTE_DarkQueen ) )

#define CC_PIECE_IS_KING(ptt) ( ( (ptt) == CC_PTE_LightKing ) || ( (ptt) == CC_PTE_DarkKing ) )

#define CC_PIECE_IS_PEGASUS(ptt) ( ( (ptt) == CC_PTE_LightPegasus ) || ( (ptt) == CC_PTE_DarkPegasus ) )

#define CC_PIECE_IS_PYRAMID(ptt) ( ( (ptt) == CC_PTE_LightPyramid ) || ( (ptt) == CC_PTE_DarkPyramid ) )

#define CC_PIECE_IS_UNICORN(ptt) ( ( (ptt) == CC_PTE_LightUnicorn ) || ( (ptt) == CC_PTE_DarkUnicorn ) )

#define CC_PIECE_IS_WAVE(ptt) ( ( (ptt) == CC_PTE_LightWave ) || ( (ptt) == CC_PTE_DarkWave ) )

#define CC_PIECE_IS_STAR(ptt) ( ( (ptt) == CC_PTE_BrightStar ) || ( (ptt) == CC_PTE_DimStar ) )

#define CC_PIECE_IS_CENTAUR(ptt) ( ( (ptt) == CC_PTE_LightCentaur ) || ( (ptt) == CC_PTE_DarkCentaur ) )

#define CC_PIECE_IS_SCOUT(ptt) ( ( (ptt) == CC_PTE_LightScout ) || ( (ptt) == CC_PTE_DarkScout ) )

#define CC_PIECE_IS_GRENADIER(ptt) ( ( (ptt) == CC_PTE_LightGrenadier ) || ( (ptt) == CC_PTE_DarkGrenadier ) )

#define CC_PIECE_IS_SERPENT(ptt) ( ( (ptt) == CC_PTE_LightSerpent ) || ( (ptt) == CC_PTE_DarkSerpent ) )

#define CC_PIECE_IS_SHAMAN(ptt) ( ( (ptt) == CC_PTE_LightShaman ) || ( (ptt) == CC_PTE_DarkShaman ) )

#define CC_PIECE_IS_MONOLITH(ptt) ( (ptt) == CC_PTE_Monolith )

#define CC_PIECE_IS_STARCHILD(ptt) ( ( (ptt) == CC_PTE_LightStarchild ) || ( (ptt) == CC_PTE_DarkStarchild ) )

#define CC_PIECE_IS_TROOPER(ptt) ( ( (ptt) == CC_PTE_DarkGrenadier )       \
                                || ( (ptt) == CC_PTE_DarkScout )           \
                                || ( (ptt) == CC_PTE_LightScout )          \
                                || ( (ptt) == CC_PTE_LightGrenadier ) )

#define CC_PIECE_IS_PRIVATE(ptt) ( ( (ptt) == CC_PTE_DarkGrenadier )       \
                                || ( (ptt) == CC_PTE_DarkScout )           \
                                || ( (ptt) == CC_PTE_DarkPawn )            \
                                || ( (ptt) == CC_PTE_LightPawn )           \
                                || ( (ptt) == CC_PTE_LightScout )          \
                                || ( (ptt) == CC_PTE_LightGrenadier ) )

#define CC_PIECE_IS_MATERIAL(ptt) ( ( (ptt) != CC_PTE_DarkWave )           \
                                 && ( (ptt) != CC_PTE_LightWave )          \
                                 && ( CC_PIECE_IS_VALID( (ptt) ) ) )

#define CC_PIECE_IS_CELESTIAL(ptt) ( ( (ptt) == CC_PTE_DarkStarchild )     \
                                  || ( (ptt) == CC_PTE_DarkWave )          \
                                  || ( (ptt) == CC_PTE_LightWave )         \
                                  || ( (ptt) == CC_PTE_LightStarchild )    \
                                  || ( (ptt) == CC_PTE_Monolith ) )


#define CC_PIECE_HAS_OWNER(ptt) ( ( (ptt) != CC_PTE_DimStar )              \
                               && ( (ptt) != CC_PTE_BrightStar )           \
                               && ( (ptt) != CC_PTE_Monolith )             \
                               && ( CC_PIECE_IS_VALID( (ptt) ) ) )

#define CC_PIECE_IS_ACTIVATOR(ptt) ( ( (ptt) != CC_PTE_DimStar )           \
                                  && ( (ptt) != CC_PTE_DarkWave )          \
                                  && ( (ptt) != CC_PTE_LightWave )         \
                                  && ( (ptt) != CC_PTE_BrightStar )        \
                                  && ( (ptt) != CC_PTE_Monolith )          \
                                  && ( CC_PIECE_IS_VALID( (ptt) ) ) )

#define CC_PIECE_CAN_ACTIVATE(ptt) ( ( (ptt) != CC_PTE_DimStar )           \
                                  && ( (ptt) != CC_PTE_BrightStar )        \
                                  && ( (ptt) != CC_PTE_Monolith )          \
                                  && ( CC_PIECE_IS_VALID( (ptt) ) ) )

#define CC_PIECE_CAN_ACTIVATE_STAR(ptt) ( ( (ptt) == CC_PTE_DarkStarchild )        \
                                       || ( (ptt) == CC_PTE_LightStarchild ) )

#define CC_PIECE_CAN_CAPTURE(ptt) ( ( (ptt) != CC_PTE_DarkStarchild )      \
                                 && ( (ptt) != CC_PTE_DimStar )            \
                                 && ( (ptt) != CC_PTE_DarkWave )           \
                                 && ( (ptt) != CC_PTE_LightWave )          \
                                 && ( (ptt) != CC_PTE_BrightStar )         \
                                 && ( (ptt) != CC_PTE_LightStarchild )     \
                                 && ( (ptt) != CC_PTE_Monolith )           \
                                 && ( CC_PIECE_IS_VALID( (ptt) ) ) )

#define CC_PIECE_CAN_CAPTURE_EN_PASSANT(ptt) ( CC_PIECE_IS_PRIVATE( (ptt) ) )

#define CC_PIECE_CAN_BE_CAPTURED_EN_PASSANT(ptt) ( CC_PIECE_IS_PRIVATE( (ptt) ) )

#define CC_PIECE_IS_PASIVE(ptt) ( ( (ptt) == CC_PTE_DimStar )              \
                               || ( (ptt) == CC_PTE_DarkWave )             \
                               || ( (ptt) == CC_PTE_DarkPyramid )          \
                               || ( (ptt) == CC_PTE_LightPyramid )         \
                               || ( (ptt) == CC_PTE_LightWave )            \
                               || ( (ptt) == CC_PTE_BrightStar ) )

#define CC_PIECE_IS_ACTIVE(ptt) ( ( (ptt) != CC_PTE_DimStar )              \
                               && ( (ptt) != CC_PTE_DarkWave )             \
                               && ( (ptt) != CC_PTE_DarkPyramid )          \
                               && ( (ptt) != CC_PTE_LightPyramid )         \
                               && ( (ptt) != CC_PTE_LightWave )            \
                               && ( (ptt) != CC_PTE_BrightStar )           \
                               && ( CC_PIECE_IS_VALID( (ptt) ) ) )

#define CC_PIECE_IS_WEIGHTLESS(ptt) ( ( (ptt) == CC_PTE_DarkStarchild )    \
                                   || ( (ptt) == CC_PTE_DarkWave )         \
                                   || ( (ptt) == CC_PTE_LightWave )        \
                                   || ( (ptt) == CC_PTE_LightStarchild ) )

#define CC_PIECE_CAN_BE_ACTIVATED(ptt) ( ( (ptt) != CC_PTE_DarkKing )      \
                                      && ( (ptt) != CC_PTE_None )          \
                                      && ( (ptt) != CC_PTE_LightKing )     \
                                      && ( (ptt) != CC_PTE_Monolith )      \
                                      && ( CC_PIECE_IS_VALID( (ptt) ) ) )

#define CC_PIECE_CAN_BE_CAPTURED(ptt) ( ( (ptt) != CC_PTE_DimStar )        \
                                     && ( (ptt) != CC_PTE_DarkKing )       \
                                     && ( (ptt) != CC_PTE_LightKing )      \
                                     && ( (ptt) != CC_PTE_BrightStar )     \
                                     && ( (ptt) != CC_PTE_Monolith )       \
                                     && ( CC_PIECE_IS_VALID( (ptt) ) ) )

#define CC_PAWN_CAN_BE_PROMOTED_TO(ptt) ( ( (ptt) != CC_PTE_DimStar )      \
                                       && ( (ptt) != CC_PTE_DarkKing )     \
                                       && ( (ptt) != CC_PTE_DarkPawn )     \
                                       && ( (ptt) != CC_PTE_LightPawn )    \
                                       && ( (ptt) != CC_PTE_LightKing )    \
                                       && ( (ptt) != CC_PTE_BrightStar )   \
                                       && ( (ptt) != CC_PTE_Monolith )     \
                                       && ( CC_PIECE_IS_VALID( (ptt) ) ) )

#define CC_PIECE_CAN_CASTLE(ptt) ( ( (ptt) == CC_PTE_DarkKing )        \
                                || ( (ptt) == CC_PTE_DarkRook )        \
                                || ( (ptt) == CC_PTE_LightRook )       \
                                || ( (ptt) == CC_PTE_LightKing ) )

#define CC_PIECE_CAN_DISPLACE(ptt) ( ( (ptt) == CC_PTE_DarkSerpent )       \
                                  || ( (ptt) == CC_PTE_LightSerpent ) )

#define CC_PIECE_CAN_BE_DISPLACED(ptt) ( ( (ptt) == CC_PTE_DarkPawn )      \
                                      || ( (ptt) == CC_PTE_LightPawn ) )

#define CC_PIECE_CAN_BE_DISPLACED_TRANCE_JOURNEY(ptt) ( ( (ptt) != CC_PTE_DimStar )       \
                                                     && ( (ptt) != CC_PTE_DarkKing )      \
                                                     && ( (ptt) != CC_PTE_LightKing )     \
                                                     && ( (ptt) != CC_PTE_BrightStar )    \
                                                     && ( (ptt) != CC_PTE_Monolith )      \
                                                     && ( CC_PIECE_IS_VALID( (ptt) ) ) )

#define CC_PIECE_CAN_BE_CONVERTED(ptt) ( ( (ptt) != CC_PTE_DimStar )       \
                                      && ( (ptt) != CC_PTE_DarkKing )      \
                                      && ( (ptt) != CC_PTE_LightKing )     \
                                      && ( (ptt) != CC_PTE_BrightStar )    \
                                      && ( (ptt) != CC_PTE_Monolith )      \
                                      && ( CC_PIECE_IS_VALID( (ptt) ) ) )

#define CC_PIECE_CAN_BE_DEMOTED(ptt) ( ( (ptt) != CC_PTE_DimStar )         \
                                    && ( (ptt) != CC_PTE_DarkKing )        \
                                    && ( (ptt) != CC_PTE_LightKing )       \
                                    && ( (ptt) != CC_PTE_BrightStar )      \
                                    && ( (ptt) != CC_PTE_Monolith )        \
                                    && ( CC_PIECE_IS_VALID( (ptt) ) ) )

#define CC_PIECE_CAN_BE_RESURRECTED(ptt) ( ( (ptt) != CC_PTE_DimStar )      \
                                        && ( (ptt) != CC_PTE_DarkKing )     \
                                        && ( (ptt) != CC_PTE_LightKing )    \
                                        && ( (ptt) != CC_PTE_BrightStar )   \
                                        && ( (ptt) != CC_PTE_Monolith )     \
                                        && ( CC_PIECE_IS_VALID( (ptt) ) ) )

#define CC_PIECE_IS_TELEPORTER(ptt) ( ( (ptt) == CC_PTE_DimStar )      \
                                   || ( (ptt) == CC_PTE_BrightStar )   \
                                   || ( (ptt) == CC_PTE_Monolith ) )

#define CC_PIECE_CAN_BE_TELEPORTED(ptt) ( ( (ptt) != CC_PTE_DimStar )      \
                                       && ( (ptt) != CC_PTE_DarkKing )     \
                                       && ( (ptt) != CC_PTE_LightKing )    \
                                       && ( (ptt) != CC_PTE_BrightStar )   \
                                       && ( (ptt) != CC_PTE_Monolith )     \
                                       && ( CC_PIECE_IS_VALID( (ptt) ) ) )

#define CC_PIECE_IS_COMPLETELY_TRANSPARENT(ptt)  ( ( (ptt) == CC_PTE_DarkStarchild )   \
                                                || ( (ptt) == CC_PTE_LightStarchild ) )

#define CC_PIECE_IS_TRANSPARENT(ptt)  ( ( (ptt) == CC_PTE_DarkWave )           \
                                     || ( (ptt) == CC_PTE_LightWave )          \
                                     || ( (ptt) == CC_PTE_DarkStarchild )      \
                                     || ( (ptt) == CC_PTE_LightStarchild ) )

#define CC_PIECE_IS_SEMI_TRANSPARENT(ptt) ( ( (ptt) != CC_PTE_Monolith )      \
                                         && ( CC_PIECE_IS_VALID( (ptt) ) ) )

#define CC_PIECE_IS_SEMI_OPAQUE(ptt) ( ( (ptt) != CC_PTE_Monolith )          \
                                    && ( (ptt) != CC_PTE_DarkWave )          \
                                    && ( (ptt) != CC_PTE_LightWave )         \
                                    && ( (ptt) != CC_PTE_DarkStarchild )     \
                                    && ( (ptt) != CC_PTE_LightStarchild )    \
                                    && ( CC_PIECE_IS_VALID( (ptt) ) ) )

#define CC_PIECE_IS_OPAQUE(ptt) ( (ptt) == CC_PTE_Monolith )

#define CC_PIECE_IS_DIVERGENT(ptt) ( ( (ptt) == CC_PTE_DarkStarchild )     \
                                  || ( (ptt) == CC_PTE_DarkShaman )        \
                                  || ( (ptt) == CC_PTE_LightShaman )       \
                                  || ( (ptt) == CC_PTE_LightStarchild ) )

#define CC_PIECE_CAN_BE_DIVERGED(ptt) ( ( (ptt) != CC_PTE_DimStar )        \
                                     && ( (ptt) != CC_PTE_DarkStarchild )  \
                                     && ( (ptt) != CC_PTE_DarkCentaur )    \
                                     && ( (ptt) != CC_PTE_DarkSerpent )    \
                                     && ( (ptt) != CC_PTE_DarkKing )       \
                                     && ( (ptt) != CC_PTE_None )           \
                                     && ( (ptt) != CC_PTE_LightKing )      \
                                     && ( (ptt) != CC_PTE_LightSerpent )   \
                                     && ( (ptt) != CC_PTE_LightCentaur )   \
                                     && ( (ptt) != CC_PTE_LightStarchild ) \
                                     && ( (ptt) != CC_PTE_BrightStar )     \
                                     && ( (ptt) != CC_PTE_Monolith )       \
                                     && ( CC_PIECE_IS_VALID( (ptt) ) ) )

#define CC_WAVE_CAN_BE_DIVERGED(activator) ( ( (activator) != CC_PTE_DarkCentaur )       \
                                          && ( (activator) != CC_PTE_DarkSerpent )       \
                                          && ( (activator) != CC_PTE_DarkUnicorn )       \
                                          && ( (activator) != CC_PTE_None )              \
                                          && ( (activator) != CC_PTE_LightUnicorn )      \
                                          && ( (activator) != CC_PTE_LightSerpent )      \
                                          && ( (activator) != CC_PTE_LightCentaur )      \
                                          && ( CC_PIECE_IS_ACTIVATOR( (activator) ) ) )

#define CC_PIECE_IS_SINGLE_STEP (ptt) ( CC_PIECE_IS_PAWN(ptt)             \
                                     || CC_PIECE_IS_KNIGHT(ptt)           \
                                     || CC_PIECE_IS_KING(ptt)             \
                                     || CC_PIECE_IS_UNICORN(ptt)          \
                                     || CC_PIECE_IS_STARCHILD(ptt)        \
                                     || CC_PIECE_IS_STAR(ptt) )

#define CC_PIECE_IS_ONE_STEP(ptt) ( CC_PIECE_IS_BISHOP(ptt)        \
                                 || CC_PIECE_IS_ROOK(ptt)          \
                                 || CC_PIECE_IS_QUEEN(ptt)         \
                                 || CC_PIECE_IS_PEGASUS(ptt)       \
                                 || CC_PIECE_IS_PYRAMID(ptt)       \
                                 || CC_PIECE_IS_GRENADIER(ptt)     \
                                 || CC_PIECE_IS_SHAMAN(ptt) )

#define CC_PIECE_IS_TWO_STEP(ptt) ( CC_PIECE_IS_CENTAUR(ptt) )

#define CC_WAVE_IS_TWO_STEP(activator) ( CC_PIECE_IS_UNICORN(activator)   \
                                      || CC_PIECE_IS_CENTAUR(activator)   \
                                      || CC_PIECE_IS_SERPENT(activator) )

#define CC_PIECE_HAS_NEW_STEP_AFTER_EACH(ptt)  ( CC_PIECE_IS_SERPENT(ptt)         \
                                              || CC_PIECE_IS_SCOUT(ptt)           \
                                              || CC_PIECE_IS_MONOLITH(ptt) )

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


typedef char (*cc_piece_fp_char_value_t)( CcPieceTagType ptt );

CcPieceTagType cc_piece_from_symbol( char symbol, bool is_light );

bool cc_piece_symbol_is_valid( char c );

CcPieceTagType cc_piece_opposite( CcPieceTagType ptt );

char cc_piece_as_char( CcPieceTagType ptt );

// TODO :: DOCS
CcPieceTagType cc_piece_from_char( char piece, char tag );

char const * cc_piece_label( CcPieceTagType ptt, bool capitalize, bool empty_field );

char cc_piece_symbol( CcPieceTagType ptt );

CcPieceTagType cc_piece_demoting_to( CcPieceTagType ptt );

bool cc_piece_is_dark( CcPieceTagType ptt );

bool cc_piece_is_light( CcPieceTagType ptt );

bool cc_piece_has_color( CcPieceTagType ptt );

bool cc_piece_has_shade( CcPieceTagType ptt );

bool cc_piece_has_prefix( CcPieceTagType ptt );

char const * cc_piece_prefix( CcPieceTagType ptt, bool capitalize );

bool cc_piece_has_congruent_type( char symbol, CcPieceTagType ptt );

bool cc_piece_is_equal( char symbol, bool is_light, CcPieceTagType ptt );

bool cc_piece_has_same_type( CcPieceTagType pe_1, CcPieceTagType pe_2 );

bool cc_piece_has_same_color( CcPieceTagType pe_1, CcPieceTagType pe_2 );

bool cc_piece_has_same_shade( CcPieceTagType pe_1, CcPieceTagType pe_2 );

bool cc_piece_is_opposite( CcPieceTagType pe_1, CcPieceTagType pe_2 );

bool cc_piece_has_same_owner( CcPieceTagType pe_1, CcPieceTagType pe_2 );

bool cc_piece_has_different_owner( CcPieceTagType pe_1, CcPieceTagType pe_2 );

bool cc_piece_is_owned_figure( CcPieceTagType ptt );

bool cc_piece_is_figure( CcPieceTagType ptt );

char const * cc_piece_as_string( CcPieceTagType ptt, bool capitalize, bool empty_field );


#endif /* __CC_PIECE_H__ */
