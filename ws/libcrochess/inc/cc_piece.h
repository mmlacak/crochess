// Copyright (c) 2021 Mario Mlačak, mmlacak@gmail.com
// Licensed under GNU GPL v3+ license. See LICENSING, COPYING files for details.

#ifndef __CC_PIECE_H__
#define __CC_PIECE_H__

#include <stdbool.h>


#define CC_PIECE_IS_ENUMERATOR(pte) ( ( CC_PTE_DimStar <= (pte) ) && ( (pte) <= CC_PTE_Monolith ) )

#define CC_PIECE_IS_VALID(pte) ( ( (pte) != CC_PTE_None ) && ( CC_PTE_DimStar <= (pte) ) && ( (pte) <= CC_PTE_Monolith ) )

#define CC_PIECE_IS_NONE(pte) ( (pte) == CC_PTE_None )


#define CC_PIECE_IS_JUST_PAWN(pte) ( ( (pte) == CC_PTE_LightPawn ) || ( (pte) == CC_PTE_DarkPawn ) )

#define CC_PIECE_IS_PAWN_CAN_RUSH(pte) ( ( (pte) == CC_PTE_LightPawn_CanRush ) || ( (pte) == CC_PTE_DarkPawn_CanRush ) )

#define CC_PIECE_IS_PAWN_RUSHED_PREVIOUS(pte) ( ( (pte) == CC_PTE_LightPawn_RushedPrevious ) || ( (pte) == CC_PTE_DarkPawn_RushedPrevious ) )

#define CC_PIECE_IS_PAWN_RUSHED_CURRENT(pte) ( ( (pte) == CC_PTE_LightPawn_RushedCurrent ) || ( (pte) == CC_PTE_DarkPawn_RushedCurrent ) )

#define CC_PIECE_IS_PAWN_DELAYED_PROMOTION(pte) ( ( (pte) == CC_PTE_LightPawn_DelayedPromotion ) || ( (pte) == CC_PTE_DarkPawn_DelayedPromotion ) )

#define CC_PIECE_IS_PAWN(pte) ( CC_PIECE_IS_JUST_PAWN( (pte) ) ||                               \
                                CC_PIECE_IS_PAWN_CAN_RUSH( (pte) ) ||                           \
                                CC_PIECE_IS_PAWN_RUSHED_PREVIOUS( (pte) ) ||                    \
                                CC_PIECE_IS_PAWN_RUSHED_CURRENT( (pte) ) )

#define CC_PIECE_IS_KNIGHT(pte) ( ( (pte) == CC_PTE_LightKnight ) || ( (pte) == CC_PTE_DarkKnight ) )

#define CC_PIECE_IS_BISHOP(pte) ( ( (pte) == CC_PTE_LightBishop ) || ( (pte) == CC_PTE_DarkBishop ) )

#define CC_PIECE_IS_JUST_ROOK(pte) ( ( (pte) == CC_PTE_LightRook ) || ( (pte) == CC_PTE_DarkRook ) )

#define CC_PIECE_IS_ROOK_CAN_CASTLE(pte) ( ( (pte) == CC_PTE_LightRook_CanCastle ) || ( (pte) == CC_PTE_DarkRook_CanCastle ) )

#define CC_PIECE_IS_ROOK(pte) ( CC_PIECE_IS_JUST_ROOK( (pte) ) || CC_PIECE_IS_ROOK_CAN_CASTLE( (pte) ) )

#define CC_PIECE_IS_QUEEN(pte) ( ( (pte) == CC_PTE_LightQueen ) || ( (pte) == CC_PTE_DarkQueen ) )

#define CC_PIECE_IS_JUST_KING(pte) ( ( (pte) == CC_PTE_LightKing ) || ( (pte) == CC_PTE_DarkKing ) )

#define CC_PIECE_IS_KING_CAN_CASTLE(pte) ( ( (pte) == CC_PTE_LightKing_CanCastle ) || ( (pte) == CC_PTE_DarkKing_CanCastle ) )

#define CC_PIECE_IS_KING(pte) ( CC_PIECE_IS_JUST_KING( (pte) ) || CC_PIECE_IS_KING_CAN_CASTLE( (pte) ) )

#define CC_PIECE_IS_PEGASUS(pte) ( ( (pte) == CC_PTE_LightPegasus ) || ( (pte) == CC_PTE_DarkPegasus ) )

#define CC_PIECE_IS_PYRAMID(pte) ( ( (pte) == CC_PTE_LightPyramid ) || ( (pte) == CC_PTE_DarkPyramid ) )

#define CC_PIECE_IS_UNICORN(pte) ( ( (pte) == CC_PTE_LightUnicorn ) || ( (pte) == CC_PTE_DarkUnicorn ) )

#define CC_PIECE_IS_WAVE(pte) ( ( (pte) == CC_PTE_LightWave ) || ( (pte) == CC_PTE_DarkWave ) )

#define CC_PIECE_IS_STAR(pte) ( ( (pte) == CC_PTE_BrightStar ) || ( (pte) == CC_PTE_DimStar ) )

#define CC_PIECE_IS_CENTAUR(pte) ( ( (pte) == CC_PTE_LightCentaur ) || ( (pte) == CC_PTE_DarkCentaur ) )

#define CC_PIECE_IS_JUST_SCOUT(pte) ( ( (pte) == CC_PTE_LightScout ) || ( (pte) == CC_PTE_DarkScout ) )

#define CC_PIECE_IS_SCOUT_CAN_RUSH(pte) ( ( (pte) == CC_PTE_LightScout_CanRush ) || ( (pte) == CC_PTE_DarkScout_CanRush ) )

#define CC_PIECE_IS_SCOUT_RUSHED_PREVIOUS(pte) ( ( (pte) == CC_PTE_LightScout_RushedPrevious ) || ( (pte) == CC_PTE_DarkScout_RushedPrevious ) )

#define CC_PIECE_IS_SCOUT_RUSHED_CURRENT(pte) ( ( (pte) == CC_PTE_LightScout_RushedCurrent ) || ( (pte) == CC_PTE_DarkScout_RushedCurrent ) )

#define CC_PIECE_IS_SCOUT(pte) ( CC_PIECE_IS_JUST_SCOUT( (pte) ) ||                             \
                                 CC_PIECE_IS_SCOUT_CAN_RUSH( (pte) ) ||                         \
                                 CC_PIECE_IS_SCOUT_RUSHED_PREVIOUS( (pte) ) ||                  \
                                 CC_PIECE_IS_SCOUT_RUSHED_CURRENT( (pte) ) )

#define CC_PIECE_IS_JUST_GRENADIER(pte) ( ( (pte) == CC_PTE_LightGrenadier ) || ( (pte) == CC_PTE_DarkGrenadier ) )

#define CC_PIECE_IS_GRENADIER_CAN_RUSH(pte) ( ( (pte) == CC_PTE_LightGrenadier_CanRush ) || ( (pte) == CC_PTE_DarkGrenadier_CanRush ) )

#define CC_PIECE_IS_GRENADIER_RUSHED_PREVIOUS(pte) ( ( (pte) == CC_PTE_LightGrenadier_RushedPrevious ) || ( (pte) == CC_PTE_DarkGrenadier_RushedPrevious ) )

#define CC_PIECE_IS_GRENADIER_RUSHED_CURRENT(pte) ( ( (pte) == CC_PTE_LightGrenadier_RushedCurrent ) || ( (pte) == CC_PTE_DarkGrenadier_RushedCurrent ) )

#define CC_PIECE_IS_GRENADIER(pte) ( CC_PIECE_IS_JUST_GRENADIER( (pte) ) ||                     \
                                     CC_PIECE_IS_GRENADIER_CAN_RUSH( (pte) ) ||                 \
                                     CC_PIECE_IS_GRENADIER_RUSHED_PREVIOUS( (pte) ) ||          \
                                     CC_PIECE_IS_GRENADIER_RUSHED_CURRENT( (pte) ) )

#define CC_PIECE_IS_SERPENT(pte) ( ( (pte) == CC_PTE_LightSerpent ) || ( (pte) == CC_PTE_DarkSerpent ) )

#define CC_PIECE_IS_SHAMAN(pte) ( ( (pte) == CC_PTE_LightShaman ) || ( (pte) == CC_PTE_DarkShaman ) )

#define CC_PIECE_IS_MONOLITH(pte) ( (pte) == CC_PTE_Monolith )

#define CC_PIECE_IS_STARCHILD(pte) ( ( (pte) == CC_PTE_LightStarchild ) || ( (pte) == CC_PTE_DarkStarchild ) )


// TODO :: Wave cannot activate Pyramid, only material pieces

#define CC_PIECE_IS_TROOPER(pte) ( CC_PIECE_IS_SCOUT( (pte) ) || CC_PIECE_IS_GRENADIER( (pte) ) )

#define CC_PIECE_IS_PRIVATE(pte) ( CC_PIECE_IS_PAWN( (pte) ) || CC_PIECE_IS_SCOUT( (pte) ) || CC_PIECE_IS_GRENADIER( (pte) ) )

#define CC_PIECE_IS_MATERIAL(pte) ( ( (pte) != CC_PTE_DarkWave )           \
                                 && ( (pte) != CC_PTE_LightWave ) )

#define CC_PIECE_IS_CELESTIAL(pte) ( ( (pte) == CC_PTE_DarkStarchild )     \
                                  || ( (pte) == CC_PTE_DarkWave )          \
                                  || ( (pte) == CC_PTE_LightWave )         \
                                  || ( (pte) == CC_PTE_LightStarchild )    \
                                  || ( (pte) == CC_PTE_Monolith ) )

#define CC_PIECE_IS_DARK(pte) ( ( CC_PTE_DarkStarchild <= (pte) ) && ( (pte) <= CC_PTE_DarkPawn ) )

#define CC_PIECE_IS_LIGHT(pte) ( ( CC_PTE_LightPawn <= (pte) ) && ( (pte) <= CC_PTE_LightStarchild ) )


#define CC_PIECE_HAS_OPPOSITE(pte) ( ( (pte) != CC_PTE_None ) && ( CC_PTE_DimStar <= (pte) ) && ( (pte) < CC_PTE_Monolith ) )

#define CC_PIECE_HAS_OWNER(pte) ( ( (pte) != CC_PTE_DimStar )              \
                               && ( (pte) != CC_PTE_BrightStar )           \
                               && ( (pte) != CC_PTE_Monolith ) )

#define CC_PIECE_IS_ACTIVATOR(pte) ( ( (pte) != CC_PTE_DimStar )           \
                                  && ( (pte) != CC_PTE_DarkWave )          \
                                  && ( (pte) != CC_PTE_LightWave )         \
                                  && ( (pte) != CC_PTE_BrightStar )        \
                                  && ( (pte) != CC_PTE_Monolith ) )

#define CC_PIECE_CAN_ACTIVATE(pte) ( ( (pte) != CC_PTE_DimStar )           \
                                  && ( (pte) != CC_PTE_BrightStar )        \
                                  && ( (pte) != CC_PTE_Monolith ) )

#define CC_PIECE_CAN_ACTIVATE_STAR(pte) ( ( (pte) == CC_PTE_DarkStarchild )        \
                                       || ( (pte) == CC_PTE_LightStarchild ) )

#define CC_PIECE_CAN_CAPTURE(pte) ( ( (pte) != CC_PTE_DarkStarchild )      \
                                 && ( (pte) != CC_PTE_DimStar )            \
                                 && ( (pte) != CC_PTE_DarkWave )           \
                                 && ( (pte) != CC_PTE_LightWave )          \
                                 && ( (pte) != CC_PTE_BrightStar )         \
                                 && ( (pte) != CC_PTE_LightStarchild )     \
                                 && ( (pte) != CC_PTE_Monolith ) )

#define CC_PIECE_CAN_CAPTURE_EN_PASSANT(pte) ( CC_PIECE_IS_PRIVATE( (pte) ) )

#define CC_PIECE_CAN_BE_CAPTURED_EN_PASSANT(pte) ( CC_PIECE_IS_PRIVATE( (pte) ) )

#define CC_PIECE_IS_PASIVE(pte) ( ( (pte) == CC_PTE_DimStar )              \
                               || ( (pte) == CC_PTE_DarkWave )             \
                               || ( (pte) == CC_PTE_DarkPyramid )          \
                               || ( (pte) == CC_PTE_LightPyramid )         \
                               || ( (pte) == CC_PTE_LightWave )            \
                               || ( (pte) == CC_PTE_BrightStar ) )

#define CC_PIECE_IS_ACTIVE(pte) ( ( (pte) != CC_PTE_DimStar )              \
                               && ( (pte) != CC_PTE_DarkWave )             \
                               && ( (pte) != CC_PTE_DarkPyramid )          \
                               && ( (pte) != CC_PTE_LightPyramid )         \
                               && ( (pte) != CC_PTE_LightWave )            \
                               && ( (pte) != CC_PTE_BrightStar ) )

#define CC_PIECE_IS_WEIGHTLESS(pte) ( ( (pte) == CC_PTE_DarkStarchild )    \
                                   || ( (pte) == CC_PTE_DarkWave )         \
                                   || ( (pte) == CC_PTE_LightWave )        \
                                   || ( (pte) == CC_PTE_LightStarchild ) )

// TODO :: DOCS :: Wave cannot activate Pyramid, only material pieces
#define CC_PIECE_CAN_BE_ACTIVATED(pte) ( ( (pte) != CC_PTE_DarkKing_CanCastle )     \
                                      && ( (pte) != CC_PTE_DarkKing )               \
                                      && ( (pte) != CC_PTE_None )                   \
                                      && ( (pte) != CC_PTE_LightKing )              \
                                      && ( (pte) != CC_PTE_LightKing_CanCastle )    \
                                      && ( (pte) != CC_PTE_Monolith ) )

#define CC_PIECE_CAN_BE_CAPTURED(pte) ( ( (pte) != CC_PTE_DimStar )                 \
                                     && ( (pte) != CC_PTE_DarkKing_CanCastle )      \
                                     && ( (pte) != CC_PTE_DarkKing )                \
                                     && ( (pte) != CC_PTE_LightKing )               \
                                     && ( (pte) != CC_PTE_LightKing_CanCastle )     \
                                     && ( (pte) != CC_PTE_BrightStar )              \
                                     && ( (pte) != CC_PTE_Monolith ) )

#define CC_PAWN_CAN_BE_PROMOTED_TO(pte) ( CC_PIECE_IS_KNIGHT( (pte) )           \
                                       || CC_PIECE_IS_BISHOP( (pte) )           \
                                       || CC_PIECE_IS_JUST_ROOK( (pte) )        \
                                       || CC_PIECE_IS_QUEEN( (pte) )            \
                                       || CC_PIECE_IS_PEGASUS( (pte) )          \
                                       || CC_PIECE_IS_PYRAMID( (pte) )          \
                                       || CC_PIECE_IS_UNICORN( (pte) )          \
                                       || CC_PIECE_IS_WAVE( (pte) )             \
                                       || CC_PIECE_IS_CENTAUR( (pte) )          \
                                       || CC_PIECE_IS_JUST_SCOUT( (pte) )       \
                                       || CC_PIECE_IS_JUST_GRENADIER( (pte) )   \
                                       || CC_PIECE_IS_SERPENT( (pte) )          \
                                       || CC_PIECE_IS_SHAMAN( (pte) )           \
                                       || CC_PIECE_IS_STARCHILD( (pte) ) )

#define CC_PIECE_CAN_RUSH(pte) ( ( (pte) == CC_PTE_DarkGrenadier_CanRush )       \
                              || ( (pte) == CC_PTE_DarkScout_CanRush )           \
                              || ( (pte) == CC_PTE_DarkPawn_CanRush )            \
                              || ( (pte) == CC_PTE_LightPawn_CanRush )           \
                              || ( (pte) == CC_PTE_LightScout_CanRush )          \
                              || ( (pte) == CC_PTE_LightGrenadier_CanRush ) )

#define CC_PIECE_RUSHED_PREVIOUS(pte) ( ( (pte) == CC_PTE_DarkGrenadier_RushedPrevious )          \
                                     || ( (pte) == CC_PTE_DarkScout_RushedPrevious )              \
                                     || ( (pte) == CC_PTE_DarkPawn_RushedPrevious )               \
                                     || ( (pte) == CC_PTE_LightPawn_RushedPrevious )              \
                                     || ( (pte) == CC_PTE_LightScout_RushedPrevious )             \
                                     || ( (pte) == CC_PTE_LightGrenadier_RushedPrevious ) )

#define CC_PIECE_RUSHED_CURRENT(pte) ( ( (pte) == CC_PTE_DarkGrenadier_RushedPrevious )           \
                                    || ( (pte) == CC_PTE_DarkScout_RushedPrevious )               \
                                    || ( (pte) == CC_PTE_DarkPawn_RushedPrevious )                \
                                    || ( (pte) == CC_PTE_LightPawn_RushedPrevious )               \
                                    || ( (pte) == CC_PTE_LightScout_RushedPrevious )              \
                                    || ( (pte) == CC_PTE_LightGrenadier_RushedPrevious ) )

#define CC_PIECE_RUSHED(pte) ( ( CC_PIECE_RUSHED_PREVIOUS( (pte) ) )        \
                            || ( CC_PIECE_RUSHED_CURRENT( (pte) ) ) )

#define CC_PIECE_CAN_BE_PROMOTED(pte) ( ( (pte) == CC_PTE_DarkPawn_DelayedPromotion )                \
                                     || ( (pte) == CC_PTE_DarkPawn )                                 \
                                     || ( (pte) == CC_PTE_LightPawn )                                \
                                     || ( (pte) == CC_PTE_LightPawn_DelayedPromotion ) )

#define CC_PIECE_IS_TAGGED_FOR_PROMOTION(pte) ( ( (pte) == CC_PTE_DarkPawn_DelayedPromotion )        \
                                             || ( (pte) == CC_PTE_LightPawn_DelayedPromotion ) )

#define CC_PIECE_HAS_TAG(pte) ( ( CC_PIECE_CAN_RUSH( (pte) ) )                  \
                             || ( CC_PIECE_RUSHED_PREVIOUS( (pte) ) )           \
                             || ( CC_PIECE_RUSHED_CURRENT( (pte) ) )            \
                             || ( CC_PIECE_IS_TAGGED_FOR_PROMOTION( (pte) ) ) )

#define CC_PIECE_CAN_CASTLE(pte) ( ( (pte) == CC_PTE_DarkKing_CanCastle )       \
                                || ( (pte) == CC_PTE_DarkRook_CanCastle )       \
                                || ( (pte) == CC_PTE_LightRook_CanCastle )      \
                                || ( (pte) == CC_PTE_LightKing_CanCastle ) )

#define CC_PIECE_CAN_DISPLACE(pte) ( ( (pte) == CC_PTE_DarkSerpent )            \
                                  || ( (pte) == CC_PTE_LightSerpent ) )

#define CC_PIECE_CAN_BE_DISPLACED(pte) ( CC_PIECE_IS_PAWN( (pte) ) )

#define CC_PIECE_CAN_BE_DISPLACED_TRANCE_JOURNEY(pte) ( ( (pte) != CC_PTE_DimStar )                 \
                                                     && ( (pte) != CC_PTE_DarkKing_CanCastle )      \
                                                     && ( (pte) != CC_PTE_DarkKing )                \
                                                     && ( (pte) != CC_PTE_LightKing )               \
                                                     && ( (pte) != CC_PTE_LightKing_CanCastle )     \
                                                     && ( (pte) != CC_PTE_BrightStar )              \
                                                     && ( (pte) != CC_PTE_Monolith ) )

#define CC_PIECE_CAN_BE_CONVERTED(pte) ( ( (pte) != CC_PTE_DimStar )                \
                                      && ( (pte) != CC_PTE_DarkKing_CanCastle )     \
                                      && ( (pte) != CC_PTE_DarkKing )               \
                                      && ( (pte) != CC_PTE_LightKing )              \
                                      && ( (pte) != CC_PTE_LightKing_CanCastle )    \
                                      && ( (pte) != CC_PTE_BrightStar )             \
                                      && ( (pte) != CC_PTE_Monolith ) )

#define CC_PIECE_CAN_BE_DEMOTED(pte) ( ( (pte) != CC_PTE_DimStar )                  \
                                    && ( (pte) != CC_PTE_DarkKing_CanCastle )       \
                                    && ( (pte) != CC_PTE_DarkKing )                 \
                                    && ( (pte) != CC_PTE_LightKing )                \
                                    && ( (pte) != CC_PTE_LightKing_CanCastle )      \
                                    && ( (pte) != CC_PTE_BrightStar )               \
                                    && ( (pte) != CC_PTE_Monolith ) )

#define CC_PIECE_CAN_BE_RESURRECTED(pte) ( ( (pte) != CC_PTE_DimStar )              \
                                        && ( (pte) != CC_PTE_DarkKing_CanCastle )   \
                                        && ( (pte) != CC_PTE_DarkKing )             \
                                        && ( (pte) != CC_PTE_LightKing )            \
                                        && ( (pte) != CC_PTE_LightKing_CanCastle )  \
                                        && ( (pte) != CC_PTE_BrightStar )           \
                                        && ( (pte) != CC_PTE_Monolith ) )

#define CC_PIECE_IS_TELEPORTER(pte) ( ( (pte) == CC_PTE_DimStar )               \
                                   || ( (pte) == CC_PTE_BrightStar )            \
                                   || ( (pte) == CC_PTE_Monolith ) )

#define CC_PIECE_CAN_BE_TELEPORTED(pte) ( ( (pte) != CC_PTE_DimStar )               \
                                       && ( (pte) != CC_PTE_DarkKing_CanCastle )    \
                                       && ( (pte) != CC_PTE_DarkKing )              \
                                       && ( (pte) != CC_PTE_LightKing )             \
                                       && ( (pte) != CC_PTE_LightKing_CanCastle )   \
                                       && ( (pte) != CC_PTE_BrightStar )            \
                                       && ( (pte) != CC_PTE_Monolith ) )

#define CC_PIECE_IS_COMPLETELY_TRANSPARENT(pte)  ( ( (pte) == CC_PTE_DarkStarchild )    \
                                                || ( (pte) == CC_PTE_LightStarchild ) )

#define CC_PIECE_IS_TRANSPARENT(pte)  ( ( (pte) == CC_PTE_DarkWave )           \
                                     || ( (pte) == CC_PTE_LightWave )          \
                                     || ( (pte) == CC_PTE_DarkStarchild )      \
                                     || ( (pte) == CC_PTE_LightStarchild ) )

#define CC_PIECE_IS_SEMI_TRANSPARENT(pte) ( (pte) != CC_PTE_Monolith )

#define CC_PIECE_IS_SEMI_OPAQUE(pte) ( ( (pte) != CC_PTE_Monolith )          \
                                    && ( (pte) != CC_PTE_DarkWave )          \
                                    && ( (pte) != CC_PTE_LightWave )         \
                                    && ( (pte) != CC_PTE_DarkStarchild )     \
                                    && ( (pte) != CC_PTE_LightStarchild ) )

#define CC_PIECE_IS_OPAQUE(pte) ( (pte) == CC_PTE_Monolith )

#define CC_PIECE_IS_DIVERGENT(pte) ( ( (pte) == CC_PTE_DarkStarchild )     \
                                  || ( (pte) == CC_PTE_DarkShaman )        \
                                  || ( (pte) == CC_PTE_LightShaman )       \
                                  || ( (pte) == CC_PTE_LightStarchild ) )

// todo :: MAYBE :: Scout, Grenadier cannot diverge
#define CC_PIECE_CAN_BE_DIVERGED(pte)   ( CC_PIECE_IS_PAWN( (pte) )             \
                                       || CC_PIECE_IS_KNIGHT( (pte) )           \
                                       || CC_PIECE_IS_BISHOP( (pte) )           \
                                       || CC_PIECE_IS_ROOK( (pte) )             \
                                       || CC_PIECE_IS_QUEEN( (pte) )            \
                                       || CC_PIECE_IS_PEGASUS( (pte) )          \
                                       || CC_PIECE_IS_PYRAMID( (pte) )          \
                                       || CC_PIECE_IS_UNICORN( (pte) )          \
                                       || CC_PIECE_IS_WAVE( (pte) )             \
                                       || CC_PIECE_IS_SCOUT( (pte) )            \
                                       || CC_PIECE_IS_GRENADIER( (pte) )        \
                                       || CC_PIECE_IS_SHAMAN( (pte) ) )

#define CC_WAVE_CAN_BE_DIVERGED(activator) ( ( (activator) != CC_PTE_DarkCentaur )       \
                                          && ( (activator) != CC_PTE_DarkSerpent )       \
                                          && ( (activator) != CC_PTE_DarkUnicorn )       \
                                          && ( (activator) != CC_PTE_None )              \
                                          && ( (activator) != CC_PTE_LightUnicorn )      \
                                          && ( (activator) != CC_PTE_LightSerpent )      \
                                          && ( (activator) != CC_PTE_LightCentaur )      \
                                          && ( CC_PIECE_IS_ACTIVATOR( (activator) ) ) )

#define CC_PIECE_IS_SINGLE_STEP(pte) ( CC_PIECE_IS_PAWN(pte)             \
                                    || CC_PIECE_IS_KNIGHT(pte)           \
                                    || CC_PIECE_IS_KING(pte)             \
                                    || CC_PIECE_IS_STARCHILD(pte)        \
                                    || CC_PIECE_IS_STAR(pte) )

#define CC_PIECE_IS_SINGLE_STEP_ALTERNATING(pte) ( CC_PIECE_IS_UNICORN(pte) )

#define CC_PIECE_IS_ONE_STEP(pte) ( CC_PIECE_IS_BISHOP(pte)        \
                                 || CC_PIECE_IS_ROOK(pte)          \
                                 || CC_PIECE_IS_QUEEN(pte)         \
                                 || CC_PIECE_IS_PEGASUS(pte)       \
                                 || CC_PIECE_IS_PYRAMID(pte)       \
                                 || CC_PIECE_IS_GRENADIER(pte)     \
                                 || CC_PIECE_IS_SHAMAN(pte) )

#define CC_PIECE_IS_TWO_STEP(pte) ( CC_PIECE_IS_CENTAUR(pte) )

#define CC_WAVE_IS_TWO_STEP(activator) ( CC_PIECE_IS_UNICORN(activator)   \
                                      || CC_PIECE_IS_CENTAUR(activator)   \
                                      || CC_PIECE_IS_SERPENT(activator) )

#define CC_PIECE_HAS_NEW_STEP_AFTER_EACH(pte)  ( CC_PIECE_IS_SERPENT(pte)         \
                                              || CC_PIECE_IS_SCOUT(pte)           \
                                              || CC_PIECE_IS_MONOLITH(pte) )


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

typedef signed char CcPieceTagType;


typedef char (*cc_piece_fp_char_value_t)( CcPieceTagType ptt );

CcPieceTagType cc_piece_from_symbol( char symbol, bool is_light );

bool cc_piece_symbol_is_valid( char c );

CcPieceTagType cc_piece_strip_tag( CcPieceTagType ptt );

CcPieceTagType cc_piece_opposite( CcPieceTagType ptt );

char cc_piece_as_char( CcPieceTagType ptt );

CcPieceTagType cc_piece_from_char( char piece, char tag );

char const * cc_piece_label( CcPieceTagType ptt, bool capitalize, bool empty_field );

char cc_piece_symbol( CcPieceTagType ptt );

CcPieceTagType cc_piece_demoting_to( CcPieceTagType ptt );

bool cc_piece_has_color( CcPieceTagType ptt );

bool cc_piece_has_shade( CcPieceTagType ptt );

bool cc_piece_has_prefix( CcPieceTagType ptt );

char const * cc_piece_prefix( CcPieceTagType ptt, bool capitalize );

bool cc_piece_has_congruent_type( char symbol, CcPieceTagType ptt );

bool cc_piece_is_equal( char symbol, bool is_light, CcPieceTagType ptt );

bool cc_piece_has_same_type( CcPieceTagType ptt_1, CcPieceTagType ptt_2 );

bool cc_piece_has_same_color( CcPieceTagType ptt_1, CcPieceTagType ptt_2 );

bool cc_piece_has_same_shade( CcPieceTagType ptt_1, CcPieceTagType ptt_2 );

bool cc_piece_is_opposite( CcPieceTagType ptt_1, CcPieceTagType ptt_2 );

bool cc_piece_has_same_owner( CcPieceTagType ptt_1, CcPieceTagType ptt_2 );

bool cc_piece_has_different_owner( CcPieceTagType ptt_1, CcPieceTagType ptt_2 );

bool cc_piece_is_owned_figure( CcPieceTagType ptt );

bool cc_piece_is_figure( CcPieceTagType ptt );

bool cc_piece_is_one_step( CcPieceTagType piece, CcPieceTagType activator );

bool cc_piece_is_two_step( CcPieceTagType piece, CcPieceTagType activator );

bool cc_piece_is_many_steps( CcPieceTagType piece );

char const * cc_piece_as_string( CcPieceTagType ptt, bool capitalize, bool empty_field );


#endif /* __CC_PIECE_H__ */
