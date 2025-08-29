// Copyright (c) 2021, 2022, 2024, 2025 Mario Mlačak, mmlacak@gmail.com
// Licensed under GNU GPL v3+ license. See LICENSING, COPYING files for details.

#ifndef __CC_TYPED_STEP_DEFS_H__
#define __CC_TYPED_STEP_DEFS_H__

#include <stddef.h>

#include "cc_variant.h"
#include "cc_piece.h"
// #include "cc_chessboard.h"
// #include "cc_pos.h"
#include "cc_typed_step.h"


#define CC_STEPS_LEN_GUARD_DATA_TERMINATED (0)


#define CC_STEPS_PAWN_LEN (3)
#define CC_STEPS_SIDEWAYS_PAWN_LEN (5)

#define CC_STEPS_KNIGHT_LEN (8)
#define CC_STEPS_BISHOP_LEN (4)
#define CC_STEPS_ROOK_LEN (4)
#define CC_STEPS_QUEEN_LEN (8)
#define CC_STEPS_KING_LEN (CC_STEPS_QUEEN_LEN)

#define CC_STEPS_PEGASUS_LEN (CC_STEPS_KNIGHT_LEN)
#define CC_STEPS_PYRAMID_LEN (CC_STEPS_ROOK_LEN)
#define CC_STEPS_SHORT_UNICORN_LEN (CC_STEPS_KNIGHT_LEN)
#define CC_STEPS_LONG_UNICORN_LEN (16)

#define CC_STEPS_STAR_LEN (CC_STEPS_QUEEN_LEN)
#define CC_STEPS_SHORT_CENTAUR_LEN (CC_STEPS_SHORT_UNICORN_LEN)
#define CC_STEPS_LONG_CENTAUR_LEN (CC_STEPS_LONG_UNICORN_LEN)

#define CC_STEPS_DIAGONAL_SERPENT_LEN (2)
#define CC_STEPS_COLOR_CHANGE_SERPENT_LEN (4)
#define CC_STEPS_DISPLACEMENT_SERPENT_LEN (4)
// all Serpent steps == left diagonal + right diagonal + color-change + displacement steps
#define CC_STEPS_ALL_SERPENT_LEN (CC_STEPS_DIAGONAL_SERPENT_LEN + CC_STEPS_DIAGONAL_SERPENT_LEN + CC_STEPS_COLOR_CHANGE_SERPENT_LEN + CC_STEPS_DISPLACEMENT_SERPENT_LEN)

#define CC_STEPS_LIGHT_SHAMAN_LEN (CC_STEPS_KNIGHT_LEN + CC_STEPS_LONG_UNICORN_LEN) // Steps + capture-steps.
#define CC_STEPS_DARK_SHAMAN_LEN (CC_STEPS_LONG_UNICORN_LEN + CC_STEPS_KNIGHT_LEN) // Steps + capture-steps.

#define CC_STEPS_SCOUT_LEN (5)
#define CC_STEPS_GRENADIER_LEN (CC_STEPS_ROOK_LEN + CC_STEPS_BISHOP_LEN) // Ordinary steps + capture-steps.

#define CC_STEPS_MIRACLE_STARCHILD_LEN (CC_STEPS_QUEEN_LEN)
#define CC_STEPS_STARTING_MONOLITH_LEN (CC_STEPS_KNIGHT_LEN)

#define CC_STEPS_DISPLACEMENT_TRANCE_JOURNEY_LEN (44)


#define CC_STEPS_PAWN_SIZE (CC_STEPS_PAWN_LEN + 1)
#define CC_STEPS_SIDEWAYS_PAWN_SIZE (CC_STEPS_SIDEWAYS_PAWN_LEN + 1)

#define CC_STEPS_KNIGHT_SIZE (CC_STEPS_KNIGHT_LEN + 1)
#define CC_STEPS_BISHOP_SIZE (CC_STEPS_BISHOP_LEN + 1)
#define CC_STEPS_ROOK_SIZE (CC_STEPS_ROOK_LEN + 1)
#define CC_STEPS_QUEEN_SIZE (CC_STEPS_QUEEN_LEN + 1)
#define CC_STEPS_KING_SIZE (CC_STEPS_QUEEN_SIZE)

#define CC_STEPS_PEGASUS_SIZE (CC_STEPS_KNIGHT_SIZE)
#define CC_STEPS_PYRAMID_SIZE (CC_STEPS_ROOK_SIZE)
#define CC_STEPS_SHORT_UNICORN_SIZE (CC_STEPS_KNIGHT_SIZE)
#define CC_STEPS_LONG_UNICORN_SIZE (CC_STEPS_LONG_UNICORN_LEN + 1)

#define CC_STEPS_STAR_SIZE (CC_STEPS_QUEEN_SIZE)
#define CC_STEPS_SHORT_CENTAUR_SIZE (CC_STEPS_SHORT_UNICORN_SIZE)
#define CC_STEPS_LONG_CENTAUR_SIZE (CC_STEPS_LONG_UNICORN_SIZE)

#define CC_STEPS_DIAGONAL_SERPENT_SIZE (CC_STEPS_DIAGONAL_SERPENT_LEN + 1)
#define CC_STEPS_ALL_SERPENT_SIZE (CC_STEPS_ALL_SERPENT_LEN + 1)

#define CC_STEPS_LIGHT_SHAMAN_SIZE (CC_STEPS_LIGHT_SHAMAN_LEN + 1)
#define CC_STEPS_DARK_SHAMAN_SIZE (CC_STEPS_DARK_SHAMAN_LEN + 1)

#define CC_STEPS_SCOUT_SIZE (CC_STEPS_SCOUT_LEN + 1)
#define CC_STEPS_GRENADIER_SIZE (CC_STEPS_GRENADIER_LEN + 1)

#define CC_STEPS_MIRACLE_STARCHILD_SIZE (CC_STEPS_QUEEN_SIZE)
#define CC_STEPS_STARTING_MONOLITH_SIZE (CC_STEPS_STARTING_MONOLITH_LEN + 1)

#define CC_STEPS_DISPLACEMENT_TRANCE_JOURNEY_SIZE (CC_STEPS_DISPLACEMENT_TRANCE_JOURNEY_LEN + 1)


extern CcTypedStep const CC_STEPS_LIGHT_PAWN[ CC_STEPS_PAWN_SIZE ];
extern CcTypedStep const CC_STEPS_DARK_PAWN[ CC_STEPS_PAWN_SIZE ];
extern CcTypedStep const CC_STEPS_LIGHT_SIDEWAYS_PAWN[ CC_STEPS_SIDEWAYS_PAWN_SIZE ];
extern CcTypedStep const CC_STEPS_DARK_SIDEWAYS_PAWN[ CC_STEPS_SIDEWAYS_PAWN_SIZE ];

extern CcTypedStep const CC_STEPS_KNIGHT[ CC_STEPS_KNIGHT_SIZE ];
extern CcTypedStep const CC_STEPS_BISHOP[ CC_STEPS_BISHOP_SIZE ];
extern CcTypedStep const CC_STEPS_ROOK[ CC_STEPS_ROOK_SIZE ];
extern CcTypedStep const CC_STEPS_QUEEN[ CC_STEPS_QUEEN_SIZE ];
#define CC_STEPS_KING (CC_STEPS_QUEEN)

#define CC_STEPS_PEGASUS (CC_STEPS_KNIGHT)
#define CC_STEPS_PYRAMID (CC_STEPS_ROOK)
#define CC_STEPS_SHORT_UNICORN (CC_STEPS_KNIGHT)
extern CcTypedStep const CC_STEPS_LONG_UNICORN[ CC_STEPS_LONG_UNICORN_SIZE ];

#define CC_STEPS_STAR (CC_STEPS_QUEEN)
#define CC_STEPS_SHORT_CENTAUR (CC_STEPS_SHORT_UNICORN)
#define CC_STEPS_LONG_CENTAUR (CC_STEPS_LONG_UNICORN)

extern CcTypedStep const CC_STEPS_SERPENT_LEFT[ CC_STEPS_DIAGONAL_SERPENT_SIZE ];
extern CcTypedStep const CC_STEPS_SERPENT_RIGHT[ CC_STEPS_DIAGONAL_SERPENT_SIZE ];
extern CcTypedStep const CC_STEPS_ALL_SERPENT[ CC_STEPS_ALL_SERPENT_SIZE ];

extern CcTypedStep const CC_STEPS_LIGHT_SHAMAN[ CC_STEPS_LIGHT_SHAMAN_SIZE ];
extern CcTypedStep const CC_STEPS_DARK_SHAMAN[ CC_STEPS_DARK_SHAMAN_SIZE ];

extern CcTypedStep const CC_STEPS_LIGHT_SCOUT[ CC_STEPS_SCOUT_SIZE ];
extern CcTypedStep const CC_STEPS_DARK_SCOUT[ CC_STEPS_SCOUT_SIZE ];
extern CcTypedStep const CC_STEPS_GRENADIER[ CC_STEPS_GRENADIER_SIZE ];

extern CcTypedStep const CC_STEPS_MIRACLE_STARCHILD[ CC_STEPS_MIRACLE_STARCHILD_SIZE ];
#define CC_STEPS_STARTING_MONOLITH (CC_STEPS_KNIGHT)

extern CcTypedStep const CC_STEPS_DISPLACEMENT_TRANCE_JOURNEY[ CC_STEPS_DISPLACEMENT_TRANCE_JOURNEY_SIZE ];


CcStepTypeEnum cc_get_step_type( CcPos step,
                                 CcStepTypeEnum filter__d,
                                 CcTypedStep const steps[],
                                 size_t steps_len__d );

bool cc_is_typed_step_valid( CcTypedStep step,
                             CcStepTypeEnum filter__d,
                             CcTypedStep const steps[],
                             size_t steps_len__d );


#define CC_GET_LIGHT_PAWN_STEP_TYPE(step) \
    ( cc_get_step_type( (step), CC_STE_None, CC_STEPS_LIGHT_PAWN, CC_STEPS_PAWN_LEN ) )

#define CC_GET_DARK_PAWN_STEP_TYPE(step) \
    ( cc_get_step_type( (step), CC_STE_None, CC_STEPS_DARK_PAWN, CC_STEPS_PAWN_LEN ) )

#define CC_GET_LIGHT_SIDEWAYS_PAWN_STEP_TYPE(step) \
    ( cc_get_step_type( (step), CC_STE_None, CC_STEPS_LIGHT_SIDEWAYS_PAWN, CC_STEPS_SIDEWAYS_PAWN_LEN ) )

#define CC_GET_DARK_SIDEWAYS_PAWN_STEP_TYPE(step) \
    ( cc_get_step_type( (step), CC_STE_None, CC_STEPS_DARK_SIDEWAYS_PAWN, CC_STEPS_SIDEWAYS_PAWN_LEN ) )


#define CC_GET_KNIGHT_STEP_TYPE(step) \
    ( cc_get_step_type( (step), CC_STE_None, CC_STEPS_KNIGHT, CC_STEPS_KNIGHT_LEN ) )

#define CC_GET_BISHOP_STEP_TYPE(step) \
    ( cc_get_step_type( (step), CC_STE_None, CC_STEPS_BISHOP, CC_STEPS_BISHOP_LEN ) )

#define CC_GET_ROOK_STEP_TYPE(step) \
    ( cc_get_step_type( (step), CC_STE_None, CC_STEPS_ROOK, CC_STEPS_ROOK_LEN ) )

#define CC_GET_QUEEN_STEP_TYPE(step) \
    ( cc_get_step_type( (step), CC_STE_None, CC_STEPS_QUEEN, CC_STEPS_QUEEN_LEN ) )

#define CC_GET_KING_STEP_TYPE(step) \
    ( cc_get_step_type( (step), CC_STE_None, CC_STEPS_KING, CC_STEPS_KING_LEN ) )


#define CC_GET_PEGASUS_STEP_TYPE(step) \
    ( cc_get_step_type( (step), CC_STE_None, CC_STEPS_PEGASUS, CC_STEPS_PEGASUS_LEN ) )

#define CC_GET_PYRAMID_STEP_TYPE(step) \
    ( cc_get_step_type( (step), CC_STE_None, CC_STEPS_PYRAMID, CC_STEPS_PYRAMID_LEN ) )

#define CC_GET_UNICORN_SHORT_STEP_TYPE(step) \
    ( cc_get_step_type( (step), CC_STE_None, CC_STEPS_SHORT_UNICORN, CC_STEPS_SHORT_UNICORN_LEN ) )

#define CC_GET_UNICORN_LONG_STEP_TYPE(step) \
    ( cc_get_step_type( (step), CC_STE_None, CC_STEPS_LONG_UNICORN, CC_STEPS_LONG_UNICORN_LEN ) )


#define CC_GET_STAR_STEP_TYPE(step) \
    ( cc_get_step_type( (step), CC_STE_None, CC_STEPS_STAR, CC_STEPS_STAR_LEN ) )

#define CC_GET_CENTAUR_SHORT_STEP_TYPE(step) \
    ( cc_get_step_type( (step), CC_STE_None, CC_STEPS_SHORT_CENTAUR, CC_STEPS_SHORT_CENTAUR_LEN ) )

#define CC_GET_CENTAUR_LONG_STEP_TYPE(step) \
    ( cc_get_step_type( (step), CC_STE_None, CC_STEPS_LONG_CENTAUR, CC_STEPS_LONG_CENTAUR_LEN ) )


#define CC_GET_SERPENT_LEFT_STEP_TYPE(step) \
    ( cc_get_step_type( (step), CC_STE_None, CC_STEPS_SERPENT_LEFT, CC_STEPS_DIAGONAL_SERPENT_LEN ) )

#define CC_GET_SERPENT_RIGHT_STEP_TYPE(step) \
    ( cc_get_step_type( (step), CC_STE_None, CC_STEPS_SERPENT_RIGHT, CC_STEPS_DIAGONAL_SERPENT_LEN ) )

#define CC_GET_SERPENT_STEP_TYPE(step) \
    ( cc_get_step_type( (step), CC_STE_None, CC_STEPS_ALL_SERPENT, CC_STEPS_ALL_SERPENT_LEN ) )


#define CC_GET_LIGHT_SCOUT_STEP_TYPE(step) \
    ( cc_get_step_type( (step), CC_STE_None, CC_STEPS_LIGHT_SCOUT, CC_STEPS_SCOUT_LEN ) )

#define CC_GET_DARK_SCOUT_STEP_TYPE(step) \
    ( cc_get_step_type( (step), CC_STE_None, CC_STEPS_DARK_SCOUT, CC_STEPS_SCOUT_LEN ) )

#define CC_GET_GRENADIER_STEP_TYPE(step) \
    ( cc_get_step_type( (step), CC_STE_None, CC_STEPS_GRENADIER, CC_STEPS_GRENADIER_LEN ) )


#define CC_GET_LIGHT_SHAMAN_STEP_TYPE(step) \
    ( cc_get_step_type( (step), CC_STE_None, CC_STEPS_LIGHT_SHAMAN, CC_STEPS_LIGHT_SHAMAN_LEN ) )

#define CC_GET_DARK_SHAMAN_STEP_TYPE(step) \
    ( cc_get_step_type( (step), CC_STE_None, CC_STEPS_DARK_SHAMAN, CC_STEPS_DARK_SHAMAN_LEN ) )

#define CC_GET_STARCHILD_MIRACLE_STEP_TYPE(step) \
    ( cc_get_step_type( (step), CC_STE_None, CC_STEPS_MIRACLE_STARCHILD, CC_STEPS_MIRACLE_STARCHILD_LEN ) )


bool cc_convert_typed_steps_to_links( CcTypedStep const steps[],
                                      size_t steps_len__d,
                                      CcTypedStepLink ** steps__o );


bool cc_iter_typed_steps( CcTypedStep const steps[],
                          size_t steps_len__d,
                          CcStepTypeEnum filter__d,
                          CcTypedStep const ** step__iod );

#define CC_ITER_LIGHT_PAWN_STEPS(step__iod,filter__d) \
    ( cc_iter_typed_steps( CC_STEPS_LIGHT_PAWN, CC_STEPS_PAWN_LEN, (filter__d), (step__iod) ) )

#define CC_ITER_DARK_PAWN_STEPS(step__iod,filter__d) \
    ( cc_iter_typed_steps( CC_STEPS_DARK_PAWN, CC_STEPS_PAWN_LEN, (filter__d), (step__iod) ) )

#define CC_ITER_LIGHT_SIDEWAYS_PAWN_STEPS(step__iod,filter__d) \
    ( cc_iter_typed_steps( CC_STEPS_LIGHT_SIDEWAYS_PAWN, CC_STEPS_SIDEWAYS_PAWN_LEN, (filter__d), (step__iod) ) )

#define CC_ITER_DARK_SIDEWAYS_PAWN_STEPS(step__iod,filter__d) \
    ( cc_iter_typed_steps( CC_STEPS_DARK_SIDEWAYS_PAWN, CC_STEPS_SIDEWAYS_PAWN_LEN, (filter__d), (step__iod) ) )


#define CC_ITER_KNIGHT_STEPS(step__iod,filter__d) \
    ( cc_iter_typed_steps( CC_STEPS_KNIGHT, CC_STEPS_KNIGHT_LEN, (filter__d), (step__iod) ) )

#define CC_ITER_BISHOP_STEPS(step__iod,filter__d) \
    ( cc_iter_typed_steps( CC_STEPS_BISHOP, CC_STEPS_BISHOP_LEN, (filter__d), (step__iod) ) )

#define CC_ITER_ROOK_STEPS(step__iod,filter__d) \
    ( cc_iter_typed_steps( CC_STEPS_ROOK, CC_STEPS_ROOK_LEN, (filter__d), (step__iod) ) )

#define CC_ITER_QUEEN_STEPS(step__iod,filter__d) \
    ( cc_iter_typed_steps( CC_STEPS_QUEEN, CC_STEPS_QUEEN_LEN, (filter__d), (step__iod) ) )

#define CC_ITER_KING_STEPS(step__iod,filter__d) \
    ( cc_iter_typed_steps( CC_STEPS_KING, CC_STEPS_KING_LEN, (filter__d), (step__iod) ) )


#define CC_ITER_PEGASUS_STEPS(step__iod,filter__d) \
    ( cc_iter_typed_steps( CC_STEPS_PEGASUS, CC_STEPS_PEGASUS_LEN, (filter__d), (step__iod) ) )

#define CC_ITER_PYRAMID_STEPS(step__iod,filter__d) \
    ( cc_iter_typed_steps( CC_STEPS_PYRAMID, CC_STEPS_PYRAMID_LEN, (filter__d), (step__iod) ) )

#define CC_ITER_UNICORN_SHORT_STEPS(step__iod,filter__d) \
    ( cc_iter_typed_steps( CC_STEPS_SHORT_UNICORN, CC_STEPS_SHORT_UNICORN_LEN, (filter__d), (step__iod) ) )

#define CC_ITER_UNICORN_LONG_STEPS(step__iod,filter__d) \
    ( cc_iter_typed_steps( CC_STEPS_LONG_UNICORN, CC_STEPS_LONG_UNICORN_LEN, (filter__d), (step__iod) ) )


#define CC_ITER_STAR_STEPS(step__iod,filter__d) \
    ( cc_iter_typed_steps( CC_STEPS_STAR, CC_STEPS_STAR_LEN, (filter__d), (step__iod) ) )

#define CC_ITER_CENTAUR_SHORT_STEPS(step__iod,filter__d) \
    ( cc_iter_typed_steps( CC_STEPS_SHORT_CENTAUR, CC_STEPS_SHORT_CENTAUR_LEN, (filter__d), (step__iod) ) )

#define CC_ITER_CENTAUR_LONG_STEPS(step__iod,filter__d) \
    ( cc_iter_typed_steps( CC_STEPS_LONG_CENTAUR, CC_STEPS_LONG_CENTAUR_LEN, (filter__d), (step__iod) ) )


#define CC_ITER_SERPENT_LEFT_STEPS(step__iod,filter__d) \
    ( cc_iter_typed_steps( CC_STEPS_SERPENT_LEFT, CC_STEPS_DIAGONAL_SERPENT_LEN, (filter__d), (step__iod) ) )

#define CC_ITER_SERPENT_RIGHT_STEPS(step__iod,filter__d) \
    ( cc_iter_typed_steps( CC_STEPS_SERPENT_RIGHT, CC_STEPS_DIAGONAL_SERPENT_LEN, (filter__d), (step__iod) ) )

#define CC_ITER_SERPENT_STEPS(step__iod,filter__d) \
    ( cc_iter_typed_steps( CC_STEPS_ALL_SERPENT, CC_STEPS_ALL_SERPENT_LEN, (filter__d), (step__iod) ) )


#define CC_ITER_LIGHT_SCOUT_STEPS(step__iod,filter__d) \
    ( cc_iter_typed_steps( CC_STEPS_LIGHT_SCOUT, CC_STEPS_SCOUT_LEN, (filter__d), (step__iod) ) )

#define CC_ITER_DARK_SCOUT_STEPS(step__iod,filter__d) \
    ( cc_iter_typed_steps( CC_STEPS_DARK_SCOUT, CC_STEPS_SCOUT_LEN, (filter__d), (step__iod) ) )

#define CC_ITER_GRENADIER_STEPS(step__iod,filter__d) \
    ( cc_iter_typed_steps( CC_STEPS_GRENADIER, CC_STEPS_GRENADIER_LEN, (filter__d), (step__iod) ) )


#define CC_ITER_LIGHT_SHAMAN_STEPS(step__iod,filter__d) \
    ( cc_iter_typed_steps( CC_STEPS_LIGHT_SHAMAN, CC_STEPS_LIGHT_SHAMAN_LEN, (filter__d), (step__iod) ) )

#define CC_ITER_DARK_SHAMAN_STEPS(step__iod,filter__d) \
    ( cc_iter_typed_steps( CC_STEPS_DARK_SHAMAN, CC_STEPS_DARK_SHAMAN_LEN, (filter__d), (step__iod) ) )

#define CC_ITER_STARCHILD_MIRACLE_STEPS(step__iod,filter__d) \
    ( cc_iter_typed_steps( CC_STEPS_MIRACLE_STARCHILD, CC_STEPS_MIRACLE_STARCHILD_LEN, (filter__d), (step__iod) ) )


#define CC_ITER_DISPLACEMENT_TRANCE_JOURNEY_STEPS(step__iod,filter__d) \
    ( cc_iter_typed_steps( CC_STEPS_DISPLACEMENT_TRANCE_JOURNEY, CC_STEPS_DISPLACEMENT_TRANCE_JOURNEY_LEN, (filter__d), (step__iod) ) )


bool cc_iter_monolith_steps( cc_uint_t step_index,
                             CcTypedStep * step__io );

bool cc_iter_piece_steps( CcPieceTagType piece,
                          bool sideways_pawns,
                          bool short_step,
                          CcSerpentDiagonalEnum serpent_diagonal,
                          CcStepTypeEnum filter__d,
                          CcTypedStep const ** step__iod );

// TODO :: DOCS
bool cc_fetch_piece_step( CcPieceTagType piece,
                          CcPos pos,
                          CcPieceTagType activator,
                          cc_uint_t board_size,
                          CcTypedStepLink * steps,
                          size_t step_index,
                          CcTypedStep * step__o );


#endif /* __CC_TYPED_STEP_DEFS_H__ */
