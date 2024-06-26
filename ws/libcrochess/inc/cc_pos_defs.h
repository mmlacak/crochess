// Copyright (c) 2021, 2022, 2024 Mario Mlačak, mmlacak@gmail.com
// Licensed under GNU GPL v3+ license. See LICENSING, COPYING files for details.

#ifndef __CC_POS_DEFS_H__
#define __CC_POS_DEFS_H__

#include <stddef.h>

#include "cc_variant.h"
#include "cc_piece.h"
// #include "cc_chessboard.h"
#include "cc_pos.h"


/**
    @file cc_pos_defs.h
    @brief Position, step definitions, checkers.
*/


/**
    Value to ignore array size constraint on various functions,
    and use invalid position as a guard to terminate loops.
*/
#define CC_STEPS_LEN_INVALID_DATA_TERMINATED (0)

/** @defgroup step_generator_array The step generator array
 *  The step generator constants and arrays are meant to be used via `cc_gen_pos()`,
    and `cc_is_step_valid()`.

    @see cc_gen_pos(), cc_is_step_valid()
 *  @{
 */

/** @defgroup step_generator_lengths The step generator array lengths
 *  The step generator array lengths.

    @note
    Length of an array is count of useful data in it, without terminating data,
    similar how `strlen()` does not count zero-terminating `char` in strings.
 *  @{
 */

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
#define CC_STEPS_SERPENT_LEN (2)
#define CC_STEPS_ALL_SERPENT_LEN (CC_STEPS_BISHOP_LEN)

#define CC_STEPS_LIGHT_SHAMAN_LEN (CC_STEPS_KNIGHT_LEN + CC_STEPS_LONG_UNICORN_LEN) // Steps + capture-steps.
#define CC_STEPS_DARK_SHAMAN_LEN (CC_STEPS_LONG_UNICORN_LEN + CC_STEPS_KNIGHT_LEN) // Steps + capture-steps.

#define CC_STEPS_SCOUT_LEN (5)
#define CC_STEPS_GRENADIER_LEN (CC_STEPS_ROOK_LEN + CC_STEPS_BISHOP_LEN) // Ordinary steps + capture-steps.

#define CC_STEPS_MIRACLE_STARCHILD_LEN (CC_STEPS_QUEEN_LEN)
#define CC_STEPS_STARTING_MONOLITH_LEN (CC_STEPS_KNIGHT_LEN)

/** @} */ // end of step_generator_lengths

/** @defgroup step_generator_sizes The step generator array sizes
 *  The step generator array sizes.

    @note
    Size of an array is count of all items in an it, including guard data,
    i.e. terminating position.
    For the same array, size is larger by 1 than the length.
 *  @{
 */

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
#define CC_STEPS_SERPENT_SIZE (CC_STEPS_SERPENT_LEN + 1)
#define CC_STEPS_ALL_SERPENT_SIZE (CC_STEPS_BISHOP_SIZE)

#define CC_STEPS_LIGHT_SHAMAN_SIZE (CC_STEPS_LIGHT_SHAMAN_LEN + 1)
#define CC_STEPS_DARK_SHAMAN_SIZE (CC_STEPS_DARK_SHAMAN_LEN + 1)

#define CC_STEPS_SCOUT_SIZE (CC_STEPS_SCOUT_LEN + 1)
#define CC_STEPS_GRENADIER_SIZE (CC_STEPS_GRENADIER_LEN + 1)

#define CC_STEPS_MIRACLE_STARCHILD_SIZE (CC_STEPS_QUEEN_SIZE)
#define CC_STEPS_STARTING_MONOLITH_SIZE (CC_STEPS_STARTING_MONOLITH_LEN + 1)

/** @} */ // end of step_generator_sizes

/** @defgroup step_generator_arrays The step generator arrays
 *  The step generator arrays are constants defining all valid steps,
    for a pieces of similar movement, e.g. Bishop and Serpent share
    the same array, for Serpent's regular movement.

    @note
    All arrays, beside their length and size, have a terminating position,
    similar to zero-terminating strings.

    @note
    Terminating position is a `CcTypedStep` with coordinates past normal off-board
    calculation. Currently, it's defined as `CC_POS_INVALID`, which is
    `{ CC_INVALID_COORD, CC_INVALID_COORD }`.

    @see CcTypedStep, CC_POS_INVALID, CC_INVALID_COORD
 *  @{
 */

extern CcTypedStep const CC_STEPS_LIGHT_PAWN[ CC_STEPS_PAWN_SIZE ];
extern CcTypedStep const CC_STEPS_DARK_PAWN[ CC_STEPS_PAWN_SIZE ];
extern CcTypedStep const CC_STEPS_LIGHT_SIDEWAYS_PAWN[ CC_STEPS_SIDEWAYS_PAWN_SIZE ];
extern CcTypedStep const CC_STEPS_DARK_SIDEWAYS_PAWN[ CC_STEPS_SIDEWAYS_PAWN_SIZE ];

extern CcTypedStep const CC_STEPS_KNIGHT[ CC_STEPS_KNIGHT_SIZE ];
extern CcTypedStep const CC_STEPS_BISHOP[ CC_STEPS_BISHOP_SIZE ];
extern CcTypedStep const CC_STEPS_ROOK[ CC_STEPS_ROOK_SIZE ]; // Also, Serpent's color-changing move.
extern CcTypedStep const CC_STEPS_QUEEN[ CC_STEPS_QUEEN_SIZE ];
#define CC_STEPS_KING (CC_STEPS_QUEEN)

#define CC_STEPS_PEGASUS (CC_STEPS_KNIGHT)
#define CC_STEPS_PYRAMID (CC_STEPS_ROOK)
#define CC_STEPS_SHORT_UNICORN (CC_STEPS_KNIGHT)
extern CcTypedStep const CC_STEPS_LONG_UNICORN[ CC_STEPS_LONG_UNICORN_SIZE ];

#define CC_STEPS_STAR (CC_STEPS_QUEEN)
#define CC_STEPS_SHORT_CENTAUR (CC_STEPS_SHORT_UNICORN)
#define CC_STEPS_LONG_CENTAUR (CC_STEPS_LONG_UNICORN)
extern CcTypedStep const CC_STEPS_SERPENT_LEFT[ CC_STEPS_SERPENT_SIZE ];
extern CcTypedStep const CC_STEPS_SERPENT_RIGHT[ CC_STEPS_SERPENT_SIZE ];
#define CC_STEPS_ALL_SERPENT (CC_STEPS_BISHOP)

extern CcTypedStep const CC_STEPS_LIGHT_SHAMAN[ CC_STEPS_LIGHT_SHAMAN_SIZE ];
extern CcTypedStep const CC_STEPS_DARK_SHAMAN[ CC_STEPS_DARK_SHAMAN_SIZE ];

extern CcTypedStep const CC_STEPS_LIGHT_SCOUT[ CC_STEPS_SCOUT_SIZE ];
extern CcTypedStep const CC_STEPS_DARK_SCOUT[ CC_STEPS_SCOUT_SIZE ];
extern CcTypedStep const CC_STEPS_GRENADIER[ CC_STEPS_GRENADIER_SIZE ];

extern CcTypedStep const CC_STEPS_MIRACLE_STARCHILD[ CC_STEPS_MIRACLE_STARCHILD_SIZE ];
#define CC_STEPS_STARTING_MONOLITH (CC_STEPS_KNIGHT)

/** @} */ // end of step_generator_arrays

/** @} */ // end of step_generator_array


/**
    Function checking if step is valid, by searching a given array holding all valid steps for a piece.

    @param step A step to check.
    @param steps An array of all valid steps.
    @param steps_len__d _Optional_, array length.

    @note
    If `steps_len__d` is not used (i.e. it's `0` == `CC_STEPS_LEN_INVALID_DATA_TERMINATED`),
    `steps` array *must* be terminated with invalid position (i.e. `CC_POS_INVALID`) as a guard.

    @return `true` if step is valid (i.e. found in a given `steps` array), `false` otherwise.
*/
bool cc_is_step_valid( CcTypedStep step, CcTypedStep const steps[], size_t steps_len__d );


/** @defgroup step_is_valid_macros The step validity macros
 *  The step validity macro conveniences are meant to be used instead of `cc_is_step_valid()`.

    @see cc_is_step_valid()
 *  @{
 */

/** @defgroup step_is_valid_base_macros The step validity base macros
 *  The step validity base macros define common movement among pieces.
 *  @{
 */

#define CC_LIGHT_PAWN_STEP_IS_VALID(step) \
    ( cc_is_step_valid( (step), CC_STEPS_LIGHT_PAWN, CC_STEPS_PAWN_LEN ) )

#define CC_DARK_PAWN_STEP_IS_VALID(step) \
    ( cc_is_step_valid( (step), CC_STEPS_DARK_PAWN, CC_STEPS_PAWN_LEN ) )

#define CC_LIGHT_SIDEWAYS_PAWN_STEP_IS_VALID(step) \
    ( cc_is_step_valid( (step), CC_STEPS_LIGHT_SIDEWAYS_PAWN, CC_STEPS_SIDEWAYS_PAWN_LEN ) )

#define CC_DARK_SIDEWAYS_PAWN_STEP_IS_VALID(step) \
    ( cc_is_step_valid( (step), CC_STEPS_DARK_SIDEWAYS_PAWN, CC_STEPS_SIDEWAYS_PAWN_LEN ) )


#define CC_KNIGHT_STEP_IS_VALID(step) \
    ( cc_is_step_valid( (step), CC_STEPS_KNIGHT, CC_STEPS_KNIGHT_LEN ) )

#define CC_BISHOP_STEP_IS_VALID(step) \
    ( cc_is_step_valid( (step), CC_STEPS_BISHOP, CC_STEPS_BISHOP_LEN ) )

#define CC_ROOK_STEP_IS_VALID(step) \
    ( cc_is_step_valid( (step), CC_STEPS_ROOK, CC_STEPS_ROOK_LEN ) )

#define CC_QUEEN_STEP_IS_VALID(step) \
    ( cc_is_step_valid( (step), CC_STEPS_QUEEN, CC_STEPS_QUEEN_LEN ) )

#define CC_KING_STEP_IS_VALID(step) \
    ( cc_is_step_valid( (step), CC_STEPS_KING, CC_STEPS_KING_LEN ) )


#define CC_PEGASUS_STEP_IS_VALID(step) \
    ( cc_is_step_valid( (step), CC_STEPS_PEGASUS, CC_STEPS_PEGASUS_LEN ) )

#define CC_PYRAMID_STEP_IS_VALID(step) \
    ( cc_is_step_valid( (step), CC_STEPS_PYRAMID, CC_STEPS_PYRAMID_LEN ) )

#define CC_UNICORN_SHORT_STEP_IS_VALID(step) \
    ( cc_is_step_valid( (step), CC_STEPS_SHORT_UNICORN, CC_STEPS_SHORT_UNICORN_LEN ) )

#define CC_UNICORN_LONG_STEP_IS_VALID(step) \
    ( cc_is_step_valid( (step), CC_STEPS_LONG_UNICORN, CC_STEPS_LONG_UNICORN_LEN ) )


#define CC_STAR_STEP_IS_VALID(step) \
    ( cc_is_step_valid( (step), CC_STEPS_STAR, CC_STEPS_STAR_LEN ) )

#define CC_CENTAUR_SHORT_STEP_IS_VALID(step) \
    ( cc_is_step_valid( (step), CC_STEPS_SHORT_CENTAUR, CC_STEPS_SHORT_CENTAUR_LEN ) )

#define CC_CENTAUR_LONG_STEP_IS_VALID(step) \
    ( cc_is_step_valid( (step), CC_STEPS_LONG_CENTAUR, CC_STEPS_LONG_CENTAUR_LEN ) )

#define CC_SERPENT_LEFT_STEP_IS_VALID(step) \
    ( cc_is_step_valid( (step), CC_STEPS_SERPENT_LEFT, CC_STEPS_SERPENT_LEN ) )

#define CC_SERPENT_RIGHT_STEP_IS_VALID(step) \
    ( cc_is_step_valid( (step), CC_STEPS_SERPENT_RIGHT, CC_STEPS_SERPENT_LEN ) )

#define CC_SERPENT_STEP_IS_VALID(step) \
    ( cc_is_step_valid( (step), CC_STEPS_ALL_SERPENT, CC_STEPS_ALL_SERPENT_LEN ) )


#define CC_LIGHT_SCOUT_STEP_IS_VALID(step) \
    ( cc_is_step_valid( (step), CC_STEPS_LIGHT_SCOUT, CC_STEPS_SCOUT_LEN ) )

#define CC_DARK_SCOUT_STEP_IS_VALID(step) \
    ( cc_is_step_valid( (step), CC_STEPS_DARK_SCOUT, CC_STEPS_SCOUT_LEN ) )

#define CC_GRENADIER_STEP_IS_VALID(step) \
    ( cc_is_step_valid( (step), CC_STEPS_GRENADIER, CC_STEPS_GRENADIER_LEN ) )


#define CC_LIGHT_SHAMAN_STEP_IS_VALID(step) \
    ( cc_is_step_valid( (step), CC_STEPS_LIGHT_SHAMAN, CC_STEPS_LIGHT_SHAMAN_LEN ) )

#define CC_DARK_SHAMAN_STEP_IS_VALID(step) \
    ( cc_is_step_valid( (step), CC_STEPS_DARK_SHAMAN, CC_STEPS_DARK_SHAMAN_LEN ) )

#define CC_STARCHILD_MIRACLE_STEP_IS_VALID(step) \
    ( cc_is_step_valid( (step), CC_STEPS_MIRACLE_STARCHILD, CC_STEPS_MIRACLE_STARCHILD_LEN ) )


/** @} */ // end of step_is_valid_base_macros


/** @defgroup step_is_valid_derived_macros The step validity derived macros
 *  The step validity derived macros share definition with base macros.
 *  @{
 */

#define CC_SERPENT_COLOR_CHANGE_STEP_IS_VALID(step) \
    ( cc_is_step_valid( (step), CC_STEPS_ROOK, CC_STEPS_ROOK_LEN ) )


/** @} */ // end of step_is_valid_derived_macros

/** @} */ // end of step_is_valid_macros


bool cc_is_same_color( CcPieceEnum piece, CcPos pos );

bool cc_convert_steps_to_pos_link( CcTypedStep const steps[],
                                   size_t steps_len,
                                   CcTypedStepLink ** steps__iod_a );


// TODO :: FIX
//
// static bool cc_starting_steps_pawn( CcVariantEnum variant,
//                                     CcPieceEnum piece,
//                                     CcTypedStepLink ** starting_steps__o );

// static bool cc_starting_steps_unicorn( CcPieceEnum piece,
//                                        CcTypedStep pos,
//                                        CcTypedStepLink ** starting_steps__o );

// static bool cc_starting_steps_centaur( CcPieceEnum piece,
//                                        CcTypedStep pos,
//                                        CcTypedStepLink ** starting_steps__o );

// bool cc_starting_steps( CcVariantEnum variant,
//                         CcPieceEnum piece,
//                         CcPieceEnum activator,
//                         CcTypedStep pos,
//                         CcTypedStepLink ** starting_steps__o );
//
// TODO :: FIX


#endif /* __CC_POS_DEFS_H__ */
