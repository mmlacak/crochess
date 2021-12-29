// Copyright (c) 2021 Mario Mlačak, mmlacak@gmail.com
// Licensed under GNU GPL v3+ license. See LICENSING, COPYING files for details.

#ifndef __CC_GEN_POS_H__
#define __CC_GEN_POS_H__

#include <stddef.h>
#include <stdbool.h>

#include "cc_pos.h"

/**
    @file cc_gen_pos.h
    @brief Step generator constants, arrays, and related functions.
*/


/** @defgroup step_generator_array The step generator array
 *  The step generator constants and arrays are meant to be used via `cc_gen_pos()`,
    and `cc_gen_pos_is_valid_step()`.

    @see cc_gen_pos(), cc_gen_pos_is_valid_step()
 *  @{
 */

/** @defgroup step_generator_lengths The step generator array lengths
 *  The step generator array lengths.

    @note
    Length of an array is count of useful data in it, without terminating data,
    similar how `strlen()` does not count zero-terminating `char` in strings.
 *  @{
 */

#define CC_GEN_POS_PAWN_STEPS_LEN (3)
#define CC_GEN_POS_BISHOP_STEPS_LEN (4)
#define CC_GEN_POS_ROOK_STEPS_LEN (4)
#define CC_GEN_POS_QUEEN_STEPS_LEN (8)
#define CC_GEN_POS_KNIGHT_STEPS_LEN (8)
#define CC_GEN_POS_UNICORN_STEPS_LEN (16)

/** @} */ // end of step_generator_lengths

/** @defgroup step_generator_sizes The step generator array sizes
 *  The step generator array sizes.

    @note
    Size of an array is count of all items in an it, including guard data,
    i.e. terminating position.
    For the same array, size is larger by 1 than the length.
 *  @{
 */

#define CC_GEN_POS_PAWN_STEPS_SIZE (CC_GEN_POS_PAWN_STEPS_LEN + 1)
#define CC_GEN_POS_BISHOP_STEPS_SIZE (CC_GEN_POS_BISHOP_STEPS_LEN + 1)
#define CC_GEN_POS_ROOK_STEPS_SIZE (CC_GEN_POS_ROOK_STEPS_LEN + 1)
#define CC_GEN_POS_QUEEN_STEPS_SIZE (CC_GEN_POS_QUEEN_STEPS_LEN + 1)
#define CC_GEN_POS_KNIGHT_STEPS_SIZE (CC_GEN_POS_KNIGHT_STEPS_LEN + 1)
#define CC_GEN_POS_UNICORN_STEPS_SIZE (CC_GEN_POS_UNICORN_STEPS_LEN + 1)

/** @} */ // end of step_generator_sizes

/** @defgroup step_generator_arrays The step generator arrays
 *  The step generator arrays are constants defining all valid steps,
    for a pieces of similar movement, e.g. Bishop and Serpent share
    the same array, for Serpent's regular movement.

    @note
    All arrays, beside their length and size, have a terminating position,
    similar to zero-terminating strings.

    @note
    Terminating position is a `CcPos` with coordinates past normal off-board
    calculation. Currently, it's defined as
    `{ CC_INVALID_OFF_BOARD_COORD_MIN, CC_INVALID_OFF_BOARD_COORD_MIN }`.

    @see CcPos, CC_INVALID_OFF_BOARD_COORD_MIN
 *  @{
 */

extern CcPos const CC_GEN_POS_LIGHT_PAWN_STEPS[ CC_GEN_POS_PAWN_STEPS_SIZE ];
extern CcPos const CC_GEN_POS_DARK_PAWN_STEPS[ CC_GEN_POS_PAWN_STEPS_SIZE ];
extern CcPos const CC_GEN_POS_BISHOP_STEPS[ CC_GEN_POS_BISHOP_STEPS_SIZE ]; // Also, Serpent.
extern CcPos const CC_GEN_POS_ROOK_STEPS[ CC_GEN_POS_ROOK_STEPS_SIZE ]; // Also, Serpent's color-changing move.
extern CcPos const CC_GEN_POS_QUEEN_STEPS[ CC_GEN_POS_QUEEN_STEPS_SIZE ];
extern CcPos const CC_GEN_POS_KNIGHT_STEPS[ CC_GEN_POS_KNIGHT_STEPS_SIZE ];
extern CcPos const CC_GEN_POS_UNICORN_STEPS[ CC_GEN_POS_UNICORN_STEPS_SIZE ];

/** @} */ // end of step_generator_arrays

/** @} */ // end of step_generator_array


/**
    Function is a position generator.

    @param pos__io _Input/output_ parameter, generated position.
    @param step A step.
    @param from_or_to Flag, whether next position is in direction of a `step`, or opposite.

    @return `true` is successful, `false` otherwise.
*/
bool cc_gen_pos( CcPos * restrict pos__io,
                 CcPos step,
                 bool from_or_to );

/**
    Function checking if step is valid, by searching a
    given array holding all valid steps for a piece.

    @param step A step to check
    @param array An array of all valid steps.
    @param array_len Array length.

    @return `true` if step is valid, `false` otherwise.
*/
bool cc_gen_pos_is_valid_step( CcPos step,
                               CcPos const array[  ],
                               size_t array_len );


/** @defgroup step_is_valid_macros The step validity macros
 *  The step validity macro conveniences are meant to be used instead of `cc_gen_pos_is_valid_step()`.

    @see cc_gen_pos_is_valid_step()
 *  @{
 */

/** @defgroup step_is_valid_base_macros The step validity base macros
 *  The step validity base macros define common movement among pieces.
 *  @{
 */

#define CC_GEN_POS_LIGHT_PAWN_STEP_IS_VALID(step) \
    ( cc_gen_pos_is_valid_step( (step), CC_GEN_POS_LIGHT_PAWN_STEPS, CC_GEN_POS_PAWN_STEPS_LEN ) )

#define CC_GEN_POS_DARK_PAWN_STEP_IS_VALID(step) \
    ( cc_gen_pos_is_valid_step( (step), CC_GEN_POS_DARK_PAWN_STEPS, CC_GEN_POS_PAWN_STEPS_LEN ) )

#define CC_GEN_POS_BISHOP_STEP_IS_VALID(step) \
    ( cc_gen_pos_is_valid_step( (step), CC_GEN_POS_BISHOP_STEPS, CC_GEN_POS_BISHOP_STEPS_LEN ) )

#define CC_GEN_POS_ROOK_STEP_IS_VALID(step) \
    ( cc_gen_pos_is_valid_step( (step), CC_GEN_POS_ROOK_STEPS, CC_GEN_POS_ROOK_STEPS_LEN ) )

#define CC_GEN_POS_QUEEN_STEP_IS_VALID(step) \
    ( cc_gen_pos_is_valid_step( (step), CC_GEN_POS_QUEEN_STEPS, CC_GEN_POS_QUEEN_STEPS_LEN ) )

#define CC_GEN_POS_KNIGHT_STEP_IS_VALID(step) \
    ( cc_gen_pos_is_valid_step( (step), CC_GEN_POS_KNIGHT_STEPS, CC_GEN_POS_KNIGHT_STEPS_LEN ) )

#define CC_GEN_POS_UNICORN_STEP_IS_VALID(step) \
    ( cc_gen_pos_is_valid_step( (step), CC_GEN_POS_UNICORN_STEPS, CC_GEN_POS_UNICORN_STEPS_LEN ) )

/** @} */ // end of step_is_valid_base_macros


/** @defgroup step_is_valid_derived_macros The step validity derived macros
 *  The step validity derived macros share definition with base macrose.
 *  @{
 */

#define CC_GEN_POS_SERPENT_STEP_IS_VALID(step) \
    ( cc_gen_pos_is_valid_step( (step), CC_GEN_POS_BISHOP_STEPS, CC_GEN_POS_BISHOP_STEPS_LEN ) )

#define CC_GEN_POS_SERPENT_COLOR_CHANGE_STEP_IS_VALID(step) \
    ( cc_gen_pos_is_valid_step( (step), CC_GEN_POS_ROOK_STEPS, CC_GEN_POS_ROOK_STEPS_LEN ) )

/** @} */ // end of step_is_valid_derived_macros

/** @} */ // end of step_is_valid_macros

#endif /* __CC_GEN_POS_H__ */
