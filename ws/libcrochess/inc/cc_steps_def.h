// Copyright (c) 2021, 2022 Mario Mlačak, mmlacak@gmail.com
// Licensed under GNU GPL v3+ license. See LICENSING, COPYING files for details.

#ifndef __CC_STEPS_DEF_H__
#define __CC_STEPS_DEF_H__

#include <stddef.h>

// #include "cc_piece.h"
// #include "cc_chessboard.h"
#include "cc_pos.h"


/**
    @file cc_steps_def.h
    @brief Step checkers.
*/


/** @defgroup step_generator_array The step generator array
 *  The step generator constants and arrays are meant to be used via `cc_gen_pos()`,
    and `cc_steps_pos_is_valid()`.

    @see cc_gen_pos(), cc_steps_pos_is_valid()
 *  @{
 */

/** @defgroup step_generator_lengths The step generator array lengths
 *  The step generator array lengths.

    @note
    Length of an array is count of useful data in it, without terminating data,
    similar how `strlen()` does not count zero-terminating `char` in strings.
 *  @{
 */

#define CC_STEPS_PAWN_LEN (1)
#define CC_STEPS_CAPTURE_PAWN_LEN (2)
#define CC_STEPS_SIDEWAYS_PAWN_LEN (3)

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

#define CC_STEPS_LIGHT_SHAMAN_LEN (CC_STEPS_KNIGHT_LEN)
#define CC_STEPS_CAPTURE_LIGHT_SHAMAN_LEN (CC_STEPS_LONG_UNICORN_LEN)
#define CC_STEPS_DARK_SHAMAN_LEN (CC_STEPS_LONG_UNICORN_LEN)
#define CC_STEPS_CAPTURE_DARK_SHAMAN_LEN (CC_STEPS_KNIGHT_LEN)

#define CC_STEPS_MONOLITH_LEN (4)
#define CC_STEPS_MIRACLE_STARCHILD_LEN (CC_STEPS_QUEEN_LEN)

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
#define CC_STEPS_CAPTURE_PAWN_SIZE (CC_STEPS_CAPTURE_PAWN_LEN + 1)
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

#define CC_STEPS_LIGHT_SHAMAN_SIZE (CC_STEPS_KNIGHT_SIZE)
#define CC_STEPS_CAPTURE_LIGHT_SHAMAN_SIZE (CC_STEPS_LONG_UNICORN_SIZE)
#define CC_STEPS_DARK_SHAMAN_SIZE (CC_STEPS_LONG_UNICORN_SIZE)
#define CC_STEPS_CAPTURE_DARK_SHAMAN_SIZE (CC_STEPS_KNIGHT_SIZE)

#define CC_STEPS_MONOLITH_SIZE (CC_STEPS_MONOLITH_LEN + 1)
#define CC_STEPS_MIRACLE_STARCHILD_SIZE (CC_STEPS_QUEEN_SIZE)

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
    calculation. Currently, it's defined as `CC_POS_INVALID`, which is
    `{ CC_INVALID_COORD, CC_INVALID_COORD }`.

    @see CcPos, CC_POS_INVALID, CC_INVALID_COORD
 *  @{
 */

extern CcPos const CC_STEPS_LIGHT_PAWN[ CC_STEPS_PAWN_SIZE ];
extern CcPos const CC_STEPS_DARK_PAWN[ CC_STEPS_PAWN_SIZE ];
extern CcPos const CC_STEPS_CAPTURE_LIGHT_PAWN[ CC_STEPS_CAPTURE_PAWN_SIZE ];
extern CcPos const CC_STEPS_CAPTURE_DARK_PAWN[ CC_STEPS_CAPTURE_PAWN_SIZE ];
extern CcPos const CC_STEPS_LIGHT_SIDEWAYS_PAWN[ CC_STEPS_SIDEWAYS_PAWN_SIZE ];
extern CcPos const CC_STEPS_DARK_SIDEWAYS_PAWN[ CC_STEPS_SIDEWAYS_PAWN_SIZE ];

extern CcPos const CC_STEPS_KNIGHT[ CC_STEPS_KNIGHT_SIZE ];
extern CcPos const CC_STEPS_BISHOP[ CC_STEPS_BISHOP_SIZE ];
extern CcPos const CC_STEPS_ROOK[ CC_STEPS_ROOK_SIZE ]; // Also, Serpent's color-changing move.
extern CcPos const CC_STEPS_QUEEN[ CC_STEPS_QUEEN_SIZE ];
#define CC_STEPS_KING (CC_STEPS_QUEEN)

#define CC_STEPS_PEGASUS (CC_STEPS_KNIGHT)
#define CC_STEPS_PYRAMID (CC_STEPS_ROOK)
#define CC_STEPS_SHORT_UNICORN (CC_STEPS_KNIGHT)
extern CcPos const CC_STEPS_LONG_UNICORN[ CC_STEPS_LONG_UNICORN_SIZE ];

#define CC_STEPS_STAR (CC_STEPS_QUEEN)
#define CC_STEPS_SHORT_CENTAUR (CC_STEPS_SHORT_UNICORN)
#define CC_STEPS_LONG_CENTAUR (CC_STEPS_LONG_UNICORN)
extern CcPos const CC_STEPS_SERPENT_LEFT[ CC_STEPS_SERPENT_SIZE ];
extern CcPos const CC_STEPS_SERPENT_RIGHT[ CC_STEPS_SERPENT_SIZE ];

#define CC_STEPS_LIGHT_SHAMAN (CC_STEPS_KNIGHT)
#define CC_STEPS_CAPTURE_LIGHT_SHAMAN (CC_STEPS_LONG_UNICORN)
#define CC_STEPS_DARK_SHAMAN (CC_STEPS_LONG_UNICORN)
#define CC_STEPS_CAPTURE_DARK_SHAMAN (CC_STEPS_KNIGHT)

extern CcPos const CC_STEPS_MONOLITH_LEFT[ CC_STEPS_MONOLITH_SIZE ];
extern CcPos const CC_STEPS_MONOLITH_RIGHT[ CC_STEPS_MONOLITH_SIZE ];
#define CC_STEPS_MIRACLE_STARCHILD (CC_STEPS_QUEEN)

/** @} */ // end of step_generator_arrays

/** @} */ // end of step_generator_array


/**
    Function checking if step is valid, by searching a
    given array holding all valid steps for a piece.

    @param step A step to check
    @param array An array of all valid steps.
    @param array_len Array length.

    @return `true` if step is valid, `false` otherwise.
*/
bool cc_steps_pos_is_valid( CcPos step,
                            CcPos const array[  ],
                            size_t array_len );


/** @defgroup step_is_valid_macros The step validity macros
 *  The step validity macro conveniences are meant to be used instead of `cc_steps_pos_is_valid()`.

    @see cc_steps_pos_is_valid()
 *  @{
 */

/** @defgroup step_is_valid_base_macros The step validity base macros
 *  The step validity base macros define common movement among pieces.
 *  @{
 */

#define CC_LIGHT_PAWN_STEP_IS_VALID(step) \
    ( cc_steps_pos_is_valid( (step), CC_STEPS_LIGHT_PAWN, CC_STEPS_PAWN_LEN ) )

#define CC_DARK_PAWN_STEP_IS_VALID(step) \
    ( cc_steps_pos_is_valid( (step), CC_STEPS_DARK_PAWN, CC_STEPS_PAWN_LEN ) )

#define CC_LIGHT_PAWN_CAPTURE_STEP_IS_VALID(step) \
    ( cc_steps_pos_is_valid( (step), CC_STEPS_CAPTURE_LIGHT_PAWN, CC_STEPS_CAPTURE_PAWN_LEN ) )

#define CC_DARK_PAWN_CAPTURE_STEP_IS_VALID(step) \
    ( cc_steps_pos_is_valid( (step), CC_STEPS_CAPTURE_DARK_PAWN, CC_STEPS_CAPTURE_PAWN_LEN ) )

#define CC_LIGHT_SIDEWAYS_PAWN_STEP_IS_VALID(step) \
    ( cc_steps_pos_is_valid( (step), CC_STEPS_LIGHT_SIDEWAYS_PAWN, CC_STEPS_SIDEWAYS_PAWN_LEN ) )

#define CC_DARK_SIDEWAYS_PAWN_STEP_IS_VALID(step) \
    ( cc_steps_pos_is_valid( (step), CC_STEPS_DARK_SIDEWAYS_PAWN, CC_STEPS_SIDEWAYS_PAWN_LEN ) )


#define CC_KNIGHT_STEP_IS_VALID(step) \
    ( cc_steps_pos_is_valid( (step), CC_STEPS_KNIGHT, CC_STEPS_KNIGHT_LEN ) )

#define CC_BISHOP_STEP_IS_VALID(step) \
    ( cc_steps_pos_is_valid( (step), CC_STEPS_BISHOP, CC_STEPS_BISHOP_LEN ) )

#define CC_ROOK_STEP_IS_VALID(step) \
    ( cc_steps_pos_is_valid( (step), CC_STEPS_ROOK, CC_STEPS_ROOK_LEN ) )

#define CC_QUEEN_STEP_IS_VALID(step) \
    ( cc_steps_pos_is_valid( (step), CC_STEPS_QUEEN, CC_STEPS_QUEEN_LEN ) )

#define CC_KING_STEP_IS_VALID(step) \
    ( cc_steps_pos_is_valid( (step), CC_STEPS_KING, CC_STEPS_KING_LEN ) )


#define CC_PEGASUS_STEP_IS_VALID(step) \
    ( cc_steps_pos_is_valid( (step), CC_STEPS_PEGASUS, CC_STEPS_PEGASUS_LEN ) )

#define CC_PYRAMID_STEP_IS_VALID(step) \
    ( cc_steps_pos_is_valid( (step), CC_STEPS_PYRAMID, CC_STEPS_PYRAMID_LEN ) )

#define CC_UNICORN_SHORT_STEP_IS_VALID(step) \
    ( cc_steps_pos_is_valid( (step), CC_STEPS_SHORT_UNICORN, CC_STEPS_SHORT_UNICORN_LEN ) )

#define CC_UNICORN_LONG_STEP_IS_VALID(step) \
    ( cc_steps_pos_is_valid( (step), CC_STEPS_LONG_UNICORN, CC_STEPS_LONG_UNICORN_LEN ) )


#define CC_STAR_STEP_IS_VALID(step) \
    ( cc_steps_pos_is_valid( (step), CC_STEPS_STAR, CC_STEPS_STAR_LEN ) )

#define CC_CENTAUR_SHORT_STEP_IS_VALID(step) \
    ( cc_steps_pos_is_valid( (step), CC_STEPS_SHORT_CENTAUR, CC_STEPS_SHORT_CENTAUR_LEN ) )

#define CC_CENTAUR_LONG_STEP_IS_VALID(step) \
    ( cc_steps_pos_is_valid( (step), CC_STEPS_LONG_CENTAUR, CC_STEPS_LONG_CENTAUR_LEN ) )

#define CC_SERPENT_LEFT_STEP_IS_VALID(step) \
    ( cc_steps_pos_is_valid( (step), CC_STEPS_SERPENT_LEFT, CC_STEPS_SERPENT_LEN ) )

#define CC_SERPENT_RIGHT_STEP_IS_VALID(step) \
    ( cc_steps_pos_is_valid( (step), CC_STEPS_SERPENT_RIGHT, CC_STEPS_SERPENT_LEN ) )


#define CC_LIGHT_SHAMAN_STEP_IS_VALID(step) \
    ( cc_steps_pos_is_valid( (step), CC_STEPS_LIGHT_SHAMAN, CC_STEPS_LIGHT_SHAMAN_LEN ) )

#define CC_LIGHT_SHAMAN_CAPTURE_STEP_IS_VALID(step) \
    ( cc_steps_pos_is_valid( (step), CC_STEPS_CAPTURE_LIGHT_SHAMAN, CC_STEPS_CAPTURE_LIGHT_SHAMAN_LEN ) )

#define CC_DARK_SHAMAN_STEP_IS_VALID(step) \
    ( cc_steps_pos_is_valid( (step), CC_STEPS_DARK_SHAMAN, CC_STEPS_DARK_SHAMAN_LEN ) )

#define CC_DARK_SHAMAN_CAPTURE_STEP_IS_VALID(step) \
    ( cc_steps_pos_is_valid( (step), CC_STEPS_CAPTURE_DARK_SHAMAN, CC_STEPS_CAPTURE_DARK_SHAMAN_LEN ) )


#define CC_MONOLITH_LEFT_STEP_IS_VALID(step) \
    ( cc_steps_pos_is_valid( (step), CC_STEPS_MONOLITH_LEFT, CC_STEPS_MONOLITH_LEN ) )

#define CC_MONOLITH_RIGHT_STEP_IS_VALID(step) \
    ( cc_steps_pos_is_valid( (step), CC_STEPS_MONOLITH_RIGHT, CC_STEPS_MONOLITH_LEN ) )

#define CC_STARCHILD_MIRACLE_STEP_IS_VALID(step) \
    ( cc_steps_pos_is_valid( (step), CC_STEPS_MIRACLE_STARCHILD, CC_STEPS_MIRACLE_STARCHILD_LEN ) )


/** @} */ // end of step_is_valid_base_macros


/** @defgroup step_is_valid_derived_macros The step validity derived macros
 *  The step validity derived macros share definition with base macrose.
 *  @{
 */

#define CC_SERPENT_STEP_IS_VALID(step) \
    ( cc_steps_pos_is_valid( (step), CC_STEPS_BISHOP, CC_STEPS_BISHOP_LEN ) )

#define CC_SERPENT_COLOR_CHANGE_STEP_IS_VALID(step) \
    ( cc_steps_pos_is_valid( (step), CC_STEPS_ROOK, CC_STEPS_ROOK_LEN ) )

/** @} */ // end of step_is_valid_derived_macros

/** @} */ // end of step_is_valid_macros


#endif /* __CC_STEPS_DEF_H__ */
