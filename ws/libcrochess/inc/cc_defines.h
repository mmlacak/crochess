// Copyright (c) 2021 Mario Mlačak, mmlacak@gmail.com
// Licensed under GNU GPL v3+ license. See LICENSING, COPYING files for details.

#ifndef __CC_DEFINES_H__
#define __CC_DEFINES_H__

#include "limits.h"


/**
    @file cc_defines.h
    @brief Constants and macros used throughout project.
*/

/**
    Smallest, invalid off-board coordinate.

    No valid trance-journey starting from any chessboard field could get to this coordinate,
    and still make it back to any on-board field.

    Used for e.g. missing coordinates, for which invalid value ascertains no useage.
*/
// + number, so that value isn't get by accident, e.g. by simply flipping bits, ...
#define CC_INVALID_OFF_BOARD_COORD_MIN (INT_MIN + 3583)

/**
    Off-board coordinate.

    Used for e.g. missing coordinates, if notation contains only destination field.
*/
#define CC_OFF_BOARD_COORD (-1)

/**
    The smallest valid on-board coordinate.
*/
#define CC_MIN_BOARD_COORD (0)

/**
    The largest valid on-board coordinate.

    This is the largest valid coordinate for the largest board, used by One variant.
    For other variants actual upper limit is smaller, see `cc_variant_board_size()`.
*/
#define CC_MAX_BOARD_COORD (25)

/**
    The largest valid board size.

    This is the largest valid board size for the largest board, used by One variant.
    For other variants actual upper limit is smaller, see `cc_variant_board_size()`.
*/
#define CC_MAX_BOARD_SIZE (26)


/**
    Macro to check if a given coordinate is valid.

    @param coord A coordinate.

    @return `1` if valid, `0` otherwise.
*/
#define CC_IS_COORD_VALID(coord) ( (coord) != CC_INVALID_OFF_BOARD_COORD_MIN )

/**
    Macro to check if a given position is valid.

    @param i File, position along horizontal axis, integer.
    @param j Rank, position along vertical axis, integer.

    @return `1` if valid, `0` otherwise.
*/
#define CC_IS_POS_VALID(i,j) ( ( (i) != CC_INVALID_OFF_BOARD_COORD_MIN ) && ( (j) != CC_INVALID_OFF_BOARD_COORD_MIN ) )

/**
    Macro to check if a given coordinate is on board.

    @param board_size A chessboard size, integer.
    @param coord A coordinate, integer.

    @warning
    All arguments are cast to `int`.

    @return `1` if on board, `0` otherwise.
*/
#define CC_IS_COORD_ON_BOARD(board_size,coord) ( ( CC_MIN_BOARD_COORD <= (int)(coord) ) && ( (int)(coord) < (int)(board_size) ) )

/**
    Macro to check if a given position is on board.

    @param board_size A chessboard size, integer.
    @param i File, position along horizontal axis, integer.
    @param j Rank, position along vertical axis, integer.

    @warning
    All arguments are cast to `int`.

    @return `1` if on board, `0` otherwise.
*/
#define CC_IS_POS_ON_BOARD(board_size,i,j) ( ( CC_MIN_BOARD_COORD <= (int)(i) ) && ( (int)(i) < (int)(board_size) ) && ( CC_MIN_BOARD_COORD <= (int)(j) ) && ( (int)(j) < (int)(board_size) ) )


/**
    Macro to inline comparing, producing smaller value.

    @param x A number.
    @param y An other number

    @return Smaller number of the two given.
*/
#define CC_MIN(x,y) ( ( (y) > (x) ) ? (x) : (y) )

/**
    Macro to inline comparing, producing larger value.

    @param x A number.
    @param y An other number

    @return Larger number of the two given.
*/
#define CC_MAX(x,y) ( ( (x) > (y) ) ? (x) : (y) )

/**
    Macro to inline sign function.

    @param i A number.

    @note
    Sign of a number is defined as `1` for positive numbers,
    `-1` for negative numbers, `0` otherwise.

    @return Sign of a given number.
*/
#define CC_SIGN(i) ( ( (i) > 0 ) ? (1) : ( ( (i) < 0 ) ? (-1) : (0) ) )


/**
    Macro to free() with cast.

    @param ptr A pointer.

    @note
    Pointer is just cast to `void *`, before being handled over to `free()`.

    @return Nothing.
*/
#define CC_FREE(ptr) free( (void *)(ptr) )

/**
    Macro to free() with cast, and setting inner pointer to NULL.

    @param ptr_ptr A pointer to pointer.

    @note
    Inner pointer is just cast to `void *`, before being handled over to `free()`,
    then set to `NULL`.

    @return Nothing.
*/
#define CC_FREE_NULL(ptr_ptr)           \
{                                       \
    free( (void *)( *(ptr_ptr) ) );     \
    *(ptr_ptr) = NULL;                  \
}


// /**
//     Macro to cast any pointer-to-pointer into const-pointer-to-pointer-to-type,
//     i.e. `type **` --> `type ** const`.
// */
// #define CC_CAST_T_P_PC(type,ptr_ptr) ( (type ** const)(ptr_ptr) )


#endif /* __CC_DEFINES_H__ */
