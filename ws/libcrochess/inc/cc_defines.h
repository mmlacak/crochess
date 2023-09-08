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
    Macro to evaluate logical XOR.

    @param i A number, preferably integer.
    @param j A number, preferably integer.

    @return Logical XOR between arguments.
*/
#define CC_XOR(i,j) ( ( (!(!(i))) + (!(!(j))) ) == 1 )

/**
    Smallest, invalid off-board coordinate.

    No valid trance-journey starting from any chessboard field could get to this coordinate,
    and still make it back to any on-board field.

    Used for e.g. missing coordinates, for which invalid value ascertains no useage.
*/
// + number, so that value can't be get by accident, e.g. by simply flipping bits, ...
#define CC_INVALID_COORD (INT_MIN + 3583)

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
    Macro to convert numerical file value into char.

    @param byte_file Rank, position along vertical axis, numerical value.

    @warning
    Value of `byte_file` is expected to be in a range of [0, 25],
    undefined behavior if it's not.

    @return File character if argument within range, undefined behavior otherwise.
*/
#define CC_CONVERT_BYTE_INTO_FILE_CHAR(byte_file) ( 'a' + (byte_file) )

/**
    Macro to convert char into numerical file value.

    @param char_file Rank, position along vertical axis, char value.

    @warning
    Value of `char_file` is expected to be in a range of [``'a'``, ``'z'``],
    undefined behavior if it's not.

    @return File number if argument within range, undefined behavior otherwise.
*/
#define CC_CONVERT_FILE_CHAR_INTO_NUM(char_file) ( (char_file) - 'a' )

/**
    Macro to convert string into numerical rank value.

    @param char_ptr_rank Rank, position along vertical axis, string pointer value.

    @warning
    Given string must be zero-terminated.

    @see atoi()

    @return Rank number if successful, undefined behavior otherwise.
*/
#define CC_CONVERT_RANK_STR_INTO_NUM(char_ptr_rank) ( atoi( (char_ptr_rank) ) - 1 )

/**
    Macro to check if a given coordinate is valid.

    @param coord A coordinate.

    @return `1` if valid, `0` otherwise.
*/
#define CC_IS_COORD_VALID(coord) ( (coord) != CC_INVALID_COORD )

/**
    Macro to check if a given position is valid.

    @param i File, position along horizontal axis, integer.
    @param j Rank, position along vertical axis, integer.

    @return `1` if valid, `0` otherwise.
*/
#define CC_IS_COORD_2_VALID(i,j) ( ( (i) != CC_INVALID_COORD ) && ( (j) != CC_INVALID_COORD ) )

/**
    Macro to check if a given position is light.

    @param i File, position along horizontal axis, integer.
    @param j Rank, position along vertical axis, integer.

    @return `1` if light, `0` otherwise.
*/
#define CC_IS_FIELD_LIGHT(i,j) ( ((i) + (j)) % 2 != 0 )

/**
    Macro to check if a given position is dark.

    @param i File, position along horizontal axis, integer.
    @param j Rank, position along vertical axis, integer.

    @return `1` if dark, `0` otherwise.
*/
#define CC_IS_FIELD_DARK(i,j) ( ((i) + (j)) % 2 == 0 )

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
#define CC_IS_COORD_2_ON_BOARD(board_size,i,j)              \
    ( ( CC_IS_COORD_ON_BOARD( (board_size), (i) ) ) &&      \
      ( CC_IS_COORD_ON_BOARD( (board_size), (j) ) ) )
// ( ( CC_MIN_BOARD_COORD <= (int)(i) ) && ( (int)(i) < (int)(board_size) ) && ( CC_MIN_BOARD_COORD <= (int)(j) ) && ( (int)(j) < (int)(board_size) ) )

/**
    Macro to check if a given disambiguation (i.e. partial position) is on board.

    @param board_size A chessboard size, integer.
    @param i File, position along horizontal axis, integer.
    @param j Rank, position along vertical axis, integer.

    @warning
    All arguments are cast to `int`.

    @return `1` if on board, `0` otherwise.
*/
#define CC_IS_ANY_COORD_ON_BOARD(board_size,i,j)            \
    ( ( CC_IS_COORD_ON_BOARD( (board_size), (i) ) ) ||      \
      ( CC_IS_COORD_ON_BOARD( (board_size), (j) ) ) )

/**
    Macro to check if a given position is on a light side of a chessboard.

    @param board_size A chessboard size, integer.
    @param rank Rank, position along vertical axis, integer.

    @warning
    All arguments are cast to `int`.

    @return `1` if on a light side, `0` otherwise.
*/
#define CC_IS_FIELD_ON_LIGHT_SIDE(board_size,rank) ( ( 0 <= (int)(rank) ) && ( (int)(rank) < ( (int)(board_size) / 2 ) ) )

/**
    Macro to check if a given position is on a dark side of a chessboard.

    @param board_size A chessboard size, integer.
    @param rank Rank, position along vertical axis, integer.

    @warning
    All arguments are cast to `int`.

    @return `1` if on a dark side, `0` otherwise.
*/
#define CC_IS_FIELD_ON_DARK_SIDE(board_size,rank) ( ( ( (int)(board_size) / 2 ) <= (int)(rank) ) && ( (int)(rank) < (int)(board_size) ) )


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
#define CC_FREE_NULL(ptr_ptr) {         \
    free( (void *)( *(ptr_ptr) ) );     \
    *(ptr_ptr) = NULL; }

/**
    Macro to call `printf()`, depending on a compile-time constant.

    @param fmt Formatting string.
    @param ... Variadic parameters, as used by `printf()`.

    @note
    Compile-time constant which controls definition of this macro is `__CC_STR_PRINT_INFO__`.

    @return
    The same as `printf()`, i.e. an `int` value.
    Number of `char`s printed, an error code if negative.
*/
#ifdef __CC_STR_PRINT_INFO__
#define CC_PRINTF_IF_INFO(fmt,...) printf( fmt __VA_OPT__(,) __VA_ARGS__ )
#else // __CC_STR_PRINT_INFO__
#define CC_PRINTF_IF_INFO(fmt,...) 0
#endif // __CC_STR_PRINT_INFO__


#endif /* __CC_DEFINES_H__ */
