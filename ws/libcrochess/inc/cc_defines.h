// Copyright (c) 2021 Mario Mlačak, mmlacak@gmail.com
// Licensed under GNU GPL v3+ license. See LICENSING, COPYING files for details.

#ifndef __CC_DEFINES_H__
#define __CC_DEFINES_H__

#include "stdbool.h"
#include "limits.h"

/**
    @file cc_defines.h
    @brief Constants and macros used throughout project.
*/


/**
    Convenience shorthand for unsigned integer types.
*/
typedef unsigned char uchar;
typedef unsigned short ushort;
typedef unsigned int uint;

/**
    Minimum value, used for all unsigned integer types.
*/
#define CC_UNSIGNED_MIN (0)


/**
    Enumerates void and Boolean values.

    Void value is used to represent undefined, uninitialized, or error state.
*/
typedef enum CcMaybeBoolEnum {
    CC_MBE_Void = -1, /**< Void (undefined, uninitialized, or error) value. */
    CC_MBE_False = 0, /**< Boolean `false` value. */
    CC_MBE_True = 1, /**< Boolean `true` value. */
} CcMaybeBoolEnum;

/**
    Macro to convert `bool` value into `CcMaybeBoolEnum`.

    @param bool_val Boolean value.

    @return `CcMaybeBoolEnum` value.
*/
#define CC_BOOL_TO_MAYBE(bool_val) ( (bool_val) ? CC_MBE_True : CC_MBE_False )

/**
    Macro to check if `CcMaybeBoolEnum` value is `CC_MBE_True`.

    @param maybe_bool `CcMaybeBoolEnum` value.

    @return `true` if check passes, `false` otherwise.
*/
#define CC_MAYBE_IS_TRUE(maybe_bool) ( ( (maybe_bool) == CC_MBE_True ) ? true : false )

/**
    Macro to check if `CcMaybeBoolEnum` value is `CC_MBE_False`.

    @param maybe_bool `CcMaybeBoolEnum` value.

    @return `true` if check passes, `false` otherwise.
*/
#define CC_MAYBE_IS_FALSE(maybe_bool) ( ( (maybe_bool) == CC_MBE_False ) ? true : false )

/**
    Macro to check if `CcMaybeBoolEnum` value is `CC_MBE_Void`.

    @param maybe_bool `CcMaybeBoolEnum` value.

    @return `true` if check passes, `false` otherwise.
*/
#define CC_MAYBE_IS_VOID(maybe_bool) ( ( (maybe_bool) == CC_MBE_Void ) ? true : false )


/**
    Macro to evaluate logical XOR.

    @param bool_1 A boolean expression, can be integer.
    @param bool_2 Another boolean expression, can be integer.

    @note
    In case of integer(s), one has to be non-zero, while the other has to be zero, for `XOR` to return `true`.
    Arguments are converted to `bool`s (so, non-zero integer is 1, otherwise it's 0), then compared.

    @see
    [https://en.wikipedia.org/wiki/Bitwise_operations_in_C#Logical_equivalents](https://en.wikipedia.org/wiki/Bitwise_operations_in_C#Logical_equivalents),
    [https://www.reddit.com/r/C_Programming/comments/2cruz3/comment/cjih6wt/](https://www.reddit.com/r/C_Programming/comments/2cruz3/comment/cjih6wt/)

    @return Logical XOR between arguments.
*/
#define CC_XOR(bool_1,bool_2) ( ((bool)(bool_1)) != ((bool)(bool_2)) )

/**
    Smallest, invalid off-board coordinate.

    No valid trance-journey starting from any chessboard field could get to this coordinate,
    and still make it back to any on-board field.

    Used for e.g. missing coordinates, for which invalid value ascertains no usage.
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
    The smallest valid board size.

    This is the smallest valid board size for the smallest board, used by Classic Chess variant.
*/
#define CC_MIN_BOARD_SIZE (8)

/**
    The largest valid board size.

    This is the largest valid board size for the largest board, used by One variant.
    For other variants actual upper limit is smaller, see `cc_variant_board_size()`.
*/
#define CC_MAX_BOARD_SIZE (26)

/**
    Light field check constant.
*/
#define CC_FIELD_COLOR_LIGHT (1)

/**
    Dark field check constant.
*/
#define CC_FIELD_COLOR_DARK (0)


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
    Macro to check if a given position is light or dark.

    @param i File, position along horizontal axis, integer.
    @param j Rank, position along vertical axis, integer.
    @param fc Field color.

    @note
    Parameter `fc` must be either `0` or `1`, which checks if field is dark or light color, respectively.

    @note
    Macros `CC_FIELD_COLOR_DARK` (== `0`) and `CC_FIELD_COLOR_LIGHT` (== `1`) can be used instead of hard-coded values.

    @see
    CC_FIELD_COLOR_DARK, CC_FIELD_COLOR_LIGHT

    @return `1` if field is in given color, `0` otherwise.
*/
#define CC_IS_FIELD_COLOR(i,j,fc) ( ((i) + (j)) % 2 == (fc) )

/**
    Macro to check if a given board size is valid, i.e. between board size minimum and maximum values.

    @param board_size A chessboard size, integer.

    @warning
    All arguments are cast to `int`.

    @return `1` if valid board size, `0` otherwise.
*/
// TODO :: check if board_size is even
#define CC_IS_BOARD_SIZE_VALID(board_size)                  \
    ( ( CC_MIN_BOARD_SIZE <= (int)(board_size) ) &&         \
      ( (int)(board_size) <= CC_MAX_BOARD_SIZE ) )

/**
    Macro to check if a given coordinate is on board.

    @param board_size A chessboard size, integer.
    @param coord A coordinate, integer.

    @note
    Does not check if board size is valid.

    @warning
    All arguments are cast to `int`.

    @return `1` if on board, `0` otherwise.
*/
#define CC_IS_COORD_ON_BOARD(board_size,coord)              \
    ( ( CC_MIN_BOARD_COORD <= (int)(coord) ) &&             \
      ( (int)(coord) < (int)(board_size) ) )

/**
    Macro to check if a given coordinate is on board.

    @param board_size A chessboard size, integer.
    @param coord A coordinate, integer.

    @note
    Does check if board size is valid.

    @warning
    All arguments are cast to `int`.

    @return `1` if on board, `0` otherwise.
*/
#define CC_IS_COORD_ON_VALID_BOARD(board_size,coord)        \
    ( ( CC_IS_BOARD_SIZE_VALID( (board_size) ) ) &&         \
      ( CC_MIN_BOARD_COORD <= (int)(coord) ) &&             \
      ( (int)(coord) < (int)(board_size) ) )

/**
    Macro to check if a given position is on board.

    @param board_size A chessboard size, integer.
    @param i File, position along horizontal axis, integer.
    @param j Rank, position along vertical axis, integer.

    @warning
    All arguments are cast to `int`.

    @return `1` if on board, `0` otherwise.
*/
#define CC_IS_POS_ON_BOARD(board_size,i,j)                  \
    ( ( CC_IS_BOARD_SIZE_VALID( (board_size) ) ) &&         \
      ( CC_IS_COORD_ON_BOARD( (board_size), (i) ) ) &&      \
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
    ( ( CC_IS_BOARD_SIZE_VALID( (board_size) ) ) &&         \
      ( ( CC_IS_COORD_ON_BOARD( (board_size), (i) ) ) ||    \
        ( CC_IS_COORD_ON_BOARD( (board_size), (j) ) ) ) )

/**
    Macro to check if a given position is on a light side of a chessboard.

    @param board_size A chessboard size, integer.
    @param rank Rank, position along vertical axis, integer.

    @warning
    All arguments are cast to `int`.

    @return `1` if on a light side, `0` otherwise.
*/
#define CC_IS_FIELD_ON_LIGHT_SIDE(board_size,rank)          \
    ( ( CC_IS_BOARD_SIZE_VALID( (board_size) ) ) &&         \
      ( 0 <= (int)(rank) ) &&                               \
      ( (int)(rank) < ( (int)(board_size) / 2 ) ) )

/**
    Macro to check if a given position is on a dark side of a chessboard.

    @param board_size A chessboard size, integer.
    @param rank Rank, position along vertical axis, integer.

    @warning
    All arguments are cast to `int`.

    @return `1` if on a dark side, `0` otherwise.
*/
#define CC_IS_FIELD_ON_DARK_SIDE(board_size,rank)           \
    ( ( CC_IS_BOARD_SIZE_VALID( (board_size) ) ) &&         \
      ( ( (int)(board_size) / 2 ) <= (int)(rank) ) &&       \
      ( (int)(rank) < (int)(board_size) ) )


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
    *(ptr_ptr) = NULL;                  \
}

#define CC_DEFAULT_ENTITY_STRING "<default>"

/**
    Macro to rewind queue pointer to its first item.

    @param ptr_queue Pointer, queue.

    @warning
    Pointer to queue `ptr_queue` must be valid (non-`NULL` pointer).

    @warning
    Pointer to queue `ptr_queue` must be valid variable, not expression.

    @warning
    Queue struct must have `prev` member, which points to previous item in that queue.

    @return Nothing.
*/
#define CC_REWIND(ptr_queue) { while ( (ptr_queue)->prev ) (ptr_queue) = (ptr_queue)->prev; }

/**
    Macro to fast-forward list pointer to its last item.

    @param ptr_lst Pointer, list.

    @warning
    Pointer to list `ptr_lst` must be valid (non-`NULL` pointer).

    @warning
    Pointer to list `ptr_lst` must be valid variable, not expression.

    @warning
    List struct must have `next` member, which points to next item in that list.

    @return Nothing.
*/
#define CC_FASTFORWARD(ptr_lst) { while ( (ptr_lst)->next ) (ptr_lst) = (ptr_lst)->next; }

/**
    Macro to rewind sequence pointer by one of its members.

    @param ptr_seq Pointer, sequence.
    @param ptr_item Pointer, member to iterate over.

    @warning
    Pointer to sequence `ptr_seq` must be valid (non-`NULL` pointer).

    @warning
    Pointer to sequence `ptr_seq` must be valid variable, not expression.

    @return Nothing.
*/
#define CC_REWIND_BY(ptr_seq,ptr_item) { while ( (ptr_item) ) (ptr_seq) = (ptr_item); }

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
