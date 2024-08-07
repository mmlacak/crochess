// Copyright (c) 2021 Mario Mlačak, mmlacak@gmail.com
// Licensed under GNU GPL v3+ license. See LICENSING, COPYING files for details.

#ifndef __CC_DEFINES_H__
#define __CC_DEFINES_H__

#include "stdbool.h"
#include "limits.h"


typedef signed char cc_schar_t;
typedef signed short cc_sshort_t;
typedef signed int cc_sint_t;
typedef signed long cc_slong_t;
typedef signed long long cc_sll_t;

typedef unsigned char cc_uchar_t;
typedef unsigned short cc_ushort_t;
typedef unsigned int cc_uint_t;
typedef unsigned long cc_ulong_t;
typedef unsigned long long cc_ull_t;

#define CC_UNSIGNED_MIN (0)

#define CC_MIN(x,y) ( ( (y) > (x) ) ? (x) : (y) )

#define CC_MAX(x,y) ( ( (x) > (y) ) ? (x) : (y) )

#define CC_SIGN(i) ( ( (i) > 0 ) ? (1) : ( ( (i) < 0 ) ? (-1) : (0) ) )

#define CC_XOR(to_bool_1,to_bool_2) ( ((bool)(to_bool_1)) != ((bool)(to_bool_2)) )

#define CC_FREE(ptr) free( (void *)(ptr) )

#define CC_FREE_AND_NULL(ptr_ptr) {     \
    free( (void *)( *(ptr_ptr) ) );     \
    *(ptr_ptr) = NULL;                  \
}


typedef enum CcMaybeBoolEnum {
    CC_MBE_Void = -1,
    CC_MBE_False = 0,
    CC_MBE_True = 1,
} CcMaybeBoolEnum;

#define CC_BOOL_TO_MAYBE(bool_val) ( (bool_val) ? CC_MBE_True : CC_MBE_False )

#define CC_MAYBE_IS_TRUE(maybe_bool) ( ( (maybe_bool) == CC_MBE_True ) ? true : false )

#define CC_MAYBE_IS_FALSE(maybe_bool) ( ( (maybe_bool) == CC_MBE_False ) ? true : false )

#define CC_MAYBE_IS_VOID(maybe_bool) ( ( (maybe_bool) == CC_MBE_Void ) ? true : false )


#define CC_INVALID_COORD (INT_MIN + 3583) // + number, so that value can't be get by accident, e.g. by simply flipping bits, ...

#define CC_MIN_BOARD_COORD (0)

#define CC_MAX_BOARD_COORD (25)

#define CC_MIN_BOARD_SIZE (8)

#define CC_MAX_BOARD_SIZE (26)

#define CC_FIELD_COLOR_LIGHT (1)

#define CC_FIELD_COLOR_DARK (0)


#define CC_CONVERT_BYTE_INTO_FILE_CHAR(byte_file) ( 'a' + (byte_file) )

#define CC_CONVERT_FILE_CHAR_INTO_NUM(char_file) ( (char_file) - 'a' )

#define CC_CONVERT_RANK_STR_INTO_NUM(char_ptr_rank) ( atoi( (char_ptr_rank) ) - 1 )

#define CC_IS_COORD_VALID(coord) ( (coord) != CC_INVALID_COORD )

#define CC_IS_COORD_2_VALID(i,j) ( ( (i) != CC_INVALID_COORD ) && ( (j) != CC_INVALID_COORD ) )

#define CC_IS_FIELD_LIGHT(i,j) ( ((i) + (j)) % 2 != 0 )

#define CC_IS_FIELD_DARK(i,j) ( ((i) + (j)) % 2 == 0 )

#define CC_IS_FIELD_COLOR(i,j,fc) ( ((i) + (j)) % 2 == (fc) )

#define CC_IS_BOARD_SIZE_VALID(board_size)                  \
    ( ( CC_MIN_BOARD_SIZE <= (int)(board_size) ) &&         \
      ( (int)(board_size) <= CC_MAX_BOARD_SIZE ) &&         \
      ( (int)(board_size) % 2 == 0 ) )

#define CC_IS_COORD_ON_BOARD(board_size,coord)              \
    ( ( CC_MIN_BOARD_COORD <= (int)(coord) ) &&             \
      ( (int)(coord) < (int)(board_size) ) )

#define CC_IS_COORD_ON_VALID_BOARD(board_size,coord)        \
    ( ( CC_IS_BOARD_SIZE_VALID( (board_size) ) ) &&         \
      ( CC_MIN_BOARD_COORD <= (int)(coord) ) &&             \
      ( (int)(coord) < (int)(board_size) ) )

#define CC_IS_POS_ON_BOARD(board_size,i,j)                  \
    ( ( CC_IS_COORD_ON_BOARD( (board_size), (i) ) ) &&      \
      ( CC_IS_COORD_ON_BOARD( (board_size), (j) ) ) )

#define CC_IS_POS_ON_VALID_BOARD(board_size,i,j)            \
    ( ( CC_IS_BOARD_SIZE_VALID( (board_size) ) ) &&         \
      ( CC_IS_COORD_ON_BOARD( (board_size), (i) ) ) &&      \
      ( CC_IS_COORD_ON_BOARD( (board_size), (j) ) ) )

#define CC_IS_ANY_COORD_ON_BOARD(board_size,i,j)            \
    ( ( CC_IS_COORD_ON_BOARD( (board_size), (i) ) ) ||      \
      ( CC_IS_COORD_ON_BOARD( (board_size), (j) ) ) )

#define CC_IS_ANY_COORD_ON_VALID_BOARD(board_size,i,j)      \
    ( ( CC_IS_BOARD_SIZE_VALID( (board_size) ) ) &&         \
      ( ( CC_IS_COORD_ON_BOARD( (board_size), (i) ) ) ||    \
        ( CC_IS_COORD_ON_BOARD( (board_size), (j) ) ) ) )

#define CC_IS_FIELD_ON_LIGHT_SIDE(board_size,rank)          \
    ( ( CC_IS_BOARD_SIZE_VALID( (board_size) ) ) &&         \
      ( 0 <= (int)(rank) ) &&                               \
      ( (int)(rank) < ( (int)(board_size) / 2 ) ) )

#define CC_IS_FIELD_ON_DARK_SIDE(board_size,rank)           \
    ( ( CC_IS_BOARD_SIZE_VALID( (board_size) ) ) &&         \
      ( ( (int)(board_size) / 2 ) <= (int)(rank) ) &&       \
      ( (int)(rank) < (int)(board_size) ) )


#define CC_DEFAULT_VALUE_STRING "?" // <!> Step link and losing tag symbols have max len of 2.

#define CC_REWIND(ptr_var_queue) { while ( (ptr_var_queue)->prev__w ) (ptr_var_queue) = (ptr_var_queue)->prev__w; }

#define CC_FASTFORWARD(ptr_var_lst) { while ( (ptr_var_lst)->next ) (ptr_var_lst) = (ptr_var_lst)->next; }

#define CC_REWIND_BY(ptr_var_seq,ptr_item) { while ( (ptr_item) ) (ptr_var_seq) = (ptr_item); }

// <!> Does not work for array parameters --> converted into pointers!
// e.g. void print_array_param( int ar[] ) --> ar array is now pointer.
// sizeof(ar) --> 8 bytes (pointer)
// sizeof(ar) / sizeof(ar[0]) --> 1 == 8 bytes / 8 bytes (both are pointers)
//
// https://stackoverflow.com/questions/37538/how-do-i-determine-the-size-of-my-array-in-c
#define CC_ARRAY_SIZE(array) ( (size_t)( sizeof(array) / sizeof( (array)[ 0 ] ) ) )

// TODO :: can be removed, no rush
//
// #ifdef __CC_STR_PRINT_INFO__
// #define CC_PRINTF_IF_INFO(fmt,...) printf( fmt __VA_OPT__(,) __VA_ARGS__ )
// #else // __CC_STR_PRINT_INFO__
// #define CC_PRINTF_IF_INFO(fmt,...) 0
// #endif // __CC_STR_PRINT_INFO__
//
// TODO :: can be removed, no rush


#endif /* __CC_DEFINES_H__ */
