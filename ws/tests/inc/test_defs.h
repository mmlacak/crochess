// Copyright (c) 2024 Mario Mlačak, mmlacak@gmail.com
// Licensed under GNU GPL v3+ license. See LICENSING, COPYING files for details.

#ifndef __TEST_DEFS_H__
#define __TEST_DEFS_H__

#include <stdbool.h>

#include "cc_defines.h"


#define TEST_ALL_MOVES (-1)

typedef struct TestMoveArgs {
    char const * an_str;
    char const * setup__d;
    char const * check_setup__d;
    char const * check_end__d;
    ull error_code;
} TestMoveArgs;

#define TEST_OK (0)
#define TEST_FAIL (1)

TestMoveArgs test_move_args( char const * an_str,
                             char const * setup__d,
                             char const * check_setup__d,
                             char const * check_end__d,
                             ull error_code );

#define TEST_MOVE_ARGS_INVALID    \
    { .an_str = NULL, .setup__d = NULL, .check_setup__d = NULL, .check_end__d = NULL, .error_code = 0 }

#define TEST_MOVE_ARGS_INVALID_CAST ( (TestMoveArgs)TEST_MOVE_ARGS_INVALID )

#define TEST_MOVE_ARGS(an,setup,check_setup,check_end,error)    \
    { .an_str = an, .setup__d = setup, .check_setup__d = check_setup, .check_end__d = check_end, .error_code = error }

extern TestMoveArgs const TEST_MOVE_ARGS_ARRAY[ ];
extern size_t const TEST_MOVE_ARGS_ARRAY_SIZE;

bool test_move_args_are_equal( TestMoveArgs tma_1, TestMoveArgs tma_2 );

bool test_move_args_are_invalid( TestMoveArgs tma );

bool test_move_args_iter( TestMoveArgs ** tma__iod );

TestMoveArgs * test_move_args_fetch( size_t index );


#endif /* __TEST_DEFS_H__ */
