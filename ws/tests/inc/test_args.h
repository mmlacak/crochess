// Copyright (c) 2024 Mario Mlačak, mmlacak@gmail.com
// Licensed under GNU GPL v3+ license. See LICENSING, COPYING files for details.

#ifndef __TEST_ARGS_H__
#define __TEST_ARGS_H__

#include <stdbool.h>

#include "cc_defines.h"


#define TESTS_DO_ALL (0)

typedef struct TestArgs {
    char const * an_str;
    char const * setup__d;
    char const * check_setup__d;
    char const * check_end__d;
    cc_ull_t error_code;
} TestArgs;

#define TEST_OK (0)
#define TEST_FAIL (1)

TestArgs test_args( char const * an_str,
                    char const * setup__d,
                    char const * check_setup__d,
                    char const * check_end__d,
                    cc_ull_t error_code );

#define TEST_ARGS_INVALID    \
    { .an_str = NULL, .setup__d = NULL, .check_setup__d = NULL, .check_end__d = NULL, .error_code = 0 }

#define TEST_ARGS_INVALID_CAST ( (TestArgs)TEST_ARGS_INVALID )

#define TEST_ARGS(an,setup,check_setup,check_end,error)    \
    { .an_str = an, .setup__d = setup, .check_setup__d = check_setup, .check_end__d = check_end, .error_code = error }

extern TestArgs const TEST_ARGS_ARRAY[ ];
extern size_t const TEST_ARGS_ARRAY_SIZE;

bool test_args_are_equal( TestArgs tma_1, TestArgs tma_2 );

bool test_args_are_invalid( TestArgs tma );

bool test_args_iter( TestArgs ** tma__iod );

TestArgs * test_args_fetch( size_t index );


#endif /* __TEST_ARGS_H__ */
