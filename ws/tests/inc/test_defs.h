// Copyright (c) 2024 Mario Mlačak, mmlacak@gmail.com
// Licensed under GNU GPL v3+ license. See LICENSING, COPYING files for details.

#ifndef __TEST_DEFS_H__
#define __TEST_DEFS_H__

// #include "cc_game.h"
//
//
// bool test_move( char const * an_str,
//                 char const * setup__d,
//                 char const * check_setup__d,
//                 char const * check_end__d,
//                 CcGame ** game__iodr );
//
//
// bool tests_move( int test_number );


typedef struct TestMoveArgs {
    char const * an_str;
    char const * setup__d;
    char const * check_setup__d;
    char const * check_end__d;
    size_t error_code;
} TestMoveArgs;

#define TEST_OK (0)
#define TEST_FAIL (1)

TestMoveArgs test_move_args( char const * an_str,
                             char const * setup__d,
                             char const * check_setup__d,
                             char const * check_end__d,
                             size_t error_code );

#define TEST_MOVE_ARGS_INVALID    \
    { .an_str = NULL, .setup__d = NULL, .check_setup__d = NULL, .check_end__d = NULL, .error_code = 0 }

#define TEST_MOVE_ARGS(an,setup,check_setup,check_end,error)    \
    { .an_str = an, .setup__d = setup, .check_setup__d = check_setup, .check_end__d = check_end, .error_code = error }

extern TestMoveArgs const TEST_MOVE_ARGS_ARRAY[ ];


#endif /* __TEST_DEFS_H__ */
