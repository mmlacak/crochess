// Copyright (c) 2021 Mario Mlačak, mmlacak@gmail.com
// Licensed under GNU GPL v3+ license. See LICENSE, COPYING files for details.

#ifndef __TEST_UTILS_H__
#define __TEST_UTILS_H__

#include "cc_chessboard.h"
#include "cc_move.h"

#include "cc_format_moves.h"

extern char const TESTS_MOVE_TEST_SEPARATOR[];
extern char const TESTS_MOVE_NOTATION_SEPARATOR[];
extern char const TESTS_MOVE_CHESSBOARD_SEPARATOR[];
extern char const TESTS_MOVE_MISC_SEPARATOR[];


typedef struct TestPrints
{
    bool do_print_chessboard;
    bool do_print_move;
    CcFormatMove format_move;
} TestPrints;

TestPrints test_prints( bool do_print_chessboard, bool do_print_move, CcFormatMove format_move );


bool test_duplicates( CcMove const * const restrict moves );


#endif /* __TEST_UTILS_H__ */
