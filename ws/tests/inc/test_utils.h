// Copyright (c) 2021 Mario Mlačak, mmlacak@gmail.com
// Licensed under 3-clause (modified) BSD license. See LICENSE for details.

#ifndef __TEST_UTILS_H__
#define __TEST_UTILS_H__

#include "cc_chessboard.h"
#include "cc_move.h"

#include "cc_format_moves.h"


typedef struct TestPrints
{
    bool do_print_chessboard;
    bool do_print_move;
    CcFormatMove format_move;
} TestPrints;

TestPrints test_prints( bool do_print_chessboard, bool do_print_move, CcFormatMove format_move );


#endif /* __TEST_UTILS_H__ */
