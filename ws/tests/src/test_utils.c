// Copyright (c) 2021 Mario Mlačak, mmlacak@gmail.com
// Licensed under 3-clause (modified) BSD license. See LICENSE for details.

#include "test_utils.h"


TestPrints test_prints( bool do_print_chessboard, bool do_print_move, CcFormatMove format_move )
{
    TestPrints tp = { .do_print_chessboard = do_print_chessboard,
                      .do_print_move = do_print_move,
                      .format_move = format_move };
    return tp;
}
