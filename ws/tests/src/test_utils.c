// Copyright (c) 2021 Mario Mlačak, mmlacak@gmail.com
// Licensed under 3-clause (modified) BSD license. See LICENSE for details.

#include "test_utils.h"


char const TESTS_MOVE_TEST_SEPARATOR[] = " === === === === === === === === === === === === === === === === === === === === \n";
char const TESTS_MOVE_NOTATION_SEPARATOR[] = " ... ... ... ... ... ... ... ... ... ... ... ... ... ... ... ... ... ... ... ... \n";
char const TESTS_MOVE_CHESSBOARD_SEPARATOR[] = " --- --- --- --- --- --- --- --- --- --- --- --- --- --- --- --- --- --- --- --- \n";
char const TESTS_MOVE_MISC_SEPARATOR[] = " *** *** *** *** *** *** *** *** *** *** *** *** *** *** *** *** *** *** *** *** \n";


TestPrints test_prints( bool do_print_chessboard, bool do_print_move, CcFormatMove format_move )
{
    TestPrints tp = { .do_print_chessboard = do_print_chessboard,
                      .do_print_move = do_print_move,
                      .format_move = format_move };
    return tp;
}
