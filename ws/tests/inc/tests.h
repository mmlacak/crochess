// Copyright (c) 2021 Mario Mlačak, mmlacak@gmail.com
// Licensed under 3-clause (modified) BSD license. See LICENSE for details.

#ifndef __TESTS_H__
#define __TESTS_H__

#include <stdbool.h>

#include "cc_format_moves.h"

#include "test_msgs.h"


extern char const CROCHESS_TESTS_VERSION[];


TestMsg * test();

bool get_print_chessboard_from_cli_arg();
bool get_print_move_from_cli_arg();
CcFormatMove get_format_move_from_cli_arg();
int get_test_number_from_cli_arg();

bool test_parser( CcChessboard const * const restrict cb,
                  char const * const restrict move_str );

int main(void);


#endif /* __TESTS_H__ */
