// Copyright (c) 2021 Mario Mlačak, mmlacak@gmail.com
// Licensed under GNU GPL v3+ license. See LICENSING, COPYING files for details.

#ifndef __TESTS_H__
#define __TESTS_H__

#include <stdbool.h>

#include "cc_format_moves.h"

#include "test_msgs.h"


extern char CROCHESS_TESTS_VERSION[];


TestMsg * test();

bool get_print_chessboard_from_cli_arg( char const * restrict str,
                                        char const ** restrict first_io,
                                        char const ** restrict end_io );

bool get_print_move_from_cli_arg( char const * restrict str,
                                  char const ** restrict first_io,
                                  char const ** restrict end_io );

CcFormatMove get_format_move_from_cli_arg( char const * restrict str,
                                           char const ** restrict first_io,
                                           char const ** restrict end_io );

int get_integer_from_cli_arg( char const * restrict str,
                              int default_num,
                              char const ** restrict first_io,
                              char const ** restrict end_io );

int main(void);


#endif /* __TESTS_H__ */
