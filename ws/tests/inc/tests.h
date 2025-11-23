// Copyright (c) 2021 Mario Mlačak, mmlacak@gmail.com
// Licensed under GNU GPL v3+ license. See LICENSING, COPYING files for details.

#ifndef __TESTS_H__
#define __TESTS_H__

#include <stdbool.h>

#include "cc_game.h"

#include "test_msgs.h"


extern char const CROCHESS_TESTS_VERSION[];

#ifdef __WITH_LINE_NOISE__
extern char const CROCHESS_TESTS_HISTORY_FILE_NAME[];
#define CROCHESS_TESTS_HISTORY_LENGTH (1000)
#endif // __WITH_LINE_NOISE__


int get_integer_from_cli_arg( char const * str,
                              int default_num,
                              char const ** first_io,
                              char const ** end_io );

bool print_all_moves( CcMove * moves, bool is_score );

char const * get_game_status_label( CcGameStatusEnum gse );

char const * test_error();


int main(void);


#endif /* __TESTS_H__ */
