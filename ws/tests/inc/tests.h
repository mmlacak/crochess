// Copyright (c) 2021 Mario Mlačak, mmlacak@gmail.com
// Licensed under GNU GPL v3+ license. See LICENSING, COPYING files for details.

#ifndef __TESTS_H__
#define __TESTS_H__

#include <stdbool.h>

#include "test_msgs.h"


extern char const CROCHESS_TESTS_VERSION[];

#ifdef __WITH_LINE_NOISE__
extern char const CROCHESS_TESTS_HISTORY_FILE_NAME[];
#define CROCHESS_TESTS_HISTORY_LENGTH (1000)
#endif // __WITH_LINE_NOISE__


int get_integer_from_cli_arg( char const * restrict str,
                              int default_num,
                              char const ** restrict first_io,
                              char const ** restrict end_io );

bool cc_move_print_all( CcMove * restrict moves );


int main(void);


#endif /* __TESTS_H__ */
