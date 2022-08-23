// Copyright (c) 2021 Mario Mlačak, mmlacak@gmail.com
// Licensed under GNU GPL v3+ license. See LICENSING, COPYING files for details.

#ifndef __TESTS_H__
#define __TESTS_H__

#include <stdbool.h>

#include "test_msgs.h"


extern char const CROCHESS_TESTS_VERSION[];

int get_integer_from_cli_arg( char const * restrict str,
                              int default_num,
                              char const ** restrict first_io,
                              char const ** restrict end_io );

void test_gcd( int x, int y );
void test_pos_step( int i1, int j1, int i2, int j2 );

char * test_str_append_into( char const * restrict buffer,
                             char * restrict str__io,
                             size_t size_dest__d,
                             char const * restrict str,
                             size_t max_len__d );

bool test_move( char const * restrict an_str,
                CcGame * restrict game__io );

int main(void);


#endif /* __TESTS_H__ */
