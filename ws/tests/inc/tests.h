// Copyright (c) 2021 Mario Mlačak, mmlacak@gmail.com
// Licensed under GNU GPL v3+ license. See LICENSING, COPYING files for details.

#ifndef __TESTS_H__
#define __TESTS_H__

#include <stdbool.h>

#include "test_msgs.h"


extern char const CROCHESS_TESTS_VERSION[];


void test_gcd( int x, int y );
void test_pos_step( int i1, int j1, int i2, int j2 );

int main(void);


#endif /* __TESTS_H__ */
