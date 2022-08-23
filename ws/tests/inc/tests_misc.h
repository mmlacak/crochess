// Copyright (c) 2022 Mario Mlačak, mmlacak@gmail.com
// Licensed under GNU GPL v3+ license. See LICENSING, COPYING files for details.

#ifndef __TESTS_MISC_H__
#define __TESTS_MISC_H__


void test_gcd( int x, int y );
void test_pos_step( int i1, int j1, int i2, int j2 );

char * test_str_append_into( char const * restrict buffer,
                             char * restrict str__io,
                             size_t size_dest__d,
                             char const * restrict str,
                             size_t max_len__d );


bool tests_gcds();
bool tests_pos_steps();
bool tests_str_append_into();

bool tests_misc( int test_number );


#endif /* __TESTS_MISC_H__ */
