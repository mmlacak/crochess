// Copyright (c) 2022 Mario Mlačak, mmlacak@gmail.com
// Licensed under GNU GPL v3+ license. See LICENSING, COPYING files for details.

#ifndef __TESTS_MOVE_H__
#define __TESTS_MOVE_H__


bool test_move( char const * restrict an_str,
                char const * restrict start_setup__d,
                char const * restrict end_setup__d,
                CcGame ** restrict game__iodr );


bool tests_move( int test_number );


#endif /* __TESTS_MOVE_H__ */
