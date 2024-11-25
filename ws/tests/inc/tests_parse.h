// Copyright (c) 2024 Mario Mlačak, mmlacak@gmail.com
// Licensed under GNU GPL v3+ license. See LICENSING, COPYING files for details.

#ifndef __TESTS_PARSE_H__
#define __TESTS_PARSE_H__

#include <stdbool.h>

#include "cc_game.h"


bool test_parse( char const * an_str,
                 char const * setup__d,
                 char const * check_setup__d,
                 char const * check_end__d,
                 CcGame ** game__iodr );

bool tests_parse( int test_number );

bool tests_skip_disambiguation( int test_number );

bool tests_next_ply_link( int test_number );


#endif /* __TESTS_PARSE_H__ */
