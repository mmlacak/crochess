// Copyright (c) 2021 Mario Mlačak, mmlacak@gmail.com
// Licensed under GNU GPL v3+ license. See LICENSE, COPYING files for details.

#ifndef __TESTS_DO_MOVE_H__
#define __TESTS_DO_MOVE_H__

#include <stdbool.h>

#include "test_utils.h"


bool test_do_move_single_ply( int const index, TestPrints const tp );
bool test_do_move_cascading_plies( int const index, TestPrints const tp );
bool test_do_move_castling( int const index, TestPrints const tp );
bool test_do_move_tag_and_promotion( int const index, TestPrints const tp );
bool test_do_move_conversion( int const index, TestPrints const tp, bool const is_failed );
bool test_do_move_demotion( int const index, TestPrints const tp );
bool test_do_move_resurrection( int const index,
                                TestPrints const tp,
                                bool const is_failed,
                                bool const is_oblationing );

bool test_do_move_teleportation( int const index, TestPrints const tp, bool const is_failed );
bool test_do_move_teleportation_wave( int const index, TestPrints const tp, bool const is_oblationing );
bool test_do_move_trance_journey( int const index, TestPrints const tp, bool const is_capturing );


#endif /* __TESTS_DO_MOVE_H__ */
