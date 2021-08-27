// Copyright (c) 2021 Mario Mlačak, mmlacak@gmail.com
// Licensed under 3-clause (modified) BSD license. See LICENSE for details.

#ifndef __TESTS_DO_MOVE_H__
#define __TESTS_DO_MOVE_H__

#include <stdbool.h>

#include "test_utils.h"


bool test_do_move_single_ply( TestPrints const tp );
bool test_do_move_cascading_plies( TestPrints const tp );
bool test_do_move_castling( TestPrints const tp );
bool test_do_move_tag_and_promotion( TestPrints const tp );
bool test_do_move_conversion( TestPrints const tp, bool const is_failed );
bool test_do_move_demotion( TestPrints const tp );
bool test_do_move_resurrection( TestPrints const tp, bool const is_failed, bool const is_oblationing );

bool test_do_move_teleportation( TestPrints const tp, bool const is_failed );
bool test_do_move_teleportation_wave( TestPrints const tp, bool const is_oblationing );
bool test_do_move_trance_journey( TestPrints const tp, bool const is_capturing );


#endif /* __TESTS_DO_MOVE_H__ */
