// Copyright (c) 2021 Mario Mlačak, mmlacak@gmail.com
// Licensed under GNU GPL v3+ license. See LICENSING, COPYING files for details.

#ifndef __TESTS_DO_MOVE_H__
#define __TESTS_DO_MOVE_H__

#include <stdbool.h>

#include "test_utils.h"


bool test_do_move_single_ply( int index, TestPrints tp );
bool test_do_move_cascading_plies( int index, TestPrints tp );
bool test_do_move_castling( int index, TestPrints tp );
bool test_do_move_tag_and_promotion( int index, TestPrints tp );
bool test_do_move_conversion( int index, TestPrints tp, bool is_failed );
bool test_do_move_demotion( int index, TestPrints tp );
bool test_do_move_resurrection( int index,
                                TestPrints tp,
                                bool is_failed,
                                bool is_oblationing );

bool test_do_move_teleportation( int index, TestPrints tp, bool is_failed );
bool test_do_move_teleportation_wave( int index, TestPrints tp, bool is_oblationing );
bool test_do_move_trance_journey( int index, TestPrints tp, bool is_capturing );


#endif /* __TESTS_DO_MOVE_H__ */
