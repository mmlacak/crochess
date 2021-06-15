// Copyright (c) 2021 Mario Mlačak, mmlacak@gmail.com
// Licensed under 3-clause (modified) BSD license. See LICENSE for details.

#ifndef __TESTS_DO_MOVE_H__
#define __TESTS_DO_MOVE_H__

#include <stdbool.h>


bool test_do_move_single_ply( bool do_print );
bool test_do_move_cascading_plies( bool do_print );
bool test_do_move_castling( bool do_print );
bool test_do_move_tag_and_promotion( bool do_print );
bool test_do_move_conversion( bool do_print, bool is_failed );
bool test_do_move_demotion( bool do_print );
bool test_do_move_resurrection( bool do_print, bool is_failed, bool is_oblationing );

bool test_do_move_teleportation( bool do_print, bool is_failed );
bool test_do_move_teleportation_wave( bool do_print, bool is_oblationing );
bool test_do_move_trance_journey( bool do_print, bool is_capturing );


#endif /* __TESTS_DO_MOVE_H__ */
