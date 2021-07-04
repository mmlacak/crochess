// Copyright (c) 2021 Mario Mlačak, mmlacak@gmail.com
// Licensed under 3-clause (modified) BSD license. See LICENSE for details.

#ifndef __TESTS_DO_MOVE_H__
#define __TESTS_DO_MOVE_H__

#include <stdbool.h>

#include "cc_format_moves.h"

typedef struct TestPrints
{
    bool do_print_chessboard;
    bool do_print_move;
    CcFormatMove format_move;
} TestPrints;

TestPrints test_prints( bool do_print_chessboard, bool do_print_move, CcFormatMove format_move );


bool test_do_move_single_ply( TestPrints tp );
bool test_do_move_cascading_plies( TestPrints tp );
bool test_do_move_castling( TestPrints tp );
bool test_do_move_tag_and_promotion( TestPrints tp );
bool test_do_move_conversion( TestPrints tp, bool is_failed );
bool test_do_move_demotion( TestPrints tp );
bool test_do_move_resurrection( TestPrints tp, bool is_failed, bool is_oblationing );

bool test_do_move_teleportation( TestPrints tp, bool is_failed );
bool test_do_move_teleportation_wave( TestPrints tp, bool is_oblationing );
bool test_do_move_trance_journey( TestPrints tp, bool is_capturing );


#endif /* __TESTS_DO_MOVE_H__ */
