// Copyright (c) 2021 Mario Mlačak, mmlacak@gmail.com
// Licensed under GNU GPL v3+ license. See LICENSING, COPYING files for details.

#ifndef __TESTS_PARSE_MOVE_H__
#define __TESTS_PARSE_MOVE_H__

#include <stdbool.h>

#include "cc_game.h"

#include "test_utils.h"


bool test_parser( CcGame * restrict gm,
                  char const * restrict move_str,
                  TestPrints tp );


bool test_parse_move_single_ply( TestPrints tp );
// bool test_parse_move_cascading_plies( TestPrints tp );
// bool test_parse_move_castling( TestPrints tp );
// bool test_parse_move_tag_and_promotion( TestPrints tp );
// bool test_parse_move_conversion( TestPrints tp, bool is_failed );
// bool test_parse_move_demotion( TestPrints tp );
// bool test_parse_move_resurrection( TestPrints tp, bool is_failed, bool is_oblationing );

// bool test_parse_move_teleportation( TestPrints tp, bool is_failed );
// bool test_parse_move_teleportation_wave( TestPrints tp, bool is_oblationing );
// bool test_parse_move_trance_journey( TestPrints tp, bool is_capturing );


#endif /* __TESTS_PARSE_MOVE_H__ */
