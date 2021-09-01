// Copyright (c) 2021 Mario Mlačak, mmlacak@gmail.com
// Licensed under 3-clause (modified) BSD license. See LICENSE for details.

#ifndef __TESTS_PARSE_MOVE_H__
#define __TESTS_PARSE_MOVE_H__

#include <stdbool.h>

#include "cc_game.h"

#include "test_utils.h"


bool test_parser( CcGame const * const restrict gm,
                  char const * const restrict move_str,
                  TestPrints const tp );


bool test_parse_move_single_ply( TestPrints const tp );
// bool test_parse_move_cascading_plies( TestPrints const tp );
// bool test_parse_move_castling( TestPrints const tp );
// bool test_parse_move_tag_and_promotion( TestPrints const tp );
// bool test_parse_move_conversion( TestPrints const tp, bool const is_failed );
// bool test_parse_move_demotion( TestPrints const tp );
// bool test_parse_move_resurrection( TestPrints const tp, bool const is_failed, bool const is_oblationing );

// bool test_parse_move_teleportation( TestPrints const tp, bool const is_failed );
// bool test_parse_move_teleportation_wave( TestPrints const tp, bool const is_oblationing );
// bool test_parse_move_trance_journey( TestPrints const tp, bool const is_capturing );


#endif /* __TESTS_PARSE_MOVE_H__ */
