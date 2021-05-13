// Copyright (c) 2021 Mario Mlačak, mmlacak@gmail.com
// Licensed under 3-clause (modified) BSD license. See LICENSE for details.

#ifndef __TEST_H__
#define __TEST_H__

#include <stdbool.h>


bool tst_single_ply( bool do_print );
bool tst_cascading_plies( bool do_print );
bool tst_castling( bool do_print );
bool tst_tag_and_promotion( bool do_print );
bool tst_conversion( bool do_print, bool is_failed );
bool tst_demotion( bool do_print );
bool tst_resurrection( bool do_print, bool is_failed, bool is_oblationing );

bool tst_teleportation( bool do_print, bool is_failed );
bool tst_teleportation_wave( bool do_print, bool is_oblationing );
bool tst_trance_journey( bool do_print, bool is_capturing );


#endif /* __TEST_H__ */
