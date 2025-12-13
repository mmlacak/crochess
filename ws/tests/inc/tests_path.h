// Copyright (c) 2025 Mario Mlačak, mmlacak@gmail.com
// Licensed under GNU GPL v3+ license. See LICENSING, COPYING files for details.

#ifndef __TESTS_PATH_H__
#define __TESTS_PATH_H__

#include "cc_game.h"

#include "cc_typed_step.h"
#include "cc_path_ctx.h"


bool test_path_segment( CcSideEffect side_effect,
                        CcPosDesc move_from,
                        CcPosDesc ply_from,
                        CcTypedStep step,
                        char const * setup );

bool test_path_tree( CcPosDesc move_from,
                     char const * setup );

bool test_bishop_simple( char const * setup );

bool tests_path( int test_number );


#endif /* __TESTS_PATH_H__ */
