// Copyright (c) 2021 Mario Mlačak, mmlacak@gmail.com
// Licensed under 3-clause (modified) BSD license. See LICENSE for details.

#ifndef __CC_TEST_UTILS_H__
#define __CC_TEST_UTILS_H__

#include "cc_chessboard.h"
#include "cc_move.h"


bool cc_test_util_free_all( CcChessboard ** const cb_f,
                            CcMove ** const moves_f,
                            CcPly ** const plies_f,
                            CcStep ** const steps_f,
                            bool cumulative_result );


#endif /* __CC_TEST_UTILS_H__ */
