// Copyright (c) 2021 Mario Mlačak, mmlacak@gmail.com
// Licensed under 3-clause (modified) BSD license. See LICENSE for details.

#include "cc_test_utils.h"


bool cc_test_util_free_all( CcChessboard ** const cb_f,
                            CcMove ** const moves_f,
                            CcPly ** const plies_f,
                            CcStep ** const steps_f,
                            bool cumulative_result )
{
    bool results = true;

    if ( cb_f ) results = cc_chessboard_free_all( cb_f ) && results;

    if ( moves_f ) results = cc_move_free_all_moves( moves_f ) && results;

    if ( plies_f ) results = cc_ply_free_all_plies( plies_f ) && results;

    if ( steps_f ) results = cc_step_free_all_steps( steps_f ) && results;

    return ( cumulative_result && results );
}
