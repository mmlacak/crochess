// Copyright (c) 2024 Mario Mlačak, mmlacak@gmail.com
// Licensed under GNU GPL v3+ license. See LICENSING, COPYING files for details.

#include "cc_parse_utils.h"
#include "cc_checks.h"

#include "cc_do_bishop.h"


bool cc_do_bishop( char const * path_start_an,
                   char const * ply_end_an,
                   CcGame * game,
                   CcPosDesc * before_ply__io,
                   CcChessboard ** cb__io,
                   CcParseMsg ** parse_msgs__iod ) {
    if ( !path_start_an ) return false;
    if ( !ply_end_an ) return false;
    if ( !game ) return false;
    if ( !before_ply__io ) return false;
    if ( !cb__io || !*cb__io ) return false;
    if ( !parse_msgs__iod ) return false;



    return false;
}
