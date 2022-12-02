// Copyright (c) 2022 Mario Mlačak, mmlacak@gmail.com
// Licensed under GNU GPL v3+ license. See LICENSING, COPYING files for details.

#include "cc_parse_step.h"


bool cc_parse_steps( char const * restrict ply_start_an,
                     char const * restrict ply_end_an,
                     CcGame * restrict game,
                     CcPos * restrict last_destination__iod,
                     CcChessboard ** restrict cb__io,
                     CcStep ** restrict steps__io,
                     CcParseMsg ** restrict parse_msgs__iod )
{
    if ( !ply_start_an ) return false;
    if ( !ply_end_an ) return false;
    if ( !game ) return false;
    if ( !cb__io || !*cb__io ) return false;
    if ( !steps__io || !*steps__io ) return false;
    if ( !parse_msgs__iod ) return false;


    return true;
}
