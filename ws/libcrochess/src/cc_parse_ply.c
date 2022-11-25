// Copyright (c) 2022 Mario Mlačak, mmlacak@gmail.com
// Licensed under GNU GPL v3+ license. See LICENSING, COPYING files for details.

#include "cc_parse_ply.h"


bool cc_parse_plies( char const * restrict plies_an,
                     CcGame * restrict game,
                     CcPly ** restrict plies__o,
                     CcParseMsg ** restrict parse_msgs__io )
{
    if ( !plies_an ) return false;
    if ( !game ) return false;
    if ( !plies__o ) return false;
    if ( *plies__o ) return false;
    if ( !parse_msgs__io ) return false;



    return true;
}
