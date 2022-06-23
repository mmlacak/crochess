// Copyright (c) 2021, 2022 Mario Mlačak, mmlacak@gmail.com
// Licensed under GNU GPL v3+ license. See LICENSING, COPYING files for details.

// #include "cc_parse_move.h"
#include "cc_rules.h"


bool cc_make_move( char const * restrict move_an_str,
                   CcGame ** restrict game__io,
                   CcParseMsg ** restrict parse_msgs__io )
{
    if ( !move_an_str ) return false;

    if ( !game__io ) return false;
    if ( !*game__io ) return false;

    if ( !(*game__io)->chessboard ) return false;
    if ( !(*game__io)->moves ) return false;

    if ( !parse_msgs__io ) return false;
    // if ( !*parse_msgs__io ) return false;

    // TODO

    return false;
}
