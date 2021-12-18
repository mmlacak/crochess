// Copyright (c) 2021 Mario Mlačak, mmlacak@gmail.com
// Licensed under GNU GPL v3+ license. See LICENSING, COPYING files for details.

#include "cc_gen_pos.h"


bool cc_gen_pos( CcPos * restrict pos__io,
                 CcPos step,
                 bool from_or_to )
{
    if ( !pos__io ) return false;

    if ( from_or_to )
        *pos__io = cc_pos_add( *pos__io, step );
    else
        *pos__io = cc_pos_subtract( *pos__io, step );

    return true;
}
