// Copyright (c) 2021 Mario Mlačak, mmlacak@gmail.com
// Licensed under GNU GPL v3+ license. See LICENSING, COPYING files for details.

#include "cc_gen_pos.h"


bool cc_gen_pos( CcPos * const restrict pos_io,
                 CcPos const step,
                 bool const from_or_to )
{
    if ( !pos_io ) return false;

    if ( from_or_to )
        *pos_io = cc_pos_add( *pos_io, step );
    else
        *pos_io = cc_pos_subtract( *pos_io, step );

    return true;
}
