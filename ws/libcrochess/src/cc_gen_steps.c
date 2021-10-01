// Copyright (c) 2021 Mario Mlačak, mmlacak@gmail.com
// Licensed under GNU GPL v3+ license. See LICENSING, COPYING files for details.

#include "cc_gen_steps.h"


bool cc_gen_step( int * const restrict i_io,
                  int * const restrict j_io,
                  int const step_i,
                  int const step_j,
                  bool const from_or_to )
{
    if ( !i_io ) return false;
    if ( !j_io ) return false;

    if ( from_or_to )
    {
        *i_io += step_i;
        *j_io += step_j;
    }
    else
    {
        *i_io -= step_i;
        *j_io -= step_j;
    }

    return true;
}
