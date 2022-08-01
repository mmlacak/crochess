// Copyright (c) 2022 Mario Mlačak, mmlacak@gmail.com
// Licensed under GNU GPL v3+ license. See LICENSING, COPYING files for details.

#include <stdlib.h>
// #include <math.h>

#include "cc_defines.h"
#include "cc_math.h"


int cc_gcd( int x, int y )
{
    int abs_x = abs( x );
    int abs_y = abs( y );

    int max = CC_MAX( abs_x, abs_y );
    int min = CC_MIN( abs_x, abs_y );

    int rem = 0;

    while ( min > 0 )
    {
        rem = max % min;
        max = min;
        min = rem;
    }

    return max;
}
