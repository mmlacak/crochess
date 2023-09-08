// Copyright (c) 2022 Mario Mlačak, mmlacak@gmail.com
// Licensed under GNU GPL v3+ license. See LICENSING, COPYING files for details.

#include <stdlib.h>
#include <math.h>
// #include <tgmath.h>

#include "cc_defines.h"
#include "cc_math.h"

/**
    @file cc_math.c
    @brief Various math functions.
*/


int cc_gcd( int x, int y ) {
    // https://en.wikipedia.org/wiki/Euclidean_algorithm

    int abs_x = abs( x );
    int abs_y = abs( y );

    int max = CC_MAX( abs_x, abs_y );
    int min = CC_MIN( abs_x, abs_y );

    int rem = 0;

    while ( min > 0 ) {
        rem = max % min;
        max = min;
        min = rem;
    }

    return max;
}

size_t cc_diagonal( size_t size ) {
    size_t s = 2 * size * size;
    long double d = ceill( sqrtl( (long double)s ) );
    return (size_t)d;
}
