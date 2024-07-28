// Copyright (c) 2022 Mario Mlačak, mmlacak@gmail.com
// Licensed under GNU GPL v3+ license. See LICENSING, COPYING files for details.

#include <stdlib.h>
#include <math.h>
// #include <tgmath.h>

#include "cc_defines.h"
#include "cc_math.h"


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

int cc_count_of_digits( size_t n ) {
    // https://en.cppreference.com/w/c/types/limits
    // 2^64-1 == 18446744073709551615 == SIZE_MAX (C99)

    if ( n < 10llu )
        return 1;
    else if ( n < 100llu )
        return 2;
    else if ( n < 1000llu )
        return 3;
    else if ( n < 10000llu )
        return 4;
    else if ( n < 100000llu )
        return 5;
    else if ( n < 1000000llu )
        return 6;
    else if ( n < 10000000llu )
        return 7;
    else if ( n < 100000000llu )
        return 8;
    else if ( n < 1000000000llu )
        return 9;
    else if ( n < 10000000000llu )
        return 10;
    else if ( n < 100000000000llu )
        return 11;
    else if ( n < 1000000000000llu )
        return 12;
    else if ( n < 10000000000000llu )
        return 13;
    else if ( n < 100000000000000llu )
        return 14;
    else if ( n < 1000000000000000llu )
        return 15;
    else if ( n < 10000000000000000llu )
        return 16;
    else if ( n < 100000000000000000llu )
        return 17;
    else if ( n < 1000000000000000000llu )
        return 18;
    else if ( n < 10000000000000000000llu )
        return 19;
    else
        return 20;
}
