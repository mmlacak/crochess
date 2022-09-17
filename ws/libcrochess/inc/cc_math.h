// Copyright (c) 2022 Mario Mlačak, mmlacak@gmail.com
// Licensed under GNU GPL v3+ license. See LICENSING, COPYING files for details.

#ifndef __CC_MATH_H__
#define __CC_MATH_H__

/**
    @file cc_math.h
    @brief Various math functions.
*/


/**
    Function returns greatest common divisor between two given integers.

    @param x An integer.
    @param y Another integer.

    @return Greatest common divisor.
*/
int cc_gcd( int x, int y );

/**
    Function returns length of a diagonal.

    @param size Size of a chessboard, an integer.

    @return Diagonal of a chessboard, rounded up.
*/
size_t cc_diagonal( size_t size );


#endif /* __CC_MATH_H__ */
