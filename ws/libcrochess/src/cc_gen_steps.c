// Copyright (c) 2021 Mario Mlačak, mmlacak@gmail.com
// Licensed under GNU GPL v3+ license. See LICENSING, COPYING files for details.

#include "cc_gen_steps.h"


int const CC_GEN_STEP_BISHOP[ CC_GEN_STEP_BISHOP_LEN ][ CC_GEN_STEP_COORDS_LEN ] =
{
    {  1,  1 },
    { -1,  1 },
    { -1, -1 },
    {  1, -1 }
};

int const CC_GEN_STEP_ROOK[ CC_GEN_STEP_ROOK_LEN ][ CC_GEN_STEP_COORDS_LEN ] =
{
    {  1,  0 },
    {  0,  1 },
    { -1,  0 },
    {  0, -1 }
};

int const CC_GEN_STEP_QUEEN[ CC_GEN_STEP_QUEEN_LEN ][ CC_GEN_STEP_COORDS_LEN ] =
{
    {  1,  0 },
    {  1,  1 },
    {  0,  1 },
    { -1,  1 },
    { -1,  0 },
    { -1, -1 },
    {  0, -1 },
    {  1, -1 }
};

int const CC_GEN_STEP_KNIGHT[ CC_GEN_STEP_KNIGHT_LEN ][ CC_GEN_STEP_COORDS_LEN ] =
{
    {  2,  1 },
    {  1,  2 },

    { -1,  2 },
    { -2,  1 },

    { -2, -1 },
    { -1, -2 },

    {  1, -2 },
    {  2, -1 }
};

int const CC_GEN_STEP_UNICORN[ CC_GEN_STEP_UNICORN_LEN ][ CC_GEN_STEP_COORDS_LEN ] =
{
    {  4,  1 },
    {  3,  2 },
    {  2,  3 },
    {  1,  4 },

    { -1,  4 },
    { -2,  3 },
    { -3,  2 },
    { -4,  1 },

    { -4, -1 },
    { -3, -2 },
    { -2, -3 },
    { -1, -4 },

    {  1, -4 },
    {  2, -3 },
    {  3, -2 },
    {  4, -1 },
};


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