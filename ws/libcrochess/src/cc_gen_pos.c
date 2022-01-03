// Copyright (c) 2021 Mario Mlačak, mmlacak@gmail.com
// Licensed under GNU GPL v3+ license. See LICENSING, COPYING files for details.

#include "cc_defines.h"
#include "cc_gen_pos.h"

/**
    @file cc_gen_pos.c
    @brief Step generators, arrays.
*/


CcPos const CC_GEN_POS_LIGHT_PAWN_STEPS[ CC_GEN_POS_PAWN_STEPS_SIZE ] =
{
    { -1,  1 },
    {  0,  1 },
    {  1,  1 },

    { CC_INVALID_OFF_BOARD_COORD_MIN, CC_INVALID_OFF_BOARD_COORD_MIN },
};

CcPos const CC_GEN_POS_DARK_PAWN_STEPS[ CC_GEN_POS_PAWN_STEPS_SIZE ] =
{
    { -1,  -1 },
    {  0,  -1 },
    {  1,  -1 },

    { CC_INVALID_OFF_BOARD_COORD_MIN, CC_INVALID_OFF_BOARD_COORD_MIN },
};

CcPos const CC_GEN_POS_BISHOP_STEPS[ CC_GEN_POS_BISHOP_STEPS_SIZE ] =
{
    {  1,  1 },
    { -1,  1 },
    { -1, -1 },
    {  1, -1 },

    { CC_INVALID_OFF_BOARD_COORD_MIN, CC_INVALID_OFF_BOARD_COORD_MIN },
};

CcPos const CC_GEN_POS_ROOK_STEPS[ CC_GEN_POS_ROOK_STEPS_SIZE ] =
{
    {  1,  0 },
    {  0,  1 },
    { -1,  0 },
    {  0, -1 },

    { CC_INVALID_OFF_BOARD_COORD_MIN, CC_INVALID_OFF_BOARD_COORD_MIN },
};

CcPos const CC_GEN_POS_QUEEN_STEPS[ CC_GEN_POS_QUEEN_STEPS_SIZE ] =
{
    {  1,  0 },
    {  1,  1 },
    {  0,  1 },
    { -1,  1 },
    { -1,  0 },
    { -1, -1 },
    {  0, -1 },
    {  1, -1 },

    { CC_INVALID_OFF_BOARD_COORD_MIN, CC_INVALID_OFF_BOARD_COORD_MIN },
};

CcPos const CC_GEN_POS_KNIGHT_STEPS[ CC_GEN_POS_KNIGHT_STEPS_SIZE ] =
{
    {  2,  1 },
    {  1,  2 },

    { -1,  2 },
    { -2,  1 },

    { -2, -1 },
    { -1, -2 },

    {  1, -2 },
    {  2, -1 },

    { CC_INVALID_OFF_BOARD_COORD_MIN, CC_INVALID_OFF_BOARD_COORD_MIN },
};

CcPos const CC_GEN_POS_UNICORN_STEPS[ CC_GEN_POS_UNICORN_STEPS_SIZE ] =
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

    { CC_INVALID_OFF_BOARD_COORD_MIN, CC_INVALID_OFF_BOARD_COORD_MIN },
};


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

bool cc_gen_pos_is_valid_step( CcPos step,
                               CcPos const array[  ],
                               size_t array_len )
{
    for ( int k = 0; (size_t)k < array_len; ++k )
    {
        CcPos p = array[ k ];

        if ( cc_pos_is_equal( step, p ) )
            return true;
    }

    return false;
}
