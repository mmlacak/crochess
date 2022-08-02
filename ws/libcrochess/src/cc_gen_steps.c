// Copyright (c) 2021, 2022 Mario Mlačak, mmlacak@gmail.com
// Licensed under GNU GPL v3+ license. See LICENSING, COPYING files for details.

#include "cc_gen_steps.h"

/**
    @file cc_gen_steps.c
    @brief Step generators, arrays.
*/


CcPos const CC_STEPS_LIGHT_PAWN[ CC_STEPS_PAWN_SIZE ] =
{
    { .i = -1, .j =  1 },
    { .i =  0, .j =  1 },
    { .i =  1, .j =  1 },

    CC_POS_INVALID,
};

CcPos const CC_STEPS_DARK_PAWN[ CC_STEPS_PAWN_SIZE ] =
{
    { .i = -1, .j = -1 },
    { .i =  0, .j = -1 },
    { .i =  1, .j = -1 },

    CC_POS_INVALID,
};

CcPos const CC_STEPS_LIGHT_SIDEWAYS_PAWN[ CC_STEPS_SIDEWAYS_PAWN_SIZE ] =
{
    { .i = -1, .j =  1 },
    { .i =  0, .j =  1 },
    { .i =  1, .j =  1 },
    { .i = -1, .j =  0 },
    { .i =  1, .j =  0 },

    CC_POS_INVALID,
};

CcPos const CC_STEPS_DARK_SIDEWAYS_PAWN[ CC_STEPS_SIDEWAYS_PAWN_SIZE ] =
{
    { .i = -1, .j = -1 },
    { .i =  0, .j = -1 },
    { .i =  1, .j = -1 },
    { .i = -1, .j =  0 },
    { .i =  1, .j =  0 },

    CC_POS_INVALID,
};

CcPos const CC_STEPS_BISHOP[ CC_STEPS_BISHOP_SIZE ] =
{
    { .i =  1, .j =  1 },
    { .i = -1, .j =  1 },
    { .i = -1, .j = -1 },
    { .i =  1, .j = -1 },

    CC_POS_INVALID,
};

CcPos const CC_STEPS_ROOK[ CC_STEPS_ROOK_SIZE ] =
{
    { .i =  1, .j =  0 },
    { .i =  0, .j =  1 },
    { .i = -1, .j =  0 },
    { .i =  0, .j = -1 },

    CC_POS_INVALID,
};

CcPos const CC_STEPS_QUEEN[ CC_STEPS_QUEEN_SIZE ] =
{
    { .i =  1, .j =  0 },
    { .i =  1, .j =  1 },
    { .i =  0, .j =  1 },
    { .i = -1, .j =  1 },
    { .i = -1, .j =  0 },
    { .i = -1, .j = -1 },
    { .i =  0, .j = -1 },
    { .i =  1, .j = -1 },

    CC_POS_INVALID,
};

CcPos const CC_STEPS_KNIGHT[ CC_STEPS_KNIGHT_SIZE ] =
{
    { .i =  2, .j =  1 },
    { .i =  1, .j =  2 },

    { .i = -1, .j =  2 },
    { .i = -2, .j =  1 },

    { .i = -2, .j = -1 },
    { .i = -1, .j = -2 },

    { .i =  1, .j = -2 },
    { .i =  2, .j = -1 },

    CC_POS_INVALID,
};

CcPos const CC_STEPS_UNICORN[ CC_STEPS_UNICORN_SIZE ] =
{
    { .i =  4, .j =  1 },
    { .i =  3, .j =  2 },
    { .i =  2, .j =  3 },
    { .i =  1, .j =  4 },

    { .i = -1, .j =  4 },
    { .i = -2, .j =  3 },
    { .i = -3, .j =  2 },
    { .i = -4, .j =  1 },

    { .i = -4, .j = -1 },
    { .i = -3, .j = -2 },
    { .i = -2, .j = -3 },
    { .i = -1, .j = -4 },

    { .i =  1, .j = -4 },
    { .i =  2, .j = -3 },
    { .i =  3, .j = -2 },
    { .i =  4, .j = -1 },

    CC_POS_INVALID,
};

CcPos const CC_STEPS_SERPENT_LEFT[ CC_STEPS_SERPENT_SIZE ] =
{
    { .i = -1, .j =  1 },
    { .i =  1, .j = -1 },

    CC_POS_INVALID,
};

CcPos const CC_STEPS_SERPENT_RIGHT[ CC_STEPS_SERPENT_SIZE ] =
{
    { .i = -1, .j = -1 },
    { .i =  1, .j =  1 },

    CC_POS_INVALID,
};

bool cc_step_is_valid( CcPos step,
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


bool cc_piece_pos_iter( CcChessboard * restrict cb_before_activation,
                        CcPieceEnum piece,
                        bool include_opponent,
                        CcPos * restrict pos__io )
{
    if ( !cb_before_activation ) return false;
    if ( !pos__io ) return false;

    int size = (int)cb_before_activation->size;
    CcPos pos = *pos__io;

    // Next position to check.
    if ( !cc_chessboard_is_pos_on_board( cb_before_activation, pos.i, pos.j ) )
        pos = cc_pos( 0, 0 );
    else if ( pos.j < size - 1 )
        pos = cc_pos( pos.i, pos.j + 1 );
    else
        pos = cc_pos( pos.i + 1, 0 );

    for ( int i = pos.i; i < size; ++i )
    {
        for ( int j = pos.j; j < size; ++j )
        {
            CcPieceEnum pe = cc_chessboard_get_piece( cb_before_activation, i, j );

            if ( CC_PIECE_IS_THE_SAME( pe, piece ) ||
                 ( include_opponent && cc_piece_is_opposite( pe, piece ) ) )
            {
                *pos__io = cc_pos( i, j );
                return true;
            }
        }

        pos.j = 0;
    }

    *pos__io = CC_POS_INVALID_CAST;
    return false;
}

bool cc_check_path_args( CcChessboard * restrict cb_before_activation,
                         CcPieceEnum piece,
                         CcPos start,
                         CcPos destination )
{
    if ( !cb_before_activation ) return false;

    if ( !cc_chessboard_is_pos_on_board( cb_before_activation, start.i, start.j ) )
        return false;

    if ( !cc_chessboard_is_pos_on_board( cb_before_activation,
                                         destination.i,
                                         destination.j ) )
        return false;

    CcPieceEnum pe = cc_chessboard_get_piece( cb_before_activation, start.i, start.j );

    if ( !CC_PIECE_IS_THE_SAME( piece, pe ) )
        return false;

    return true;
}

CcPosLink * cc_path_bishop__new( CcChessboard * restrict cb_before_activation,
                                 CcPieceEnum piece,
                                 CcPos start,
                                 CcPos destination )
{
    if ( !cc_check_path_args( cb_before_activation, piece, start, destination ) )
        return NULL;

    if ( !CC_PIECE_IS_BISHOP( piece ) )
        return NULL;

    CcPos step = cc_pos_step( start, destination );

    if ( !CC_BISHOP_STEP_IS_VALID( step ) )
        return NULL;

    CcPosLink * path__a = cc_pos_link__new( start );

    for ( CcPos pos = cc_pos_add( start, step );
            !cc_pos_is_equal( pos, destination );
            cc_pos_add( pos, step ) )
    {
        cc_pos_link_append( path__a, pos );
    }

    cc_pos_link_append( path__a, destination );

    return path__a;
}

CcPosLink * cc_shortest_path__new( CcChessboard * restrict cb_before_activation,
                                   CcPieceEnum piece,
                                   CcPos start,
                                   CcPos destination )
{
    // if ( !cc_check_path_args( cb_before_activation, piece, start, destination ) )
    //     return NULL;

    if ( CC_PIECE_IS_NONE( piece ) )
        return NULL;
    else if ( CC_PIECE_IS_BISHOP( piece ) )
        return cc_path_bishop__new( cb_before_activation, piece, start, destination );



    return NULL;
}

CcPosLink * cc_longest_path__new( CcChessboard * restrict cb_before_activation,
                                  CcPieceEnum piece,
                                  CcPos start,
                                  CcPos destination )
{
    // if ( !cc_check_path_args( cb_before_activation, piece, start, destination ) )
    //     return NULL;

    if ( CC_PIECE_IS_NONE( piece ) )
        return NULL;
    else if ( CC_PIECE_IS_BISHOP( piece ) )
        return cc_path_bishop__new( cb_before_activation, piece, start, destination );



    return NULL;
}
