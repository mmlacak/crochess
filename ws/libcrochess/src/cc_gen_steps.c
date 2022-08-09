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

    return true;
}

CcPosLink * cc_link_positions( CcChessboard * restrict cb_before_activation,
                               CcPos start,
                               CcPos destination,
                               CcPos step )
{
    CcPieceEnum piece = cc_chessboard_get_piece( cb_before_activation, start.i, start.j );
    CcPieceEnum pe = CC_PE_None;
    bool piece_in_the_way = false;

    CcPosLink * path__a = cc_pos_link__new( start );

    for ( CcPos pos = cc_pos_add( start, step );
          !cc_pos_is_equal( pos, destination );
          cc_pos_add( pos, step ) )
    {
        pe = cc_chessboard_get_piece( cb_before_activation, pos.i, pos.j );

        if ( CC_PIECE_IS_WAVE( piece ) )
        {
            if ( CC_PIECE_IS_MONOLITH( pe ) )
                piece_in_the_way = true;
        }
        else
            if ( !CC_PIECE_IS_NONE( pe ) )
                piece_in_the_way = true;

        if ( piece_in_the_way )
        {
            cc_pos_link_free_all( &path__a );
            return NULL;
        }

        cc_pos_link_append( path__a, pos );
    }

    cc_pos_link_append( path__a, destination );

    return path__a;
}

bool cc_is_activation_valid( CcChessboard * restrict cb_before_activation,
                             CcPieceEnum activator,
                             CcPos start,
                             CcPos destination,
                             CcPieceEnum expected_type )
{
    if ( !cc_check_path_args( cb_before_activation, start, destination ) )
        return false;

    CcPieceEnum piece = cc_chessboard_get_piece( cb_before_activation, start.i, start.j );

    if ( CC_PIECE_IS_STAR( piece ) )
        return CC_PIECE_IS_STARCHILD( activator ) &&
               cc_piece_has_same_type( piece, expected_type );

    if ( CC_PIECE_IS_NONE( activator ) &&
         CC_PIECE_IS_ACTIVE( piece ) &&
         cc_piece_has_same_type( piece, expected_type ) )
        return true;

    if ( CC_PIECE_IS_ACTIVATOR( activator ) &&
         CC_PIECE_CAN_BE_ACTIVATED( piece ) &&
         cc_piece_has_same_type( piece, expected_type ) )
        return true;

    if ( CC_PIECE_IS_ACTIVATOR( activator ) &&
         CC_PIECE_IS_WAVE( piece ) )
        return true;

    return false;
}

bool cc_is_the_same_color( CcPieceEnum piece, CcPos pos )
{
    if ( cc_piece_is_light( piece ) && CC_IS_POS_LIGHT( pos.i, pos.j ) )
        return true;

    if ( cc_piece_is_dark( piece ) && CC_IS_POS_DARK( pos.i, pos.j ) )
        return true;

    return false;
}


CcPosLink * cc_path_bishop__new( CcChessboard * restrict cb_before_activation,
                                 CcPieceEnum activator,
                                 CcPos start,
                                 CcPos destination )
{
    // <i> Internaly calls cc_check_path_args( ... )
    if ( !cc_is_activation_valid( cb_before_activation,
                                  activator,
                                  start,
                                  destination,
                                  CC_PE_LightBishop ) )
        return NULL;

    CcPos step = cc_pos_step( start, destination );

    if ( !CC_BISHOP_STEP_IS_VALID( step ) ) return NULL;

    return cc_link_positions( cb_before_activation, start, destination, step );
}

CcPosLink * cc_path_rook__new( CcChessboard * restrict cb_before_activation,
                               CcPieceEnum activator,
                               CcPos start,
                               CcPos destination )
{
    // <i> Internaly calls cc_check_path_args( ... )
    if ( !cc_is_activation_valid( cb_before_activation,
                                  activator,
                                  start,
                                  destination,
                                  CC_PE_LightRook ) )
        return NULL;

    CcPos step = cc_pos_step( start, destination );

    if ( !CC_ROOK_STEP_IS_VALID( step ) ) return NULL;

    return cc_link_positions( cb_before_activation, start, destination, step );
}

CcPosLink * cc_path_queen__new( CcChessboard * restrict cb_before_activation,
                                CcPieceEnum activator,
                                CcPos start,
                                CcPos destination )
{
    // <i> Internaly calls cc_check_path_args( ... )
    if ( !cc_is_activation_valid( cb_before_activation,
                                  activator,
                                  start,
                                  destination,
                                  CC_PE_LightQueen ) )
        return NULL;

    CcPos step = cc_pos_step( start, destination );

    if ( !CC_QUEEN_STEP_IS_VALID( step ) ) return NULL;

    return cc_link_positions( cb_before_activation, start, destination, step );
}

CcPosLink * cc_path_king__new( CcChessboard * restrict cb_before_activation,
                               CcPieceEnum activator,
                               CcPos start,
                               CcPos destination )
{
    // <i> Internaly calls cc_check_path_args( ... )
    if ( !cc_is_activation_valid( cb_before_activation,
                                  activator,
                                  start,
                                  destination,
                                  CC_PE_LightKing ) )
        return NULL;

    CcPos step = cc_pos_step( start, destination );

    if ( !CC_QUEEN_STEP_IS_VALID( step ) ) return NULL;

    CcPos end = cc_pos_add( start, step );

    if ( !cc_pos_is_equal( end, destination ) ) return NULL;

    CcPosLink * path__a = cc_pos_link__new( start );

    cc_pos_link_append( path__a, destination );

    return path__a;
}

CcPosLink * cc_path_knight__new( CcChessboard * restrict cb_before_activation,
                                 CcPieceEnum activator,
                                 CcPos start,
                                 CcPos destination )
{
    // <i> Internaly calls cc_check_path_args( ... )
    if ( !cc_is_activation_valid( cb_before_activation,
                                  activator,
                                  start,
                                  destination,
                                  CC_PE_LightKnight ) )
        return NULL;

    CcPos step = cc_pos_step( start, destination );

    if ( !CC_KNIGHT_STEP_IS_VALID( step ) ) return NULL;

    CcPieceEnum pe = cc_chessboard_get_piece( cb_before_activation, start.i, start.j );

    if ( CC_PIECE_IS_KNIGHT( pe ) )
    {
        CcPos end = cc_pos_add( start, step );

        if ( !cc_pos_is_equal( end, destination ) ) return NULL;

        CcPosLink * path__a = cc_pos_link__new( start );

        cc_pos_link_append( path__a, destination );

        return path__a;
    }
    else if ( CC_PIECE_IS_KNIGHT( activator ) &&
              CC_PIECE_IS_WAVE( pe ) )
        return cc_link_positions( cb_before_activation, start, destination, step );

    return NULL;
}

CcPosLink * cc_path_pegasus__new( CcChessboard * restrict cb_before_activation,
                                  CcPieceEnum activator,
                                  CcPos start,
                                  CcPos destination )
{
    // <i> Internaly calls cc_check_path_args( ... )
    if ( !cc_is_activation_valid( cb_before_activation,
                                  activator,
                                  start,
                                  destination,
                                  CC_PE_LightPegasus ) )
        return NULL;

    CcPos step = cc_pos_step( start, destination );

    if ( !CC_KNIGHT_STEP_IS_VALID( step ) ) return NULL;

    return cc_link_positions( cb_before_activation, start, destination, step );
}

CcPosLink * cc_path_unicorn__new( CcChessboard * restrict cb_before_activation,
                                  CcPieceEnum activator,
                                  CcPos start,
                                  CcPos destination )
{
    // <i> Internaly calls cc_check_path_args( ... )
    if ( !cc_is_activation_valid( cb_before_activation,
                                  activator,
                                  start,
                                  destination,
                                  CC_PE_LightUnicorn ) )
        return NULL;

    CcPieceEnum pe = cc_chessboard_get_piece( cb_before_activation, start.i, start.j );
    CcPos step = cc_pos_step( start, destination );

    if ( cc_is_the_same_color( pe, start ) )
    {
        if ( !CC_KNIGHT_STEP_IS_VALID( step ) ) return NULL;
    }
    else
        if ( !CC_UNICORN_STEP_IS_VALID( step ) ) return NULL;

    if ( CC_PIECE_IS_UNICORN( pe ) )
    {
        CcPos end = cc_pos_add( start, step );

        if ( !cc_pos_is_equal( end, destination ) ) return NULL;

        CcPosLink * path__a = cc_pos_link__new( start );

        cc_pos_link_append( path__a, destination );

        return path__a;
    }
    else if ( CC_PIECE_IS_UNICORN( activator ) &&
              CC_PIECE_IS_WAVE( pe ) )
// TODO :: FIX ME :: Wave activated by Unicorn moves like free-choice Centaur !!!
        return cc_link_positions( cb_before_activation, start, destination, step );
// TODO :: FIX ME :: Wave activated by Unicorn moves like free-choice Centaur !!!

    return NULL;
}

CcPosLink * cc_path_star__new( CcChessboard * restrict cb_before_activation,
                               CcPieceEnum activator,
                               CcPos start,
                               CcPos destination )
{
    // <i> Internaly calls cc_check_path_args( ... )
    if ( !cc_is_activation_valid( cb_before_activation,
                                  activator,
                                  start,
                                  destination,
                                  CC_PE_BrightStar ) )
        return NULL;

    CcPieceEnum pe = cc_chessboard_get_piece( cb_before_activation,
                                              destination.i,
                                              destination.j );

    if ( !CC_PIECE_IS_NONE( pe ) ) return NULL;

    CcPos step = cc_pos_step( start, destination );

    if ( !CC_QUEEN_STEP_IS_VALID( step ) ) return NULL;

    CcPos end = cc_pos_add( start, step );

    if ( !cc_pos_is_equal( end, destination ) ) return NULL;

    CcPosLink * path__a = cc_pos_link__new( start );

    cc_pos_link_append( path__a, destination );

    return path__a;
}

CcPosLink * cc_path_starchild__new( CcChessboard * restrict cb_before_activation,
                                    CcPieceEnum activator,
                                    CcPos start,
                                    CcPos destination )
{
    // <i> Internaly calls cc_check_path_args( ... )
    if ( !cc_is_activation_valid( cb_before_activation,
                                  activator,
                                  start,
                                  destination,
                                  CC_PE_LightStarchild ) )
        return NULL;

    bool is_opposite_color_fields =
        CC_XOR( CC_IS_POS_LIGHT( start.i, start.j ),
                CC_IS_POS_LIGHT( destination.i, destination.j ) );

    CcPieceEnum pe = cc_chessboard_get_piece( cb_before_activation,
                                              destination.i,
                                              destination.j );

    bool is_just_step = is_opposite_color_fields &&
                        ( CC_PIECE_IS_NONE( pe ) || CC_PIECE_IS_WAVE( pe ) );

    CcPos step = cc_pos_step( start, destination );

    if ( !is_just_step )
        if ( !CC_QUEEN_STEP_IS_VALID( step ) ) return NULL;

    CcPos end = cc_pos_add( start, step );

    if ( !cc_pos_is_equal( end, destination ) ) return NULL;

    CcPosLink * path__a = cc_pos_link__new( start );

    cc_pos_link_append( path__a, destination );

    return path__a;
}


CcPosLink * cc_shortest_path__new( CcChessboard * restrict cb_before_activation,
                                   CcPieceEnum activator,
                                   CcPos start,
                                   CcPos destination )
{
    if ( CC_PIECE_IS_NONE( activator ) )
        return NULL;
    else if ( CC_PIECE_IS_BISHOP( activator ) )
        return cc_path_bishop__new( cb_before_activation, activator, start, destination );
    else if ( CC_PIECE_IS_ROOK( activator ) )
        return cc_path_rook__new( cb_before_activation, activator, start, destination );
    else if ( CC_PIECE_IS_QUEEN( activator ) )
        return cc_path_queen__new( cb_before_activation, activator, start, destination );
    else if ( CC_PIECE_IS_KING( activator ) )
        return cc_path_king__new( cb_before_activation, activator, start, destination );
    else if ( CC_PIECE_IS_KNIGHT( activator ) )
        return cc_path_knight__new( cb_before_activation, activator, start, destination );
    else if ( CC_PIECE_IS_PEGASUS( activator ) )
        return cc_path_pegasus__new( cb_before_activation, activator, start, destination );
    else if ( CC_PIECE_IS_UNICORN( activator ) )
        return cc_path_unicorn__new( cb_before_activation, activator, start, destination );
    else if ( CC_PIECE_IS_STAR( activator ) )
        return cc_path_star__new( cb_before_activation, activator, start, destination );
    else if ( CC_PIECE_IS_STARCHILD( activator ) )
        return cc_path_starchild__new( cb_before_activation, activator, start, destination );


// TODO :: check destination field


    return NULL;
}

CcPosLink * cc_longest_path__new( CcChessboard * restrict cb_before_activation,
                                  CcPieceEnum activator,
                                  CcPos start,
                                  CcPos destination )
{
    if ( CC_PIECE_IS_NONE( activator ) )
        return NULL;
    else if ( CC_PIECE_IS_BISHOP( activator ) )
        return cc_path_bishop__new( cb_before_activation, activator, start, destination );
    else if ( CC_PIECE_IS_ROOK( activator ) )
        return cc_path_rook__new( cb_before_activation, activator, start, destination );
    else if ( CC_PIECE_IS_QUEEN( activator ) )
        return cc_path_queen__new( cb_before_activation, activator, start, destination );
    else if ( CC_PIECE_IS_KING( activator ) )
        return cc_path_king__new( cb_before_activation, activator, start, destination );
    else if ( CC_PIECE_IS_KNIGHT( activator ) )
        return cc_path_knight__new( cb_before_activation, activator, start, destination );
    else if ( CC_PIECE_IS_PEGASUS( activator ) )
        return cc_path_pegasus__new( cb_before_activation, activator, start, destination );
    else if ( CC_PIECE_IS_UNICORN( activator ) )
        return cc_path_unicorn__new( cb_before_activation, activator, start, destination );
    else if ( CC_PIECE_IS_STAR( activator ) )
        return cc_path_star__new( cb_before_activation, activator, start, destination );
    else if ( CC_PIECE_IS_STARCHILD( activator ) )
        return cc_path_starchild__new( cb_before_activation, activator, start, destination );


// TODO :: check destination field


    return NULL;
}
