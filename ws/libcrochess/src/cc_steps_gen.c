// Copyright (c) 2021, 2022 Mario Mlačak, mmlacak@gmail.com
// Licensed under GNU GPL v3+ license. See LICENSING, COPYING files for details.

#include "cc_steps_def.h"
#include "cc_steps_gen.h"

/**
    @file cc_steps_gen.c
    @brief Step generators.
*/


bool cc_piece_pos_iter( CcChessboard * restrict cb_before_activation,
                        CcPos starting,
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

    bool is_comparable = cc_pos_is_valid( starting ) ||
                         cc_pos_is_disambiguation( starting );

    for ( int i = pos.i; i < size; ++i )
    {
        for ( int j = pos.j; j < size; ++j )
        {
            CcPieceEnum pe = cc_chessboard_get_piece( cb_before_activation, i, j );

            if ( CC_PIECE_IS_THE_SAME( pe, piece ) ||
                 ( include_opponent && cc_piece_is_opposite( pe, piece ) ) )
            {
                CcPos current = cc_pos( i, j );

                if ( ( !is_comparable ) ||
                       cc_pos_is_congruent( starting, current ) )
                {
                    *pos__io = current;
                    return true;
                }
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

bool cc_is_step_capture( CcPieceEnum activator,
                         CcPieceEnum piece,
                         CcPos step,
                         CcPos step_2 )
{
    if ( !CC_PIECE_IS_VALID( piece ) ) return false;

    if ( !cc_pos_is_valid( step ) ) return false;
    if ( cc_pos_is_static_step( step ) ) return false;

    if ( CC_PIECE_IS_PAWN( piece ) )
    {
        if ( cc_piece_is_light( piece ) )
            return CC_LIGHT_PAWN_CAPTURE_STEP_IS_VALID( step );
        else
            return CC_DARK_PAWN_CAPTURE_STEP_IS_VALID( step );
    }
    else if ( CC_PIECE_IS_SHAMAN( piece ) )
    {
        if ( cc_piece_is_light( piece ) )
            return CC_LIGHT_SHAMAN_CAPTURE_STEP_IS_VALID( step );
        else
            return CC_DARK_SHAMAN_CAPTURE_STEP_IS_VALID( step );
    }
    else if ( CC_PIECE_IS_WAVE( piece ) )
        return cc_is_step_capture( CC_PE_None, activator, step, step_2 );
    else if ( CC_PIECE_IS_MONOLITH( piece ) )
        return false;
    else if ( CC_PIECE_IS_STAR( piece ) )
        return false;
    else if ( CC_PIECE_IS_STARCHILD( piece ) )
        return false;

    return true;
}

bool cc_is_step_miracle( CcPieceEnum piece, CcPos step )
{
    if ( CC_PIECE_IS_STARCHILD( piece ) )
        return CC_STARCHILD_MIRACLE_STEP_IS_VALID( step );

    return false;
}

bool cc_is_ply_valid( CcChessboard * restrict cb_before_activation,
                      CcPieceEnum activator,
                      CcPos start,
                      CcPos destination,
                      CcPos step,
                      CcPos step_2 )
{
    if ( !cc_check_path_args( cb_before_activation, start, destination ) )
        return false;

    CcPieceEnum target = cc_chessboard_get_piece( cb_before_activation,
                                                  destination.i,
                                                  destination.j );

    // An empty field, always targetable.
    if ( CC_PIECE_IS_NONE( target ) ) return true;

    // Kings can't be ever captured, activated, converted, displaced, ...
    if ( CC_PIECE_IS_KING( target ) ) return false;

    bool target_is_owned = CC_PIECE_HAS_OWNER( target );

    CcPieceEnum piece = cc_chessboard_get_piece( cb_before_activation, start.i, start.j );
    bool piece_is_owned = CC_PIECE_HAS_OWNER( piece );
    bool is_same_owner = cc_piece_has_same_owner( piece, target );

    // Own Pyramid can be activated by any own piece on capture-fields, or
    // by Starchild on miracle-fields, or by Wave activated on those fields.
    if ( CC_PIECE_IS_PYRAMID( target ) )
    {
        if ( is_same_owner )
            return cc_is_step_capture( activator, piece, step, step_2 ) ||
                   cc_is_step_miracle( piece, step ) ||
                   ( CC_PIECE_IS_WAVE( piece ) &&
                     cc_is_step_miracle( activator, step ) );
        else
            return false;
    }

    // Wave can be activated by any own piece, or opponent's Wave.
    if ( CC_PIECE_IS_WAVE( target ) )
        return is_same_owner ? true : CC_PIECE_IS_WAVE( piece );

    // Wave can activate other Wave, or any other own piece, except King (already handled).
    // Pyramid can only be activated on capture- or miracle-fields (also handled).
    if ( CC_PIECE_IS_WAVE( piece ) )
    {
        if ( CC_PIECE_IS_WAVE( target ) || is_same_owner )
            return true;
        else
            return false;
    }

    if ( CC_PIECE_IS_STARCHILD( piece ) )
    {
        if ( CC_PIECE_IS_STARCHILD( target ) || CC_PIECE_IS_WAVE( target ) )
            // Starchild can activate own Starchild, Wave; on its step-fields.
            return is_same_owner;
        else if ( cc_is_step_miracle( piece, step ) )
            // Starchild can activate any own piece (except King), opponent’s
            // Starchild and any Star on its neighboring-fields.
            return ( is_same_owner ||
                     CC_PIECE_IS_STAR( target ) ||
                     CC_PIECE_IS_STARCHILD( target ) );
        else
            return false;
    }

    // Monolith, Star can only move to an empty field.
    if ( CC_PIECE_IS_MONOLITH( piece ) || CC_PIECE_IS_STAR( piece ) )
        return ( CC_PIECE_IS_NONE( target ) );

    // Any piece, own or opponent's, can teleport, except Kings, Stars, Starchilds, Monoliths.
    if ( CC_PIECE_IS_MONOLITH( target ) || CC_PIECE_IS_STAR( target ) )
        return ( !CC_PIECE_IS_KING( piece ) &&
                 !CC_PIECE_IS_STARCHILD( piece ) &&
                 !CC_PIECE_IS_MONOLITH( piece ) &&
                 !CC_PIECE_IS_STAR( piece ) );

    if ( CC_PIECE_IS_PYRAMID( piece ) )
    {
        // Pyramid can tag for promotion own Pawn on opponent's side of a chessboard.
        if ( CC_PIECE_IS_PAWN( target ) && is_same_owner )
        {
            if ( cc_piece_is_light( piece ) )
                return ( ( cc_chessboard_is_field_on_dark_side( cb_before_activation,
                                                                start.j ) ) &&
                         ( cc_chessboard_is_field_on_dark_side( cb_before_activation,
                                                                destination.j ) ) );
            else
                return ( ( cc_chessboard_is_field_on_light_side( cb_before_activation,
                                                                 start.j ) ) &&
                         ( cc_chessboard_is_field_on_light_side( cb_before_activation,
                                                                 destination.j ) ) );
        }

        // Pyramid can convert any opponent's piece on own side of a chessboard.
        if ( target_is_owned && !is_same_owner )
        {
            if ( cc_piece_is_light( piece ) )
                return ( ( cc_chessboard_is_field_on_light_side( cb_before_activation,
                                                                 start.j ) ) &&
                         ( cc_chessboard_is_field_on_light_side( cb_before_activation,
                                                                 destination.j ) ) );
            else
                return ( ( cc_chessboard_is_field_on_dark_side( cb_before_activation,
                                                                start.j ) ) &&
                         ( cc_chessboard_is_field_on_dark_side( cb_before_activation,
                                                                destination.j ) ) );
        }
    }

    // Any piece can capture opponent's piece, except Starchild, Wave, Star, Monolith.
    if ( piece_is_owned && target_is_owned && !is_same_owner )
    {
        return ( !CC_PIECE_IS_WAVE( piece ) &&
                 !CC_PIECE_IS_STARCHILD( piece ) &&
                 !CC_PIECE_IS_MONOLITH( piece ) &&
                 !CC_PIECE_IS_STAR( piece ) );
    }

    return false;
}

CcPosLink * cc_link_positions( CcChessboard * restrict cb_before_activation,
                               CcPos start,
                               CcPos destination,
                               CcPos step,
                               CcPos step_2 )
{
    if ( !cc_check_path_args( cb_before_activation, start, destination ) )
        return NULL;

    if ( ( cc_pos_is_static_step( step ) ) || ( !cc_pos_is_valid( step ) ) )
        return NULL;

    CcPieceEnum piece = cc_chessboard_get_piece( cb_before_activation, start.i, start.j );
    CcPieceEnum pe = CC_PE_None;
    bool piece_in_the_way = false;

    CcPos s = step;
    bool is_alternating_steps = ( ( !cc_pos_is_static_step( step_2 ) ) &&   \
                                  ( cc_pos_is_valid( step_2 ) ) );
    bool is_even_step = true;

    CcPosLink * path__a = cc_pos_link__new( start );

    for ( CcPos pos = cc_pos_add( start, s );
          !cc_pos_is_equal( pos, destination );
          cc_pos_add( pos, s ) )
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

        if ( is_alternating_steps )
        {
            s = is_even_step ? step_2 : step;
            is_even_step = !is_even_step;
        }
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
    if ( cc_piece_is_light( piece ) && CC_IS_FIELD_LIGHT( pos.i, pos.j ) )
        return true;

    if ( cc_piece_is_dark( piece ) && CC_IS_FIELD_DARK( pos.i, pos.j ) )
        return true;

    return false;
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
        return cc_link_positions( cb_before_activation,
                                  start,
                                  destination,
                                  step,
                                  CC_POS_STATIC_STEP_CAST );

    return NULL;
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

    return cc_link_positions( cb_before_activation,
                              start,
                              destination,
                              step,
                              CC_POS_STATIC_STEP_CAST );
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

    return cc_link_positions( cb_before_activation,
                              start,
                              destination,
                              step,
                              CC_POS_STATIC_STEP_CAST );
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

    return cc_link_positions( cb_before_activation,
                              start,
                              destination,
                              step,
                              CC_POS_STATIC_STEP_CAST );
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

    if ( !CC_KING_STEP_IS_VALID( step ) ) return NULL;

    CcPos end = cc_pos_add( start, step );

    if ( !cc_pos_is_equal( end, destination ) ) return NULL;

    CcPosLink * path__a = cc_pos_link__new( start );

    cc_pos_link_append( path__a, destination );

    return path__a;
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

    return cc_link_positions( cb_before_activation,
                              start,
                              destination,
                              step,
                              CC_POS_STATIC_STEP_CAST );
}

// TODO :: Pyramid

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
        if ( !CC_UNICORN_LONG_STEP_IS_VALID( step ) ) return NULL;

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
        return cc_link_positions( cb_before_activation,
                                  start,
                                  destination,
                                  step,
                                  CC_POS_STATIC_STEP_CAST );
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

// TODO :: Centaur

// TODO :: Serpent

// TODO :: Shaman

// TODO :: Monolith

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
        CC_XOR( CC_IS_FIELD_LIGHT( start.i, start.j ),
                CC_IS_FIELD_LIGHT( destination.i, destination.j ) );

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

// TODO :: Centaur

// TODO :: Serpent

// TODO :: Shaman

// TODO :: Monolith


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

// TODO :: Centaur

// TODO :: Serpent

// TODO :: Shaman

// TODO :: Monolith


// TODO :: check destination field


    return NULL;
}