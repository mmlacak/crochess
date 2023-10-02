// Copyright (c) 2023 Mario Mlačak, mmlacak@gmail.com
// Licensed under GNU GPL v3+ license. See LICENSING, COPYING files for details.

#include "cc_defines.h"

#include "cc_variant.h"
#include "cc_setup_misc.h"

/**
    @file cc_setup_misc.c
    @brief Misc functions related to setup, or dependant on a variant being played, size of board, etc.
*/


int cc_get_figure_initial_file( CcVariantEnum ve,
                                            CcPieceEnum pe,
                                            bool search_queen_side_first ) {
    // Not figure row pieces.
    if ( ( CC_PIECE_IS_NONE( pe ) ) ||
            ( CC_PIECE_IS_PAWN( pe ) ) ||
            ( CC_PIECE_IS_SCOUT( pe ) ) ||
            ( CC_PIECE_IS_GRENADIER( pe ) ) ||
            ( CC_PIECE_IS_MONOLITH( pe ) ) )
        return CC_INVALID_COORD;

    CcPieceEnum const * su = cc_setup_board_get( ve );
    if ( !su ) return CC_INVALID_COORD;

    size_t size = cc_variant_board_size( ve );
    int start = search_queen_side_first ? 0 : (int)(size - 1);
    int step = search_queen_side_first ? 1 : -1;

    int rank =
        cc_piece_is_light( pe ) || ( pe == CC_PE_BrightStar ) ? (int)(size - 1)
                                                              : 0;

    for ( int j = start; (0 <= j) && (j < (int)size); j += step ) {
        int z = size * rank + j;

        if ( su[ z ] == pe )
            return j;
    }

    return CC_INVALID_COORD;
}

int cc_get_initial_figure_rank( CcVariantEnum ve, bool is_light ) {
    if ( is_light ) return 0;

    size_t size = cc_variant_board_size( ve );
    return (int)(size - 1);
}

int cc_get_kings_initial_file( CcVariantEnum ve ) {
    switch ( ve ) {
        case CC_VE_ClassicalChess : return 4;
        case CC_VE_CroatianTies : return 5;
        case CC_VE_MayanAscendancy : return 6;
        case CC_VE_AgeOfAquarius : return 7;
        case CC_VE_MirandasVeil : return 8;
        case CC_VE_Nineteen : return 9;
        case CC_VE_HemerasDawn : return 10;
        case CC_VE_TamoanchanRevisited : return 11;
        case CC_VE_ConquestOfTlalocan : return 12;
        case CC_VE_Discovery : return 12;
        case CC_VE_One : return 13;

        default : return CC_INVALID_COORD;
    }
}

int cc_get_kings_max_castling_distance( CcVariantEnum ve ) {
    switch ( ve ) {
        case CC_VE_ClassicalChess : return 2;
        case CC_VE_CroatianTies : return 3;
        case CC_VE_MayanAscendancy : return 4;
        case CC_VE_AgeOfAquarius : return 5;
        case CC_VE_MirandasVeil : return 6;
        case CC_VE_Nineteen : return 6;
        case CC_VE_HemerasDawn : return 7;
        case CC_VE_TamoanchanRevisited : return 8;
        case CC_VE_ConquestOfTlalocan : return 9;
        case CC_VE_Discovery : return 9;
        case CC_VE_One : return 10;

        default : return CC_INVALID_COORD;
    }
}

bool cc_check_pos_is_king_castling_step( CcVariantEnum ve, bool is_light, int i, int j ) {
    int size = cc_variant_board_size( ve );
    if ( !CC_IS_BOARD_SIZE_VALID( size ) ) return false;

    int init_i = cc_get_kings_initial_file( ve );
    if ( !CC_IS_COORD_ON_BOARD( size, init_i ) ) return false;

    int init_j = cc_get_initial_figure_rank( ve, is_light );
    if ( !CC_IS_COORD_ON_BOARD( size, init_j ) ) return false;

    if ( j != init_j ) return false;

    int max_dist = cc_get_kings_max_castling_distance( ve );
    if ( !CC_IS_COORD_VALID( max_dist ) ) return false;

    bool is_queen_side_castling = ( i < init_i );

    int min_i =
        is_queen_side_castling ? init_i - max_dist
                               : init_i + CC_KINGS_MIN_CASTLING_DISTANCE;

    int max_i =
        is_queen_side_castling ? init_i - CC_KINGS_MIN_CASTLING_DISTANCE
                               : init_i + max_dist;

    return ( min_i <= i && i <= max_i );
}
