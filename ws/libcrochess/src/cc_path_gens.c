// Copyright (c) 2021, 2022 Mario Mlačak, mmlacak@gmail.com
// Licensed under GNU GPL v3+ license. See LICENSING, COPYING files for details.

#include "cc_pos_defs.h"
// #include "cc_path_defs.h"
#include "cc_path_utils.h"
#include "cc_path_gens.h"

/**
    @file cc_path_gens.c
    @brief Path generators.
*/


//
// Auxiliary functions.

CcRoutePin * cc_path_find_route__new( CcPathNode * restrict path_node,
                                      bool is_shortest ) {
    if ( !path_node ) return NULL;

    size_t count = 0;
    CcRoutePin * route__a = NULL;
    CcRoutePin * pw__t = NULL;

    while ( cc_route_pin_iter( path_node, &pw__t ) ) {
        if ( !route__a ) { // Not initialized search yet.
            count = cc_route_pin_count_of_steps( pw__t );

            route__a = cc_route_pin_copy_shallow__new( pw__t );
            if ( !route__a ) {
                cc_route_pin_free_all( &pw__t );
                return NULL;
            }
        } else {
            size_t c = cc_route_pin_count_of_steps( pw__t );
            bool found = is_shortest ? ( c < count ) : ( c > count );

            if ( found ) {
                cc_route_pin_free_all( &route__a );

                route__a = cc_route_pin_copy_shallow__new( pw__t );
                if ( !route__a ) {
                    cc_route_pin_free_all( &pw__t );
                    return NULL;
                }

                count = c;
            }
        }
    }

    cc_route_pin_free_all( &pw__t );

    return route__a;
}

// TODO :: DELETE
//
// CcPptLink * cc_path_find_shortest_route__new( CcPathNode * restrict path_node ) {
//     if ( !path_node ) return NULL;

//     CcRoutePin * shortest__a = cc_path_find_route__new( path_node, true );

//     CcPptLink * pl__a = cc_route_pin_assemble__new( shortest__a );

//     cc_route_pin_free_all( &shortest__a );

//     return pl__a;
// }

// CcPptLink * cc_path_find_longest_route__new( CcPathNode * restrict path_node ) {
//     if ( !path_node ) return NULL;

//     CcRoutePin * longest__a = cc_path_find_route__new( path_node, false );

//     CcPptLink * pl__a = cc_route_pin_assemble__new( longest__a );

//     cc_route_pin_free_all( &longest__a );

//     return pl__a;
// }
//
// TODO :: DELETE


static CcPathNode * cc_find_paths_from__new( CcChessboard * restrict cb,
                                             CcPieceEnum piece,
                                             CcPieceEnum activator,
                                             bool can_activate_pyramid,
                                             CcPos start ) {

    // start can be either position, or disambiguation

    return NULL; // TODO
}

static CcPathNode * cc_find_paths_to__new( CcChessboard * restrict cb,
                                           CcPieceEnum piece,
                                           CcPieceEnum activator,
                                           bool can_activate_pyramid,
                                           CcPos end ) {
    return NULL; // TODO
}

static CcPathNode * cc_find_paths_from_to__new( CcChessboard * restrict cb,
                                                CcPieceEnum piece,
                                                CcPieceEnum activator,
                                                bool can_activate_pyramid,
                                                CcPos start,
                                                CcPos end ) {

    // start can be either position, or disambiguation

    return NULL; // TODO
}


CcPathNode * cc_find_paths_all_pieces__new( CcChessboard * restrict cb,
                                            CcPieceEnum piece,
                                            CcPieceEnum activator,
                                            bool can_activate_pyramid,
                                            CcPos start__d,
                                            CcPos end__d ) {
    if ( !cb ) return NULL;

    if ( CC_PIECE_IS_NONE( piece ) ) return NULL;
    if ( CC_PIECE_IS_WAVE( piece ) && CC_PIECE_IS_NONE( activator ) ) return NULL;

    bool is_start_on_board = cc_chessboard_is_disambiguation_on_board( cb, start__d.i, start__d.j );
    bool is_end_on_board = cc_chessboard_is_pos_on_board( cb, end__d.i, end__d.j );

    if ( !is_start_on_board && !is_end_on_board ) return NULL; // <1>

    if ( cc_chessboard_is_pos_on_board( cb, start__d.i, start__d.j ) ) {
        CcPieceEnum pe = cc_chessboard_get_piece( cb, start__d.i, start__d.j );

        // TODO :: check Shaman/Starchild initiating trance-/sense-journey fits
        if ( !CC_PIECE_IS_EQUAL( pe, piece ) && // before activation
             !CC_PIECE_IS_EQUAL( pe, activator ) && // after activation
             !CC_PIECE_IS_WAVE( pe ) ) // after activation
                return NULL;
    }

    CcPathNode * paths__a = NULL;

    if ( is_start_on_board ) {
        if ( is_end_on_board ) {
            paths__a = cc_find_paths_from_to__new( cb,
                                                   piece,
                                                   activator,
                                                   can_activate_pyramid,
                                                   start__d,
                                                   end__d );
        } else {
            paths__a = cc_find_paths_from__new( cb,
                                                piece,
                                                activator,
                                                can_activate_pyramid,
                                                start__d );
        }
    } else { // is_end_on_board is true, otherwise would bail-out at <1>.
        paths__a = cc_find_paths_to__new( cb,
                                          piece,
                                          activator,
                                          can_activate_pyramid,
                                          end__d );
    }

    return paths__a;
}

CcPathNode * cc_find_paths_unique_piece__new( CcChessboard * restrict cb,
                                              CcPieceEnum piece,
                                              CcPieceEnum activator,
                                              bool can_activate_pyramid,
                                              CcPos start__d,
                                              CcPos end__d ) {
    CcPathNode * paths__a = cc_find_paths_all_pieces__new( cb,
                                                           piece,
                                                           activator,
                                                           can_activate_pyramid,
                                                           start__d,
                                                           end__d );
    if ( !paths__a ) return NULL;

    if ( cc_chessboard_is_pos_on_board( cb, start__d.i, start__d.j ) )
        return paths__a; // Start and piece specified --> already unique.

    CcPathNode * pl = paths__a;
    CcPos pos_1st = pl->path->ppt.pos; // Grab 1st position of 1st path.

    while ( pl ) {
        if ( !CC_POS_IS_EQUAL( pos_1st, pl->path->ppt.pos ) ) { // Start pos differs from 1st --> not unique!
            cc_path_node_free_all( &paths__a );
            return NULL;
        }

        pl = pl->alt_path;
    }

    return paths__a;
}

CcPathNode * cc_find_shortest_paths__new( CcChessboard * restrict cb,
                                          CcPieceEnum piece,
                                          CcPieceEnum activator,
                                          bool can_activate_pyramid,
                                          CcPos start__d,
                                          CcPos end__d ) {
    CcPathNode * paths__a = cc_find_paths_unique_piece__new( cb,
                                                             piece,
                                                             activator,
                                                             can_activate_pyramid,
                                                             start__d,
                                                             end__d );
    if ( !paths__a ) return NULL;



    return NULL; // TODO
}

CcPathNode * cc_find_longest_paths__new( CcChessboard * restrict cb,
                                         CcPieceEnum piece,
                                         CcPieceEnum activator,
                                         bool can_activate_pyramid,
                                         CcPos start__d,
                                         CcPos end__d ) {
    CcPathNode * paths__a = cc_find_paths_unique_piece__new( cb,
                                                             piece,
                                                             activator,
                                                             can_activate_pyramid,
                                                             start__d,
                                                             end__d );
    if ( !paths__a ) return NULL;



    return NULL; // TODO
}
