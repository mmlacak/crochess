// Copyright (c) 2021, 2022 Mario Mlačak, mmlacak@gmail.com
// Licensed under GNU GPL v3+ license. See LICENSING, COPYING files for details.

#include "cc_path_defs.h"
#include "cc_path_utils.h"
#include "cc_path_gens.h"

/**
    @file cc_path_gens.c
    @brief Path generators.
*/


static CcPathLink * cc_find_paths_from__new( CcChessboard * restrict cb,
                                             CcPieceEnum piece,
                                             CcPieceEnum activator,
                                             bool can_activate_pyramid,
                                             CcPos start ) {
    return NULL; // TODO
}

static CcPathLink * cc_find_paths_to__new( CcChessboard * restrict cb,
                                           CcPieceEnum piece,
                                           CcPieceEnum activator,
                                           bool can_activate_pyramid,
                                           CcPos end ) {
    return NULL; // TODO
}

static CcPathLink * cc_find_paths_from_to__new( CcChessboard * restrict cb,
                                                CcPieceEnum piece,
                                                CcPieceEnum activator,
                                                bool can_activate_pyramid,
                                                CcPos start,
                                                CcPos end ) {
    return NULL; // TODO
}


CcPathLink * cc_find_paths_all_pieces__new( CcChessboard * restrict cb,
                                            CcPieceEnum piece,
                                            CcPieceEnum activator,
                                            bool can_activate_pyramid,
                                            CcPos start__d,
                                            CcPos end__d ) {
    if ( !cb ) return NULL;

    if ( CC_PIECE_IS_NONE( piece ) ) return NULL;
    if ( CC_PIECE_IS_WAVE( piece ) && CC_PIECE_IS_NONE( activator ) ) return NULL;

    bool is_start_on_board = cc_chessboard_is_pos_on_board( cb, start__d.i, start__d.j );
    bool is_end_on_board = cc_chessboard_is_pos_on_board( cb, end__d.i, end__d.j );

    if ( !is_start_on_board && !is_end_on_board ) return NULL; // <1>

    CcPathLink * paths__a = NULL;

    if ( is_start_on_board ) {
        CcPieceEnum pe = cc_chessboard_get_piece( cb, start__d.i, start__d.j );

        if ( !CC_PIECE_IS_EQUAL( pe, piece ) &&
             !CC_PIECE_IS_EQUAL( pe, activator ) &&
             !CC_PIECE_IS_WAVE( pe ) ) // TODO :: check Shaman/Starchild initiating trance-/sense-journey fits
                return NULL;

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

CcPathLink * cc_find_paths_unique_piece__new( CcChessboard * restrict cb,
                                              CcPieceEnum piece,
                                              CcPieceEnum activator,
                                              bool can_activate_pyramid,
                                              CcPos start__d,
                                              CcPos end__d ) {
    CcPathLink * paths__a = cc_find_paths_all_pieces__new( cb,
                                                           piece,
                                                           activator,
                                                           can_activate_pyramid,
                                                           start__d,
                                                           end__d );
    if ( !paths__a ) return NULL;

    if ( cc_chessboard_is_pos_on_board( cb, start__d.i, start__d.j ) )
        return paths__a; // Start and piece specified --> already unique.

    CcPathLink * pl = paths__a;
    CcPos pos_1st = pl->path->ppt.pos; // Grab 1st position of 1st path.

    while ( pl ) {
        if ( !CC_POS_IS_EQUAL( pos_1st, pl->path->ppt.pos ) ) { // Start pos differs from 1st --> not unique!
            cc_path_link_free_all( &paths__a );
            return NULL;
        }

        pl = pl->alt_path;
    }

    return paths__a;
}

CcPathLink * cc_find_shortest_paths__new( CcChessboard * restrict cb,
                                          CcPieceEnum piece,
                                          CcPieceEnum activator,
                                          bool can_activate_pyramid,
                                          CcPos start__d,
                                          CcPos end__d ) {
    CcPathLink * paths__a = cc_find_paths_unique_piece__new( cb,
                                                             piece,
                                                             activator,
                                                             can_activate_pyramid,
                                                             start__d,
                                                             end__d );
    if ( !paths__a ) return NULL;



    return NULL; // TODO
}

CcPathLink * cc_find_longest_paths__new( CcChessboard * restrict cb,
                                         CcPieceEnum piece,
                                         CcPieceEnum activator,
                                         bool can_activate_pyramid,
                                         CcPos start__d,
                                         CcPos end__d ) {
    CcPathLink * paths__a = cc_find_paths_unique_piece__new( cb,
                                                             piece,
                                                             activator,
                                                             can_activate_pyramid,
                                                             start__d,
                                                             end__d );
    if ( !paths__a ) return NULL;



    return NULL; // TODO
}
