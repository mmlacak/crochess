// Copyright (c) 2021, 2022 Mario Mlačak, mmlacak@gmail.com
// Licensed under GNU GPL v3+ license. See LICENSING, COPYING files for details.

#ifndef __CC_PATH_GENS_H__
#define __CC_PATH_GENS_H__

#include "cc_piece.h"
#include "cc_chessboard.h"
#include "cc_pos.h"
#include "cc_path.h"


/**
    @file cc_path_gens.h
    @brief Path generators.
*/


//
// Auxiliary functions.

// TODO :: DOCS
CcRoutePin * cc_path_find_route__new( CcPathNode * path_node,
                                      bool is_shortest );

// TODO :: DELETE
//
// // TODO :: find shortest route
// CcPptLink * cc_path_find_shortest_route__new( CcPathNode * path_node );

// // TODO :: find longest route
// CcPptLink * cc_path_find_longest_route__new( CcPathNode * path_node );
//
// TODO :: DELETE


// static CcPathNode * cc_find_paths_from__new( CcChessboard * cb,
//                                              CcPieceEnum piece,
//                                              CcPieceEnum activator,
//                                              bool can_activate_pyramid,
//                                              CcPos start );

// static CcPathNode * cc_find_paths_to__new( CcChessboard * cb,
//                                            CcPieceEnum piece,
//                                            CcPieceEnum activator,
//                                            bool can_activate_pyramid,
//                                            CcPos end );

// static CcPathNode * cc_find_paths_from_to__new( CcChessboard * cb,
//                                                 CcPieceEnum piece,
//                                                 CcPieceEnum activator,
//                                                 bool can_activate_pyramid,
//                                                 CcPos start,
//                                                 CcPos end );


// TODO :: TODO
//
// CcPathNode * cc_find_paths_all_pieces__new( CcChessboard * cb,
//                                             CcPieceEnum piece,
//                                             CcPieceEnum activator,
//                                             bool can_activate_pyramid,
//                                             CcPos start__d,
//                                             CcPos end__d );

// CcPathNode * cc_find_paths_unique_piece__new( CcChessboard * cb,
//                                               CcPieceEnum piece,
//                                               CcPieceEnum activator,
//                                               bool can_activate_pyramid,
//                                               CcPos start__d,
//                                               CcPos end__d );

// CcPathNode * cc_find_shortest_paths__new( CcChessboard * cb,
//                                           CcPieceEnum piece,
//                                           CcPieceEnum activator,
//                                           bool can_activate_pyramid,
//                                           CcPos start__d,
//                                           CcPos end__d );

// CcPathNode * cc_find_longest_paths__new( CcChessboard * cb,
//                                          CcPieceEnum piece,
//                                          CcPieceEnum activator,
//                                          bool can_activate_pyramid,
//                                          CcPos start__d,
//                                          CcPos end__d );
//
// TODO :: TODO


#endif /* __CC_PATH_GENS_H__ */
