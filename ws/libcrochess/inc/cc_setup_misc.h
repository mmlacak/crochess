// Copyright (c) 2023 Mario Mlačak, mmlacak@gmail.com
// Licensed under GNU GPL v3+ license. See LICENSING, COPYING files for details.

#ifndef __CC_SETUP_MISC_H__
#define __CC_SETUP_MISC_H__

#include "cc_setup_board.h"
// #include "cc_setup_tags.h"


#define CC_KING_MIN_CASTLING_DISTANCE (2)


int cc_find_initial_figure_file( CcVariantType ve,
                                 CcPieceTagType pe,
                                 bool search_queen_side_first );

int cc_get_kings_max_castling_distance( CcVariantType ve );

bool cc_check_pos_is_king_castling_step( CcVariantType ve,
                                         CcPieceTagType king,
                                         int pos_i,
                                         int pos_j,
                                         bool * is_queen_side__o,
                                         int * min_i__o,
                                         int * max_i__o );


#endif /* __CC_SETUP_MISC_H__ */
