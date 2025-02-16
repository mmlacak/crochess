// Copyright (c) 2024 Mario Mlačak, mmlacak@gmail.com
// Licensed under GNU GPL v3+ license. See LICENSING, COPYING files for details.

#ifndef __CC_PATH_H__
#define __CC_PATH_H__

#include <stdbool.h>

#include "cc_defines.h"
// #include "cc_str_utils.h"

#include "cc_piece.h"
#include "cc_tag.h"
#include "cc_step.h"
#include "cc_side_effect.h"
#include "cc_pos.h"


//
// Linked path segments.

// TODO :: DOCS
typedef struct CcPathLink {
    CcSideEffect side_effect; /* A possible side-effect on previously encountered piece. */

    CcStep * steps; /* Steps performed, fields visited. */

    CcPieceEnum encountered_piece; /* Piece encountered at the very last field in the list above. */
    CcTagEnum encountered_tag; /* Tag encountered at the very last field in the list above. */

    CcMomentum momentum; /* Momentum a moving piece had after all performed steps. */

    struct CcPathLink * fork;
    struct CcPathLink * alt;
    struct CcPathLink * sub;
    struct CcPathLink * next;
    struct CcPathLink * back__w; /* Back-link to parent node. */
} CcPathLink;

CcPathLink * cc_path_link__new( CcSideEffect side_effect,
                                CcStep ** steps__d_n,
                                CcPieceEnum encountered_piece,
                                CcTagEnum encountered_tag,
                                CcMomentum momentum );

CcPathLink * cc_path_link_append( CcPathLink ** pl__iod_a,
                                  CcSideEffect side_effect,
                                  CcStep ** steps__d_n,
                                  CcPieceEnum encountered_piece,
                                  CcTagEnum encountered_tag,
                                  CcMomentum momentum );

CcPathLink * cc_path_link_extend( CcPathLink ** pl__iod_a,
                                  CcPathLink ** pl__n );

CcPathLink * cc_path_link_add_fork( CcPathLink ** pl_step__a,
                                    CcPathLink ** pl_fork__n );

CcPathLink * cc_path_link_add_alter( CcPathLink ** pl_step__a,
                                     CcPathLink ** pl_alt__n );

// static CcMaybeBoolEnum _cc_path_link_subs_is_valid( CcPathLink * pl_subs );

// TODO :: DOCS
CcPathLink * cc_path_link_add_subs( CcPathLink ** pl_step__a,
                                    CcPathLink ** pl_sub__n );

// static bool _cc_path_link_steps_are_valid( CcStep * steps );

// static bool _cc_path_link_is_valid( CcPathLink * path_link, bool has_steps );

bool cc_path_link_is_valid( CcPathLink * path_link );

CcPathLink * cc_path_link_duplicate_all__new( CcPathLink * path_link );

bool cc_path_link_free_all( CcPathLink ** pl__f );

size_t cc_path_link_len( CcPathLink * path_link, bool count_all );

size_t cc_path_link_count_all_seqments( CcPathLink * path_link );

char * cc_path_link_node_to_string__new( CcPathLink * path_link_node );

// TODO :: rethink (maybe?)
// char * cc_path_link_to_string__new( CcPathLink * path_link );


#endif /* __CC_PATH_H__ */
