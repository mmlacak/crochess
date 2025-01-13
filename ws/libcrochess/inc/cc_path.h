// Copyright (c) 2024 Mario Mlačak, mmlacak@gmail.com
// Licensed under GNU GPL v3+ license. See LICENSING, COPYING files for details.

#ifndef __CC_PATH_H__
#define __CC_PATH_H__

#include <stdbool.h>

#include "cc_defines.h"
// #include "cc_str_utils.h"

// #include "cc_piece.h"
// #include "cc_tag.h"
// #include "cc_step.h"
#include "cc_side_effect.h"
#include "cc_pos.h"


//
// Linked path segments.

typedef struct CcPathLink {
    CcPosLink * steps;
    CcSideEffect side_effect;

    struct CcPathLink * fork;
    struct CcPathLink * alt;

    struct CcPathLink * back__w;
    struct CcPathLink * next;
} CcPathLink;

CcPathLink * cc_path_link__new( CcPosLink * steps, CcSideEffect side_effect );

CcPathLink * cc_path_link_append( CcPathLink ** pl__iod_a,
                                  CcPosLink * steps,
                                  CcSideEffect side_effect );

CcPathLink * cc_path_link_extend( CcPathLink ** pl__iod_a,
                                  CcPathLink ** pl__n );

CcPathLink * cc_path_link_fork( CcPathLink ** pl_step__a,
                                CcPathLink ** pl_fork__n );

CcPathLink * cc_path_link_alternate( CcPathLink ** pl_step__a,
                                     CcPathLink ** pl_alt__n );

// static bool _cc_path_link_steps_are_valid( CcPosLink * steps );

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
