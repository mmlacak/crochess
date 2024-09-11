// Copyright (c) 2024 Mario Mlačak, mmlacak@gmail.com
// Licensed under GNU GPL v3+ license. See LICENSING, COPYING files for details.

#ifndef __CC_PATH_H__
#define __CC_PATH_H__

#include <stdbool.h>

// #include "cc_defines.h"
// #include "cc_str_utils.h"

// #include "cc_piece.h"
// #include "cc_tag.h"
#include "cc_pos.h"


//
// Linked path segments.

typedef struct CcPathLink {
    CcPos pos;
    cc_uint_t momentum;

    struct CcPathLink * diverge;
    struct CcPathLink * alt;
    struct CcPathLink * next;
} CcPathLink;

CcPathLink * cc_path_link__new( CcPos pos, cc_uint_t momentum );

CcPathLink * cc_path_link_append( CcPathLink ** pl__iod_a,
                                  CcPos pos,
                                  cc_uint_t momentum );

CcPathLink * cc_path_link_extend( CcPathLink ** pl__iod_a,
                                  CcPathLink ** pl__n );

CcPathLink * cc_path_link_diverge( CcPathLink ** pl_step__a,
                                   CcPathLink ** pl_alt__n );

CcPathLink * cc_path_link_duplicate_all__new( CcPathLink * path_link );

bool cc_path_link_free_all( CcPathLink ** pl__f );

size_t cc_path_link_len( CcPathLink * path_link, bool count_all );

size_t cc_path_link_count_all_seqments( CcPathLink * path_link );

// static bool _cc_path_link_segment_to_string( CcPathLink * path_link,
//                                              size_t depth,
//                                              CcMaybeBoolEnum path_diverged,
//                                              char ** str__io_a,
//                                              char const * str_end );

// static char * _cc_path_link_to_string( CcPathLink * path_link,
//                                        size_t depth,
//                                        CcMaybeBoolEnum path_diverged,
//                                        char * str_start__io,
//                                        char const * str_end );

char * cc_path_link_to_string__new( CcPathLink * path_link );

//
// Linked list of path segments.

typedef struct CcPathWeakLink {
    CcPathLink * pl__w;

    struct CcPathWeakLink * next;
} CcPathWeakLink;

CcPathWeakLink * cc_path_weak_link__new( CcPathLink * pl );

CcPathWeakLink * cc_path_weak_link_append( CcPathWeakLink ** pwl__iod_a,
                                           CcPathLink * pl );

CcPathWeakLink * cc_path_weak_link_extend( CcPathWeakLink ** pwl__iod_a,
                                           CcPathWeakLink ** pwl__n );

bool cc_path_weak_link_free_all( CcPathWeakLink ** pwl__f );

size_t cc_path_weak_link_len( CcPathWeakLink * pwl );


#endif /* __CC_PATH_H__ */
