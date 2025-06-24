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

typedef struct CcPathLink {
    CcSideEffect side_effect; /* A possible side-effect on previous step, mostly on previously encountered piece. */

    CcStep * steps; /* Steps performed, fields visited. */

    CcPieceTagType encounter; /* Piece encountered at the very last field in the list above, and its tag. */

    CcActivationDesc act_desc; /* Activation descriptor for a moving piece, its momentum usage and momentum it had after all performed steps. */

    struct CcPathLink * fork;
    struct CcPathLink * alt;
    struct CcPathLink * sub;
    struct CcPathLink * next;
    struct CcPathLink * back__w; /* Back-link to parent node. */
} CcPathLink;

CcPathLink * cc_path_link__new( CcSideEffect side_effect,
                                CcStep ** steps__d_n,
                                CcPieceTagType encounter,
                                CcActivationDesc act_desc );

CcPathLink * cc_path_link_append( CcPathLink ** pl__iod_a,
                                  CcSideEffect side_effect,
                                  CcStep ** steps__d_n,
                                  CcPieceTagType encounter,
                                  CcActivationDesc act_desc );

CcPathLink * cc_path_link_extend( CcPathLink ** pl__iod_a,
                                  CcPathLink ** pl__n );

CcPathLink * cc_path_link_add_fork( CcPathLink ** pl_step__a,
                                    CcPathLink ** pl_fork__n );

CcPathLink * cc_path_link_add_alter( CcPathLink ** pl_step__a,
                                     CcPathLink ** pl_alt__n );

// static CcMaybeBoolEnum _cc_path_link_subs_is_valid( CcPathLink * pl_subs );

CcPathLink * cc_path_link_add_subs( CcPathLink ** pl_step__a,
                                    CcPathLink ** pl_sub__n );

// static bool _cc_path_link_steps_are_valid( CcStep * steps );

// static bool _cc_path_link_is_valid( CcPathLink * path_link, bool has_steps );

bool cc_path_link_is_valid( CcPathLink * path_link );

CcPathLink * cc_path_link_duplicate_all__new( CcPathLink * path_link );

bool cc_path_link_free_all( CcPathLink ** pl__f );

size_t cc_path_link_len( CcPathLink * path_link, bool count_all );

size_t cc_path_link_count_all_seqments( CcPathLink * path_link );

//
// Node linkage.

typedef enum CcPathLinkNodeLinkageEnum {
    CC_PLNLE_NoLinkage,
    CC_PLNLE_Fork,
    CC_PLNLE_Alt,
    CC_PLNLE_Sub,
    CC_PLNLE_Next,
} CcPathLinkNodeLinkageEnum;

#define CC_PATH_LINK_NODE_LINKAGE_IS_ENUMERATOR(plnle) ( ( CC_PLNLE_NoLinkage <= (plnle) ) && ( (plnle) <= CC_PLNLE_Next ) ) // <!> Keep in-sync with CcPathLinkNodeLinkageEnum.

#define CC_PATH_LINK_NODE_LINKAGE_IS_VALID(plnle) CC_PATH_LINK_NODE_LINKAGE_IS_ENUMERATOR( (plnle) ) // All enumerations are also valid.

#define CC_MAX_LEN_PATH_LINK_NODE_LINKAGE_STRING (4)

#define CC_SIZE_PATH_LINK_NODE_LINKAGE_STRING (CC_MAX_LEN_PATH_LINK_NODE_LINKAGE_STRING + 1)

char const * cc_path_link_node_linkage_as_string( CcPathLinkNodeLinkageEnum plnle );

CcPathLinkNodeLinkageEnum cc_path_link_node_linkage( CcPathLink * path_link_node );

char * cc_path_link_node_to_string__new( cc_uchar_t depth,
                                         CcPathLink * path_link_node );

// TODO :: rethink (maybe?)
// char * cc_path_link_to_string__new( CcPathLink * path_link );


#endif /* __CC_PATH_H__ */
