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

typedef struct CcPathNode {
    CcSideEffect side_effect; /* A possible side-effect on previous step, mostly on previously encountered piece. */

    CcStep * steps; /* Steps performed, fields visited. */

    CcPieceTagType encounter; /* Piece encountered at the very last field in the list above, and its tag. */

    CcActivationDesc act_desc; /* <i> Non-cached, stored data of CcPlyContext->act_desc as move, its plies progresses. */ /* Activation descriptor for a moving piece, its momentum usage and momentum it had after all performed steps. */

    struct CcPathNode * fork;
    struct CcPathNode * alt;
    struct CcPathNode * sub;
    // struct CcPathNode * next; // TODO :: DELETE :: DOCS
    struct CcPathNode * back__w; /* Back-link to parent node. */
} CcPathNode;

CcPathNode * cc_path_node__new( CcSideEffect side_effect,
                                CcStep ** steps__d_n,
                                CcPieceTagType encounter,
                                CcActivationDesc act_desc );

CcPathNode * cc_path_node_add_fork( CcPathNode ** pl_step__a,
                                    CcPathNode ** pl_fork__n );

CcPathNode * cc_path_node_add_alter( CcPathNode ** pl_step__a,
                                     CcPathNode ** pl_alt__n );

// static CcMaybeBoolEnum _cc_path_node_subs_is_valid( CcPathNode * pl_subs );

CcPathNode * cc_path_node_add_subs( CcPathNode ** pl_step__a,
                                    CcPathNode ** pl_sub__n );

CcSideEffect * cc_path_node_last_step_side_effect( CcPathNode * pl_node );

CcMaybeBoolEnum cc_path_node_last_step_side_effect_is_none( CcPathNode * pl_node );

CcMaybeBoolEnum cc_path_node_is_leaf( CcPathNode * pl_node );

// static bool _cc_path_node_steps_are_valid( CcStep * steps );

// static bool _cc_path_node_is_valid( CcPathNode * path_link, bool has_steps );

bool cc_path_node_is_valid( CcPathNode * path_link );

CcPathNode * cc_path_node_duplicate_all__new( CcPathNode * path_link );

bool cc_path_node_free_all( CcPathNode ** pl__f );

// TODO :: DOCS :: bool count_all
size_t cc_path_node_len( CcPathNode * path_link );

size_t cc_path_node_count_all_segments( CcPathNode * path_link );

char * cc_path_node_to_string__new( cc_uchar_t depth,
                                         CcPathNode * path_link_node );

// TODO :: rethink (maybe?)
// char * cc_path_node_to_string__new( CcPathNode * path_link );

//
// Node linkage.

typedef enum CcPathLinkNodeLinkageEnum {
    CC_PLNLE_NoLinkage,
    CC_PLNLE_Fork,
    CC_PLNLE_Alt,
    CC_PLNLE_Sub,
} CcPathLinkNodeLinkageEnum;

#define CC_PATH_LINK_NODE_LINKAGE_IS_ENUMERATOR(plnle) ( ( CC_PLNLE_NoLinkage <= (plnle) ) && ( (plnle) <= CC_PLNLE_Sub ) ) // <!> Keep in-sync with CcPathLinkNodeLinkageEnum.

#define CC_PATH_LINK_NODE_LINKAGE_IS_VALID(plnle) CC_PATH_LINK_NODE_LINKAGE_IS_ENUMERATOR( (plnle) ) // All enumerations are also valid.

#define CC_MAX_LEN_PATH_LINK_NODE_LINKAGE_STRING (4)

#define CC_SIZE_PATH_LINK_NODE_LINKAGE_STRING (CC_MAX_LEN_PATH_LINK_NODE_LINKAGE_STRING + 1)

char const * cc_path_node_linkage_as_string( CcPathLinkNodeLinkageEnum plnle );

CcPathLinkNodeLinkageEnum cc_path_node_linkage( CcPathNode * path_link_node );

char const * cc_path_node_linkage_to_string( CcPathNode * path_link_node );

//
// Linked path side-effects.

typedef struct CcPathSideEffectLink {
    CcPathLinkNodeLinkageEnum link;
    CcSideEffect side_effect;
    struct CcPathSideEffectLink * next;
} CcPathSideEffectLink;

CcPathSideEffectLink * cc_path_side_effect_link__new( CcPathLinkNodeLinkageEnum link,
                                                      CcSideEffect side_effect );

CcPathSideEffectLink * cc_path_side_effect_link_append( CcPathSideEffectLink ** side_effect_link__iod_a,
                                                        CcPathLinkNodeLinkageEnum link,
                                                        CcSideEffect side_effect );

CcPathSideEffectLink * cc_path_side_effect_link_duplicate_all__new( CcPathSideEffectLink * side_effect_link );

CcPathSideEffectLink * cc_path_side_effect_link_extend( CcPathSideEffectLink ** side_effect_link__iod_a,
                                                        CcPathSideEffectLink ** side_effect_link__n );

bool cc_path_side_effect_link_free_all( CcPathSideEffectLink ** side_effect_link__f );

size_t cc_path_side_effect_link_len( CcPathSideEffectLink * side_effect_link );

char * cc_path_side_effect_link_to_string__new( CcPathSideEffectLink * side_effect_link );


#endif /* __CC_PATH_H__ */
