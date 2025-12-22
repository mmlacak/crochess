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
// Path node segments.

typedef struct CcPathNode {
    CcSideEffect side_effect; /* A possible side-effect on previous step, mostly on previously encountered piece. */

    CcStep * steps; /* Steps performed, fields visited. */

    CcPieceTagType encounter; /* Piece encountered at the very last field in the list above, and its tag. */

    CcActivationDesc act_desc; /* <i> Non-cached, stored data of CcPlyContext->act_desc as move, its plies progresses. */ /* Activation descriptor for a moving piece, its momentum usage and momentum it had after all performed steps. */

    struct CcPathNode * fork;
    struct CcPathNode * alt;
    struct CcPathNode * sub;
    struct CcPathNode * back__w; /* Back-link to parent node. */
} CcPathNode;

//
// Path node linkage.

typedef enum CcPathNodeLinkageEnum {
    CC_PNLE_None,
    CC_PNLE_Fork,
    CC_PNLE_Alt,
    CC_PNLE_Sub,
} CcPathNodeLinkageEnum;

#define CC_PATH_NODE_LINKAGE_IS_ENUMERATOR(pnle) ( ( CC_PNLE_None <= (pnle) ) && ( (pnle) <= CC_PNLE_Sub ) ) // <!> Keep in-sync with CcPathNodeLinkageEnum.

#define CC_PATH_NODE_LINKAGE_IS_VALID(pnle) ( ( CC_PNLE_None < (pnle) ) && ( (pnle) <= CC_PNLE_Sub ) ) // <!> Keep in-sync with CcPathNodeLinkageEnum.

#define CC_MAX_LEN_PATH_NODE_LINKAGE_STRING (4)

#define CC_SIZE_PATH_NODE_LINKAGE_STRING (CC_MAX_LEN_PATH_NODE_LINKAGE_STRING + 1)

char const * cc_path_node_linkage_as_string( CcPathNodeLinkageEnum pnle );

CcPathNodeLinkageEnum cc_path_node_linkage( CcPathNode * path_node );

char const * cc_path_node_linkage_to_string( CcPathNode * path_node );

//
// Path node functions.

CcPathNode * cc_path_node__new( CcSideEffect side_effect,
                                CcStep ** steps__d_n,
                                CcPieceTagType encounter,
                                CcActivationDesc act_desc );

CcPathNode * cc_path_node_add_forks( CcPathNode ** pn_step__a,
                                     CcPathNode ** pn_fork__n );

CcPathNode * cc_path_node_add_alters( CcPathNode ** pn_step__a,
                                      CcPathNode ** pn_alt__n );

// static CcMaybeBoolEnum _cc_path_node_subs_is_valid( CcPathNode * pn_subs );
CcPathNode * cc_path_node_add_subs( CcPathNode ** pn_step__a,
                                    CcPathNode ** pn_sub__n );

CcSideEffect * cc_path_node_last_step_side_effect( CcPathNode * path_node );

CcMaybeBoolEnum cc_path_node_last_step_side_effect_is_none( CcPathNode * path_node );

CcMaybeBoolEnum cc_path_node_is_leaf( CcPathNode * path_node );

CcMaybeBoolEnum cc_path_node_is_root( CcPathNode * path_node );

CcPathNode * cc_path_node_get_node( CcPathNode * path_node,
                                    CcPathNodeLinkageEnum preference );

// static bool _cc_path_node_steps_are_valid( CcStep * steps );
// static bool _cc_path_node_is_valid( CcPathNode * path_node, bool has_steps );
bool cc_path_node_is_valid( CcPathNode * path_node );

// static CcPathNode * _cc_path_node_duplicate_all__new( CcPathNode * path_root );
CcPathNode * cc_path_node_duplicate_all__new( CcPathNode * path_node );

// static bool _cc_path_node_free_all( CcPathNode ** path_root__f );
bool cc_path_node_free_all( CcPathNode ** path_node__f );

// static size_t _cc_path_node_count( CcPathNode * path_root );
size_t cc_path_node_count( CcPathNode * path_node );

size_t cc_path_node_count_all_segments( CcPathNode * path_node );

// static char * _cc_path_node_to_string__new( cc_uchar_t depth,
//                                             CcPathNode * path_root );
char * cc_path_node_to_string__new( CcPathNode * path_node );

//
// Path linked list.

typedef struct CcPathLink {
    struct CcPathNode * node__w;
    struct CcPathLink * next;
} CcPathLink;

CcPathLink * cc_path_link__new( CcPathNode * path_node );

CcPathLink * cc_path_link_append( CcPathLink ** path_link__iod_a,
                                  CcPathNode * path_node );

CcPathLink * cc_path_link_duplicate_all__new( CcPathLink * path_link );

CcPathLink * cc_path_link_extend( CcPathLink ** path_link__iod_a,
                                  CcPathLink ** path_link__n );

bool cc_path_link_free_all( CcPathLink ** path_link__f );

size_t cc_path_link_len( CcPathLink * path_link );

//
// Path tree iterator.

// static bool _cc_path_node_walk_to_leaf( CcPathNode * path_root,
//                                         CcPathLink ** root_to_leaf__iod );
// static bool _cc_path_node_iter_to_leaf( CcPathNode * path_root,
//                                         CcPathLink ** root_to_leaf__iod );
// TODO :: DOCS
bool cc_path_node_iter_to_leaf( CcPathNode * path_node,
                                CcPathLink ** root_to_leaf__iod );

//
// Linked path side-effects.

typedef struct CcPathSideEffectLink {
    CcPathNodeLinkageEnum link;
    CcSideEffect side_effect;
    struct CcPathSideEffectLink * next;
} CcPathSideEffectLink;

CcPathSideEffectLink * cc_path_side_effect_link__new( CcPathNodeLinkageEnum link,
                                                      CcSideEffect side_effect );

CcPathSideEffectLink * cc_path_side_effect_link_append( CcPathSideEffectLink ** side_effect_link__iod_a,
                                                        CcPathNodeLinkageEnum link,
                                                        CcSideEffect side_effect );

CcPathSideEffectLink * cc_path_side_effect_link_duplicate_all__new( CcPathSideEffectLink * side_effect_link );

CcPathSideEffectLink * cc_path_side_effect_link_extend( CcPathSideEffectLink ** side_effect_link__iod_a,
                                                        CcPathSideEffectLink ** side_effect_link__n );

bool cc_path_side_effect_link_free_all( CcPathSideEffectLink ** side_effect_link__f );

size_t cc_path_side_effect_link_len( CcPathSideEffectLink * side_effect_link );

char * cc_path_side_effect_link_to_string__new( CcPathSideEffectLink * side_effect_link );


#endif /* __CC_PATH_H__ */
