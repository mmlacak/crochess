// Copyright (c) 2024 Mario Mlačak, mmlacak@gmail.com
// Licensed under GNU GPL v3+ license. See LICENSING, COPYING files for details.

#include <stdlib.h>
// #include <string.h>
// #include <stdio.h>

// #include "cc_defines.h"
// #include "cc_math.h"
// #include "cc_pos.h"
#include "cc_path.h"


//
// Path node linkage.

char const * cc_path_node_linkage_as_string( CcPathNodeLinkageEnum pnle ) {
    switch ( pnle ) {
        case CC_PNLE_None : return "";
        case CC_PNLE_Fork : return "< ";
        case CC_PNLE_Alt : return "^ ";
        case CC_PNLE_Sub : return "% ";
        default : return "? ";
    }
}

CcPathNodeLinkageEnum cc_path_node_linkage( CcPathNode * path_node ) {
    if ( !path_node ) return CC_PNLE_None;

    CcPathNode * pln = path_node->back__w;

    if ( !pln ) return CC_PNLE_None;

    if ( pln->fork == path_node )
        return CC_PNLE_Fork;
    else if ( pln->alt == path_node )
        return CC_PNLE_Alt;
    else if ( pln->sub == path_node )
        return CC_PNLE_Sub;
    else
        return CC_PNLE_None;
}

char const * cc_path_node_linkage_to_string( CcPathNode * path_node ) {
    CcPathNodeLinkageEnum pnle = cc_path_node_linkage( path_node );
    return cc_path_node_linkage_as_string( pnle );
}

//
// Path node functions.

CcPathNode * cc_path_node__new( CcSideEffect side_effect,
                                CcStep ** steps__d_n,
                                CcPieceTagType encounter,
                                CcActivationDesc act_desc ) {
    CcPathNode * pl__t = CC_MALLOC( sizeof( CcPathNode ) );
    if ( !pl__t ) return NULL;

    pl__t->side_effect = side_effect;

    if ( steps__d_n && *steps__d_n ) {
        pl__t->steps = *steps__d_n;
        *steps__d_n = NULL;
    } else
        pl__t->steps = NULL;

    pl__t->encounter = encounter;

    pl__t->act_desc = act_desc;

    pl__t->visited = false;
    pl__t->yielded = false;

    pl__t->fork = NULL;
    pl__t->alt = NULL;
    pl__t->sub = NULL;
    pl__t->back__w = NULL;

    return pl__t;
}

CcPathNode * cc_path_node_add_forks( CcPathNode ** pn_step__a,
                                     CcPathNode ** pn_fork__n ) {
    if ( !pn_step__a ) return NULL;
    if ( !*pn_step__a ) return NULL;

    if ( !pn_fork__n ) return NULL;
    if ( !*pn_fork__n ) return NULL;

    // Sanity checks, back-links shouldn't point to somewhere else ...
    if ( ( *pn_step__a )->back__w ) return NULL;
    if ( ( *pn_fork__n )->back__w ) return NULL;

    if ( ( *pn_step__a )->fork ) {
        CcPathNode * pl = ( *pn_step__a )->fork;

        while ( pl->alt ) {
            pl = pl->alt;
        }

        pl->alt = *pn_fork__n;
        ( *pn_fork__n )->back__w = pl;
    } else {
        ( *pn_step__a )->fork = *pn_fork__n;
        ( *pn_fork__n )->back__w = *pn_step__a;
    }

    // Ownership transferred.
    CcPathNode * pl__w = *pn_fork__n;
    *pn_fork__n = NULL;

    return pl__w;
}

CcPathNode * cc_path_node_add_alters( CcPathNode ** pn_step__a,
                                      CcPathNode ** pn_alt__n ) {
    if ( !pn_step__a ) return NULL;
    if ( !*pn_step__a ) return NULL;

    if ( !pn_alt__n ) return NULL;
    if ( !*pn_alt__n ) return NULL;

    // Sanity checks, back-links shouldn't point to somewhere else ...
    if ( ( *pn_step__a )->back__w ) return NULL;
    if ( ( *pn_alt__n )->back__w ) return NULL;

    CcPathNode * pl__w = *pn_step__a;

    while ( pl__w->alt ) {
        pl__w = pl__w->alt;
    }

    // Ownership transfer.
    pl__w->alt = *pn_alt__n;
    ( *pn_alt__n )->back__w = pl__w;

    *pn_alt__n = NULL;

    return pl__w;
}

static CcMaybeBoolEnum _cc_path_node_subs_is_valid( CcPathNode * pn_subs ) {
    if ( !pn_subs ) return CC_MBE_Void;

    CcPathNode * pl = pn_subs;
    CcPathNode * old = NULL;

    while ( pl ) {
        // For 1st node ( pl == pn_subs ), checks there is no back-link ...
        // Otherwise, checks back-link points to its parent ...
        if ( pl->back__w != old ) return CC_MBE_Void;

        if ( pl->steps ) return CC_MBE_False;
        if ( pl->fork ) return CC_MBE_False;
        if ( pl->alt ) return CC_MBE_False;

        if ( pl->encounter != CC_PTE_None ) return CC_MBE_False;

        if ( !CC_SIDE_EFFECT_TYPE_IS_VALID( pl->side_effect.type ) )
            return CC_MBE_False;

        old = pl;
        pl = pl->sub;
    }

    return CC_MBE_True;
}

CcPathNode * cc_path_node_add_subs( CcPathNode ** pn_step__a,
                                    CcPathNode ** pn_sub__n ) {
    if ( !pn_step__a ) return NULL;
    if ( !*pn_step__a ) return NULL;

    if ( !pn_sub__n ) return NULL;
    if ( !*pn_sub__n ) return NULL;

    // Sanity checks.
    if ( ( *pn_step__a )->back__w ) return NULL;
    if ( _cc_path_node_subs_is_valid( *pn_sub__n ) != CC_MBE_True )
        return NULL;

    CcPathNode * pl__w = *pn_step__a;

    while ( pl__w->sub ) {
        pl__w = pl__w->sub;
    }

    // Ownership transfer.
    pl__w->sub = *pn_sub__n;
    ( *pn_sub__n )->back__w = pl__w;

    *pn_sub__n = NULL;

    return pl__w;
}

CcSideEffect * cc_path_node_last_step_side_effect( CcPathNode * path_node ) {
    if ( !path_node ) return NULL;
    if ( !path_node->steps ) return NULL;

    CcStep * s = path_node->steps;

    CC_FASTFORWARD( s );

    return &( s->side_effect );
}

CcMaybeBoolEnum cc_path_node_last_step_side_effect_is_valid( CcPathNode * path_node,
                                                             bool include_none ) {
    if ( !path_node ) return CC_MBE_Void;

    CcSideEffect * se__w = cc_path_node_last_step_side_effect( path_node );
    if ( !se__w ) return CC_MBE_Void;

    if ( !CC_SIDE_EFFECT_TYPE_IS_ENUMERATOR( se__w->type ) ) return CC_MBE_False;

    return include_none ? CC_MBE_True
                        : CC_BOOL_TO_MAYBE( se__w->type != CC_SETE_None );
}

CcMaybeBoolEnum cc_path_node_is_leaf( CcPathNode * path_node ) {
    if ( !path_node ) return CC_MBE_Void;

    // if ( path_node->fork || path_node->alt || path_node->sub )
    if ( CC_PATH_NODE_IS_PARENT( path_node ) )
        return CC_MBE_False;

    return CC_MBE_True;
}

CcMaybeBoolEnum cc_path_node_is_root( CcPathNode * path_node ) {
    if ( !path_node ) return CC_MBE_Void;

    return path_node->back__w ? CC_MBE_False
                              : CC_MBE_True;
}

static bool _cc_path_node_set_all_flags( CcPathNode * path_node__io, bool visited, bool emitted ) {
    if ( !path_node__io ) return false;

    CcPathNode * pn = path_node__io;
    bool result = true;

    if ( pn ) {
        pn->visited = visited;
        pn->yielded = visited && emitted;

        if ( pn->fork )
            result = _cc_path_node_set_all_flags( pn->fork, visited, emitted ) && result;

        if ( pn->alt )
            result = _cc_path_node_set_all_flags( pn->alt, visited, emitted ) && result;

        if ( pn->sub )
            result = _cc_path_node_set_all_flags( pn->sub, visited, emitted ) && result;
    }

    return result;
}

bool cc_path_node_set_all_flags( CcPathNode * path_tree__io,
                                 bool visited,
                                 bool emitted ) {
    if ( !path_tree__io ) return false;

    CcPathNode * root = path_tree__io;

    CC_REWIND_BY( root, root->back__w );

    return _cc_path_node_set_all_flags( root, visited, emitted );
}

bool cc_path_node_iter_init( CcPathNode ** path_node__io ) {
    if ( !path_node__io ) return false;
    if ( !*path_node__io ) return false;

    CcPathNode * root = *path_node__io;

    CC_REWIND_BY( root, root->back__w );

    *path_node__io = root;

    return CC_PATH_NODE_RESET_ALL_FLAGS( *path_node__io );
}

CcMaybeBoolEnum cc_path_node_subflags_are_all_set( CcPathNode * path_node, bool check_yielded ) { // todo :: MAYBE :: RETHINK :: recursion while yielding
    if ( !path_node ) return CC_MBE_Void;

    if ( check_yielded ) {
        if ( !path_node->yielded )
            return CC_MBE_False;
    } else {
        if ( !path_node->visited )
            return CC_MBE_False;
    }

    if ( path_node->fork )
        if ( !cc_path_node_subflags_are_all_set( path_node->fork, check_yielded ) )
            return CC_MBE_False;

    if ( path_node->alt )
        if ( !cc_path_node_subflags_are_all_set( path_node->alt, check_yielded ) )
            return CC_MBE_False;

    if ( path_node->sub )
        if ( !cc_path_node_subflags_are_all_set( path_node->sub, check_yielded ) )
            return CC_MBE_False;

    return CC_MBE_True;
}

CcMaybeBoolEnum cc_path_node_iter_next( CcPathNode ** path_node__io ) {
    if ( !path_node__io ) return CC_MBE_Void;
    if ( !*path_node__io ) return CC_MBE_Void;

    CcPathNode * pn = *path_node__io;

    #ifdef __CC_DEBUG__
    {
        char * steps_str__a = cc_step_all_to_string__new( pn->steps );
        printf( "%p->%d|%d:%d|%d:%d|'%s'.\n",
                (void*)pn, !CC_PATH_NODE_HAS_CONTINUATION( pn ),
                pn->visited, CC_PATH_NODE_ALL_SUBNODES_ARE_VISITED( pn ),
                pn->yielded, CC_PATH_NODE_ALL_SUBNODES_ARE_YIELDED( pn ),
                steps_str__a );
        CC_FREE_AND_NULL( &steps_str__a );
    }
    #endif // __CC_DEBUG__

    if ( !pn->visited || !pn->yielded ) {
        pn->visited = true;
        if ( !CC_PATH_NODE_HAS_CONTINUATION( pn ) ) {
            pn->yielded = true;
            return CC_MBE_True;
        }

        CcMaybeBoolEnum has_side_effect = cc_path_node_last_step_side_effect_is_valid( pn, false );
        if ( has_side_effect != CC_MBE_False ) {
            pn->yielded = true;
            return has_side_effect;
        }
    } else {
        if ( CC_PATH_NODE_ALL_SUBNODES_ARE_VISITED( pn ) )
            return CC_MBE_False;
    }

    CcMaybeBoolEnum result = CC_MBE_Void;

    if ( pn->fork && ( CC_PATH_NODE_ALL_SUBNODES_ARE_YIELDED( pn->fork ) != CC_MBE_True ) ) {
        *path_node__io = pn->fork;
        result = cc_path_node_iter_next( path_node__io );
        if ( result != CC_MBE_False ) return result;
    }

    if ( pn->alt && ( CC_PATH_NODE_ALL_SUBNODES_ARE_YIELDED( pn->alt ) != CC_MBE_True ) ) {
        *path_node__io = pn->alt;
        result = cc_path_node_iter_next( path_node__io );
        if ( result != CC_MBE_False ) return result;
    }

    if ( pn->sub && ( CC_PATH_NODE_ALL_SUBNODES_ARE_YIELDED( pn->sub ) != CC_MBE_True ) ) {
        *path_node__io = pn->sub;
        result = cc_path_node_iter_next( path_node__io );
        if ( result != CC_MBE_False ) return result;
    }

    if ( pn->back__w ) {
        *path_node__io = pn->back__w;
        result = cc_path_node_iter_next( path_node__io );
        if ( result != CC_MBE_False ) return result;
    }

    return CC_MBE_False;
}

static bool _cc_path_node_steps_are_valid( CcStep * steps ) {
    if ( !steps ) return false;

    CcStep * s = steps;

    while ( s ) {
        if ( !CC_STEP_LINK_TYPE_IS_VALID( s->link ) ) return false;
        if ( !CC_POS_IS_VALID( s->field ) ) return false;
        if ( !CC_SIDE_EFFECT_TYPE_IS_ENUMERATOR( s->side_effect.type ) ) return false;

        s = s->next;
    }

    return true;
}

static bool _cc_path_node_is_valid( CcPathNode * path_node, bool has_steps ) {
    if ( !path_node ) return false;

    CcPathNode * pl = path_node;

    if ( !CC_SIDE_EFFECT_TYPE_IS_ENUMERATOR( pl->side_effect.type ) ) return false;

    if ( CC_SIDE_EFFECT_TYPE_TERMINATES_PLY( pl->side_effect.type ) ) {
        // If path isn't actually terminating ...
        if ( pl->steps || pl->fork || pl->alt ) return false;
    }

    if ( pl->steps ) { // If there is a path segment, it has to be valid.
        if ( !_cc_path_node_steps_are_valid( pl->steps ) )
            return false;
    } else { // Final side-effect (e.g. a capture) terminating a path.
        if ( !CC_SIDE_EFFECT_TYPE_CAN_TERMINATE_PLY( pl->side_effect.type ) ) return false;

        // If path isn't actually terminating ...
        if ( pl->fork || pl->alt ) return false;
    }

    if ( !CC_PIECE_IS_ENUMERATOR( pl->encounter ) )
        return false;

    if ( !CC_MOMENTUM_USAGE_IS_ENUMERATOR( pl->act_desc.usage ) )
        return false;

    //
    // Check links.

    int links = 0;

    if ( pl->fork ) {
        if ( pl->fork->back__w != pl ) return false;
        if ( !_cc_path_node_is_valid( pl->fork, true ) ) return false;
        ++links;
    }

    if ( pl->alt ) {
        if ( pl->alt->back__w != pl ) return false;
        if ( !_cc_path_node_is_valid( pl->alt, true ) ) return false;
        ++links;
    }

    if ( pl->sub ) {
        if ( pl->sub->back__w != pl ) return false;
        if ( _cc_path_node_subs_is_valid( pl->sub ) != CC_MBE_True ) return false;
        ++links;
    }

    if ( links == 0 )
        return has_steps; // No links --> terminal node.
        // If also root node, it should not be terminal, without having at least initial and terminal steps.

    return true;
}

bool cc_path_node_is_valid( CcPathNode * path_tree ) {
    if ( !path_tree ) return false;

    CcPathNode * root = path_tree;

    CC_REWIND_BY( root, root->back__w );

    bool has_steps = ( cc_step_count( root->steps ) > 1 ); // Initial step should not be the only one, if root is the only node.

    if ( !_cc_path_node_is_valid( root, has_steps ) ) return false;

    return true;
}

static CcPathNode * _cc_path_node_duplicate_all__new( CcPathNode * path_node ) {
    if ( !path_node ) return NULL;

    CcPathNode * pl__a = NULL;
    CcPathNode * from = path_node;

    if ( from ) {
        CcStep * steps__t = cc_step_duplicate_all__new( from->steps );
        if ( !steps__t ) return NULL;

        pl__a = cc_path_node__new( from->side_effect,
                                   &steps__t, // Ownership transfer, do not free( steps__t ).
                                   from->encounter,
                                   from->act_desc );
        if ( !pl__a ) {
            cc_step_free_all( &steps__t );
            return NULL;
        }

        if ( from->fork ) {
            if ( ( pl__a->fork = _cc_path_node_duplicate_all__new( from->fork ) ) ) {
                pl__a->fork->back__w = pl__a;
            } else {
                cc_path_node_free_all( &pl__a );
                return NULL;
            }
        }

        if ( from->alt ) {
            if ( ( pl__a->alt = _cc_path_node_duplicate_all__new( from->alt ) ) ) {
                pl__a->alt->back__w = pl__a;
            } else {
                cc_path_node_free_all( &pl__a );
                return NULL;
            }
        }

        if ( from->sub ) {
            if ( ( pl__a->sub = _cc_path_node_duplicate_all__new( from->sub ) ) ) {
                pl__a->sub->back__w = pl__a;
            } else {
                cc_path_node_free_all( &pl__a );
                return NULL;
            }
        }
    }

    return pl__a;
}

CcPathNode * cc_path_node_duplicate_all__new( CcPathNode * path_tree ) {
    if ( !path_tree ) return NULL;

    CcPathNode * pn = path_tree;

    CC_REWIND_BY( pn, pn->back__w );

    return _cc_path_node_duplicate_all__new( pn );
}

static bool _cc_path_node_free_all( CcPathNode ** path_node__f ) {
    if ( !path_node__f ) return false;
    if ( !*path_node__f ) return true;

    bool result = true;
    CcPathNode * pl = *path_node__f;

    if ( pl ) {
        result = cc_step_free_all( &( pl->steps ) ) && result;

        if ( pl->fork )
            result = _cc_path_node_free_all( &( pl->fork ) ) && result;

        if ( pl->alt )
            result = _cc_path_node_free_all( &( pl->alt ) ) && result;

        if ( pl->sub )
            result = _cc_path_node_free_all( &( pl->sub ) ) && result;

        // <!> pl->back__w is weak pointer, not an owner, so it must not be free()-ed.

        CC_FREE( pl );
    }

    *path_node__f = NULL;
    return result;
}

bool cc_path_node_free_all( CcPathNode ** path_tree__f ) {
    if ( !path_tree__f ) return false;
    if ( !*path_tree__f ) return true;

    CcPathNode * pl = *path_tree__f;

    CC_REWIND_BY( pl, pl->back__w );

    bool result = _cc_path_node_free_all( &pl );

    *path_tree__f = NULL;
    return result;
}

static size_t _cc_path_node_count( CcPathNode * path_node ) {
    if ( !path_node ) return 0;

    size_t count = 0;
    CcPathNode * pl = path_node;

    if ( pl ) {
        ++count;

        if ( pl->fork ) {
            count += _cc_path_node_count( pl->fork );
        }

        if ( pl->alt ) {
            count += _cc_path_node_count( pl->alt );
        }

        if ( pl->sub ) {
            count += _cc_path_node_count( pl->sub );
        }
    }

    return count;
}

size_t cc_path_node_count( CcPathNode * path_tree ) {
    if ( !path_tree ) return 0;

    CcPathNode * pl = path_tree;

    CC_REWIND_BY( pl, pl->back__w );

    return _cc_path_node_count( pl );
}

static size_t _cc_path_node_count_all_segments( CcPathNode * path_node ) { // TODO :: RETHINK :: ???
    if ( !path_node ) return 0;

    CcPathNode * pl = path_node;

    size_t count = ( pl->steps ) ? 1 : 0; // TODO :: RETHINK :: length of steps ???

    if ( pl ) {
        if ( pl->fork ) {
            count += _cc_path_node_count_all_segments( pl->fork );
        }

        if ( pl->alt ) {
            count += _cc_path_node_count_all_segments( pl->alt );
        }

        // Substitute paths should not contain path segments.
        //
        // if ( pl->sub ) {
        //     count += _cc_path_node_count_all_segments( pl->sub );
        // }
    }

    return count;
}

size_t cc_path_node_count_all_segments( CcPathNode * path_tree ) { // TODO :: RETHINK :: ???
    if ( !path_tree ) return 0;

    CcPathNode * pl = path_tree;

    CC_REWIND_BY( pl, pl->back__w );

    return _cc_path_node_count_all_segments( pl );
}

static char * _cc_path_node_to_string__new( cc_uchar_t depth,
                                            CcPathNode * path_node ) {
    if ( !path_node ) return NULL;

    cc_uint_t tabs_len = 2 * depth; // Depth --> 2-spaces.
    char * tabs_str__a = cc_str_pad__new( ' ', tabs_len );
    if ( !tabs_str__a ) return NULL;

    char const * plnle_str = cc_path_node_linkage_to_string( path_node );
    cc_char_16 se_str = CC_CHAR_16_EMPTY;

    if ( !cc_side_effect_to_str( path_node->side_effect, &se_str ) )
        return NULL;

    char * steps_str__a = cc_step_all_to_string__new( path_node->steps );
    if ( !steps_str__a ) {
        CC_FREE( tabs_str__a );
        return NULL;
    }

    size_t str_len = cc_str_len( tabs_str__a, NULL, tabs_len );
    str_len += cc_str_len( plnle_str, NULL, CC_SIZE_PATH_NODE_LINKAGE_STRING );
    str_len += cc_str_len( steps_str__a, NULL, CC_SIZE_BUFFER );
    str_len += cc_str_len( se_str, NULL, CC_SIZE_CHAR_16 );
    str_len += 2 + 1 + 1 + 2; // For .encounter (i.e. CcPieceTagType); +2 for ' @', +1 for piece symbol, +1 for tag char, +2 for '|'.
    str_len += 3 + CC_SIZE_CHAR_32; // For .act_desc (i.e. CcActivationDesc); +3 for ' #' preceeding act desc.
    str_len += 2; // +2 for new line char(s), any of \LF (Linux, ...), \CR\LF (Windows, ...), or \CR (others)

    size_t str_size = str_len + 1; // +1 for '\0'

    char * pln_str__t = CC_MALLOC( str_size );
    if ( !pln_str__t ) {
        CC_FREE( tabs_str__a );
        CC_FREE( steps_str__a );
        return NULL;
    }

    char * pln_str = pln_str__t;
    char const * pln_end__w = pln_str__t + str_size;

    // Tabs.
    char const * end_tabs_str__w = cc_str_append_into( pln_str, pln_end__w, CC_SIZE_IGNORE, tabs_str__a, NULL, tabs_len );
    if ( !end_tabs_str__w ) {
        CC_FREE( tabs_str__a );
        CC_FREE( steps_str__a );
        CC_FREE( pln_str__t );
        return NULL;
    }
    pln_str = (char *)end_tabs_str__w;

    // Node linkage.
    char const * end_plnle_str__w = cc_str_append_into( pln_str, pln_end__w, CC_SIZE_IGNORE, plnle_str, NULL, CC_SIZE_PATH_NODE_LINKAGE_STRING );
    if ( !end_plnle_str__w ) {
        CC_FREE( tabs_str__a );
        CC_FREE( steps_str__a );
        CC_FREE( pln_str__t );
        return NULL;
    }
    pln_str = (char *)end_plnle_str__w;

    // Side-effect.
    char const * end_se_str__w = cc_str_append_into( pln_str, pln_end__w, CC_SIZE_IGNORE, se_str, NULL, CC_SIZE_CHAR_16 );
    if ( !end_se_str__w ) {
        CC_FREE( tabs_str__a );
        CC_FREE( steps_str__a );
        CC_FREE( pln_str__t );
        return NULL;
    }
    pln_str = (char *)end_se_str__w;

    // Steps.
    char const * end_steps__w = cc_str_append_into( pln_str, pln_end__w, CC_SIZE_IGNORE, steps_str__a, NULL, CC_SIZE_BUFFER );
    if ( !end_steps__w ) {
        CC_FREE( tabs_str__a );
        CC_FREE( steps_str__a );
        CC_FREE( pln_str__t );
        return NULL;
    }
    pln_str = (char *)end_steps__w;

    // Encountered piece and its tag.
    char piece_symbol = cc_piece_as_char( path_node->encounter );
    char tag_chr = cc_tag_as_char( path_node->encounter );

    *pln_str++ = '|';
    *pln_str++ = '@';
    *pln_str++ = piece_symbol;
    *pln_str++ = tag_chr;
    *pln_str = '\0';

    // Activation descriptor.
    cc_char_32 act_desc_str = CC_CHAR_32_EMPTY;

    if ( !cc_activation_desc_as_string( path_node->act_desc, &act_desc_str ) ) {
        CC_FREE( tabs_str__a );
        CC_FREE( steps_str__a );
        CC_FREE( pln_str__t );
        return NULL;
    }

    *pln_str++ = '|';
    // *pln_str++ = '#';
    *pln_str = '\0';

    char const * end_act_desc__w = cc_str_append_into( pln_str, pln_end__w, CC_SIZE_IGNORE, act_desc_str, NULL, CC_SIZE_CHAR_32 );
    if ( !end_act_desc__w ) {
        CC_FREE( tabs_str__a );
        CC_FREE( steps_str__a );
        CC_FREE( pln_str__t );
        return NULL;
    }
    // pln_str = (char *)end_act_desc__w; // Not needed anymore.

    CC_FREE( tabs_str__a );
    CC_FREE( steps_str__a );

    // Recursive stuff.
    char * pln_fork__t = NULL;
    char * pln_alt__t = NULL;
    char * pln_sub__t = NULL;
    char const * fmt = "\n%s\n%s\n%s";
    cc_uchar_t new_depth = depth + 1; // depth + 1 --> all forks, alts, sub path nodes are sub-nodes

    cc_uint_t str_len_empty = 2 * new_depth;
    // 2* --> 2 spaces for each tabulation

    cc_uint_t str_size_empty = str_len_empty + 1;
    // +1 --> '\0', i.e. null-terminating char

    if ( path_node->fork ) {
        pln_fork__t = _cc_path_node_to_string__new( new_depth, path_node->fork );
        if ( !pln_fork__t ) {
           CC_FREE( pln_str__t );
           return NULL;
        }
    } else {
        pln_fork__t = cc_str_pad__new( ' ', str_size_empty );
        if ( !pln_fork__t ) {
           CC_FREE( pln_str__t );
           return NULL;
        }

        *( pln_fork__t + str_len_empty ) = '<';
    }

    if ( path_node->alt ) {
        pln_alt__t = _cc_path_node_to_string__new( new_depth, path_node->alt );
        if ( !pln_alt__t ) {
            CC_FREE( pln_fork__t );
            CC_FREE( pln_str__t );
            return NULL;
        }
    } else {
        pln_alt__t = cc_str_pad__new( ' ', str_size_empty );
        if ( !pln_alt__t ) {
            CC_FREE( pln_fork__t );
            CC_FREE( pln_str__t );
            return NULL;
        }

        *( pln_alt__t + str_len_empty ) = '^';
    }

    if ( path_node->sub ) {
        pln_sub__t = _cc_path_node_to_string__new( new_depth, path_node->sub );
        if ( !pln_sub__t ) {
            CC_FREE( pln_alt__t );
            CC_FREE( pln_fork__t );
            CC_FREE( pln_str__t );
            return NULL;
        }
    } else {
        pln_sub__t = cc_str_pad__new( ' ', str_size_empty );
        if ( !pln_sub__t ) {
            CC_FREE( pln_alt__t );
            CC_FREE( pln_fork__t );
            CC_FREE( pln_str__t );
            return NULL;
        }

        *( pln_sub__t + str_len_empty ) = '%';
    }

    char * pln_str__a = NULL;

    if ( path_node->fork || path_node->alt || path_node->sub ) {
        // <!> pln_str__t ownership is transferred in-function, do not free( pln_str__t ) afterwards.
        //     Other strings do not have their ownership transferred, so do free() those.
        pln_str__a = cc_str_append_fmt__new( &pln_str__t, CC_MAX_LEN_ZERO_TERMINATED, fmt, pln_fork__t, pln_alt__t, pln_sub__t );
    } else {
        pln_str__a = pln_str__t; // Ownership transfer, do not free( pln_str__t ).
    }

    CC_FREE( pln_sub__t );
    CC_FREE( pln_alt__t );
    CC_FREE( pln_fork__t );

    return pln_str__a;
}

char * cc_path_node_to_string__new( CcPathNode * path_tree ) {
    if ( !path_tree ) return NULL;

    CcPathNode * pn = path_tree;

    CC_REWIND_BY( pn, pn->back__w );

    return _cc_path_node_to_string__new( 0, pn );
}

//
// Path linked list.

CcPathLink * cc_path_link__new( CcPathNode * path_node ) {
    if ( !path_node ) return NULL;

    CcPathLink * pl__t = CC_MALLOC( sizeof( CcPathLink ) );
    if ( !pl__t ) return NULL;

    pl__t->node__w = path_node;
    pl__t->next = NULL;

    return pl__t;
}

CcPathLink * cc_path_link_prepend( CcPathLink ** path_link__iod_a,
                                   CcPathNode * path_node ) {
    if ( !path_link__iod_a ) return NULL;

    CcPathLink * pl__t = cc_path_link__new( path_node );
    if ( !pl__t ) return NULL;

    if ( !*path_link__iod_a ) {
        *path_link__iod_a = pl__t; // Ownership transfer.
    } else {
        CcPathLink * pl = *path_link__iod_a;
        pl__t->next = pl; // Prepend.
        *path_link__iod_a = pl__t; // Ownership transfer.
    }

    return pl__t; // Weak pointer.
}

CcPathLink * cc_path_link_append( CcPathLink ** path_link__iod_a,
                                  CcPathNode * path_node ) {
    if ( !path_link__iod_a ) return NULL;

    CcPathLink * pl__t = cc_path_link__new( path_node );
    if ( !pl__t ) return NULL;

    if ( !*path_link__iod_a ) {
        *path_link__iod_a = pl__t; // Ownership transfer.
    } else {
        CcPathLink * pl = *path_link__iod_a;
        CC_FASTFORWARD( pl );
        pl->next = pl__t; // Append + ownership transfer.
    }

    return pl__t; // Weak pointer.
}

CcPathLink * cc_path_link_duplicate_all__new( CcPathLink * path_link ) {
    if ( !path_link ) return NULL;

    CcPathLink * path_link__a = NULL;
    CcPathLink * from = path_link;

    while ( from ) {
        CcPathLink * pl__w = cc_path_link_append( &path_link__a, from->node__w );
        if ( !pl__w ) { // Failed append --> ownership not transferred ...
            cc_path_link_free_all( &path_link__a );
            return NULL;
        }

        from = from->next;
    }

    return path_link__a;
}

CcPathLink * cc_path_link_extend( CcPathLink ** path_link__iod_a,
                                  CcPathLink ** path_link__n ) {
    if ( !path_link__iod_a ) return NULL;
    if ( !path_link__n ) return NULL;

    if ( !*path_link__n ) return *path_link__iod_a;

    if ( !*path_link__iod_a ) {
        // Ownership transfer.
        *path_link__iod_a = *path_link__n;
        *path_link__n = NULL;

        return *path_link__iod_a;
    }

    CcPathLink * last = *path_link__iod_a;
    CC_FASTFORWARD( last );

    // Ownership transfer.
    last->next = *path_link__n;
    *path_link__n = NULL;

    return last->next;
}

bool cc_path_link_from_nodes( CcPathNode * path_node,
                              CcPathLink ** path_link__o_a ) {
    if ( !path_node ) return false;
    if ( !path_link__o_a ) return false;
    if ( *path_link__o_a ) return false;

    if ( CC_PATH_NODE_IS_PARENT( path_node ) &&
         !cc_path_node_last_step_side_effect_is_valid( path_node, false ) )
            return false;

    CcPathNode * pn = path_node;
    CcPathLink * pl__t = NULL;

    while ( pn ) {
        if ( !cc_path_link_prepend( &pl__t, pn ) ) {
            cc_path_link_free_all( &pl__t );
            return false;
        }

        pn = pn->back__w;
    }

    if ( pl__t ) {
        *path_link__o_a = pl__t; // Ownership transfer.
        return true;
    }

    return false;
}

bool cc_path_link_to_steps( CcPathLink * path_link,
                            CcStep ** steps__o_a ) {
    if ( !path_link ) return false;
    if ( !steps__o_a ) return false;
    if ( *steps__o_a ) return false;

    CcPathLink * pl = path_link;
    CcPathNode * pn = NULL;
    CcPathNode * sub = NULL;

    CcStep * s = NULL;
    CcStep * steps = NULL;
    CcStep * steps__t = NULL;
    CcSideEffectLink * sel__t = NULL;

    while ( pl ) {
        pn = pl->node__w;
        steps = pn->steps;

        if ( steps__t ) {
            s = steps__t;
            CC_FASTFORWARD( s );

            // Overwrite last side-effect in previous node with the one in this node.
            s->side_effect = steps->side_effect;
        }

        if ( !cc_step_extend( &steps__t, &steps ) ) {
            cc_side_effect_link_free_all( &sel__t );
            cc_step_free_all( &steps__t );
            return false;
        }

        sub = pn->sub;
        sel__t = NULL;

        while ( sub ) {
            if ( !cc_side_effect_link_append( &sel__t, sub->side_effect ) ) {
                cc_side_effect_link_free_all( &sel__t );
                cc_step_free_all( &steps__t );
                return false;
            }

            sub = sub->sub;
        }

        if ( sel__t ) {
            s = steps;
            CC_FASTFORWARD( s );
            s->tentative = sel__t; // Owhership transfer, sel__t is now weak pointer.
            // sel__t = NULL; // Not really needed, not (re)used afterwards.
        }

        pl = pl->next;
    }

    if ( steps__t ) {
        *steps__o_a = steps__t;
        return true;
    }

    return false;
}

bool cc_path_link_free_all( CcPathLink ** path_link__f ) {
    if ( !path_link__f ) return false;
    if ( !*path_link__f ) return true;

    CcPathLink * pl = *path_link__f;
    CcPathLink * tmp = NULL;

    while ( pl ) {
        tmp = pl->next;
        CC_FREE( pl );
        pl = tmp;
    }

    *path_link__f = NULL;
    return true;
}

size_t cc_path_link_len( CcPathLink * path_link ) {
    if ( !path_link ) return 0;

    size_t len = 0;
    CcPathLink * pl = path_link;

    while ( pl ) {
        ++len;
        pl = pl->next;
    }

    return len;
}

//
// Path tree iterator.

// TODO :: DELETE :: DOCS :: ???
// bool cc_path_node_iter_to_leaf( CcPathNode * path_node,
//                                 CcPathLink ** root_to_leaf__iod ) {
//     if ( !path_node ) return false;
//     if ( !root_to_leaf__iod ) return false;
//     if ( *root_to_leaf__iod ) return false; // TODO :: FIX :: not in iterator !!!

//     CcPathNode * pn = path_node;

//     CC_REWIND_BY( pn, pn->back__w ); // TODO :: FIX :: not in iterator !!!

//     return _cc_path_node_iter_to_leaf( pn, root_to_leaf__iod );
// }
// TODO :: DELETE :: DOCS :: ???


//
// Linked side-effects.

CcPathSideEffectLink * cc_path_side_effect_link__new( CcPathNodeLinkageEnum link,
                                                      CcSideEffect side_effect ) {
    CcPathSideEffectLink * se__t = CC_MALLOC( sizeof( CcPathSideEffectLink ) );
    if ( !se__t ) return NULL;

    se__t->link = link;
    se__t->side_effect = side_effect;
    se__t->next = NULL;

    return se__t;
}

CcPathSideEffectLink * cc_path_side_effect_link_append( CcPathSideEffectLink ** side_effect_link__iod_a,
                                                        CcPathNodeLinkageEnum link,
                                                        CcSideEffect se ) {
    if ( !side_effect_link__iod_a ) return NULL;

    CcPathSideEffectLink * se__t = cc_path_side_effect_link__new( link, se );
    if ( !se__t ) return NULL;

    if ( !*side_effect_link__iod_a ) {
        *side_effect_link__iod_a = se__t; // Ownership transfer.
    } else {
        CcPathSideEffectLink * se = *side_effect_link__iod_a;
        CC_FASTFORWARD( se );
        se->next = se__t; // Append + ownership transfer.
    }

    return se__t; // Weak pointer.
}

CcPathSideEffectLink * cc_path_side_effect_link_duplicate_all__new( CcPathSideEffectLink * side_effect_link ) {
    if ( !side_effect_link ) return NULL;

    CcPathSideEffectLink * side_effect_link__a = NULL;
    CcPathSideEffectLink * from = side_effect_link;

    while ( from ) {
        CcPathSideEffectLink * se__w = cc_path_side_effect_link_append( &side_effect_link__a, from->link, from->side_effect );
        if ( !se__w ) { // Failed append --> ownership not transferred ...
            cc_path_side_effect_link_free_all( &side_effect_link__a );
            return NULL;
        }

        from = from->next;
    }

    return side_effect_link__a;
}

CcPathSideEffectLink * cc_path_side_effect_link_extend( CcPathSideEffectLink ** side_effect_link__iod_a,
                                                        CcPathSideEffectLink ** side_effect_link__n ) {
    if ( !side_effect_link__iod_a ) return NULL;
    if ( !side_effect_link__n ) return NULL;

    if ( !*side_effect_link__n ) return *side_effect_link__iod_a;

    if ( !*side_effect_link__iod_a ) {
        // Ownership transfer.
        *side_effect_link__iod_a = *side_effect_link__n;
        *side_effect_link__n = NULL;

        return *side_effect_link__iod_a;
    }

    CcPathSideEffectLink * last = *side_effect_link__iod_a;
    CC_FASTFORWARD( last );

    // Ownership transfer.
    last->next = *side_effect_link__n;
    *side_effect_link__n = NULL;

    return last->next;
}

bool cc_path_side_effect_link_free_all( CcPathSideEffectLink ** side_effect_link__f ) {
    if ( !side_effect_link__f ) return false;
    if ( !*side_effect_link__f ) return true;

    CcPathSideEffectLink * se = *side_effect_link__f;
    CcPathSideEffectLink * tmp = NULL;

    while ( se ) {
        tmp = se->next;
        CC_FREE( se );
        se = tmp;
    }

    *side_effect_link__f = NULL;
    return true;
}

size_t cc_path_side_effect_link_len( CcPathSideEffectLink * side_effect_link ) {
    if ( !side_effect_link ) return 0;

    size_t len = 0;
    CcPathSideEffectLink * se = side_effect_link;

    while ( se ) {
        ++len;
        se = se->next;
    }

    return len;
}

char * cc_path_side_effect_link_to_string__new( CcPathSideEffectLink * side_effect_link ) {
    if ( !side_effect_link ) return NULL;

    size_t len = cc_path_side_effect_link_len( side_effect_link );

    size_t size = len * (1 + 1 + CC_SIZE_CHAR_16 + 1) + 1;
    //   1 --> { (separator)
    // + 1 --> CcPathNodeLinkageEnum char
    // + CC_SIZE_CHAR_16 --> size of side-effect string
    // + 1 --> } (separator)
    // + 1 --> '\0'

    char * str__a = CC_MALLOC( size );
    if ( !str__a ) return NULL;

    if ( !cc_str_pad( str__a, '\0', size ) ) {
        CC_FREE( str__a );
        return NULL;
    }

    char * s = str__a;
    CcPathSideEffectLink * sdl = side_effect_link;

    while ( sdl ) {
        *s++ = '{';

        switch ( sdl->link ) {
            case CC_PNLE_None : *s++ = ' '; break;
            case CC_PNLE_Fork : *s++ = '<'; break;
            case CC_PNLE_Alt : *s++ = '^'; break;
            case CC_PNLE_Sub : *s++ = '%'; break;
            default : *s++ = '?'; break;
        }

        if ( s >= str__a + size - CC_SIZE_CHAR_16 - 1 ) {
            // - CC_SIZE_CHAR_16 --> size of side-effect string
            // - 1 --> final '\0'

            CC_FREE( str__a );
            return NULL;
        }

        if ( !cc_side_effect_to_str( sdl->side_effect, (cc_char_16 *)s ) ) {
            CC_FREE( str__a );
            return NULL;
        }

        while ( *s != '\0' ) ++s;
        if ( s >= str__a + size ) {
            CC_FREE( str__a );
            return NULL;
        }

        *s++ = '}';
        sdl = sdl->next;
    }

    return str__a;
}
