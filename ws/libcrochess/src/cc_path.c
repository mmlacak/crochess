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
// Linked path segments.

CcPathLink * cc_path_link__new( CcSideEffect side_effect,
                                CcStep ** steps__d_n,
                                CcPieceTagType encounter,
                                CcActivationDesc act_desc ) {
    CcPathLink * pl__t = malloc( sizeof( CcPathLink ) );
    if ( !pl__t ) return NULL;

    pl__t->side_effect = side_effect;

    if ( steps__d_n && *steps__d_n ) {
        pl__t->steps = *steps__d_n;
        *steps__d_n = NULL;
    } else
        pl__t->steps = NULL;

    pl__t->encounter = encounter;

    pl__t->act_desc = act_desc;

    pl__t->fork = NULL;
    pl__t->alt = NULL;
    pl__t->sub = NULL;
    pl__t->next = NULL;
    pl__t->back__w = NULL;

    return pl__t;
}

CcPathLink * cc_path_link_append( CcPathLink ** pl__iod_a,
                                  CcSideEffect side_effect,
                                  CcStep ** steps__d_n,
                                  CcPieceTagType encounter,
                                  CcActivationDesc act_desc ) {
    if ( !pl__iod_a ) return NULL;

    CcPathLink * pl__t = cc_path_link__new( side_effect, steps__d_n, encounter, act_desc );
    if ( !pl__t ) return NULL;

    if ( !*pl__iod_a ) {
        *pl__iod_a = pl__t; // Ownership transfer.
    } else {
        CcPathLink * pl = *pl__iod_a;

        CC_FASTFORWARD( pl );

        pl->next = pl__t; // Append + ownership transfer.
        pl__t->back__w = pl;
    }

    return pl__t; // Weak pointer.
}

CcPathLink * cc_path_link_extend( CcPathLink ** pl__iod_a,
                                  CcPathLink ** pl__n ) {
    if ( !pl__iod_a ) return NULL;
    if ( !pl__n ) return NULL;

    if ( !*pl__n ) return *pl__iod_a;

    if ( !*pl__iod_a ) {
        // Ownership transfer.
        *pl__iod_a = *pl__n;
        *pl__n = NULL;

        return *pl__iod_a;
    }

    CcPathLink * last = *pl__iod_a;
    CC_FASTFORWARD( last );

    // Ownership transfer.
    last->next = *pl__n;
    ( *pl__n )->back__w = last;
    *pl__n = NULL;

    return last->next;
}

CcPathLink * cc_path_link_add_fork( CcPathLink ** pl_step__a,
                                    CcPathLink ** pl_fork__n ) {
    if ( !pl_step__a ) return NULL;
    if ( !*pl_step__a ) return NULL;

    if ( !pl_fork__n ) return NULL;
    if ( !*pl_fork__n ) return NULL;

    // Sanity checks, back-links shouldn't point to somewhere else ...
    if ( ( *pl_step__a )->back__w ) return NULL;
    if ( ( *pl_fork__n )->back__w ) return NULL;

    if ( ( *pl_step__a )->fork ) {
        CcPathLink * pl = ( *pl_step__a )->fork;

        while ( pl->alt ) {
            pl = pl->alt;
        }

        pl->alt = *pl_fork__n;
        ( *pl_fork__n )->back__w = pl;
    } else {
        ( *pl_step__a )->fork = *pl_fork__n;
        ( *pl_fork__n )->back__w = *pl_step__a;
    }

    // Ownership transferred.
    CcPathLink * pl__w = *pl_fork__n;
    *pl_fork__n = NULL;

    return pl__w;
}

CcPathLink * cc_path_link_add_alter( CcPathLink ** pl_step__a,
                                     CcPathLink ** pl_alt__n ) {
    if ( !pl_step__a ) return NULL;
    if ( !*pl_step__a ) return NULL;

    if ( !pl_alt__n ) return NULL;
    if ( !*pl_alt__n ) return NULL;

    // Sanity checks, back-links shouldn't point to somewhere else ...
    if ( ( *pl_step__a )->back__w ) return NULL;
    if ( ( *pl_alt__n )->back__w ) return NULL;

    CcPathLink * pl__w = *pl_step__a;

    while ( pl__w->alt ) {
        pl__w = pl__w->alt;
    }

    // Ownership transfer.
    pl__w->alt = *pl_alt__n;
    ( *pl_alt__n )->back__w = pl__w;

    *pl_alt__n = NULL;

    return pl__w;
}

static CcMaybeBoolEnum _cc_path_link_subs_is_valid( CcPathLink * pl_subs ) {
    if ( !pl_subs ) return CC_MBE_Void;

    CcPathLink * pl = pl_subs;
    CcPathLink * old = NULL;

    while ( pl ) {
        // For 1st node ( pl == pl_subs ), checks there is no back-link ...
        // Otherwise, checks back-link points to its parent ...
        if ( pl->back__w != old ) return CC_MBE_Void;

        if ( pl->steps ) return CC_MBE_False;
        if ( pl->fork ) return CC_MBE_False;
        if ( pl->alt ) return CC_MBE_False;
        if ( pl->next ) return CC_MBE_False;

        if ( pl->encounter != CC_PTE_None ) return CC_MBE_False;

        if ( !CC_SIDE_EFFECT_TYPE_IS_VALID( pl->side_effect.type ) )
            return CC_MBE_False;

        old = pl;
        pl = pl->sub;
    }

    return CC_MBE_True;
}

CcPathLink * cc_path_link_add_subs( CcPathLink ** pl_step__a,
                                    CcPathLink ** pl_sub__n ) {
    if ( !pl_step__a ) return NULL;
    if ( !*pl_step__a ) return NULL;

    if ( !pl_sub__n ) return NULL;
    if ( !*pl_sub__n ) return NULL;

    // Sanity checks.
    if ( ( *pl_step__a )->back__w ) return NULL;
    if ( _cc_path_link_subs_is_valid( *pl_sub__n ) != CC_MBE_True )
        return NULL;

    CcPathLink * pl__w = *pl_step__a;

    while ( pl__w->sub ) {
        pl__w = pl__w->sub;
    }

    // Ownership transfer.
    pl__w->sub = *pl_sub__n;
    ( *pl_sub__n )->back__w = pl__w;

    *pl_sub__n = NULL;

    return pl__w;
}

static bool _cc_path_link_steps_are_valid( CcStep * steps ) {
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

static bool _cc_path_link_is_valid( CcPathLink * path_link, bool has_steps ) {
    if ( !path_link ) return false;

    CcPathLink * pl = path_link;

    if ( !CC_SIDE_EFFECT_TYPE_IS_ENUMERATOR( pl->side_effect.type ) ) return false;

    if ( CC_SIDE_EFFECT_TYPE_TERMINATES_PLY( pl->side_effect.type ) ) {
        // If path isn't actually terminating ...
        if ( ( pl->steps ) || ( pl->fork ) || ( pl->alt ) || ( pl->next ) ) return false;
    }

    if ( pl->steps ) { // If there is a path segment, it has to be valid.
        if ( !_cc_path_link_steps_are_valid( pl->steps ) )
            return false;
    } else { // Final side-effect (e.g. a capture) terminating a path.
        if ( !CC_SIDE_EFFECT_TYPE_CAN_TERMINATE_PLY( pl->side_effect.type ) ) return false;

        // If path isn't actually terminating ...
        if ( ( pl->fork ) || ( pl->alt ) || ( pl->next ) ) return false;
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
        if ( !_cc_path_link_is_valid( pl->fork, true ) ) return false;
        ++links;
    }

    if ( pl->alt ) {
        if ( pl->alt->back__w != pl ) return false;
        if ( !_cc_path_link_is_valid( pl->alt, true ) ) return false;
        ++links;
    }

    if ( pl->sub ) {
        if ( pl->sub->back__w != pl ) return false;
        if ( _cc_path_link_subs_is_valid( pl->sub ) != CC_MBE_True ) return false;
        ++links;
    }

    if ( pl->next ) {
        if ( pl->next->back__w != pl ) return false;
        if ( !_cc_path_link_is_valid( pl->next, true ) ) return false;
        ++links;
    }

    if ( links == 0 )
        return has_steps; // No links --> terminal node.
        // If also root node, it should not be terminal, without having at least initial and terminal steps.

    return true;
}

bool cc_path_link_is_valid( CcPathLink * path_link ) {
    if ( !path_link ) return false;

    CcPathLink * root = path_link;

    CC_REWIND_BY( root, root->back__w );

    bool has_steps = ( cc_step_count( root->steps ) > 1 ); // Initial step should not be the only one, if root is the only node.

    if ( !_cc_path_link_is_valid( root, has_steps ) ) return false;

    return true;
}

CcPathLink * cc_path_link_duplicate_all__new( CcPathLink * path_link ) {
    if ( !path_link ) return NULL;
    if ( path_link->back__w ) return NULL;

    CcPathLink * pl__a = NULL;
    CcPathLink * from = path_link;
    bool result = true;

    while ( from ) {
        CcPathLink * pd__w = cc_path_link_append( &pl__a,
                                                  from->side_effect,
                                                  &from->steps,
                                                  from->encounter,
                                                  from->act_desc );

        if ( !pd__w ) { // Failed append --> ownership not transferred ...
            result = false;
            break;
        }

        if ( from->fork ) {
            if ( ( pd__w->fork = cc_path_link_duplicate_all__new( from->fork ) ) ) {
                pd__w->fork->back__w = pd__w;
            } else {
                result = false;
                break;
            }
        }

        if ( from->alt ) {
            if ( ( pd__w->alt = cc_path_link_duplicate_all__new( from->alt ) ) ) {
                pd__w->alt->back__w = pd__w;
            } else {
                result = false;
                break;
            }
        }

        if ( from->sub ) {
            if ( ( pd__w->sub = cc_path_link_duplicate_all__new( from->sub ) ) ) {
                pd__w->sub->back__w = pd__w;
            } else {
                result = false;
                break;
            }
        }

        from = from->next;
    }

    if ( !result ) {
        cc_path_link_free_all( &pl__a );
        return NULL;
    }

    return pl__a;
}

bool cc_path_link_free_all( CcPathLink ** pl__f ) {
    if ( !pl__f ) return false;
    if ( !*pl__f ) return true;

    CcPathLink * pl = *pl__f;
    CcPathLink * tmp = NULL;
    bool result = true;

    while ( pl ) {
        result = cc_step_free_all( &( pl->steps ) ) && result;

        if ( pl->fork )
            result = cc_path_link_free_all( &( pl->fork ) ) && result;

        if ( pl->alt )
            result = cc_path_link_free_all( &( pl->alt ) ) && result;

        if ( pl->sub )
            result = cc_path_link_free_all( &( pl->sub ) ) && result;

        // <!> pl->back__w is weak pointer, not an owner, so it must not be free()-ed.

        tmp = pl->next;
        CC_FREE( pl );
        pl = tmp;
    }

    *pl__f = NULL;
    return result;
}

size_t cc_path_link_len( CcPathLink * path_link, bool count_all ) {
    if ( !path_link ) return 0;

    size_t len = 0;
    CcPathLink * pl = path_link;

    while ( pl ) {
        ++len;

        if ( count_all ) {
            if ( pl->fork ) {
                len += cc_path_link_len( pl->fork, count_all );
            }

            if ( pl->alt ) {
                len += cc_path_link_len( pl->alt, count_all );
            }

            if ( pl->sub ) {
                len += cc_path_link_len( pl->sub, count_all );
            }
        }

        pl = pl->next;
    }

    return len;
}

size_t cc_path_link_count_all_seqments( CcPathLink * path_link ) {
    if ( !path_link ) return 0;

    size_t count = 1; // = 1, for this path segment
    CcPathLink * pl = path_link;

    while ( pl ) {
        if ( pl->fork ) {
            count += cc_path_link_count_all_seqments( pl->fork );
        }

        if ( pl->alt ) {
            count += cc_path_link_count_all_seqments( pl->alt );
        }

        // Substitute paths shouldn't have any segments.
        //
        // if ( pl->sub ) {
        //     count += cc_path_link_count_all_seqments( pl->sub );
        // }

        pl = pl->next;
    }

    return count;
}

char * cc_path_link_node_to_string__new( cc_uchar_t depth,
                                         CcPathLink * path_link_node ) {
    if ( !path_link_node ) return NULL;

    cc_uint_t tabs_len = 2 * depth; // Depth --> 2-spaces.
    char * tabs_str__a = cc_str_pad__new( ' ', tabs_len );
    if ( !tabs_str__a ) return NULL;

    char const * plnle_str = cc_path_link_node_linkage_to_string( path_link_node );
    cc_char_16 se_str = CC_CHAR_16_EMPTY;

    if ( !cc_side_effect_to_str( path_link_node->side_effect, &se_str ) )
        return NULL;

    char * steps_str__a = cc_step_all_to_string__new( path_link_node->steps );
    if ( !steps_str__a ) {
        CC_FREE( tabs_str__a );
        return NULL;
    }

    size_t str_len = cc_str_len( tabs_str__a, NULL, tabs_len );
    str_len += cc_str_len( plnle_str, NULL, CC_SIZE_PATH_LINK_NODE_LINKAGE_STRING );
    str_len += cc_str_len( steps_str__a, NULL, CC_SIZE_BUFFER );
    str_len += cc_str_len( se_str, NULL, CC_SIZE_CHAR_16 );
    str_len += 2 + 1 + 1; // For .encounter (i.e. CcPieceTagType); +2 for ' @', +1 for piece symbol, +1 for tag char.
    str_len += 3 + CC_SIZE_CHAR_32; // For .act_desc (i.e. CcActivationDesc); +3 for ' #' preceeding act desc.

    size_t str_size = str_len + 1; // +1 for '\0'

    char * pln_str__a = malloc( str_size );
    if ( !pln_str__a ) {
        CC_FREE( tabs_str__a );
        CC_FREE( steps_str__a );
        return NULL;
    }

    if ( !cc_str_clear( pln_str__a, str_size ) ) { // TODO :: DEBUG :: not really needed
        CC_FREE( tabs_str__a );
        CC_FREE( steps_str__a );
        CC_FREE( pln_str__a );
        return NULL;
    }

    char * pln_str = pln_str__a;
    char const * pln_end__w = pln_str__a + str_size;

    // Tabs.
    char const * end_tabs_str__w = cc_str_append_into( pln_str, pln_end__w, CC_SIZE_IGNORE, tabs_str__a, NULL, tabs_len );
    if ( !end_tabs_str__w ) {
        CC_FREE( tabs_str__a );
        CC_FREE( steps_str__a );
        CC_FREE( pln_str__a );
        return NULL;
    }
    pln_str = (char *)end_tabs_str__w;

    // Node linkage.
    char const * end_plnle_str__w = cc_str_append_into( pln_str, pln_end__w, CC_SIZE_IGNORE, plnle_str, NULL, CC_SIZE_PATH_LINK_NODE_LINKAGE_STRING );
    if ( !end_plnle_str__w ) {
        CC_FREE( tabs_str__a );
        CC_FREE( steps_str__a );
        CC_FREE( pln_str__a );
        return NULL;
    }
    pln_str = (char *)end_plnle_str__w;

    // Side-effect.
    char const * end_se_str__w = cc_str_append_into( pln_str, pln_end__w, CC_SIZE_IGNORE, se_str, NULL, CC_SIZE_CHAR_16 );
    if ( !end_se_str__w ) {
        CC_FREE( tabs_str__a );
        CC_FREE( steps_str__a );
        CC_FREE( pln_str__a );
        return NULL;
    }
    pln_str = (char *)end_se_str__w;

    // Steps.
    char const * end_steps__w = cc_str_append_into( pln_str, pln_end__w, CC_SIZE_IGNORE, steps_str__a, NULL, CC_SIZE_BUFFER );
    if ( !end_steps__w ) {
        CC_FREE( tabs_str__a );
        CC_FREE( steps_str__a );
        CC_FREE( pln_str__a );
        return NULL;
    }
    pln_str = (char *)end_steps__w;

    // Encountered piece and its tag.
    char piece_symbol = cc_piece_as_char( path_link_node->encounter );
    char tag_chr = cc_tag_as_char( path_link_node->encounter );

    *pln_str++ = '|';
    *pln_str++ = '@';
    *pln_str++ = piece_symbol;
    *pln_str++ = tag_chr;
    *pln_str = '\0';

    // Activation descriptor.
    cc_char_32 act_desc_str = CC_CHAR_32_EMPTY;

    if ( !cc_activation_desc_as_string( path_link_node->act_desc, &act_desc_str ) ) {
        CC_FREE( tabs_str__a );
        CC_FREE( steps_str__a );
        CC_FREE( pln_str__a );
        return NULL;
    }

    *pln_str++ = '|';
    // *pln_str++ = '#';
    *pln_str = '\0';

    char const * end_act_desc__w = cc_str_append_into( pln_str, pln_end__w, CC_SIZE_IGNORE, act_desc_str, NULL, CC_SIZE_CHAR_32 );
    if ( !end_act_desc__w ) {
        CC_FREE( tabs_str__a );
        CC_FREE( steps_str__a );
        CC_FREE( pln_str__a );
        return NULL;
    }
    // pln_str = (char *)end_act_desc__w; // Not needed anymore.

    CC_FREE( tabs_str__a );
    CC_FREE( steps_str__a );

    return pln_str__a;
}

//
// Node linkage.

char const * cc_path_link_node_linkage_as_string( CcPathLinkNodeLinkageEnum plnle ) {
    switch ( plnle ) {
        case CC_PLNLE_NoLinkage : return "";
        case CC_PLNLE_Fork : return "* ";
        case CC_PLNLE_Alt : return "& ";
        case CC_PLNLE_Sub : return "~ ";
        case CC_PLNLE_Next : return "> ";
        default : return "? ";
    }
}

CcPathLinkNodeLinkageEnum cc_path_link_node_linkage( CcPathLink * path_link_node ) {
    if ( !path_link_node ) return CC_PLNLE_NoLinkage;

    CcPathLink * pln = path_link_node->back__w;

    if ( !pln ) return CC_PLNLE_NoLinkage;

    if ( pln->fork == path_link_node )
        return CC_PLNLE_Fork;
    else if ( pln->alt == path_link_node )
        return CC_PLNLE_Alt;
    else if ( pln->sub == path_link_node )
        return CC_PLNLE_Sub;
    else if ( pln->next == path_link_node )
        return CC_PLNLE_Next;
    else
        return CC_PLNLE_NoLinkage;
}

char const * cc_path_link_node_linkage_to_string( CcPathLink * path_link_node ) {
    CcPathLinkNodeLinkageEnum plnle = cc_path_link_node_linkage( path_link_node );
    return cc_path_link_node_linkage_as_string( plnle );
}


//
// Linked side-effects.

CcPathSideEffectLink * cc_path_side_effect_link__new( CcPathLinkNodeLinkageEnum link,
                                                      CcSideEffect side_effect ) {
    CcPathSideEffectLink * se__t = malloc( sizeof( CcPathSideEffectLink ) );
    if ( !se__t ) return NULL;

    se__t->link = link;
    se__t->side_effect = side_effect;
    se__t->next = NULL;

    return se__t;
}

CcPathSideEffectLink * cc_path_side_effect_link_append( CcPathSideEffectLink ** side_effect_link__iod_a,
                                                        CcPathLinkNodeLinkageEnum link,
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
