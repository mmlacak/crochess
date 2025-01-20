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
                                CcPosLink * fields,
                                CcPieceEnum encountered_piece,
                                CcTagEnum encountered_tag,
                                CcMomentum momentum ) {
    CcPathLink * pl__t = malloc( sizeof( CcPathLink ) );
    if ( !pl__t ) return NULL;

    pl__t->side_effect = side_effect;

    pl__t->fields = fields;

    pl__t->encountered_piece = encountered_piece;
    pl__t->encountered_tag = encountered_tag;

    pl__t->momentum = momentum;

    pl__t->fork = NULL;
    pl__t->alt = NULL;
    pl__t->next = NULL;
    pl__t->back__w = NULL;

    return pl__t;
}

CcPathLink * cc_path_link_append( CcPathLink ** pl__iod_a,
                                  CcSideEffect side_effect,
                                  CcPosLink * fields,
                                  CcPieceEnum encountered_piece,
                                  CcTagEnum encountered_tag,
                                  CcMomentum momentum ) {
    if ( !pl__iod_a ) return NULL;

    CcPathLink * pl__t = cc_path_link__new( side_effect, fields, encountered_piece, encountered_tag, momentum );
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

CcPathLink * cc_path_link_fork( CcPathLink ** pl_step__a,
                                CcPathLink ** pl_fork__n ) {
    if ( !pl_step__a ) return NULL;
    if ( !*pl_step__a ) return NULL;

    if ( !pl_fork__n ) return NULL;
    if ( !*pl_fork__n ) return NULL;

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

CcPathLink * cc_path_link_alternate( CcPathLink ** pl_step__a,
                                     CcPathLink ** pl_alt__n ) {
    if ( !pl_step__a ) return NULL;
    if ( !*pl_step__a ) return NULL;

    if ( !pl_alt__n ) return NULL;
    if ( !*pl_alt__n ) return NULL;

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

static bool _cc_path_link_steps_are_valid( CcPosLink * fields ) {
    if ( !fields ) return false;

    CcPosLink * s = fields;

    while ( s ) {
        if ( !CC_POS_IS_VALID( s->pos ) ) return false;

        s = s->next;
    }

    return true;
}

static bool _cc_path_link_is_valid( CcPathLink * path_link, bool has_steps ) {
    if ( !path_link ) return false;

    CcPathLink * pl = path_link;

    if ( !CC_SIDE_EFFECT_TYPE_IS_ENUMERATOR( pl->side_effect.type ) ) return false;

    if ( !_cc_path_link_steps_are_valid( pl->fields ) )
        return false;

    if ( !CC_PIECE_IS_ENUMERATOR( pl->encountered_piece ) )
        return false;

    if ( !CC_TAG_IS_ENUMERATOR( pl->encountered_tag ) )
        return false;

    if ( !CC_MOMENTUM_USAGE_IS_ENUMERATOR( pl->momentum.usage ) )
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

    if ( pl->next ) {
        if ( pl->next->back__w != pl ) return false;
        if ( !_cc_path_link_is_valid( pl->next, true ) ) return false;
        ++links;
    }

    if ( links == 0 )
        return has_steps; // No links --> terminal node.
        // If also root node, it should not be terminal, without having at least initial and terminal fields.

    return true;
}

bool cc_path_link_is_valid( CcPathLink * path_link ) {
    if ( !path_link ) return false;

    CcPathLink * root = path_link;

    CC_REWIND_BY( root, root->back__w );

    bool has_steps = ( cc_pos_link_len( root->fields ) > 1 ); // Initial step should not be the only one, if root is the only node.

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
        CcPathLink * pd__w = cc_path_link_append( &pl__a, from->side_effect,
                                                          from->fields,
                                                          from->encountered_piece,
                                                          from->encountered_tag,
                                                          from->momentum );

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
        result = cc_pos_link_free_all( &( pl->fields ) ) && result;

        if ( pl->fork )
            result = cc_path_link_free_all( &( pl->fork ) ) && result;

        if ( pl->alt )
            result = cc_path_link_free_all( &( pl->alt ) ) && result;

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

        pl = pl->next;
    }

    return count;
}

char * cc_path_link_node_to_string__new( CcPathLink * path_link_node ) {
    if ( !path_link_node ) return NULL;

    cc_char_16 se_str = CC_CHAR_16_EMPTY;

    if ( cc_side_effect_to_str( path_link_node->side_effect, &se_str ) )
        return NULL;

    char * pos_str__a = cc_pos_link_to_string__new( path_link_node->fields );
    if ( !pos_str__a ) return NULL;

    size_t str_size = cc_str_len( pos_str__a, NULL, CC_SIZE_BUFFER );

    str_size += cc_str_len( se_str, NULL, CC_SIZE_CHAR_16 );

    // TODO :: add .encountered_piece .encountered_tag .momentum

    size_t unused = str_size + 1; // +1 for '\0'

    char * pln_str__a = malloc( unused );
    if ( !pln_str__a ) {
        CC_FREE( pos_str__a );
        return NULL;
    }

    char * end_se__w = cc_str_append_into( pln_str__a, unused, se_str, CC_SIZE_CHAR_16 );
    if ( !end_se__w ) {
        CC_FREE( pos_str__a );
        CC_FREE( pln_str__a );
        return NULL;
    }

    unused -= ( end_se__w - pln_str__a );

    char * end_pos__w = cc_str_append_into( end_se__w, unused, pos_str__a, CC_SIZE_BUFFER );
    if ( !end_pos__w ) {
        CC_FREE( pos_str__a );
        CC_FREE( pln_str__a );
        return NULL;
    }

    // unused -= ( end_pos__w - end_se__w ); // Not needed yet.

    // TODO :: add .encountered_piece .encountered_tag .momentum

    CC_FREE( pos_str__a );

    return pln_str__a;
}
