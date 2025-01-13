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

CcPathLink * cc_path_link__new( CcPosLink * steps, CcSideEffect side_effect ) {
    CcPathLink * pl__t = malloc( sizeof( CcPathLink ) );
    if ( !pl__t ) return NULL;

    pl__t->steps = steps;
    pl__t->side_effect = side_effect;

    pl__t->fork = NULL;
    pl__t->alt = NULL;

    pl__t->back__w = NULL;
    pl__t->next = NULL;

    return pl__t;
}

CcPathLink * cc_path_link_append( CcPathLink ** pl__iod_a,
                                  CcPosLink * steps,
                                  CcSideEffect side_effect ) {
    if ( !pl__iod_a ) return NULL;

    CcPathLink * pl__t = cc_path_link__new( steps, side_effect );
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

static bool _cc_path_link_steps_are_valid( CcPosLink * steps ) {
    if ( !steps ) return false;

    CcPosLink * s = steps;

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

    if ( !_cc_path_link_steps_are_valid( pl->steps ) )
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
        // If also root node, it should not be terminal, without having at least initial and terminal steps.

    return true;
}

bool cc_path_link_is_valid( CcPathLink * path_link ) {
    if ( !path_link ) return false;

    CcPathLink * root = path_link;

    CC_REWIND_BY( root, root->back__w );

    bool has_steps = ( cc_pos_link_len( root->steps ) > 1 ); // Initial step should not be the only one, if root is the only node.

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
        CcPathLink * pd__w = cc_path_link_append( &pl__a, from->steps, from->side_effect );

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
        result = cc_pos_link_free_all( &( pl->steps ) ) && result;

        if ( pl->fork )
            result = cc_path_link_free_all( &( pl->fork ) ) && result;

        if ( pl->alt )
            result = cc_path_link_free_all( &( pl->alt ) ) && result;

        // [i] pl->back__w is weak pointer, not an owner, so must not be free()-ed.

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


// TODO :: DELETE DOCS :: DELETE
// // path_diverged
// // == CC_MBE_Void --> regular path segment
// // == CC_MBE_False --> alternative path segment
// // == CC_MBE_True --> forked path segment
// static char * _cc_path_link_segment_to_string( CcPathLink * path_link,
//                                                size_t depth,
//                                                CcMaybeBoolEnum path_diverged,
//                                                char * str_start__io,
//                                                char const * str_end ) {
//     if ( !path_link ) return NULL;
//     if ( !str_start__io ) return NULL;
//     if ( !str_end ) return NULL;
//
//     if ( *str_start__io != '\0' ) return NULL; // Each segment string must be properly terminated.
//
//     size_t len = cc_path_link_len( path_link, true );
//     size_t depth_size = 2 * depth; // 2 == ' ' + ' '
//
//     // <!> Keep in-sync with cc_path_link_to_string__new().
//     size_t str_size = 4 // 4 == '*' or '|' + ' ' + '\n' at the end of line + '\0' at the end of string
//                     + depth_size // for padding
//                     + 4 * len; // 4 == '.' + <file char> + 2 <rank chars>, <momentum> is disregarded
//     // todo :: rethink :: maybe add <momentum> (?)
//     size_t unused = str_size;
//
//     if ( str_end < ( str_start__io + str_size ) ) return NULL;
//
//     char * str__t = str_start__io; // str__t == position transfer variable (not ownership)
//
//     for ( size_t i = 0; i < depth_size; ++i ) {
//         *str__t++ = ' ';
//     }
//
//     unused -= depth_size;
//
//     if ( path_diverged == CC_MBE_True ) { // fork path
//         *str__t++ = '*';
//         *str__t++ = ' ';
//         unused -= 2;
//     } else if ( path_diverged == CC_MBE_False ) { // alt path
//         *str__t++ = '|';
//         *str__t++ = ' ';
//         unused -= 2;
//     } // path_diverged == CC_MBE_Void --> just a regular path segment --> nothing more to do
//
//     CcPathLink * pl = path_link;
//     cc_char_8 pos_c8 = CC_CHAR_8_EMPTY;
//     char const * end__t = str__t; // end__t == position transfer variable (not ownership)
//
//     while ( pl ) {
//         // TODO :: FIX :: pos --> steps
//         // if ( !cc_pos_to_string( pl->pos, &pos_c8 ) )
//         //     return NULL;
//
//         end__t = cc_str_append_into( str__t, unused, pos_c8, CC_MAX_LEN_CHAR_8 );
//         if ( !end__t )  return NULL;
//
//         unused -= ( end__t - str__t );
//         str__t = (char *)end__t;
//
//         pl = pl->next;
//     }
//
//     *str__t++ = '\n';
//     *str__t = '\0';
//
//     return str__t;
// }

// static char * _cc_path_link_to_string( CcPathLink * path_link,
//                                        size_t depth,
//                                        CcMaybeBoolEnum path_diverged,
//                                        char * str_start__io,
//                                        char const * str_end ) {
//     if ( !path_link ) return NULL;
//     if ( !str_start__io ) return NULL;
//     if ( !str_end ) return NULL;
//
//     char * str__t = str_start__io;
//     char * end__t = NULL;
//     CcPathLink * pl = path_link;
//
//     while ( pl ) {
//         end__t = _cc_path_link_segment_to_string( pl, depth, path_diverged, str__t, str_end );
//         if ( !end__t ) return NULL;
//
//         str__t = end__t;
//
//         if ( pl->fork ) {
//             end__t = _cc_path_link_to_string( pl->fork, depth + 1, CC_MBE_True, str__t, str_end );
//             if ( !end__t ) return NULL;
//
//             str__t = end__t;
//         }
//
//         if ( pl->alt ) {
//             end__t = _cc_path_link_to_string( pl->alt, depth, CC_MBE_False, str__t, str_end );
//             if ( !end__t ) return NULL;
//
//             str__t = end__t;
//         }
//
//         pl = pl->next;
//     }
//
//     return str__t;
// }

// char * cc_path_link_to_string__new( CcPathLink * path_link ) {
//     if ( !path_link ) return NULL;
//
//     size_t total_len = cc_path_link_len( path_link, true );
//     size_t count = cc_path_link_count_all_seqments( path_link );
//     size_t max_depth = count - 1; // Depth start from 0, and there are `count` of them.
//
//     // <!> Keep in-sync with _cc_path_link_segment_to_string().
//     size_t str_size = 2 * max_depth * ( count - 1 ) // 2 == ' ' + ' ' for padding, count - 1 == first segment is not padded
//                     + 3 * count // 3 == '*' or '|' + ' ' + '\n' at the end of line
//                     + 4 * total_len // 4 == '.' + <file char> + 2 <rank chars>; <momentum> is disregarded
//                     + 1; // 1 == '\0' at the end of string
//
//     char * pl_str__a = malloc( str_size );
//     if ( !pl_str__a ) return NULL;
//
//     // *pl_str__a = '\0';
//     if ( !cc_str_clear( pl_str__a, str_size ) ) {
//         CC_FREE( pl_str__a );
//         return NULL;
//     }
//
//     char const * pl_end = pl_str__a + str_size;
//
//     char * end = _cc_path_link_to_string( path_link, 0, CC_MBE_Void, pl_str__a, pl_end );
//     if ( !end ) {
//         CC_FREE( pl_str__a );
//         return NULL;
//     }
//
//     return pl_str__a;
// }
// TODO :: DELETE DOCS :: DELETE

char * cc_path_link_node_to_string__new( CcPathLink * path_link_node ) {
    if ( !path_link_node ) return NULL;

    cc_char_16 se_str = CC_CHAR_16_EMPTY;

    if ( cc_side_effect_to_str( path_link_node->side_effect, &se_str ) )
        return NULL;

    char * pos_str__a = cc_pos_link_to_string__new( path_link_node->steps );
    if ( !pos_str__a ) return NULL;

    size_t str_size = cc_str_len( pos_str__a, NULL, CC_SIZE_BUFFER );

    str_size += cc_str_len( se_str, NULL, CC_SIZE_CHAR_16 );

    size_t unused = str_size + 1; // +1 for '\0'

    char * pln_str__a = malloc( unused );
    if ( !pln_str__a ) {
        CC_FREE( pos_str__a );
        return NULL;
    }

    char * end_pos__w = cc_str_append_into( pln_str__a, unused, pos_str__a, CC_SIZE_BUFFER );
    if ( !end_pos__w ) {
        CC_FREE( pos_str__a );
        CC_FREE( pln_str__a );
        return NULL;
    }

    unused -= ( end_pos__w - pln_str__a );

    char * end_se__w = cc_str_append_into( end_pos__w, unused, se_str, CC_SIZE_CHAR_16 );
    if ( !end_se__w ) {
        CC_FREE( pos_str__a );
        CC_FREE( pln_str__a );
        return NULL;
    }

    // unused -= ( end_se__w - end_pos__w ); // Not needed anymore.

    CC_FREE( pos_str__a );

    return pln_str__a;
}
