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

CcPathLink * cc_path_link__new( CcPos pos, cc_uint_t momentum ) {
    CcPathLink * pl__t = malloc( sizeof( CcPathLink ) );
    if ( !pl__t ) return NULL;

    pl__t->pos = pos;
    pl__t->momentum = momentum;

    pl__t->diverge = NULL;
    pl__t->alt = NULL;
    pl__t->next = NULL;

    return pl__t;
}

CcPathLink * cc_path_link_append( CcPathLink ** pl__iod_a,
                                  CcPos pos,
                                  cc_uint_t momentum ) {
    if ( !pl__iod_a ) return NULL;

    CcPathLink * pl__t = cc_path_link__new( pos, momentum );
    if ( !pl__t ) return NULL;

    if ( !*pl__iod_a ) {
        *pl__iod_a = pl__t; // Ownership transfer.
    } else {
        CcPathLink * pl = *pl__iod_a;
        CC_FASTFORWARD( pl );
        pl->next = pl__t; // Append + ownership transfer.
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
    *pl__n = NULL;

    return last->next;
}

CcPathLink * cc_path_link_diverge( CcPathLink ** pl_step__a,
                                   CcPathLink ** pl_alt__n ) {
    if ( !pl_step__a ) return NULL;
    if ( !*pl_step__a ) return NULL;

    if ( !pl_alt__n ) return NULL;
    if ( !*pl_alt__n ) return NULL;

    if ( ( *pl_step__a )->diverge ) {
        CcPathLink * pl = ( *pl_step__a )->diverge;

        while ( pl->alt ) {
            pl = pl->alt;
        }

        pl->alt = *pl_alt__n;
    } else {
        ( *pl_step__a )->diverge = *pl_alt__n;
    }

    CcPathLink * pl__w = *pl_alt__n;
    *pl_alt__n = NULL;

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

    pl__w->alt = *pl_alt__n;
    *pl_alt__n = NULL;

    return pl__w;
}

CcPathLink * cc_path_link_duplicate_all__new( CcPathLink * path_link ) {
    if ( !path_link ) return NULL;

    CcPathLink * pl__a = NULL;
    CcPathLink * from = path_link;
    bool result = true;

    while ( from ) {
        CcPathLink * pd__w = cc_path_link_append( &pl__a, from->pos, from->momentum );

        if ( !pd__w ) { // Failed append --> ownership not transferred ...
            result = false;
            break;
        }

        if ( from->diverge ) {
            if ( !( pd__w->diverge = cc_path_link_duplicate_all__new( from->diverge ) ) ) {
                result = false;
                break;
            }
        }

        if ( from->alt ) {
            if ( !( pd__w->alt = cc_path_link_duplicate_all__new( from->alt ) ) ) {
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
        if ( pl->diverge )
            result = cc_path_link_free_all( &( pl->diverge ) ) && result;

        if ( pl->alt )
            result = cc_path_link_free_all( &( pl->alt ) ) && result;

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
            if ( pl->diverge ) {
                len += cc_path_link_len( pl->diverge, count_all );
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
        if ( pl->diverge ) {
            count += cc_path_link_count_all_seqments( pl->diverge );
        }

        if ( pl->alt ) {
            count += cc_path_link_count_all_seqments( pl->alt );
        }

        pl = pl->next;
    }

    return count;
}


// path_diverged
// == CC_MBE_Void --> regular path segment
// == CC_MBE_False --> alternative path segment
// == CC_MBE_True --> diverged path segment
static char * _cc_path_link_segment_to_string( CcPathLink * path_link,
                                               size_t depth,
                                               CcMaybeBoolEnum path_diverged,
                                               char * str_start__io,
                                               char const * str_end ) {
    if ( !path_link ) return NULL;
    if ( !str_start__io ) return NULL;
    if ( !str_end ) return NULL;

    if ( *str_start__io != '\0' ) return NULL; // Each segment string must be properly terminated.

    size_t len = cc_path_link_len( path_link, false );
    size_t depth_size = 2 * depth; // 2 == ' ' + ' '

    // <!> Keep in-sync with cc_path_link_to_string__new().
    size_t str_size = 4 // 4 == '*' or '|' + ' ' + '\n' at the end of line + '\0' at the end of string
                    + depth_size // for padding
                    + 4 * len; // 4 == '.' + <file char> + 2 <rank chars>, <momentum> is disregarded
    // TODO :: maybe add <momentum> (?)
    size_t unused = str_size;

    if ( str_end < ( str_start__io + str_size ) ) return NULL;

    char * str__t = str_start__io; // str__t == position transfer variable (not ownership)

    for ( size_t i = 0; i < depth_size; ++i ) {
        *str__t++ = ' ';
    }

    unused -= depth_size;

    if ( CC_MAYBE_IS_TRUE( path_diverged ) ) { // diverge path
        *str__t++ = '*';
        *str__t++ = ' ';
        unused -= 2;
    } else if ( CC_MAYBE_IS_FALSE( path_diverged ) ) { // alt path
        *str__t++ = '|';
        *str__t++ = ' ';
        unused -= 2;
    } // path_diverged == CC_MBE_Void --> just a regular path segment --> nothing more to do

    CcPathLink * pl = path_link;
    cc_char_8 pos_c8 = CC_CHAR_8_EMPTY;
    char const * end__t = str__t; // end__t == position transfer variable (not ownership)

    while ( pl ) {
        if ( !cc_pos_to_string( pl->pos, &pos_c8 ) )
            return NULL;

        end__t = cc_str_append_into( str__t, unused, pos_c8, CC_MAX_LEN_CHAR_8 );
        if ( !end__t )  return NULL;

        unused -= ( end__t - str__t );
        str__t = (char *)end__t;

        pl = pl->next;
    }

    *str__t++ = '\n';
    *str__t = '\0';

    return str__t;
}

static char * _cc_path_link_to_string( CcPathLink * path_link,
                                       size_t depth,
                                       CcMaybeBoolEnum path_diverged,
                                       char * str_start__io,
                                       char const * str_end ) {
    if ( !path_link ) return NULL;
    if ( !str_start__io ) return NULL;
    if ( !str_end ) return NULL;

    char * str__t = str_start__io;
    char * end__t = NULL;
    CcPathLink * pl = path_link;

    while ( pl ) {
        end__t = _cc_path_link_segment_to_string( pl, depth, path_diverged, str__t, str_end );
        if ( !end__t ) return NULL;

        str__t = end__t;

        if ( pl->diverge ) {
            end__t = _cc_path_link_to_string( pl->diverge, depth + 1, CC_MBE_True, str__t, str_end );
            if ( !end__t ) return NULL;

            str__t = end__t;
        }

        if ( pl->alt ) {
            end__t = _cc_path_link_to_string( pl->alt, depth, CC_MBE_False, str__t, str_end );
            if ( !end__t ) return NULL;

            str__t = end__t;
        }

        pl = pl->next;
    }

    return str__t;
}

char * cc_path_link_to_string__new( CcPathLink * path_link ) {
    if ( !path_link ) return NULL;

    size_t total_len = cc_path_link_len( path_link, true );
    size_t count = cc_path_link_count_all_seqments( path_link );
    size_t max_depth = count - 1; // Depth start from 0, and there are `count` of them.

    // <!> Keep in-sync with _cc_path_link_segment_to_string().
    size_t str_size = 2 * max_depth * ( count - 1 ) // 2 == ' ' + ' ' for padding, count - 1 == first segment is not padded
                    + 3 * count // 3 == '*' or '|' + ' ' + '\n' at the end of line
                    + 4 * total_len // 4 == '.' + <file char> + 2 <rank chars>; <momentum> is disregarded
                    + 1; // 1 == '\0' at the end of string

    char * pl_str__a = malloc( str_size );
    if ( !pl_str__a ) return NULL;

    // *pl_str__a = '\0';
    if ( !cc_str_clear( pl_str__a, str_size ) ) {
        CC_FREE( pl_str__a );
        return NULL;
    }

    char const * pl_end = pl_str__a + str_size;

    char * end = _cc_path_link_to_string( path_link, 0, CC_MBE_Void, pl_str__a, pl_end );
    if ( !end ) {
        CC_FREE( pl_str__a );
        return NULL;
    }

    return pl_str__a;
}

//
// Linked list of path segments.

CcPathWeakLink * cc_path_weak_link__new( CcPathLink * pl ) {
    CcPathWeakLink * pwl__a = malloc( sizeof( CcPathWeakLink ) );
    if ( !pwl__a ) return NULL;

    pwl__a->pl__w = pl; // Weak pointer, no ownership transfer.
    pwl__a->next = NULL;

    return pwl__a;
}

CcPathWeakLink * cc_path_weak_link_append( CcPathWeakLink ** pwl__iod_a,
                                           CcPathLink * pl ) {
    if ( !pwl__iod_a ) return NULL;

    CcPathWeakLink * pwl__t = cc_path_weak_link__new( pl );
    if ( !pwl__t ) return NULL;

    if ( !*pwl__iod_a ) {
        *pwl__iod_a = pwl__t; // Ownership transfer.
    } else {
        CcPathWeakLink * pwl = *pwl__iod_a;
        CC_FASTFORWARD( pwl );
        pwl->next = pwl__t; // Append + ownership transfer.
    }

    return pwl__t; // Weak pointer.
}

CcPathWeakLink * cc_path_weak_link_extend( CcPathWeakLink ** pwl__iod_a,
                                           CcPathWeakLink ** pwl__n ) {
    if ( !pwl__iod_a ) return NULL;
    if ( !pwl__n ) return NULL;

    if ( !*pwl__n ) return *pwl__iod_a;

    if ( !*pwl__iod_a ) {
        // Ownership transfer.
        *pwl__iod_a = *pwl__n;
        *pwl__n = NULL;

        return *pwl__iod_a;
    }

    CcPathWeakLink * last = *pwl__iod_a;
    CC_FASTFORWARD( last );

    // Ownership transfer.
    last->next = *pwl__n;
    *pwl__n = NULL;

    return last->next;
}

bool cc_path_weak_link_free_all( CcPathWeakLink ** pwl__f ) {
    if ( !pwl__f ) return false;
    if ( !*pwl__f ) return true;

    CcPathWeakLink * pwl = *pwl__f;
    CcPathWeakLink * tmp = NULL;

    while ( pwl ) {
        // <!> pl__w is weak pointer, not to be free()-ed.

        tmp = pwl->next;
        CC_FREE( pwl );
        pwl = tmp;
    }

    *pwl__f = NULL;
    return true;
}

size_t cc_path_weak_link_len( CcPathWeakLink * pwl ) {
    if ( !pwl ) return 0;

    size_t len = 0;
    CcPathWeakLink * p = pwl;

    while ( p ) {
        ++len;
        p = p->next;
    }

    return len;
}
