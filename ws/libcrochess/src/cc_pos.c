// Copyright (c) 2021 Mario Mlačak, mmlacak@gmail.com
// Licensed under GNU GPL v3+ license. See LICENSING, COPYING files for details.

#include <stdlib.h>
#include <string.h>
#include <stdio.h>

#include "cc_defines.h"
#include "cc_math.h"
#include "cc_pos.h"


//
// Position.

CcPos cc_pos( int i, int j ) {
    CcPos pos = { .i = i, .j = j };
    return pos;
}

bool cc_pos_is_valid( CcPos pos ) {
    return CC_POS_IS_VALID( pos );
}

bool cc_pos_is_static_step( CcPos pos ) {
    return CC_POS_IS_STATIC_STEP( pos );
}

bool cc_pos_is_disambiguation( CcPos pos ) {
    return CC_POS_IS_DISAMBIGUATION(pos);
}

bool cc_pos_is_partial( CcPos pos ) {
    return CC_POS_IS_PARTIAL( pos );
}

bool cc_pos_is_equal( CcPos pos_1, CcPos pos_2 ) {
    return CC_POS_IS_EQUAL( pos_1, pos_2 );
}

bool cc_pos_is_congruent( CcPos pos_1, CcPos pos_2 ) {
    bool is_file = ( CC_IS_COORD_VALID( pos_1.i ) &&
                     CC_IS_COORD_VALID( pos_2.i ) );

    if ( is_file && ( pos_1.i != pos_2.i ) )
        return false;

    bool is_rank = ( CC_IS_COORD_VALID( pos_1.j ) &&
                     CC_IS_COORD_VALID( pos_2.j ) );

    if ( is_rank && ( pos_1.j != pos_2.j ) )
        return false;

    return is_file || is_rank;
}

CcPos cc_pos_add( CcPos pos, CcPos step, int count ) {
    int i = CC_INVALID_COORD;
    int j = CC_INVALID_COORD;

    if ( CC_IS_COORD_VALID( pos.i ) && CC_IS_COORD_VALID( step.i ) )
        i = pos.i + count * step.i;

    if ( CC_IS_COORD_VALID( pos.j ) && CC_IS_COORD_VALID( step.j ) )
        j = pos.j + count * step.j;

    return CC_POS_CAST( i, j );
}

CcPos cc_pos_difference( CcPos start, CcPos destination ) {
    int i = CC_INVALID_COORD;
    int j = CC_INVALID_COORD;

    if ( CC_IS_COORD_VALID( destination.i ) && CC_IS_COORD_VALID( start.i ) )
        i = destination.i - start.i;

    if ( CC_IS_COORD_VALID( destination.j ) && CC_IS_COORD_VALID( start.j ) )
        j = destination.j - start.j;

    return CC_POS_CAST( i, j );
}

CcPos cc_pos_calc_step( CcPos start, CcPos destination ) {
    int diff_i = destination.i - start.i;
    int diff_j = destination.j - start.j;

    int gcd = cc_gcd( diff_i, diff_j );
    if ( gcd == 0 ) return CC_POS_CAST_INVALID;

    diff_i /= gcd;
    diff_j /= gcd;

    return CC_POS_CAST( diff_i, diff_j );
}

bool cc_pos_to_string( CcPos pos, cc_char_8 * pos_str__o ) {
    if ( !pos_str__o ) return false;

    #define LOWER_BOUND (-100)
    #define UPPER_BOUND (1000)

    if ( CC_IS_POS_ON_VALID_BOARD( CC_MAX_BOARD_SIZE, pos.i, pos.j ) ) {
        snprintf( *pos_str__o,
                  CC_MAX_LEN_CHAR_8,
                  "%c%hhd",
                  CC_CONVERT_BYTE_INTO_FILE_CHAR( pos.i ),
                  (signed char)(pos.j + 1) );
    } else if ( CC_IS_COORD_ON_BOARD( CC_MAX_BOARD_SIZE, pos.i )
                && ( !CC_IS_COORD_VALID( pos.j ) ) ) {
        snprintf( *pos_str__o,
                  CC_MAX_LEN_CHAR_8,
                  "%c",
                  CC_CONVERT_BYTE_INTO_FILE_CHAR( pos.i ) );
    } else if ( CC_IS_COORD_ON_BOARD( CC_MAX_BOARD_SIZE, pos.j )
                && ( !CC_IS_COORD_VALID( pos.i ) ) ) {
        snprintf( *pos_str__o,
                  CC_MAX_LEN_CHAR_8,
                  "%hhd",
                  (signed char)(pos.j + 1) );
    } else {
        int count = 0; // snprintf() doesn't count '\0'

        if ( ( LOWER_BOUND < pos.i ) && ( pos.i < UPPER_BOUND ) )
            count = snprintf( *pos_str__o,
                              CC_MAX_LEN_CHAR_8,
                              "%hd,",
                              (signed short)pos.i );
        else
            count = snprintf( *pos_str__o, CC_MAX_LEN_CHAR_8, "*," );

        if ( count < 1 ) return false; // count can't be > 4

        char * p = ( (char *)pos_str__o + count );
        size_t size = CC_MAX_LEN_CHAR_8 - count;

        if ( ( LOWER_BOUND < pos.j ) && ( pos.j < UPPER_BOUND ) )
            count = snprintf( p, size, "%hd", (signed short)pos.j );
        else
            count = snprintf( p, size, "*" );

        if ( count < 1 )
            return false; // count can't be > 4
    }

    return true;
}

//
// Step + type

CcTypedStep cc_typed_step( CcPos step, CcStepTypeEnum type ) {
    CcTypedStep st = { .step = step, .type = type };
    return st;
}

bool cc_typed_step_is_equal( CcTypedStep ts_1, CcTypedStep ts_2 ) {
    return CC_TYPED_STEP_IS_EQUAL( ts_1, ts_2 );
}


//
// Linked typed steps.

CcTypedStepLink * cc_typed_step_link__new( CcTypedStep step ) {
    CcTypedStepLink * pl__a = malloc( sizeof( CcTypedStepLink ) );
    if ( !pl__a ) return NULL;

    pl__a->step = step;
    pl__a->next = NULL;

    return pl__a;
}

CcTypedStepLink * cc_typed_step_link_append( CcTypedStepLink ** ts_link__iod_a,
                                             CcTypedStep step ) {
    if ( !ts_link__iod_a ) return NULL;

    CcTypedStepLink * tsl__t = cc_typed_step_link__new( step );
    if ( !tsl__t ) return NULL;

    if ( !*ts_link__iod_a ) {
        *ts_link__iod_a = tsl__t; // Ownership transfer.
    } else {
        CcTypedStepLink * tsl = *ts_link__iod_a;
        CC_FASTFORWARD( tsl );
        tsl->next = tsl__t; // Append + ownership transfer.
    }

    return tsl__t; // Weak pointer.
}

CcTypedStepLink * cc_typed_step_link_extend( CcTypedStepLink ** ts_link__iod_a,
                                             CcTypedStepLink ** ts_link__n ) {
    if ( !ts_link__iod_a ) return NULL;
    if ( !ts_link__n ) return NULL;

    if ( !*ts_link__n ) return *ts_link__iod_a;

    if ( !*ts_link__iod_a ) {
        // Ownership transfer.
        *ts_link__iod_a = *ts_link__n;
        *ts_link__n = NULL;

        return *ts_link__iod_a;
    }

    CcTypedStepLink * last = *ts_link__iod_a;
    CC_FASTFORWARD( last );

    // Ownership transfer.
    last->next = *ts_link__n;
    *ts_link__n = NULL;

    return last->next;
}

bool cc_typed_step_link_free_all( CcTypedStepLink ** ts_link__f ) {
    if ( !ts_link__f ) return false;
    if ( !*ts_link__f ) return true;

    CcTypedStepLink * tsl = *ts_link__f;
    CcTypedStepLink * tmp = NULL;

    while ( tsl ) {
        tmp = tsl->next;
        CC_FREE( tsl );
        tsl = tmp;
    }

    *ts_link__f = NULL;
    return true;
}

size_t cc_typed_step_link_len( CcTypedStepLink * ts_link ) {
    if ( !ts_link ) return 0;

    size_t len = 0;
    CcTypedStepLink * tsl = ts_link;

    while ( tsl ) {
        ++len;
        tsl = tsl->next;
    }

    return len;
}

char * cc_typed_step_link_to_string__new( CcTypedStepLink * ts_link ) {
    if ( !ts_link ) return NULL;

    // unused len is certainly > 0, because pos_link != NULL
    signed int unused = cc_typed_step_link_len( ts_link ) *
                        ( CC_MAX_LEN_CHAR_8 + 1 );
                        // CC_MAX_LEN_CHAR_16, for position + piece
                        // +1, for separator '.' between positions

    char * pl_str__a = malloc( unused + 1 ); // +1, for '\0'
    if ( !pl_str__a ) return NULL;

    *pl_str__a = '\0';

    char * pl_str = pl_str__a;
    char * pl_end = pl_str;
    cc_char_8 pos_c8 = CC_CHAR_8_EMPTY;
    CcTypedStepLink * tsl = ts_link;

    while ( tsl && ( unused > 0 ) ) {
        if ( tsl != ts_link ) { // Not 1st pos ...
            *pl_str++ = '.';
            *pl_str = '\0';
        }

        if ( !cc_pos_to_string( tsl->step.step, &pos_c8 ) ) {
            CC_FREE( pl_str__a );
            return NULL;
        }

        pl_end = cc_str_append_into( pl_str, unused, pos_c8, CC_MAX_LEN_CHAR_16 );
        if ( !pl_end ) {
            CC_FREE( pl_str__a );
            return NULL;
        }

        unused -= ( pl_end - pl_str );
        pl_str = pl_end;

        tsl = tsl->next;
    }

    return pl_str__a;
}


//
// Position descriptor.

CcPosDesc cc_pos_desc( CcPos pos, CcPieceType piece, CcTagType tag, cc_uint_t momentum ) {
    CcPosDesc pd = { .pos = pos, .piece = piece, .tag = tag, .momentum = momentum };
    return pd;
}

bool cc_pos_desc_is_valid( CcPosDesc pd ) {
    return CC_POS_DESC_IS_VALID( pd );
}

bool cc_pos_desc_is_equal( CcPosDesc pd_1, CcPosDesc pd_2 ) {
    return CC_POS_DESC_IS_EQUAL( pd_1, pd_2 );
}

bool cc_pos_desc_is_congruent( CcPosDesc pd_1, CcPosDesc pd_2 ) {
    if ( !cc_pos_is_congruent( pd_1.pos, pd_2.pos ) ) return false;

    if ( CC_PIECE_IS_NONE( pd_1.piece ) ||
         CC_PIECE_IS_NONE( pd_2.piece ) ) return false;

    if ( !cc_piece_has_same_type( pd_1.piece, pd_2.piece ) ) return false;

    return true;
}

bool cc_pos_desc_to_string( CcPosDesc pd,
                                  cc_char_16 * pd_str__o ) {
    if ( !pd_str__o ) return false;

    if ( !cc_pos_to_string( pd.pos, (cc_char_8 *)pd_str__o ) ) return false;

    char * p = (char *)pd_str__o;

    cc_uint_t count = 0;
    while ( *p++ != '\0' ) ++count; // fast-fwd

    if ( count >= CC_MAX_LEN_CHAR_8 ) return false;

    *p++ = cc_piece_symbol( pd.piece );
    *p = '\0';

    return true;
}


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
