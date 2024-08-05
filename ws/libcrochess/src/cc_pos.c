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

bool cc_pos_to_short_string( CcPos pos, cc_char_8 * pos_str__o ) {
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

char * cc_typed_step_link_to_short_string__new( CcTypedStepLink * ts_link ) {
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

        if ( !cc_pos_to_short_string( tsl->step.step, &pos_c8 ) ) {
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
// Position + piece + tag.

CcPosDesc cc_pos_desc( CcPos pos, cc_piece piece, cc_tag tag ) {
    CcPosDesc pd = { .pos = pos, .piece = piece, .tag = tag };
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

bool cc_pos_desc_to_short_string( CcPosDesc pd,
                                  cc_char_16 * pd_str__o ) {
    if ( !pd_str__o ) return false;

    if ( !cc_pos_to_short_string( pd.pos, (cc_char_8 *)pd_str__o ) ) return false;

    char * p = (char *)pd_str__o;

    cc_uint count = 0;
    while ( *p++ != '\0' ) ++count; // fast-fwd

    if ( count >= CC_MAX_LEN_CHAR_8 ) return false;

    *p++ = cc_piece_symbol( pd.piece );
    *p = '\0';

    return true;
}


//
// Linked list of positions + pieces + tags.

CcPosDescLink * cc_pos_desc_link__new( CcPosDesc pd ) {
    CcPosDescLink * pl__t = malloc( sizeof( CcPosDescLink ) );
    if ( !pl__t ) return NULL;

    pl__t->pd = pd;
    pl__t->next = NULL;

    return pl__t;
}

CcPosDescLink * cc_pos_desc_link_append( CcPosDescLink ** pd_link__iod_a,
                                         CcPosDesc pd ) {
    if ( !pd_link__iod_a ) return NULL;

    CcPosDescLink * pl__t = cc_pos_desc_link__new( pd );
    if ( !pl__t ) return NULL;

    if ( !*pd_link__iod_a ) {
        *pd_link__iod_a = pl__t; // Ownership transfer.
    } else {
        CcPosDescLink * pl = *pd_link__iod_a;
        CC_FASTFORWARD( pl );
        pl->next = pl__t; // Append + ownership transfer.
    }

    return pl__t; // Weak pointer.
}

CcPosDescLink * cc_pos_desc_link_duplicate_all__new( CcPosDescLink * pd_link ) {
    if ( !pd_link ) return NULL;

    CcPosDescLink * pd_link__a = NULL;
    CcPosDescLink * from = pd_link;

    while ( from ) {
        CcPosDescLink * pd__w = cc_pos_desc_link_append( &pd_link__a, from->pd );

        if ( !pd__w ) { // Failed append --> ownership not transferred ...
            cc_pos_desc_link_free_all( &pd_link__a );
            return NULL;
        }

        from = from->next;
    }

    return pd_link__a;
}

CcPosDescLink * cc_pos_desc_link_extend( CcPosDescLink ** pd_link__iod_a,
                                         CcPosDescLink ** pd_link__n ) {
    if ( !pd_link__iod_a ) return NULL;
    if ( !pd_link__n ) return NULL;

    if ( !*pd_link__n ) return *pd_link__iod_a;

    if ( !*pd_link__iod_a ) {
        // Ownership transfer.
        *pd_link__iod_a = *pd_link__n;
        *pd_link__n = NULL;

        return *pd_link__iod_a;
    }

    CcPosDescLink * last = *pd_link__iod_a;
    CC_FASTFORWARD( last );

    // Ownership transfer.
    last->next = *pd_link__n;
    *pd_link__n = NULL;

    return last->next;
}

bool cc_pos_desc_link_free_all( CcPosDescLink ** pd_link__f ) {
    if ( !pd_link__f ) return false;
    if ( !*pd_link__f ) return true;

    CcPosDescLink * pl = *pd_link__f;
    CcPosDescLink * tmp = NULL;

    while ( pl ) {
        tmp = pl->next;
        CC_FREE( pl );
        pl = tmp;
    }

    *pd_link__f = NULL;
    return true;
}

size_t cc_pos_desc_link_len( CcPosDescLink * pd_link ) {
    if ( !pd_link ) return 0;

    size_t len = 0;
    CcPosDescLink * pl = pd_link;

    while ( pl ) {
        ++len;
        pl = pl->next;
    }

    return len;
}


char * cc_pos_desc_link_to_short_string__new( CcPosDescLink * pd_link ) {
    if ( !pd_link ) return NULL;

    // unused len is certainly > 0, because pd_link != NULL
    signed int unused = cc_pos_desc_link_len( pd_link ) *
                        ( CC_MAX_LEN_CHAR_8 + 1 );
                        // CC_MAX_LEN_CHAR_16, for position + piece
                        // +1, for separator '.' between positions

    char * pl_str__a = malloc( unused + 1 ); // +1, for '\0'
    if ( !pl_str__a ) return NULL;

    *pl_str__a = '\0';

    char * pl_str = pl_str__a;
    char * pl_end = pl_str;
    cc_char_16 pd_c16 = CC_CHAR_16_EMPTY;
    CcPosDescLink * pl = pd_link;

    while ( pl && ( unused > 0 ) ) {
        if ( pl != pd_link ) { // Not 1st pos ...
            *pl_str++ = '.';
            *pl_str = '\0';
        }

        if ( !cc_pos_desc_to_short_string( pl->pd, &pd_c16 ) ) {
            CC_FREE( pl_str__a );
            return NULL;
        }

        pl_end = cc_str_append_into( pl_str, unused, pd_c16, CC_MAX_LEN_CHAR_16 );
        if ( !pl_end ) {
            CC_FREE( pl_str__a );
            return NULL;
        }

        unused -= ( pl_end - pl_str );
        pl_str = pl_end;

        pl = pl->next;
    }

    return pl_str__a;
}


//
// Linked paths.

CcPathLink * cc_path_link__new( CcPosDescLink ** pd_link__n ) {
    if ( !pd_link__n ) return NULL;
    if ( !*pd_link__n ) return NULL;

    CcPathLink * path_link__a = malloc( sizeof( CcPathLink ) );
    if ( !path_link__a ) return NULL;

    path_link__a->path = *pd_link__n;
    *pd_link__n = NULL;

    path_link__a->next = NULL;

    return path_link__a;
}

CcPathLink * cc_path_link_append( CcPathLink ** path_link__iod_a,
                                  CcPosDescLink ** pd_link__n ) {
    if ( !path_link__iod_a ) return NULL;
    if ( !pd_link__n ) return NULL;

    CcPathLink * path_link__t = cc_path_link__new( pd_link__n );
    if ( !path_link__t ) return NULL;

    if ( *path_link__iod_a ) {
        CcPathLink * pl = *path_link__iod_a;

        CC_FASTFORWARD( pl );

        pl->next = path_link__t;
    } else {
        *path_link__iod_a = path_link__t;
    }

    return path_link__t;
}

CcPathLink * cc_path_link_extend( CcPathLink ** path_link__iod_a,
                                  CcPathLink ** path_link__n ) {
    if ( !path_link__iod_a ) return NULL;
    if ( !path_link__n ) return NULL;

    if ( !*path_link__n ) return *path_link__iod_a;

    if ( !*path_link__iod_a ) {
        *path_link__iod_a = *path_link__n;
        *path_link__n = NULL;

        return *path_link__iod_a;
    }

    CcPathLink * last = *path_link__iod_a;
    CC_FASTFORWARD( last );

    last->next = *path_link__n;
    *path_link__n = NULL;

    return last->next;
}

bool cc_path_link_free_all( CcPathLink ** path_link__f ) {
    if ( !path_link__f ) return false;
    if ( !*path_link__f ) return true;

    bool result = true;
    CcPathLink * pl = *path_link__f;
    CcPathLink * tmp = NULL;

    while ( pl ) {
        result = cc_pos_desc_link_free_all( &( pl->path ) ) && result;

        tmp = pl->next;
        CC_FREE( pl );
        pl = tmp;
    }

    *path_link__f = NULL;
    return result;
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
