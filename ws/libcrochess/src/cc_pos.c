// Copyright (c) 2021 Mario Mlačak, mmlacak@gmail.com
// Licensed under GNU GPL v3+ license. See LICENSING, COPYING files for details.

#include <stdlib.h>
#include <string.h>
#include <stdio.h>

#include "cc_defines.h"
#include "cc_math.h"
#include "cc_pos.h"

/**
    @file cc_pos.c
    @brief Position, position + piece, linked list of position + piece.
*/


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

CcPos cc_pos_add( CcPos pos, CcPos step, unsigned int count ) {
    int i = CC_INVALID_COORD;
    int j = CC_INVALID_COORD;

    if ( CC_IS_COORD_VALID( pos.i ) && CC_IS_COORD_VALID( step.i ) )
        i = pos.i + count * step.i;

    if ( CC_IS_COORD_VALID( pos.j ) && CC_IS_COORD_VALID( step.j ) )
        j = pos.j + count * step.j;

    return CC_POS_CAST( i, j );
}

CcPos cc_pos_subtract( CcPos pos, CcPos step, unsigned int count ) {
    int i = CC_INVALID_COORD;
    int j = CC_INVALID_COORD;

    if ( CC_IS_COORD_VALID( pos.i ) && CC_IS_COORD_VALID( step.i ) )
        i = pos.i - count * step.i;

    if ( CC_IS_COORD_VALID( pos.j ) && CC_IS_COORD_VALID( step.j ) )
        j = pos.j - count * step.j;

    return CC_POS_CAST( i, j );
}

CcPos cc_pos_difference( CcPos pos_1, CcPos pos_2 ) {
    int i = CC_INVALID_COORD;
    int j = CC_INVALID_COORD;

    if ( CC_IS_COORD_VALID( pos_1.i ) && CC_IS_COORD_VALID( pos_2.i ) )
        i = pos_1.i - pos_2.i;

    if ( CC_IS_COORD_VALID( pos_1.j ) && CC_IS_COORD_VALID( pos_2.j ) )
        j = pos_1.j - pos_2.j;

    return CC_POS_CAST( i, j );
}

CcPos cc_pos_step( CcPos start, CcPos destination ) {
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

    if ( CC_IS_POS_ON_BOARD( CC_MAX_BOARD_SIZE, pos.i, pos.j ) ) {
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
// Linked positions.

CcPosLink * cc_pos_link__new( CcPos pos ) {
    CcPosLink * pl__a = malloc( sizeof( CcPosLink ) );
    if ( !pl__a ) return NULL;

    pl__a->pos = pos;
    pl__a->next = NULL;

    return pl__a;
}

CcPosLink * cc_pos_link_append( CcPosLink ** pos_link__iod_a,
                                CcPos pos ) {
    if ( !pos_link__iod_a ) return NULL;

    CcPosLink * pl__t = cc_pos_link__new( pos );
    if ( !pl__t ) return NULL;

    if ( !*pos_link__iod_a ) {
        *pos_link__iod_a = pl__t; // Ownership transfer.
    } else {
        CcPosLink * pl = *pos_link__iod_a;
        CC_FASTFORWARD( pl );
        pl->next = pl__t; // Append + ownership transfer.
    }

    return pl__t; // Weak pointer.
}

CcPosLink * cc_pos_link_extend( CcPosLink ** pos_link__iod_a,
                                CcPosLink ** pos_link__n ) {
    if ( !pos_link__iod_a ) return NULL;
    if ( !pos_link__n ) return NULL;

    if ( !*pos_link__n ) return *pos_link__iod_a;

    if ( !*pos_link__iod_a ) {
        // Ownership transfer.
        *pos_link__iod_a = *pos_link__n;
        *pos_link__n = NULL;

        return *pos_link__iod_a;
    }

    CcPosLink * last = *pos_link__iod_a;
    CC_FASTFORWARD( last );

    // Ownership transfer.
    last->next = *pos_link__n;
    *pos_link__n = NULL;

    return last->next;
}

bool cc_pos_link_free_all( CcPosLink ** pos_link__f ) {
    if ( !pos_link__f ) return false;
    if ( !*pos_link__f ) return true;

    CcPosLink * pl = *pos_link__f;
    CcPosLink * tmp = NULL;

    while ( pl ) {
        tmp = pl->next;
        CC_FREE( pl );
        pl = tmp;
    }

    *pos_link__f = NULL;
    return true;
}

size_t cc_pos_link_len( CcPosLink * pos_link ) {
    if ( !pos_link ) return 0;

    size_t len = 0;
    CcPosLink * pl = pos_link;

    while ( pl ) {
        ++len;
        pl = pl->next;
    }

    return len;
}

char * cc_pos_link_to_short_string__new( CcPosLink * pos_link ) {
    if ( !pos_link ) return NULL;

    // unused len is certainly > 0, because pos_link != NULL
    signed int unused = cc_pos_link_len( pos_link ) *
                        ( CC_MAX_LEN_CHAR_8 + 1 );
                        // CC_MAX_LEN_CHAR_16, for position + piece
                        // +1, for separator '.' between positions

    char * pl_str__a = malloc( unused + 1 ); // +1, for '\0'
    if ( !pl_str__a ) return NULL;

    *pl_str__a = '\0';

    char * pl_str = pl_str__a;
    char * pl_end = pl_str;
    cc_char_8 pos_c8 = CC_CHAR_8_EMPTY;
    CcPosLink * pl = pos_link;

    while ( pl && ( unused > 0 ) ) {
        if ( pl != pos_link ) { // Not 1st pos ...
            *pl_str++ = '.';
            *pl_str = '\0';
        }

        if ( !cc_pos_to_short_string( pl->pos, &pos_c8 ) ) {
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

        pl = pl->next;
    }

    return pl_str__a;
}

//
// Linked paths.

CcPathLink * cc_path_link__new( CcPosLink ** pos_link__n ) {
    if ( !pos_link__n ) return NULL;
    if ( !*pos_link__n ) return NULL;

    CcPathLink * path_link__a = malloc( sizeof( CcPathLink ) );
    if ( !path_link__a ) return NULL;

    path_link__a->path = *pos_link__n;
    *pos_link__n = NULL;

    path_link__a->next = NULL;

    return path_link__a;
}

CcPathLink * cc_path_link_append( CcPathLink ** path_link__iod_a,
                                  CcPosLink ** pos_link__n ) {
    if ( !path_link__iod_a ) return NULL;
    if ( !pos_link__n ) return NULL;

    CcPathLink * path_link__t = cc_path_link__new( pos_link__n );
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
        result = cc_pos_link_free_all( &( pl->path ) ) && result;

        tmp = pl->next;
        CC_FREE( pl );
        pl = tmp;
    }

    *path_link__f = NULL;
    return result;
}


//
// Position + piece + tag.

CcPosPieceTag cc_pos_piece_tag( CcPos pos, CcPieceEnum piece, CcTagEnum tag ) {
    CcPosPieceTag ppt = { .pos = pos, .piece = piece, .tag = tag };
    return ppt;
}

bool cc_pos_piece_tag_is_valid( CcPosPieceTag ppt ) {
    return CC_POS_PIECE_TAG_IS_VALID( ppt );
}

bool cc_pos_piece_tag_is_equal( CcPosPieceTag ppt_1, CcPosPieceTag ppt_2 ) {
    return CC_POS_PIECE_TAG_IS_EQUAL( ppt_1, ppt_2 );
}

bool cc_pos_piece_tag_is_congruent( CcPosPieceTag ppt_1, CcPosPieceTag ppt_2 ) {
    if ( !cc_pos_is_congruent( ppt_1.pos, ppt_2.pos ) ) return false;

    if ( CC_PIECE_IS_NONE( ppt_1.piece ) ||
         CC_PIECE_IS_NONE( ppt_2.piece ) ) return false;

    if ( !cc_piece_has_same_type( ppt_1.piece, ppt_2.piece ) ) return false;

    return true;
}

bool cc_pos_piece_tag_to_short_string( CcPosPieceTag ppt,
                                       cc_char_16 * ppt_str__o ) {
    if ( !ppt_str__o ) return false;

    if ( !cc_pos_to_short_string( ppt.pos, (cc_char_8 *)ppt_str__o ) ) return false;

    char * p = (char *)ppt_str__o;

    unsigned int count = 0;
    while ( *p++ != '\0' ) ++count; // fast-fwd

    if ( count >= CC_MAX_LEN_CHAR_8 ) return false;

    *p++ = cc_piece_symbol( ppt.piece );
    *p = '\0';

    return true;
}


//
// Linked list of positions + pieces + tags.

CcPptLink * cc_ppt_link__new( CcPosPieceTag ppt ) {
    CcPptLink * pl__t = malloc( sizeof( CcPptLink ) );
    if ( !pl__t ) return NULL;

    pl__t->ppt = ppt;
    pl__t->next = NULL;

    return pl__t;
}

CcPptLink * cc_ppt_link_append( CcPptLink ** ppt_link__iod_a,
                                CcPosPieceTag ppt ) {
    if ( !ppt_link__iod_a ) return NULL;

    CcPptLink * pl__t = cc_ppt_link__new( ppt );
    if ( !pl__t ) return NULL;

    if ( !*ppt_link__iod_a ) {
        *ppt_link__iod_a = pl__t; // Ownership transfer.
    } else {
        CcPptLink * pl = *ppt_link__iod_a;
        CC_FASTFORWARD( pl );
        pl->next = pl__t; // Append + ownership transfer.
    }

    return pl__t; // Weak pointer.
}

CcPptLink * cc_ppt_link_duplicate_all__new( CcPptLink * ppt_link__io ) {
    if ( !ppt_link__io ) return NULL;

    CcPptLink * ppt_link__a = NULL;
    CcPptLink * from = ppt_link__io;

    while ( from ) {
        CcPptLink * ppt__w = cc_ppt_link_append( &ppt_link__a, from->ppt );

        if ( !ppt__w ) { // Failed append --> ownership not transferred ...
            cc_ppt_link_free_all( &ppt_link__a );
            return NULL;
        }

        from = from->next;
    }

    return ppt_link__a;
}

CcPptLink * cc_ppt_link_extend( CcPptLink ** ppt_link__iod_a,
                                CcPptLink ** ppt_link__n ) {
    if ( !ppt_link__iod_a ) return NULL;
    if ( !ppt_link__n ) return NULL;

    if ( !*ppt_link__n ) return *ppt_link__iod_a;

    if ( !*ppt_link__iod_a ) {
        // Ownership transfer.
        *ppt_link__iod_a = *ppt_link__n;
        *ppt_link__n = NULL;

        return *ppt_link__iod_a;
    }

    CcPptLink * last = *ppt_link__iod_a;
    CC_FASTFORWARD( last );

    // Ownership transfer.
    last->next = *ppt_link__n;
    *ppt_link__n = NULL;

    return last->next;
}

bool cc_ppt_link_free_all( CcPptLink ** ppt_link__f ) {
    if ( !ppt_link__f ) return false;
    if ( !*ppt_link__f ) return true;

    CcPptLink * pl = *ppt_link__f;
    CcPptLink * tmp = NULL;

    while ( pl ) {
        tmp = pl->next;
        CC_FREE( pl );
        pl = tmp;
    }

    *ppt_link__f = NULL;
    return true;
}

size_t cc_ppt_link_len( CcPptLink * ppt_link ) {
    if ( !ppt_link ) return 0;

    size_t len = 0;
    CcPptLink * pl = ppt_link;

    while ( pl ) {
        ++len;
        pl = pl->next;
    }

    return len;
}

CcPosLink * cc_ppt_link_convert_to_pos_link__new( CcPptLink * ppt_link ) {
    if ( !ppt_link ) return NULL;

    CcPosLink * pos_link__a = NULL;
    CcPptLink * p = ppt_link;

    while ( p ) {
        if ( !cc_pos_link_append( &pos_link__a, p->ppt.pos ) ) {
            cc_pos_link_free_all( &pos_link__a );
            return NULL;
        }

        p = p->next;
    }

    return pos_link__a;
}


char * cc_ppt_link_to_short_string__new( CcPptLink * ppt_link ) {
    if ( !ppt_link ) return NULL;

    // unused len is certainly > 0, because ppt_link != NULL
    signed int unused = cc_ppt_link_len( ppt_link ) *
                        ( CC_MAX_LEN_CHAR_8 + 1 );
                        // CC_MAX_LEN_CHAR_16, for position + piece
                        // +1, for separator '.' between positions

    char * pl_str__a = malloc( unused + 1 ); // +1, for '\0'
    if ( !pl_str__a ) return NULL;

    *pl_str__a = '\0';

    char * pl_str = pl_str__a;
    char * pl_end = pl_str;
    cc_char_16 ppt_c16 = CC_CHAR_16_EMPTY;
    CcPptLink * pl = ppt_link;

    while ( pl && ( unused > 0 ) ) {
        if ( pl != ppt_link ) { // Not 1st pos ...
            *pl_str++ = '.';
            *pl_str = '\0';
        }

        if ( !cc_pos_piece_tag_to_short_string( pl->ppt, &ppt_c16 ) ) {
            CC_FREE( pl_str__a );
            return NULL;
        }

        pl_end = cc_str_append_into( pl_str, unused, ppt_c16, CC_MAX_LEN_CHAR_16 );
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
