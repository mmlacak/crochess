// Copyright (c) 2021, 2025 Mario Mlačak, mmlacak@gmail.com
// Licensed under GNU GPL v3+ license. See LICENSING, COPYING files for details.

#include <stdbool.h>
#include <stdlib.h>
#include <string.h>
#include <stdio.h>

#include "cc_defines.h"
#include "cc_math.h"
#include "cc_pos.h"


//
// Position.

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

CcPos cc_pos_add( CcPos pos, int i, int j ) {
    int _i = CC_INVALID_COORD;
    int _j = CC_INVALID_COORD;

    if ( CC_IS_COORD_VALID( pos.i ) && CC_IS_COORD_VALID( i ) )
        _i = pos.i + i;

    if ( CC_IS_COORD_VALID( pos.j ) && CC_IS_COORD_VALID( j ) )
        _j = pos.j + j;

    return CC_POS_CAST( _i, _j );
}

CcPos cc_pos_add_steps( CcPos pos, CcPos step, int count ) {
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

bool cc_pos_are_same_color( CcPos start, CcPos destination ) {
    if ( CC_IS_FIELD_LIGHT( start.i, start.j ) &&
         CC_IS_FIELD_LIGHT( destination.i, destination.j ) )
            return true;

    if ( CC_IS_FIELD_DARK( start.i, start.j ) &&
         CC_IS_FIELD_DARK( destination.i, destination.j ) )
            return true;

    return false;
}

bool cc_pos_piece_are_same_color( CcPos pos, CcPieceTagType piece ) {
    if ( cc_piece_is_light( piece ) && CC_IS_FIELD_LIGHT( pos.i, pos.j ) )
        return true;

    if ( cc_piece_is_dark( piece ) && CC_IS_FIELD_DARK( pos.i, pos.j ) )
        return true;

    return false;
}

bool cc_pos_to_string( CcPos pos, cc_char_8 * pos_str__o ) {
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
    CcPosLink * pl__t = malloc( sizeof( CcPosLink ) );
    if ( !pl__t ) return NULL;

    pl__t->pos = pos;
    pl__t->next = NULL;

    return pl__t;
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

CcPosLink * cc_pos_link_duplicate_all__new( CcPosLink * pos_link ) {
    if ( !pos_link ) return NULL;

    CcPosLink * pos_link__a = NULL;
    CcPosLink * from = pos_link;

    while ( from ) {
        CcPosLink * pl__w = cc_pos_link_append( &pos_link__a, from->pos );
        if ( !pl__w ) { // Failed append --> ownership not transferred ...
            cc_pos_link_free_all( &pos_link__a );
            return NULL;
        }

        from = from->next;
    }

    return pos_link__a;
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

char * cc_pos_link_to_string__new( CcPosLink * pos_link ) {
    if ( !pos_link ) return NULL;

    // unused len is certainly > 0, because pos_link != NULL
    signed int unused = cc_pos_link_len( pos_link ) *
                        ( CC_MAX_LEN_CHAR_8 + 1 );
                        // CC_MAX_LEN_CHAR_8, for position
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

        if ( !cc_pos_to_string( pl->pos, &pos_c8 ) ) {
            CC_FREE( pl_str__a );
            return NULL;
        }

        pl_end = cc_str_append_into( pl_str, unused, pos_c8, CC_MAX_LEN_CHAR_8 );
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
// Position descriptor.

bool cc_pos_desc_is_congruent( CcPosDesc pd_1, CcPosDesc pd_2 ) {
    if ( !cc_pos_is_congruent( pd_1.pos, pd_2.pos ) ) return false;

    if ( CC_PIECE_IS_NONE( pd_1.piece ) ||
         CC_PIECE_IS_NONE( pd_2.piece ) ) return false;

    CcPieceTagType opposite = cc_piece_opposite( pd_2.piece );

    if ( pd_1.piece == pd_2.piece || pd_1.piece == opposite ) return true;

    return false;
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
// Linked position descriptor.

CcPosDescLink * cc_pos_desc_link__new( CcPosDesc pd ) {
    CcPosDescLink * pdl__t = malloc( sizeof( CcPosDescLink ) );
    if ( !pdl__t ) return NULL;

    pdl__t->pd = pd;
    pdl__t->next = NULL;

    return pdl__t;
}

CcPosDescLink * cc_pos_desc_link_append( CcPosDescLink ** pd_link__iod_a,
                                         CcPosDesc pd ) {
    if ( !pd_link__iod_a ) return NULL;

    CcPosDescLink * pdl__t = cc_pos_desc_link__new( pd );
    if ( !pdl__t ) return NULL;

    if ( !*pd_link__iod_a ) {
        *pd_link__iod_a = pdl__t; // Ownership transfer.
    } else {
        CcPosDescLink * pdl = *pd_link__iod_a;
        CC_FASTFORWARD( pdl );
        pdl->next = pdl__t; // Append + ownership transfer.
    }

    return pdl__t; // Weak pointer.
}

CcPosDescLink * cc_pos_desc_link_duplicate_all__new( CcPosDescLink * pd_link ) {
    if ( !pd_link ) return NULL;

    CcPosDescLink * pd_link__a = NULL;
    CcPosDescLink * from = pd_link;

    while ( from ) {
        CcPosDescLink * pdl__w = cc_pos_desc_link_append( &pd_link__a, from->pd );
        if ( !pdl__w ) { // Failed append --> ownership not transferred ...
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

    CcPosDescLink * pdl = *pd_link__f;
    CcPosDescLink * tmp = NULL;

    while ( pdl ) {
        tmp = pdl->next;
        CC_FREE( pdl );
        pdl = tmp;
    }

    *pd_link__f = NULL;
    return true;
}

size_t cc_pos_desc_link_len( CcPosDescLink * pd_link ) {
    if ( !pd_link ) return 0;

    size_t len = 0;
    CcPosDescLink * pdl = pd_link;

    while ( pdl ) {
        ++len;
        pdl = pdl->next;
    }

    return len;
}

char * cc_pos_desc_link_to_string__new( CcPosDescLink * pd_link ) {
    if ( !pd_link ) return NULL;

    // unused len is certainly > 0, because pd_link != NULL
    signed int unused = cc_pos_desc_link_len( pd_link ) *
                        ( 1 + CC_MAX_LEN_CHAR_8 + 1 + 1 );
                        // 1 +, for piece symbol
                        // CC_MAX_LEN_CHAR_8, for position
                        // + 1, for tag symbol
                        // + 1, for separator ',' between position descriptors

    char * pdl_str__a = malloc( unused + 1 ); // +1, for '\0'
    if ( !pdl_str__a ) return NULL;

    *pdl_str__a = '\0';

    char * pdl_str = pdl_str__a;
    char * pdl_end = pdl_str;
    cc_char_8 pos_c8 = CC_CHAR_8_EMPTY;
    CcPosDescLink * pdl = pd_link;

    while ( pdl && ( unused > 0 ) ) {
        if ( pdl != pd_link ) { // Not 1st pos desc ...
            *pdl_str++ = ',';
            *pdl_str = '\0';
            --unused;
        }

        // Piece as char, i.e. lower-case for dark/dim pieces.
        *pdl_str++ = cc_piece_as_char( pdl->pd.piece );
        *pdl_str = '\0';
        --unused;

        // Position.
        if ( !cc_pos_to_string( pdl->pd.pos, &pos_c8 ) ) {
            CC_FREE( pdl_str__a );
            return NULL;
        }

        pdl_end = cc_str_append_into( pdl_str, unused, pos_c8, CC_MAX_LEN_CHAR_8 );
        if ( !pdl_end ) {
            CC_FREE( pdl_str__a );
            return NULL;
        }

        unused -= ( pdl_end - pdl_str );
        pdl_str = pdl_end;

        // Tag.
        *pdl_str++ = cc_tag_as_char( pdl->pd.piece );
        *pdl_str = '\0';
        --unused;

        pdl = pdl->next;
    }

    return pdl_str__a;
}

//
// Momentum.

CcMaybeBoolEnum cc_calc_momentum( CcMomentumUsageEnum usage,
                                  cc_uint_t count,
                                  cc_uint_t * momentum__io ) {
    if ( !momentum__io ) return CC_MBE_Void;

    if ( usage == CC_MUE_Accumulating ) {
        if ( *momentum__io > UINT_MAX - count ) return CC_MBE_False;
        *momentum__io += count;
    } else if ( usage == CC_MUE_Spending ) {
        if ( *momentum__io < CC_UNSIGNED_MIN + count ) return CC_MBE_False;
        *momentum__io -= count;
    } else if ( usage == CC_MUE_NotUsing ) {
        // If usage is CC_MUE_NotUsing, momentum stays the same, e.g. for Wave.
    } else
        return CC_MBE_Void; // Enums are secretly ints.

    return CC_MBE_True;
}

//
// Activation descriptor.

CcMaybeBoolEnum cc_activation_desc_is_valid( CcActivationDesc act_desc, bool is_first_ply ) {
    if ( !CC_PIECE_IS_ENUMERATOR( act_desc.activator ) ) return CC_MBE_Void;
    if ( !CC_MOMENTUM_USAGE_IS_ENUMERATOR( act_desc.usage ) ) return CC_MBE_Void;

    if ( act_desc.momentum >= CC_MAX_BOARD_SIZE ) return CC_MBE_False;

    if ( is_first_ply ) {
        // Activator has to be CC_PTE_None, for piece starting a move.
        if ( act_desc.activator != CC_PTE_None ) return CC_MBE_False;
    } else {
        // Otherwise, actvateor has to be valid.
        if ( !CC_PIECE_IS_ACTIVATOR( act_desc.activator ) ) return CC_MBE_False;
    }

    // .usage is valid, if it's CC_MUE_NotUsing.
    // if ( act_desc.usage == CC_MUE_NotUsing ) return CC_MBE_False;

    return CC_MBE_True;
}

CcMaybeBoolEnum cc_activation_desc_calc_next_momentum( CcActivationDesc * act_desc__io, cc_uint_t count ) {
    return cc_calc_momentum( act_desc__io->usage, count, &( act_desc__io->momentum ) );
}

CcMaybeBoolEnum cc_activation_desc_update_activator( CcActivationDesc * act_desc__io, CcPieceTagType piece ) {
    if ( !act_desc__io ) return CC_MBE_Void;
    if ( !CC_PIECE_IS_ENUMERATOR( piece ) ) return CC_MBE_Void;

    CcMaybeBoolEnum is_valid = cc_activation_desc_is_valid( *act_desc__io, true ); // true --> ignore if old activator is none.
    if ( is_valid != CC_MBE_True ) return CC_MBE_Void;

    if ( CC_PIECE_IS_ACTIVATOR( piece ) ) {
        act_desc__io->activator = piece;
        return CC_MBE_True;
    } else
        return CC_MBE_False;
}

CcMaybeBoolEnum cc_activation_desc_is_usable( CcActivationDesc act_desc, bool is_first_ply ) {
    CcMaybeBoolEnum result = cc_activation_desc_is_valid( act_desc, is_first_ply );
    if ( result != CC_MBE_True ) return result;

    if ( act_desc.usage == CC_MUE_Spending )
        return ( act_desc.momentum > 0 ) ? CC_MBE_True
                                         : CC_MBE_False;

    return CC_MBE_True;
}
