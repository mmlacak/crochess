// Copyright (c) 2021 Mario Mlačak, mmlacak@gmail.com
// Licensed under GNU GPL v3+ license. See LICENSING, COPYING files for details.

#include <stdbool.h>
#include <stdlib.h>
#include <string.h>
#include <stdio.h>
#include <ctype.h>

#include "cc_math.h"
#include "cc_str_utils.h"
#include "cc_token.h"

#include "cc_piece.h"
#include "cc_tag.h"
#include "cc_variant.h"
#include "cc_setup_board.h"
#include "cc_chessboard.h"


char const CC_CHESSBOARD_SEPARATORS_SETUP_FROM_STRING[] = ",";


CcChessboard * cc_chessboard__new( CcVariantType ve, bool do_setup ) {
    CcChessboard * cb__a = CC_MALLOC( sizeof( CcChessboard ) );
    if ( !cb__a ) return NULL;

    if ( !cc_chessboard_init( cb__a, ve, do_setup ) ) {
        cc_chessboard_free_all( &cb__a );
        return NULL;
    }

    return cb__a;
}

bool cc_chessboard_init( CcChessboard * cb__io,
                         CcVariantType ve,
                         bool do_setup ) {
    if ( !cb__io ) return false;

    cb__io->type = ve;

    if ( do_setup )
        return cc_chessboard_setup( cb__io );
    else
        return cc_chessboard_clear( cb__io );
}

bool cc_chessboard_clear( CcChessboard * cb__io ) {
    if ( !cb__io ) return false;

    for ( int i = 0; i < CC_VARIANT_BOARD_SIZE_MAXIMUM; ++i ) {
        for ( int j = 0; j < CC_VARIANT_BOARD_SIZE_MAXIMUM; ++j ) {
            cb__io->board[ i ][ j ] = CC_PTE_None;
        }
    }

    return true;
}

bool cc_chessboard_setup( CcChessboard * cb__io ) {
    if ( !cb__io ) return false;

    if ( !cc_chessboard_clear( cb__io ) ) return false;

    CcPieceTagType const * su = cc_setup_board_get( cb__io->type );
    if ( !su ) return false;

    cc_uint_t size = cc_variant_board_size( cb__io->type );
    if ( !CC_IS_BOARD_SIZE_VALID( size ) ) return false;

    for ( int i = 0; i < (int)size; ++i ) {
        for ( int j = 0; j < (int)size; ++j ) {
            int x = j;
            int y = size - i - 1;
            int z = size * i + j;

            cb__io->board[ x ][ y ] = su[ z ];
        }
    }

    return true;
}

cc_uint_t cc_chessboard_get_size( CcChessboard * cb ) {
    if ( !cb ) return 0;
    return cc_variant_board_size( cb->type );
}


bool cc_chessboard_copy( CcChessboard * into__io,
                         CcChessboard * from ) {
    if ( !into__io ) return false;
    if ( !from ) return false;
    if ( !CC_VARIANT_IS_VALID( from->type ) ) return false;

    if ( !cc_chessboard_init( into__io, from->type, false ) )
        return false;

    cc_uint_t size = cc_variant_board_size( from->type );
    if ( !CC_IS_BOARD_SIZE_VALID( size ) ) return false;

    for ( int i = 0; i < (int)size; ++i ) {
        for ( int j = 0; j < (int)size; ++j ) {
            into__io->board[ i ][ j ] = from->board[ i ][ j ];
        }
    }

    return true;
}

CcChessboard * cc_chessboard_duplicate__new( CcChessboard * from ) {
    if ( !from ) return NULL;

    CcChessboard * cb__a = CC_MALLOC( sizeof( CcChessboard ) );
    if ( !cb__a ) return NULL;

    cc_chessboard_init( cb__a, from->type, false );

    if ( !cc_chessboard_copy( cb__a, from ) ) {
        cc_chessboard_free_all( &cb__a );
        return NULL;
    }

    return cb__a;
}

bool cc_chessboard_free_all( CcChessboard ** cb__f ) {
    if ( !cb__f ) return false;
    if ( !*cb__f ) return true;

    CC_FREE_AND_NULL( cb__f );
    return true;
}

bool cc_chessboard_is_coord_on_board( CcChessboard * cb, int coord ) {
    if ( !cb ) return false;

    cc_uint_t size = cc_variant_board_size( cb->type );
    if ( !CC_IS_BOARD_SIZE_VALID( size ) ) return false;

    return CC_IS_COORD_ON_BOARD( size, coord );
}

bool cc_chessboard_is_pos_on_board( CcChessboard * cb, int i, int j ) {
    if ( !cb ) return false;

    cc_uint_t size = cc_variant_board_size( cb->type );
    if ( !CC_IS_BOARD_SIZE_VALID( size ) ) return false;

    return CC_IS_POS_ON_BOARD( size, i, j );
}

bool cc_chessboard_is_disambiguation_on_board( CcChessboard * cb, int i, int j ) {
    if ( !cb ) return false;

    cc_uint_t size = cc_variant_board_size( cb->type );
    if ( !CC_IS_BOARD_SIZE_VALID( size ) ) return false;

    return CC_IS_DISAMBIGUATION_ON_BOARD( size, i, j );
}

bool cc_chessboard_is_coord_safe_off_board( CcChessboard * cb, int coord ) {
    if ( !cb ) return false;

    cc_uint_t size = cc_variant_board_size( cb->type );
    if ( !CC_IS_BOARD_SIZE_VALID( size ) ) return false;

    size_t diag = cc_diagonal( size );

    return ( ( (int)(-diag) <= coord ) && ( coord <= (int)( size + diag ) ) );
}

bool cc_chessboard_is_pos_safe_off_board( CcChessboard * cb, int i, int j ) {
    if ( !cb ) return false;

    cc_uint_t size = cc_variant_board_size( cb->type );
    if ( !CC_IS_BOARD_SIZE_VALID( size ) ) return false;

    size_t diag = cc_diagonal( size );

    return ( ( (int)(-diag) <= i ) && ( i <= (int)( size + diag ) ) && \
             ( (int)(-diag) <= j ) && ( j <= (int)( size + diag ) ) );
}

bool cc_chessboard_is_disambiguation_safe_off_board( CcChessboard * cb, int i, int j ) {
    if ( !cb ) return false;

    if ( cc_chessboard_is_pos_safe_off_board( cb, i, j ) ) return true;

    cc_uint_t size = cc_variant_board_size( cb->type );
    if ( !CC_IS_BOARD_SIZE_VALID( size ) ) return false;

    size_t diag = cc_diagonal( size );

    return ( ( ( (int)(-diag) <= i ) && ( i <= (int)( size + diag ) ) && ( j == CC_INVALID_COORD ) ) || \
             ( ( (int)(-diag) <= j ) && ( j <= (int)( size + diag ) ) && ( i == CC_INVALID_COORD ) ) );
}

bool cc_chessboard_is_field_on_light_side( CcChessboard * cb, int j ) {
    if ( !cb ) return false;

    cc_uint_t size = cc_variant_board_size( cb->type );
    // if ( !CC_IS_BOARD_SIZE_VALID( size ) ) return false; // <!> Not needed, CC_IS_FIELD_ON_LIGHT_SIDE() also checks size.

    return CC_IS_FIELD_ON_LIGHT_SIDE( size, j );
}

bool cc_chessboard_is_field_on_dark_side( CcChessboard * cb, int j ) {
    if ( !cb ) return false;

    cc_uint_t size = cc_variant_board_size( cb->type );
    // if ( !CC_IS_BOARD_SIZE_VALID( size ) ) return false; // <!> Not needed, CC_IS_FIELD_ON_DARK_SIDE() also checks size.

    return CC_IS_FIELD_ON_DARK_SIDE( size, j );
}

CcPieceTagType cc_chessboard_get_piece( CcChessboard * cb, int i, int j ) {
    if ( !cb ) return CC_PTE_None;

    if ( cc_chessboard_is_pos_on_board( cb, i, j ) )
        return cb->board[ i ][ j ];

    return CC_PTE_None;
}

bool cc_chessboard_set_piece( CcChessboard * cb__io,
                              int i,
                              int j,
                              CcPieceTagType ptt ) {
    if ( !cb__io ) return false;
    if ( !CC_PIECE_IS_ENUMERATOR( ptt ) ) return false;

    if ( cc_chessboard_is_pos_on_board( cb__io, i, j ) ) {
        cb__io->board[ i ][ j ] = ptt;

        return ( cb__io->board[ i ][ j ] == ptt );
    }

    return false;
}


bool cc_chessboard_is_equal( CcChessboard * cb, CcChessboard * cb_2 ) {
    if ( !cb ) return false;
    if ( !cb_2 ) return false;

    if ( cb->type != cb_2->type ) return false;

    cc_uint_t size = cc_variant_board_size( cb->type );
    if ( !CC_IS_BOARD_SIZE_VALID( size ) ) return false;

    for ( int i = 0; i < (int)size; ++i ) {
        for ( int j = 0; j < (int)size; ++j ) {
            if ( cb->board[ i ][ j ] != cb_2->board[ i ][ j ] ) return false;
        }
    }

    return true;
}


static char * _cc_chessboard_get_divider__new( CcChessboard * cb ) {
    if ( !cb ) return NULL;

    cc_uint_t size = cc_variant_board_size( cb->type );
    if ( !CC_IS_BOARD_SIZE_VALID( size ) ) return NULL;

    size_t len = 3 + 2 * size + 3 + 1;
    char * divider__a = calloc( 1, len );
    if ( !divider__a ) return NULL;

    for ( int i = 0; i < (int)len; ++i ) {
        if ( i < 3 ) divider__a[ i ] = ' ';
        else if ( i < 3 + 2 * (int)size - 1 ) divider__a[ i ] = '-';
        else if ( i < (int)len ) divider__a[ i ] = ' ';
    }

    divider__a[ len - 2 ] = '\n';
    divider__a[ len - 1 ] = '\0';

    return divider__a;
}

static char * _cc_chessboard_get_horizontal_ruler__new( CcChessboard * cb ) {
    if ( !cb ) return NULL;

    cc_uint_t size = cc_variant_board_size( cb->type );
    if ( !CC_IS_BOARD_SIZE_VALID( size ) ) return NULL;

    size_t len = 3 + 2 * size + 3 + 1;
    char * hr__a = calloc( 1, len );
    if ( !hr__a ) return NULL;

    char ch = 'a';
    for ( int i = 0; i < (int)len; ++i ) {
        if ( i < 3 ) {
            hr__a[ i ] = ' ';
        } else if ( i < 3 + 2 * (int)size ) {
            if ( i % 2 == 0 ) {
                hr__a[ i ] = ' ';
            } else {
                hr__a[ i ] = ch;
                ++ch;
            }
        } else if ( i < (int)len ) {
            hr__a[ i ] = ' ';
        }
    }

    hr__a[ len - 2 ] = '\n';
    hr__a[ len - 1 ] = '\0';

    return hr__a;
}

char * cc_chessboard_as_string__new( CcChessboard * cb,
                                     bool is_board_or_tag ) {
    if ( !cb ) return NULL;

    cc_uint_t size = cc_variant_board_size( cb->type );
    if ( !CC_IS_BOARD_SIZE_VALID( size ) ) return NULL;

    char * show__a = calloc( 1, 2048 );
    if ( !show__a ) return NULL;

    char * horizontal_ruler = _cc_chessboard_get_horizontal_ruler__new( cb );
    if ( !horizontal_ruler ) {
        CC_FREE( show__a );
        return NULL;
    }

    strcat( show__a, horizontal_ruler );

    char * divider__a = _cc_chessboard_get_divider__new( cb );
    if ( !divider__a ) {
        CC_FREE( show__a );
        CC_FREE( horizontal_ruler );
        return NULL;
    }

    strcat( show__a, divider__a );

    char * row__a = calloc(1, 6);
    if ( !row__a ) {
        CC_FREE( show__a );
        CC_FREE( horizontal_ruler );
        CC_FREE( divider__a );
        return NULL;
    }

    char * field__a = calloc(1, 3);
    if ( !field__a ) {
        CC_FREE( show__a );
        CC_FREE( horizontal_ruler );
        CC_FREE( divider__a );
        CC_FREE( row__a );
        return NULL;
    }

    for ( int i = 0; i < (int)size; ++i ) {
        char r = (char)( size - i );
        sprintf( row__a, "%2hhu|", r );
        strcat( show__a, row__a );

        for ( int j = 0; j < (int)size; ++j ) {
            char ch;
            int x = j;
            int y = size - i - 1;

            if ( is_board_or_tag )
                ch = cc_piece_as_char( cb->board[ x ][ y ] );
            else
                ch = cc_tag_as_char( cb->board[ x ][ y ] );

            if ( ch == ' ' ) {
                if ( CC_IS_FIELD_LIGHT( x, y ) ) ch = '.';
                else ch = ',';
            }

            if ( j < (int)size - 1 )
                sprintf( field__a, "%c ", ch );
            else
                sprintf( field__a, "%c", ch );

            strcat( show__a, field__a );
        }

        sprintf( row__a, "|%2hhu\n", r );
        strcat( show__a, row__a );
    }

    strcat( show__a, divider__a );
    strcat( show__a, horizontal_ruler );

    CC_FREE( horizontal_ruler );
    CC_FREE( divider__a );
    CC_FREE( row__a );
    CC_FREE( field__a );

    return show__a;
}

bool cc_chessboard_print( CcChessboard * cb,
                          bool is_board_or_tag ) {
    if ( !cb ) return false;

    char * show__a = cc_chessboard_as_string__new( cb, is_board_or_tag );
    if ( !show__a ) return false;

    printf( "%s", show__a );
    CC_FREE( show__a );

    return true;
}

CcChessboard * cc_chessboard_clear_from_string__new( CcChessboard * cb,
                                                     char const * setup ) {
    if ( !cb ) return NULL;
    if ( !setup ) return NULL;

    CcChessboard * cb__a = cc_chessboard_duplicate__new( cb );
    if ( !cb__a ) return NULL;

    char const * s = setup
                   + ( ( *setup == '\"' ) ? 1 : 0 );
    char const * start = NULL;
    char const * end = NULL;

    while ( cc_iter_token( s, CC_CHESSBOARD_SEPARATORS_SETUP_FROM_STRING, &start, &end ) ) {
        char const * c = start;

        char file_chr = *c++;
        int file = CC_CONVERT_FILE_CHAR_INTO_NUM( file_chr );

        cc_char_8 rank_c8 = CC_CHAR_8_EMPTY;
        rank_c8[ 0 ] = *c++;
        if ( isdigit( *c ) )
            rank_c8[ 1 ] = *c++;
        int rank = CC_CONVERT_RANK_STR_INTO_NUM( rank_c8 );

        if ( !cc_chessboard_set_piece( cb__a, file, rank, CC_PTE_None ) ) {
            cc_chessboard_free_all( &cb__a );
            return NULL;
        }
    }

    return cb__a;
}
