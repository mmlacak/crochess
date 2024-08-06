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
#include "cc_setup_tags.h"
#include "cc_chessboard.h"


char const CC_CHESSBOARD_SEPARATORS_SETUP_FROM_STRING[] = ",";


CcChessboard * cc_chessboard__new( CcVariantEnum ve, bool do_setup ) {
    CcChessboard * cb__a = malloc( sizeof( CcChessboard ) );
    if ( !cb__a ) return NULL;

    if ( !cc_chessboard_init( cb__a, ve, do_setup ) ) {
        CC_FREE( cb__a );
        return NULL;
    }

    return cb__a;
}

bool cc_chessboard_init( CcChessboard * cb__io,
                         CcVariantEnum ve,
                         bool do_setup ) {
    if ( !cb__io ) return false;

    cb__io->type = ve;
    cb__io->size = cc_variant_board_size( cb__io->type );

    if ( do_setup )
        return cc_chessboard_setup( cb__io );
    else
        return cc_chessboard_clear( cb__io );
}

bool cc_chessboard_is_size_valid( CcChessboard * cb ) {
    if ( !cb ) return false;

    cc_uint_t size = cc_variant_board_size( cb->type );
    return ( size == cb->size );
}

bool cc_chessboard_clear( CcChessboard * cb__io ) {
    if ( !cb__io ) return false;

    for ( int i = 0; i < CC_VARIANT_BOARD_SIZE_MAXIMUM; ++i ) {
        for ( int j = 0; j < CC_VARIANT_BOARD_SIZE_MAXIMUM; ++j ) {
            cb__io->board[ i ][ j ] = CC_PE_None;
            cb__io->tags[ i ][ j ] = CC_TE_None;
        }
    }

    return true;
}

bool cc_chessboard_setup( CcChessboard * cb__io ) {
    if ( !cc_chessboard_is_size_valid( cb__io ) ) return false;
    if ( !cc_chessboard_clear( cb__io ) ) return false;

    CcPieceType const * su = cc_setup_board_get( cb__io->type );
    if ( !su ) return false;

    CcTagType const * tu = cc_setup_tags_get( cb__io->type );
    if ( !tu ) return false;

    for ( int i = 0; i < (int)cb__io->size; ++i ) {
        for ( int j = 0; j < (int)cb__io->size; ++j ) {
            int x = j;
            int y = cb__io->size - i - 1;
            int z = cb__io->size * i + j;

            cb__io->board[ x ][ y ] = su[ z ];
            cb__io->tags[ x ][ y ] = tu[ z ];
        }
    }

    return true;
}


bool cc_chessboard_copy( CcChessboard * into__io,
                         CcChessboard * from ) {
    if ( !cc_chessboard_is_size_valid( into__io ) ) return false;
    if ( !cc_chessboard_is_size_valid( from ) ) return false;

    if ( !cc_chessboard_init( into__io, from->type, false ) )
        return false;

    for ( int i = 0; i < (int)into__io->size; ++i ) {
        for ( int j = 0; j < (int)into__io->size; ++j ) {
            into__io->board[ i ][ j ] = from->board[ i ][ j ];
            into__io->tags[ i ][ j ] = from->tags[ i ][ j ];
        }
    }

    return true;
}

CcChessboard * cc_chessboard_duplicate__new( CcChessboard * from ) {
    if ( !from ) return NULL;

    CcChessboard * cb__a = malloc( sizeof( CcChessboard ) );
    if ( !cb__a ) return NULL;

    cc_chessboard_init( cb__a, from->type, false );

    if ( !cc_chessboard_copy( cb__a, from ) ) {
        CC_FREE( cb__a );
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
    if ( !cc_chessboard_is_size_valid( cb ) ) return false;
    return CC_IS_COORD_ON_BOARD( cb->size, coord );
}

bool cc_chessboard_is_pos_on_board( CcChessboard * cb, int i, int j ) {
    if ( !cc_chessboard_is_size_valid( cb ) ) return false;
    return CC_IS_POS_ON_VALID_BOARD( cb->size, i, j );
}

bool cc_chessboard_is_disambiguation_on_board( CcChessboard * cb, int i, int j ) {
    if ( !cc_chessboard_is_size_valid( cb ) ) return false;

    if ( CC_IS_POS_ON_VALID_BOARD( cb->size, i, j ) ) return true;

    return ( ( CC_IS_COORD_ON_BOARD( cb->size, i ) && ( j == CC_INVALID_COORD ) )
          || ( CC_IS_COORD_ON_BOARD( cb->size, j ) && ( i == CC_INVALID_COORD ) ) );
}

bool cc_chessboard_is_coord_safe_off_board( CcChessboard * cb, int coord ) {
    if ( !cc_chessboard_is_size_valid( cb ) ) return false;

    size_t diag = cc_diagonal( cb->size );

    return ( ( (int)(-diag) <= coord ) && ( coord <= (int)( cb->size + diag ) ) );
}

bool cc_chessboard_is_pos_safe_off_board( CcChessboard * cb, int i, int j ) {
    if ( !cc_chessboard_is_size_valid( cb ) ) return false;

    size_t diag = cc_diagonal( cb->size );

    return ( ( (int)(-diag) <= i ) && ( i <= (int)( cb->size + diag ) ) && \
             ( (int)(-diag) <= j ) && ( j <= (int)( cb->size + diag ) ) );
}

bool cc_chessboard_is_disambiguation_safe_off_board( CcChessboard * cb, int i, int j ) {
    // Not needed, checked in cc_chessboard_is_pos_safe_off_board() below.
    // if ( !cc_chessboard_is_size_valid( cb ) ) return false;
    if ( cc_chessboard_is_pos_safe_off_board( cb, i, j ) ) return true;

    size_t diag = cc_diagonal( cb->size );

    return ( ( ( (int)(-diag) <= i ) && ( i <= (int)( cb->size + diag ) ) && ( j == CC_INVALID_COORD ) ) || \
             ( ( (int)(-diag) <= j ) && ( j <= (int)( cb->size + diag ) ) && ( i == CC_INVALID_COORD ) ) );
}

bool cc_chessboard_is_field_on_light_side( CcChessboard * cb, int j ) {
    if ( !cc_chessboard_is_size_valid( cb ) ) return false;
    return CC_IS_FIELD_ON_LIGHT_SIDE( cb->size, j );
}

bool cc_chessboard_is_field_on_dark_side( CcChessboard * cb, int j ) {
    if ( !cc_chessboard_is_size_valid( cb ) ) return false;
    return CC_IS_FIELD_ON_DARK_SIDE( cb->size, j );
}

int cc_chessboard_promoting_rank( CcChessboard * cb, bool is_light ) {
    if ( !cc_chessboard_is_size_valid( cb ) ) return CC_INVALID_COORD;

    if ( !is_light ) return 0;
    return (int)( cb->size - 1 );
}

int cc_chessboard_figure_rank( CcChessboard * cb, bool is_light ) {
    if ( !cc_chessboard_is_size_valid( cb ) ) return CC_INVALID_COORD;

    if ( is_light ) return 0;
    return (int)( cb->size - 1 );
}

CcPieceType cc_chessboard_get_piece( CcChessboard * cb, int i, int j ) {
    if ( cc_chessboard_is_pos_on_board( cb, i, j ) )
        return cb->board[ i ][ j ];

    return CC_PE_None;
}

CcTagType cc_chessboard_get_tag( CcChessboard * cb,
                                 int i,
                                 int j ) {
    if ( cc_chessboard_is_pos_on_board( cb, i, j ) )
        return cb->tags[ i ][ j ];

    return CC_TE_None;
}

bool cc_chessboard_set_piece_tag( CcChessboard * cb__io,
                                  int i,
                                  int j,
                                  CcPieceType pe,
                                  CcTagType tt ) {
    if ( !cb__io ) return false;

    if ( cc_chessboard_is_pos_on_board( cb__io, i, j ) ) {
        cb__io->board[ i ][ j ] = pe;
        cb__io->tags[ i ][ j ] = tt;

        return  ( ( cb__io->board[ i ][ j ] == pe ) &&
                  ( cb__io->tags[ i ][ j ] == tt ) );
    }

    return false;
}

bool cc_chessboard_set_piece( CcChessboard * cb__io,
                              int i,
                              int j,
                              CcPieceType pe ) {
    return cc_chessboard_set_piece_tag( cb__io, i, j, pe, CC_TE_None );
}

bool cc_chessboard_set_tag( CcChessboard * cb__io,
                            int i,
                            int j,
                            CcTagType tt ) {
    if ( !cb__io ) return false;

    if ( cc_chessboard_is_pos_on_board( cb__io, i, j ) ) {
        cb__io->tags[ i ][ j ] = tt;

        return ( cb__io->tags[ i ][ j ] == tt );
    }

    return false;
}


bool cc_chessboard_is_equal( CcChessboard * cb, CcChessboard * cb_2 ) {
    if ( !cc_chessboard_is_size_valid( cb ) ) return false;
    if ( !cc_chessboard_is_size_valid( cb_2 ) ) return false;

    if ( cb->type != cb_2->type ) return false;
    if ( cb->size != cb_2->size ) return false;

    for ( int i = 0; i < (int)cb->size; ++i ) {
        for ( int j = 0; j < (int)cb->size; ++j ) {
            if ( cb->board[ i ][ j ] != cb_2->board[ i ][ j ] ) return false;
            if ( cb->tags[ i ][ j ] != cb_2->tags[ i ][ j ] ) return false;
        }
    }

    return true;
}


static char * cc_chessboard_get_divider__new( CcChessboard * cb ) {
    if ( !cc_chessboard_is_size_valid( cb ) ) return NULL;

    size_t len = 3 + 2 * cb->size + 3 + 1;
    char * divider__a = calloc( 1, len );
    if ( !divider__a ) return NULL;

    for ( int i = 0; i < (int)len; ++i ) {
        if ( i < 3 ) divider__a[ i ] = ' ';
        else if ( i < 3 + 2 * (int)cb->size - 1 ) divider__a[ i ] = '-';
        else if ( i < (int)len ) divider__a[ i ] = ' ';
    }

    divider__a[ len - 2 ] = '\n';
    divider__a[ len - 1 ] = '\0';

    return divider__a;
}

static char * cc_chessboard_get_horizontal_ruler__new( CcChessboard * cb ) {
    if ( !cc_chessboard_is_size_valid( cb ) ) return NULL;

    size_t len = 3 + 2 * cb->size + 3 + 1;
    char * hr__a = calloc( 1, len );
    if ( !hr__a ) return NULL;

    char ch = 'a';
    for ( int i = 0; i < (int)len; ++i ) {
        if ( i < 3 ) {
            hr__a[ i ] = ' ';
        } else if ( i < 3 + 2 * (int)cb->size ) {
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

    char * show__a = calloc( 1, 2048 );
    if ( !show__a ) return NULL;

    char * horizontal_ruler = cc_chessboard_get_horizontal_ruler__new( cb );
    if ( !horizontal_ruler ) {
        CC_FREE( show__a );
        return NULL;
    }

    strcat( show__a, horizontal_ruler );

    char * divider__a = cc_chessboard_get_divider__new( cb );
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

    for ( int i = 0; i < (int)cb->size; ++i ) {
        char r = (char)( cb->size - i );
        sprintf( row__a, "%2hhu|", r );
        strcat( show__a, row__a );

        for ( int j = 0; j < (int)cb->size; ++j ) {
            char ch;
            int x = j;
            int y = cb->size - i - 1;

            if ( is_board_or_tag )
                ch = cc_piece_as_char( cb->board[ x ][ y ] );
            else
                ch = cc_tag_as_char( cb->tags[ x ][ y ] );

            if ( ch == ' ' ) {
                if ( CC_IS_FIELD_LIGHT( x, y ) ) ch = '.';
                else ch = ',';
            }

            if ( j < (int)cb->size - 1 )
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

// TODO :: move out
//
bool cc_chessboard_print( CcChessboard * cb,
                          bool is_board_or_tag ) {
    if ( !cb ) return false;

    char * show__a = cc_chessboard_as_string__new( cb, is_board_or_tag );
    if ( !show__a ) return false;

    printf( "%s", show__a );
    CC_FREE( show__a );

    return true;
}
//
// TODO :: move out

CcChessboard * cc_chessboard_clear_from_string__new( CcChessboard * cb,
                                                     char const * setup ) {
    if ( !cb ) return NULL;

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

        if ( !cc_chessboard_set_piece_tag( cb__a, file, rank, CC_PE_None, CC_TE_None ) ) {
            cc_chessboard_free_all( &cb__a );
            return NULL;
        }
    }

    return cb__a;
}
