// Copyright (c) 2021, 2022 Mario Mlačak, mmlacak@gmail.com
// Licensed under GNU GPL v3+ license. See LICENSING, COPYING files for details.

#include <ctype.h>
#include <stdlib.h>

#include "cc_defines.h"
#include "cc_str_utils.h"
// #include "cc_variant.h"
#include "cc_token.h"
// #include "cc_parse_defs.h"
#include "cc_game.h"

/**
    @file cc_game.c
    @brief Functions related to game storage.
*/

char const CC_GAME_SEPARATORS_SETUP_FROM_STRING[] = ",";


CcGameStatusEnum cc_game_status_next( CcGameStatusEnum gse,
                                      bool is_end,
                                      bool is_won ) {
    if ( is_end ) {
        if ( is_won ) {
            if ( gse == CC_GSE_Turn_Light ) return CC_GSE_Win_Light;
            if ( gse == CC_GSE_Turn_Dark ) return CC_GSE_Win_Dark; }
        else
            return CC_GSE_Draw;
    }

    if ( gse == CC_GSE_Turn_Light ) return CC_GSE_Turn_Dark;
    if ( gse == CC_GSE_Turn_Dark ) return CC_GSE_Turn_Light;
    if ( gse == CC_GSE_None ) return CC_GSE_None;

    return gse;
}

CcGameStatusEnum cc_game_resign( CcGameStatusEnum gse ) {
    if ( gse == CC_GSE_Turn_Light ) return CC_GSE_Win_Dark;
    if ( gse == CC_GSE_Turn_Dark ) return CC_GSE_Win_Light;
    return gse;
}


CcGame * cc_game__new( CcGameStatusEnum status,
                       CcVariantEnum ve,
                       bool do_setup ) {
    CcGame * gm__a = malloc( sizeof( CcGame ) );
    if ( !gm__a ) return NULL;

    gm__a->status = status;

    gm__a->chessboard = cc_chessboard__new( ve, do_setup );
    if ( !gm__a->chessboard ) {
        CC_FREE( gm__a );
        return NULL;
    }

    gm__a->moves = NULL;

    return gm__a;
}

CcGame * cc_game_duplicate_all__new( CcGame * restrict game ) {
    if ( !game ) return NULL;

    CcVariantEnum ve = game->chessboard ? game->chessboard->type : CC_VE_One;

    CcGame * gm__a = cc_game__new( game->status, ve, false );
    if ( !gm__a ) return NULL;

    CcChessboard * cb__t = cc_chessboard_duplicate__new( game->chessboard );
    if ( game->chessboard && ( !cb__t ) ) {
        cc_game_free_all( &gm__a );
        return NULL;
    }

    gm__a->chessboard = cb__t; // Ownership transfer --> cb__t is now weak pointer.

    CcMove * mv__t = cc_move_duplicate_all__new( game->moves );
    if ( game->moves && ( !mv__t ) ) {
        cc_game_free_all( &gm__a );
        return NULL;
    }

    gm__a->moves = mv__t; // Ownership transfer --> mv__t is now weak pointer.

    return gm__a;
}

bool cc_game_free_all( CcGame ** restrict game__f ) {
    if ( !game__f ) return false;
    if ( !*game__f ) return true;

    bool result = true;

    CcChessboard ** cb__a = &( ( *game__f )->chessboard );
    result = cc_chessboard_free_all( cb__a ) && result;

    CcMove ** mv__a = &( ( *game__f )->moves );
    result = cc_move_free_all( mv__a ) && result;

    CC_FREE_NULL( game__f );

    return result;
}

CcGame * cc_game_setup_from_string__new( char const * restrict setup,
                                         CcGame * restrict before_setup__d ) {
    if ( !setup ) return NULL;

    CcVariantEnum ve = CC_VE_One;
    CcGameStatusEnum gse = CC_GSE_Turn_Light;

    char const * s = setup;
    CcGame * game__a = NULL;

    if ( before_setup__d ) {
        game__a = cc_game_duplicate_all__new( before_setup__d ); }
    else {
        size_t len = cc_variant_from_symbol( s, &ve );
        gse = islower( *s ) ? CC_GSE_Turn_Dark : CC_GSE_Turn_Light;

        game__a = cc_game__new( gse, ve, false );

        // +1 == next char, after separator (space) following variant symbol string
        s += len + 1;
    }

    if ( !game__a ) return NULL;

    char const * start = NULL;
    char const * end = NULL;

    while ( cc_iter_token( s, CC_GAME_SEPARATORS_SETUP_FROM_STRING, &start, &end ) ) {
        char const * c = start;

        char piece_chr = *c++;
        CcPieceEnum pe = cc_piece_from_char( piece_chr );

        char file_chr = *c++;
        int file = CC_CONVERT_FILE_CHAR_INTO_NUM( file_chr );

        cc_char_8 rank_c8 = CC_CHAR_8_EMPTY;
        rank_c8[ 0 ] = *c++;
        if ( isdigit( *c ) )
            rank_c8[ 1 ] = *c++;
        int rank = CC_CONVERT_RANK_STR_INTO_NUM( rank_c8 );

        char tag = *c;
        CcTagEnum te = cc_tag_from_char( tag );

        if ( !cc_chessboard_set_piece_tag( game__a->chessboard, file, rank, pe, te ) ) {
            cc_game_free_all( &game__a );
            return NULL;
        }
    }

    return game__a;
}
