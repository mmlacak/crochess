// Copyright (c) 2021, 2022 Mario Mlačak, mmlacak@gmail.com
// Licensed under GNU GPL v3+ license. See LICENSING, COPYING files for details.

#include <ctype.h>
#include <stdlib.h>

#include "cc_defines.h"
#include "cc_str_utils.h"
// #include "cc_variant.h"
#include "cc_token.h"
#include "cc_game.h"


char const CC_GAME_SEPARATORS_SETUP_FROM_STRING[] = ",";


CcGameStatusEnum cc_game_status_next( CcGameStatusEnum gse,
                                      bool is_end,
                                      bool is_won ) {
    if ( is_end ) {
        if ( is_won ) {
            if ( gse == CC_GSE_Turn_Light ) return CC_GSE_Win_Light;
            if ( gse == CC_GSE_Turn_Dark ) return CC_GSE_Win_Dark;
        } else
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
                       CcVariantType ve,
                       bool do_setup ) {
    if ( !CC_GAME_STATUS_IS_VALID( status ) ) return NULL;
    if ( !CC_VARIANT_IS_VALID( ve) ) return NULL;

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

// TODO :: FIX :: CcPosDescLink * pdl --> CcChessboard * cb
// bool cc_game_update_chessboard( CcGame * game__io, CcPosDescLink * pdl ) {
//     if ( !game__io ) return false;
//     if ( !game__io->chessboard ) return false;
//     if ( !pdl ) return false;
//
//     CcChessboard * cb__t = cc_chessboard_duplicate__new( game__io->chessboard );
//     if ( !cb__t ) return false;
//
//     CcPosDescLink * _pdl = pdl;
//     CcPos pos = CC_POS_CAST_INVALID;
//     CcPieceTagType pt = CC_PTE_None;
//     CcTagType tt = CC_TE_None;
//
//     while ( _pdl ) {
//         pos = _pdl->pd.pos;
//         pt = _pdl->pd.piece;
//         tt = _pdl->pd.tag;
//
//         if ( !cc_chessboard_set_piece_tag( cb__t, pos.i, pos.j, pt, tt ) ) {
//             CC_FREE( cb__t );
//             return false;
//         }
//
//         _pdl = _pdl->next;
//     }
//
//     CC_FREE( game__io->chessboard );
//     game__io->chessboard = cb__t; // Ownership transfer.
//     CC_FREE( cb__t );
//
//     return true;
// }
// TODO :: FIX :: CcPosDescLink * pdl --> CcChessboard * cb

bool cc_game_free_all( CcGame ** game__f ) {
    if ( !game__f ) return false;
    if ( !*game__f ) return true;

    bool result = true;

    CcChessboard ** cb__a = &( ( *game__f )->chessboard );
    result = cc_chessboard_free_all( cb__a ) && result;

    CcMove ** mv__a = &( ( *game__f )->moves );
    result = cc_move_free_all( mv__a ) && result;

    CC_FREE_AND_NULL( game__f );

    return result;
}

CcGame * cc_game_duplicate_all__new( CcGame * game, bool copy_history ) {
    if ( !game ) return NULL;
    if ( !game->chessboard ) return NULL;

    CcVariantType ve = game->chessboard->type;

    CcGame * gm__a = cc_game__new( game->status, ve, false );
    if ( !gm__a ) return NULL;

    CcChessboard * cb__t = cc_chessboard_duplicate__new( game->chessboard );
    if ( !cb__t ) {
        cc_game_free_all( &gm__a );
        return NULL;
    }

    gm__a->chessboard = cb__t; // Ownership transfer --> cb__t is now weak pointer.

    if ( copy_history && game->moves ) {
        CcMove * mv__t = cc_move_duplicate_all__new( game->moves );
        if ( !mv__t ) {
            cc_game_free_all( &gm__a );
            return NULL;
        }

        gm__a->moves = mv__t; // Ownership transfer --> mv__t is now weak pointer.
    }

    return gm__a;
}

CcGame * cc_game_setup_from_string__new( char const * setup,
                                         CcGame * before_setup__d ) {
    if ( !setup ) return NULL;

    CcVariantType ve = CC_VE_One;
    CcGameStatusEnum gse = CC_GSE_Turn_Light;

    char const * s = setup + ( ( *setup == '\"' ) ? 1 : 0 );
    CcGame * game__a = NULL;

    if ( before_setup__d ) {
        game__a = cc_game_duplicate_all__new( before_setup__d, false );
    } else {
        size_t len = cc_variant_from_symbol( s, &ve );
        gse = islower( *s ) ? CC_GSE_Turn_Dark : CC_GSE_Turn_Light;

        game__a = cc_game__new( gse, ve, false );

        // +1 == next char, after separator (space) following variant symbol string,
        //       or after \" (double quotes) == start of a string, if no variant symbol string
        s += len;
        s += ( *s == ' ' ) ? 1 : 0;
    }

    if ( !game__a ) return NULL;

    char const * start = NULL;
    char const * end = NULL;

    while ( cc_iter_token( s, CC_GAME_SEPARATORS_SETUP_FROM_STRING, &start, &end ) ) {
        char const * c = start;

        char piece_chr = *c++;

        char file_chr = *c++;
        int file = CC_CONVERT_FILE_CHAR_INTO_NUM( file_chr );

        cc_char_8 rank_c8 = CC_CHAR_8_EMPTY;
        rank_c8[ 0 ] = *c++;
        if ( isdigit( *c ) )
            rank_c8[ 1 ] = *c++;
        int rank = CC_CONVERT_RANK_STR_INTO_NUM( rank_c8 );

        char tag = *c;
        CcPieceTagType pe = cc_piece_from_char( piece_chr, tag ); // todo :: maybe :: Pawn-sacrifice flags support (?)

        if ( !cc_chessboard_set_piece( game__a->chessboard, file, rank, pe ) ) {
            cc_game_free_all( &game__a );
            return NULL;
        }
    }

    return game__a;
}
