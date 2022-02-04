// Copyright (c) 2021 Mario Mlačak, mmlacak@gmail.com
// Licensed under GNU GPL v3+ license. See LICENSING, COPYING files for details.

#include <stdlib.h>

#include "cc_defines.h"
#include "cc_do_moves.h"
#include "cc_game.h"

/**
    @file cc_game.c
    @brief Functions related to game storage.
*/


CcGameStatusEnum cc_game_status_next( CcGameStatusEnum gse,
                                      bool is_end,
                                      bool is_won )
{
    if ( is_end )
    {
        if ( is_won )
        {
            if ( gse == CC_GSE_Turn_Light ) return CC_GSE_Win_Light;
            if ( gse == CC_GSE_Turn_Dark ) return CC_GSE_Win_Dark;
        }
        else
            return CC_GSE_Draw;
    }

    if ( gse == CC_GSE_Turn_Light ) return CC_GSE_Turn_Dark;
    if ( gse == CC_GSE_Turn_Dark ) return CC_GSE_Turn_Light;
    if ( gse == CC_GSE_None ) return CC_GSE_None;

    return gse;
}

CcGameStatusEnum cc_game_resign( CcGameStatusEnum gse )
{
    if ( gse == CC_GSE_Turn_Light ) return CC_GSE_Win_Dark;
    if ( gse == CC_GSE_Turn_Dark ) return CC_GSE_Win_Light;
    return gse;
}


CcGame * cc_game__new( CcGameStatusEnum status,
                      CcVariantEnum ve,
                      bool do_setup )
{
    CcGame * gm__a = malloc( sizeof( CcGame ) );
    if ( !gm__a ) return NULL;

    gm__a->status = status;

    gm__a->chessboard = cc_chessboard__new( ve, do_setup );
    if ( !gm__a->chessboard )
    {
        CC_FREE( gm__a );
        return NULL;
    }

    gm__a->moves = NULL;

    return gm__a;
}

CcGame * cc_game_duplicate_all__new( CcGame * restrict game )
{
    if ( !game ) return NULL;

    CcVariantEnum ve = game->chessboard ? game->chessboard->type : CC_VE_One;

    CcGame * gm__a = cc_game__new( game->status, ve, false );
    if ( !gm__a ) return NULL;

    CcChessboard * cb__t = cc_chessboard_duplicate__new( game->chessboard );
    if ( game->chessboard && ( !cb__t ) )
    {
        cc_game_free_all( &gm__a );
        return NULL;
    }

    gm__a->chessboard = cb__t; // Ownership transfer --> cb__t is now weak pointer.

    CcMove * mv__t = cc_moves_duplicate_all__new( game->moves );
    if ( game->moves && ( !mv__t ) )
    {
        cc_game_free_all( &gm__a );
        return NULL;
    }

    gm__a->moves = mv__t; // Ownership transfer --> mv__t is now weak pointer.

    return gm__a;
}

bool cc_game_free_all( CcGame ** restrict game__f )
{
    if ( !game__f ) return false;
    if ( !*game__f ) return true;

    bool result = true;

    CcChessboard ** cb__a = &( ( *game__f )->chessboard );
    result = cc_chessboard_free_all( cb__a ) && result;

    CcMove ** mv__a = &( ( *game__f )->moves );
    result = cc_moves_free_all( mv__a ) && result;

    CC_FREE_NULL( game__f );

    return result;
}

bool cc_game_move_data_free_all( CcGame ** restrict gm__f,
                                 CcChessboard ** restrict cb__f,
                                 CcMove ** restrict moves__f,
                                 CcPly ** restrict plies__f,
                                 CcStep ** restrict steps__f,
                                 bool cumulative_result )
{
    bool results = true;

    if ( gm__f ) results = cc_game_free_all( gm__f ) && results;

    if ( cb__f ) results = cc_chessboard_free_all( cb__f ) && results;

    if ( moves__f ) results = cc_moves_free_all( moves__f ) && results;

    if ( plies__f ) results = cc_plies_free_all( plies__f ) && results;

    if ( steps__f ) results = cc_steps_free_all( steps__f ) && results;

    return ( cumulative_result && results );
}
