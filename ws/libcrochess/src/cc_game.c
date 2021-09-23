// Copyright (c) 2021 Mario Mlačak, mmlacak@gmail.com
// Licensed under 3-clause (modified) BSD license. See LICENSE for details.

#include "cc_do_moves.h"
#include "cc_game.h"


CcGameStatusEnum cc_game_status_next( CcGameStatusEnum const gse,
                                      bool const is_resign,
                                      bool const is_end,
                                      bool const is_lost )
{
    if ( is_resign )
    {
        if ( gse == CC_GSE_Turn_Light ) return CC_GSE_Win_Dark;
        if ( gse == CC_GSE_Turn_Dark ) return CC_GSE_Win_Light;
    }

    if ( is_end )
    {
        if ( is_lost )
        {
            if ( gse == CC_GSE_Turn_Light ) return CC_GSE_Win_Dark;
            if ( gse == CC_GSE_Turn_Dark ) return CC_GSE_Win_Light;
        }
        else
            return CC_GSE_Draw;
    }

    if ( gse == CC_GSE_Turn_Light ) return CC_GSE_Turn_Dark;
    if ( gse == CC_GSE_Turn_Dark ) return CC_GSE_Turn_Light;
    if ( gse == CC_GSE_None ) return CC_GSE_None;

    return gse;
}


CcGame * cc_game_new( CcGameStatusEnum status,
                      CcVariantEnum ve,
                      bool const do_setup )
{
    CcGame * gm = malloc( sizeof( CcGame ) );
    if ( !gm ) return NULL;

    gm->status = status;

    gm->chessboard = cc_chessboard_new( ve, do_setup );
    if ( !gm->chessboard )
    {
        free( gm );
        return NULL;
    }

    gm->moves = NULL;

    return gm;
}

bool cc_game_free_all( CcGame ** const restrict game__f )
{
    if ( !game__f ) return false;
    if ( !*game__f ) return true;

    bool result = true;

    CcChessboard ** cb = &( ( *game__f )->chessboard );
    result = cc_chessboard_free_all( cb ) && result;

    CcMove ** mv = &( ( *game__f )->moves );
    result = cc_move_free_all_moves( mv ) && result;

    free( *game__f );
    *game__f = NULL;

    return result;
}

bool cc_game_move_data_free_all( CcGame ** const restrict gm__f,
                                 CcChessboard ** const restrict cb__f,
                                 CcMove ** const restrict moves__f,
                                 CcPly ** const restrict plies__f,
                                 CcStep ** const restrict steps__f,
                                 bool const cumulative_result )
{
    bool results = true;

    if ( gm__f ) results = cc_game_free_all( gm__f ) && results;

    if ( cb__f ) results = cc_chessboard_free_all( cb__f ) && results;

    if ( moves__f ) results = cc_move_free_all_moves( moves__f ) && results;

    if ( plies__f ) results = cc_ply_free_all_plies( plies__f ) && results;

    if ( steps__f ) results = cc_step_free_all_steps( steps__f ) && results;

    return ( cumulative_result && results );
}
