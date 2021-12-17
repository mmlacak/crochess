// Copyright (c) 2021 Mario Mlačak, mmlacak@gmail.com
// Licensed under GNU GPL v3+ license. See LICENSING, COPYING files for details.

#include "cc_defines.h"
#include "cc_do_moves.h"
#include "cc_game.h"

/**
    @file cc_game.c
    @brief Game enumerations, structures, and related functions.
*/


CcGameStatusEnum cc_game_status_next( CcGameStatusEnum const gse,
                                      bool const is_end,
                                      bool const is_won )
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

CcGameStatusEnum cc_game_resign( CcGameStatusEnum const gse )
{
    if ( gse == CC_GSE_Turn_Light ) return CC_GSE_Win_Dark;
    if ( gse == CC_GSE_Turn_Dark ) return CC_GSE_Win_Light;
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
        CC_FREE( gm );
        return NULL;
    }

    gm->moves = NULL;

    return gm;
}

CcGame * cc_game_duplicate_all_new( CcGame const * const restrict game )
{
    if ( !game ) return NULL;

    CcVariantEnum const ve = game->chessboard ? game->chessboard->type : CC_VE_One;

    CcGame * const gm__t = cc_game_new( game->status, ve, false );
    if ( !gm__t ) return NULL;

    CcChessboard * cb__t = cc_chessboard_duplicate_new( game->chessboard );
    if ( game->chessboard && ( !cb__t ) )
    {
        cc_game_free_all( CC_CAST_TC_P_PC( CcGame, &gm__t ) );
        return NULL;
    }

    gm__t->chessboard = cb__t;

    CcMove * mv__t = cc_move_duplicate_all_new( game->moves );
    if ( game->moves && ( !mv__t ) )
    {
        cc_chessboard_free_all( CC_CAST_TC_P_PC( CcChessboard, &cb__t ) );
        cc_game_free_all( CC_CAST_TC_P_PC( CcGame, &gm__t ) );
        return NULL;
    }

    gm__t->moves = mv__t;

    return gm__t;
}

bool cc_game_free_all( CcGame const ** const restrict game__f )
{
    if ( !game__f ) return false;
    if ( !*game__f ) return true;

    bool result = true;

    CcChessboard ** cb__a = &( ( *game__f )->chessboard );
    result = cc_chessboard_free_all( CC_CAST_TC_P_PC( CcChessboard, cb__a ) ) && result;

    CcMove ** mv__a = &( ( *game__f )->moves );
    result = cc_move_free_all_moves( CC_CAST_TC_P_PC( CcMove, mv__a ) ) && result;

    CC_FREE_NULL( game__f );

    return result;
}

bool cc_game_move_data_free_all( CcGame const ** const restrict gm__f,
                                 CcChessboard const ** const restrict cb__f,
                                 CcMove const ** const restrict moves__f,
                                 CcPly const ** const restrict plies__f,
                                 CcStep const ** const restrict steps__f,
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
