// Copyright (c) 2021 Mario Mlačak, mmlacak@gmail.com
// Licensed under 3-clause (modified) BSD license. See LICENSE for details.

#include "cc_do_moves.h"
#include "cc_game.h"


CcGameStatusEnum cc_game_status_next( CcGameStatusEnum const gse,
                                      bool const is_resign,
                                      bool const is_end,
                                      bool const is_win )
{
    if ( is_resign )
    {
        if ( gse == CC_GSE_Turn_Light ) return CC_GSE_Win_Dark;
        if ( gse == CC_GSE_Turn_Dark ) return CC_GSE_Win_Light;
    }

    if ( is_end )
    {
        if ( is_win )
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

bool cc_game_do_moves( CcGame * const restrict gm,
                       CcMove ** const restrict moves_n,
                       CcDoMoveEnum dme )
{
    if ( !gm ) return false;
    if ( !moves_n ) return false;
    if ( !*moves_n ) return false;

    if ( !cc_do_moves( gm->chessboard, *moves_n, dme ) )
        return false;

    if ( !cc_move_append_or_init( &( gm->moves ), moves_n ) )
        return false;

    return true;
}

bool cc_game_free_all( CcGame ** const restrict game_f )
{
    if ( !game_f ) return false;
    if ( !*game_f ) return true;

    bool result = true;

    CcChessboard ** cb = &( ( *game_f )->chessboard );
    result = cc_chessboard_free_all( cb ) && result;

    CcMove ** mv = &( ( *game_f )->moves );
    result = cc_move_free_all_moves( mv ) && result;

    free( *game_f );
    *game_f = NULL;

    return result;
}
