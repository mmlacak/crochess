// Copyright (c) 2021 Mario Mlačak, mmlacak@gmail.com
// Licensed under GNU GPL v3+ license. See LICENSING, COPYING files for details.

#include <stdlib.h>

#include "cc_defines.h"
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

bool cc_game_free_all( CcGame ** restrict game__f )
{
    if ( !game__f ) return false;
    if ( !*game__f ) return true;

    bool result = true;

    CcChessboard ** cb__a = &( ( *game__f )->chessboard );
    result = cc_chessboard_free_all( cb__a ) && result;

    CC_FREE( ( *game__f )->moves );

    CC_FREE_NULL( game__f );

    return result;
}
