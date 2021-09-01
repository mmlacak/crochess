// Copyright (c) 2021 Mario Mlačak, mmlacak@gmail.com
// Licensed under 3-clause (modified) BSD license. See LICENSE for details.

#ifndef __CC_GAME_H__
#define __CC_GAME_H__

#include <stdbool.h>

#include "cc_chessboard.h"
#include "cc_move.h"

#define CC_GAME_STATUS_IS_LIGHT_TURN(gse) ( (gse) == CC_GSE_Turn_Light )

#define CC_GAME_STATUS_IS_DARK_TURN(gse) ( (gse) == CC_GSE_Turn_Dark )


typedef enum CcGameStatusEnum
{
    CC_GSE_None,
    CC_GSE_Turn_Light,
    CC_GSE_Turn_Dark,
    CC_GSE_Win_Light,
    CC_GSE_Win_Dark,
    CC_GSE_Draw,
} CcGameStatusEnum;

CcGameStatusEnum cc_game_status_next( CcGameStatusEnum const gse,
                                      bool const is_resign,
                                      bool const is_end,
                                      bool const is_win );


typedef struct CcGame
{
    CcGameStatusEnum status;
    CcChessboard * chessboard;
    CcMove * moves;
} CcGame;

CcGame * cc_game_new( CcGameStatusEnum status,
                      CcVariantEnum ve,
                      bool const do_setup );

bool cc_game_do_moves( CcGame * const restrict gm,
                       CcMove ** const restrict moves_n,
                       CcDoMoveEnum dme );

bool cc_game_free_all( CcGame ** const restrict game_f );



#endif /* __CC_GAME_H__ */
