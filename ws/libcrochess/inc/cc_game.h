// Copyright (c) 2021, 2022 Mario Mlačak, mmlacak@gmail.com
// Licensed under GNU GPL v3+ license. See LICENSING, COPYING files for details.

#ifndef __CC_GAME_H__
#define __CC_GAME_H__

#include <stdbool.h>

#include "cc_move.h"
#include "cc_chessboard.h"
#include "cc_pos.h"
#include "cc_move.h"


extern char const CC_GAME_SEPARATORS_SETUP_FROM_STRING[];


#define CC_GAME_STATUS_IS_ENUMERATION(gse) ( ( CC_GSE_None <= (gse) ) && ( (gse) <= CC_GSE_Draw ) )

#define CC_GAME_STATUS_IS_VALID(gse) ( ( CC_GSE_None < (gse) ) && ( (gse) <= CC_GSE_Draw ) )

#define CC_GAME_STATUS_IS_TURN(gse) ( ( (gse) == CC_GSE_Turn_Light ) || ( (gse) == CC_GSE_Turn_Dark ) )

typedef enum CcGameStatusEnum {
    CC_GSE_None,
    CC_GSE_Turn_Light,
    CC_GSE_Turn_Dark,
    CC_GSE_Win_Light,
    CC_GSE_Win_Dark,
    CC_GSE_Draw,
} CcGameStatusEnum;

CcGameStatusEnum cc_game_status_next( CcGameStatusEnum gse,
                                      bool is_end,
                                      bool is_won );

CcGameStatusEnum cc_game_resign( CcGameStatusEnum gse );


typedef struct CcGame {
    CcGameStatusEnum status;
    CcChessboard * chessboard;

    CcPosDesc en_passant;
    CcPosDesc pawn_sacrifice;
    CcPosDesc starting_piece; // Current position of a piece starting current move.
    CcPos starting_pos; // Starting position in the current move.

    CcMove * moves;
} CcGame;

CcGame * cc_game__new( CcGameStatusEnum status,
                       CcVariantEnum ve,
                       bool do_setup );

bool cc_game_reset_flags( CcGame * game__io, bool reset_en_passant );

CcGame * cc_game_duplicate_all__new( CcGame * game );

bool cc_game_free_all( CcGame ** game__f );

CcGame * cc_game_setup_from_string__new( char const * setup,
                                         CcGame * before_setup__d );


#endif /* __CC_GAME_H__ */
