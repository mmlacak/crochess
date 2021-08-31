// Copyright (c) 2021 Mario Mlačak, mmlacak@gmail.com
// Licensed under 3-clause (modified) BSD license. See LICENSE for details.

#ifndef __CC_GAME_H__
#define __CC_GAME_H__

#include <stdbool.h>


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



#endif /* __CC_GAME_H__ */
