// Copyright (c) 2021 Mario Mlačak, mmlacak@gmail.com
// Licensed under 3-clause (modified) BSD license. See LICENSE for details.

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
    if ( gse == CC_GSE_None ) return CC_GSE_Turn_Light;

    return gse;
}
