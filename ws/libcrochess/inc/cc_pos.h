// Copyright (c) 2021 Mario Mlačak, mmlacak@gmail.com
// Licensed under GNU GPL v3+ license. See LICENSING, COPYING files for details.

#ifndef __CC_POS_H__
#define __CC_POS_H__

#include "cc_piece.h"


typedef struct CcPos {
    int i;
    int j;
} CcPos;

CcPos cc_pos( int const i, int const j );
CcPos cc_pos_empty();


#endif /* __CC_POS_H__ */
