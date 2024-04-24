// Copyright (c) 2024 Mario Mlačak, mmlacak@gmail.com
// Licensed under GNU GPL v3+ license. See LICENSING, COPYING files for details.

#ifndef __CC_POS_GENS_H__
#define __CC_POS_GENS_H__

#include <stdbool.h>

#include "cc_piece.h"
// #include "cc_variant.h"
#include "cc_chessboard.h"
#include "cc_pos.h"

/**
    @file cc_pos_gens.h
    @brief Position generators.
*/


#define CC_STEPS_HAS_MOVEMENT(ste) ( ( (ste) == CC_STE_All )        \
                                  || ( (ste) == CC_STE_Movement ) )

#define CC_STEPS_HAS_CAPTURE(ste) ( ( (ste) == CC_STE_All )        \
                                 || ( (ste) == CC_STE_Capture ) )

#define CC_STEPS_HAS_ALTERNATIVE(ste) ( ( (ste) == CC_STE_All )        \
                                     || ( (ste) == CC_STE_Alternative ) )


typedef enum CcStepTypeEnum {
    CC_STE_None = 0,
    CC_STE_All,
    CC_STE_Movement,
    CC_STE_Capture,
    CC_STE_Alternative, // Alternative movement, one of color-change-, entrancement-, uplifting-, miracle-steps.
} CcStepTypeEnum;


bool cc_pawn_steps( CcChessboard * cb,
                    CcPieceEnum activator,
                    CcPieceEnum piece,
                    CcPos current_pos,
                    CcStepTypeEnum steps_type,
                    CcPosLink ** steps__od );


#endif /* __CC_POS_GENS_H__ */
