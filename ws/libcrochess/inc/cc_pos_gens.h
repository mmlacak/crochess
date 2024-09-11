// Copyright (c) 2024 Mario Mlačak, mmlacak@gmail.com
// Licensed under GNU GPL v3+ license. See LICENSING, COPYING files for details.

#ifndef __CC_POS_GENS_H__
#define __CC_POS_GENS_H__

#include <stdbool.h>

#include "cc_piece.h"
// #include "cc_variant.h"
#include "cc_chessboard.h"
#include "cc_pos.h"


// DOCS
// #define CC_STEPS_HAS_MOVEMENT(ste) ( ( (ste) == CC_STE_All )        \
//                                   || ( (ste) == CC_STE_MovementOnly ) )

// DOCS
// #define CC_STEPS_HAS_CAPTURE(ste) ( ( (ste) == CC_STE_All )        \
//                                  || ( (ste) == CC_STE_CaptureOrMovement ) )

// DOCS
// #define CC_STEPS_HAS_ALTERNATIVE(ste) ( ( (ste) == CC_STE_All )        \
//                                      || ( (ste) == CC_STE_Alternative ) )


// DOCS
bool cc_pawn_steps( CcChessboard * cb,
                    CcPieceType activator,
                    CcPieceType piece,
                    CcPos current_pos,
                    CcStepTypeEnum steps_type,
                    CcTypedStepLink ** steps__o );


#endif /* __CC_POS_GENS_H__ */
