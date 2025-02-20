// Copyright (c) 2025 Mario Mlačak, mmlacak@gmail.com
// Licensed under GNU GPL v3+ license. See LICENSING, COPYING files for details.

#ifndef __CC_RULES_DEFS_H__
#define __CC_RULES_DEFS_H__

#include <stdbool.h>

// #include "cc_defines.h"
// #include "cc_piece.h"
// #include "cc_tag.h"
//
// #include "cc_pos.h"
// #include "cc_chessboard.h"
// #include "cc_move.h"
// #include "cc_game.h"


#define CC_TRANCE_JOURNEY_TYPE_IS_ENUMERATOR(tjte) ( ( CC_TJTE_None <= (tjte) ) && ( (tjte) <= CC_TJTE_DoubleCapture ) )

#define CC_TRANCE_JOURNEY_TYPE_IS_VALID(tjte) ( ( CC_TJTE_None < (tjte) ) && ( (tjte) <= CC_TJTE_DoubleCapture ) )

#define CC_TRANCE_JOURNEY_TYPE_IS_ANY_CAPTURE(tjte) ( ( (tjte) == CC_TJTE_Capture ) \
                                                   || ( (tjte) == CC_TJTE_DoubleCapture ) )

typedef enum CcTranceJourneyTypeEnum {
    CC_TJTE_None,
    CC_TJTE_Displacement,
    CC_TJTE_Capture,
    CC_TJTE_DoubleCapture,
} CcTranceJourneyTypeEnum;


#endif /* __CC_RULES_DEFS_H__ */
