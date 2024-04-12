// Copyright (c) 2024 Mario Mlačak, mmlacak@gmail.com
// Licensed under GNU GPL v3+ license. See LICENSING, COPYING files for details.

#ifndef __CC_POS_GENS_H__
#define __CC_POS_GENS_H__

#include <stdbool.h>

#include "cc_piece.h"
#include "cc_variant.h"
#include "cc_pos.h"

/**
    @file cc_pos_gens.h
    @brief Position generators.
*/


bool cc_pawn_all_steps( CcVariantEnum type,
                        CcPieceEnum activator,
                        CcPieceEnum piece,
                        CcPosLink ** restrict all_steps__od );


#endif /* __CC_POS_GENS_H__ */
