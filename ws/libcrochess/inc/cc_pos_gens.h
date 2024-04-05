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


// static bool cc_steps_gen_bail_out( CcPos * restrict previous_step__iod,
//                                    CcPos * restrict last_step__iod,
//                                    CcPosLink ** restrict previous_steps__iod_af,
//                                    CcPosLink ** restrict possible_steps__iod_af );

// static bool cc_pawn_steps( CcVariantEnum type,
//                            CcPieceEnum activator,
//                            CcPieceEnum piece,
//                            CcPosLink ** restrict steps__od );


bool cc_steps_gen( CcVariantEnum type,
                   CcPieceEnum activator,
                   CcPieceEnum piece,
                   CcPos * restrict previous_step__iod,
                   CcPos * restrict last_step__iod,
                   CcPosLink ** restrict previous_steps__iod_af,
                   CcPosLink ** restrict possible_steps__iod_af );


#endif /* __CC_POS_GENS_H__ */
