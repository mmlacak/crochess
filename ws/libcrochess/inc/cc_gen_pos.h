// Copyright (c) 2021 Mario Mlačak, mmlacak@gmail.com
// Licensed under GNU GPL v3+ license. See LICENSING, COPYING files for details.

#ifndef __CC_GEN_POS_H__
#define __CC_GEN_POS_H__

#include <stddef.h>
#include <stdbool.h>

// #include "cc_gen_steps.h"
#include "cc_pos.h"


// DOCS
#define CC_GEN_POS_PAWN_LEN (3)
#define CC_GEN_POS_BISHOP_LEN (4)
#define CC_GEN_POS_ROOK_LEN (4)
#define CC_GEN_POS_QUEEN_LEN (8)
#define CC_GEN_POS_KNIGHT_LEN (8)
#define CC_GEN_POS_UNICORN_LEN (16)

// DOCS
#define CC_GEN_POS_PAWN_SIZE (CC_GEN_POS_PAWN_LEN + 1)
#define CC_GEN_POS_BISHOP_SIZE (CC_GEN_POS_BISHOP_LEN + 1)
#define CC_GEN_POS_ROOK_SIZE (CC_GEN_POS_ROOK_LEN + 1)
#define CC_GEN_POS_QUEEN_SIZE (CC_GEN_POS_QUEEN_LEN + 1)
#define CC_GEN_POS_KNIGHT_SIZE (CC_GEN_POS_KNIGHT_LEN + 1)
#define CC_GEN_POS_UNICORN_SIZE (CC_GEN_POS_UNICORN_LEN + 1)

// DOCS
extern CcPos const CC_GEN_POS_LIGHT_PAWN[ CC_GEN_POS_PAWN_SIZE ];
extern CcPos const CC_GEN_POS_DARK_PAWN[ CC_GEN_POS_PAWN_SIZE ];
extern CcPos const CC_GEN_POS_BISHOP[ CC_GEN_POS_BISHOP_SIZE ]; // Also, Serpent.
extern CcPos const CC_GEN_POS_ROOK[ CC_GEN_POS_ROOK_SIZE ]; // Also, Serpent's color-changing move.
extern CcPos const CC_GEN_POS_QUEEN[ CC_GEN_POS_QUEEN_SIZE ];
extern CcPos const CC_GEN_POS_KNIGHT[ CC_GEN_POS_KNIGHT_SIZE ];
extern CcPos const CC_GEN_POS_UNICORN[ CC_GEN_POS_UNICORN_SIZE ];


// DOCS
bool cc_gen_pos( CcPos * restrict pos__io,
                 CcPos step,
                 bool from_or_to );

// DOCS
bool cc_gen_pos_is_valid( CcPos step,
                          CcPos const array[  ],
                          size_t array_len );


// DOCS
#define CC_GEN_POS_LIGHT_PAWN_IS_VALID(step) \
    ( cc_gen_pos_is_valid( (step), CC_GEN_POS_LIGHT_PAWN, CC_GEN_POS_PAWN_LEN ) )

#define CC_GEN_POS_DARK_PAWN_IS_VALID(step) \
    ( cc_gen_pos_is_valid( (step), CC_GEN_POS_DARK_PAWN, CC_GEN_POS_PAWN_LEN ) )

#define CC_GEN_POS_BISHOP_IS_VALID(step) \
    ( cc_gen_pos_is_valid( (step), CC_GEN_POS_BISHOP, CC_GEN_POS_BISHOP_LEN ) )

#define CC_GEN_POS_ROOK_IS_VALID(step) \
    ( cc_gen_pos_is_valid( (step), CC_GEN_POS_ROOK, CC_GEN_POS_ROOK_LEN ) )

#define CC_GEN_POS_QUEEN_IS_VALID(step) \
    ( cc_gen_pos_is_valid( (step), CC_GEN_POS_QUEEN, CC_GEN_POS_QUEEN_LEN ) )

#define CC_GEN_POS_KNIGHT_IS_VALID(step) \
    ( cc_gen_pos_is_valid( (step), CC_GEN_POS_KNIGHT, CC_GEN_POS_KNIGHT_LEN ) )

#define CC_GEN_POS_UNICORN_IS_VALID(step) \
    ( cc_gen_pos_is_valid( (step), CC_GEN_POS_UNICORN, CC_GEN_POS_UNICORN_LEN ) )


// DOCS
#define CC_GEN_POS_SERPENT_IS_VALID(step) \
    ( cc_gen_pos_is_valid( (step), CC_GEN_POS_BISHOP, CC_GEN_POS_BISHOP_LEN ) )

#define CC_GEN_POS_SERPENT_COLOR_CHANGE_IS_VALID(step) \
    ( cc_gen_pos_is_valid( (step), CC_GEN_POS_ROOK, CC_GEN_POS_ROOK_LEN ) )


#endif /* __CC_GEN_POS_H__ */
