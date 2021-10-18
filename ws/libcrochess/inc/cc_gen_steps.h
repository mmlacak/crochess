// Copyright (c) 2021 Mario Mlačak, mmlacak@gmail.com
// Licensed under GNU GPL v3+ license. See LICENSING, COPYING files for details.

#ifndef __CC_GEN_STEPS_H__
#define __CC_GEN_STEPS_H__

#include <stddef.h>
#include <stdbool.h>


// DOCS
#define CC_GEN_STEPS_COORDS_LEN 2

// DOCS
#define CC_GEN_STEPS_BISHOP_LEN 4
#define CC_GEN_STEPS_ROOK_LEN 4
#define CC_GEN_STEPS_QUEEN_LEN 8
#define CC_GEN_STEPS_KNIGHT_LEN 8
#define CC_GEN_STEPS_UNICORN_LEN 16


// DOCS
// Also, Serpent.
extern int const CC_GEN_STEPS_BISHOP[ CC_GEN_STEPS_BISHOP_LEN ][ CC_GEN_STEPS_COORDS_LEN ];

// DOCS
// Also, Serpent's color-changing move.
extern int const CC_GEN_STEPS_ROOK[ CC_GEN_STEPS_ROOK_LEN ][ CC_GEN_STEPS_COORDS_LEN ];

// DOCS
extern int const CC_GEN_STEPS_QUEEN[ CC_GEN_STEPS_QUEEN_LEN ][ CC_GEN_STEPS_COORDS_LEN ];
extern int const CC_GEN_STEPS_KNIGHT[ CC_GEN_STEPS_KNIGHT_LEN ][ CC_GEN_STEPS_COORDS_LEN ];
extern int const CC_GEN_STEPS_UNICORN[ CC_GEN_STEPS_UNICORN_LEN ][ CC_GEN_STEPS_COORDS_LEN ];


// DOCS
bool cc_gen_steps( int * const restrict i_io,
                   int * const restrict j_io,
                   int const step_i,
                   int const step_j,
                   bool const from_or_to );

// DOCS
bool cc_gen_steps_is_valid( int const step_i,
                            int const step_j,
                            int const array[  ][ CC_GEN_STEPS_COORDS_LEN ],
                            size_t const array_len );


// DOCS
#define CC_GEN_STEPS_BISHOP_IS_VALID(step_i,step_j) \
    ( cc_gen_steps_is_valid( (step_i), (step_j), CC_GEN_STEPS_BISHOP, CC_GEN_STEPS_BISHOP_LEN ) )

#define CC_GEN_STEPS_ROOK_IS_VALID(step_i,step_j) \
    ( cc_gen_steps_is_valid( (step_i), (step_j), CC_GEN_STEPS_ROOK, CC_GEN_STEPS_ROOK_LEN ) )

#define CC_GEN_STEPS_QUEEN_IS_VALID(step_i,step_j) \
    ( cc_gen_steps_is_valid( (step_i), (step_j), CC_GEN_STEPS_QUEEN, CC_GEN_STEPS_QUEEN_LEN ) )

#define CC_GEN_STEPS_KNIGHT_IS_VALID(step_i,step_j) \
    ( cc_gen_steps_is_valid( (step_i), (step_j), CC_GEN_STEPS_KNIGHT, CC_GEN_STEPS_KNIGHT_LEN ) )

#define CC_GEN_STEPS_UNICORN_IS_VALID(step_i,step_j) \
    ( cc_gen_steps_is_valid( (step_i), (step_j), CC_GEN_STEPS_UNICORN, CC_GEN_STEPS_UNICORN_LEN ) )


// DOCS
#define CC_GEN_STEPS_SERPENT_IS_VALID(step_i,step_j) \
    ( cc_gen_steps_is_valid( (step_i), (step_j), CC_GEN_STEPS_BISHOP, CC_GEN_STEPS_BISHOP_LEN ) )

#define CC_GEN_STEPS_SERPENT_COLOR_CHANGE_IS_VALID(step_i,step_j) \
    ( cc_gen_steps_is_valid( (step_i), (step_j), CC_GEN_STEPS_ROOK, CC_GEN_STEPS_ROOK_LEN ) )


#endif /* __CC_GEN_STEPS_H__ */
