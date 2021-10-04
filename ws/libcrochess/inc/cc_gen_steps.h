// Copyright (c) 2021 Mario Mlačak, mmlacak@gmail.com
// Licensed under GNU GPL v3+ license. See LICENSING, COPYING files for details.

#ifndef __CC_GEN_STEPS_H__
#define __CC_GEN_STEPS_H__

#include <stdbool.h>

#define CC_GEN_STEP_COORDS_LEN 2

#define CC_GEN_STEP_BISHOP_LEN 4
#define CC_GEN_STEP_ROOK_LEN 4
#define CC_GEN_STEP_QUEEN_LEN 8
#define CC_GEN_STEP_KNIGHT_LEN 8
#define CC_GEN_STEP_UNICORN_LEN 16


extern int const CC_GEN_STEP_BISHOP[ CC_GEN_STEP_BISHOP_LEN ][ CC_GEN_STEP_COORDS_LEN ];
extern int const CC_GEN_STEP_ROOK[ CC_GEN_STEP_ROOK_LEN ][ CC_GEN_STEP_COORDS_LEN ];
extern int const CC_GEN_STEP_QUEEN[ CC_GEN_STEP_QUEEN_LEN ][ CC_GEN_STEP_COORDS_LEN ];
extern int const CC_GEN_STEP_KNIGHT[ CC_GEN_STEP_KNIGHT_LEN ][ CC_GEN_STEP_COORDS_LEN ];
extern int const CC_GEN_STEP_UNICORN[ CC_GEN_STEP_UNICORN_LEN ][ CC_GEN_STEP_COORDS_LEN ];


bool cc_gen_step( int * const restrict i_io,
                  int * const restrict j_io,
                  int const step_i,
                  int const step_j,
                  bool const from_or_to );


#endif /* __CC_GEN_STEPS_H__ */
