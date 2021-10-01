// Copyright (c) 2021 Mario Mlačak, mmlacak@gmail.com
// Licensed under GNU GPL v3+ license. See LICENSING, COPYING files for details.

#ifndef __CC_GEN_STEPS_H__
#define __CC_GEN_STEPS_H__

#include <stdbool.h>


bool cc_gen_step( int * const restrict i_io,
                  int * const restrict j_io,
                  int const step_i,
                  int const step_j,
                  bool const from_or_to );


#endif /* __CC_GEN_STEPS_H__ */
