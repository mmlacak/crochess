// Copyright (c) 2021 Mario Mlačak, mmlacak@gmail.com
// Licensed under GNU GPL v3+ license. See LICENSING, COPYING files for details.

#ifndef __CC_GEN_POS_H__
#define __CC_GEN_POS_H__

// #include <stddef.h>
#include <stdbool.h>

#include "cc_gen_steps.h"
#include "cc_pos.h"


// DOCS
bool cc_gen_pos( CcPos * const restrict pos_io,
                 CcPos const step,
                 bool const from_or_to );


#endif /* __CC_GEN_POS_H__ */
