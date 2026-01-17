// Copyright (c) 2026 Mario Mlačak, mmlacak@gmail.com
// Licensed under GNU GPL v3+ license. See LICENSING, COPYING files for details.

#ifndef __CC_PATH_CMP_H__
#define __CC_PATH_CMP_H__

#include <stdbool.h>

#include "cc_piece.h"
// #include "cc_chessboard.h"
#include "cc_game.h"

#include "cc_pos.h"
#include "cc_typed_step_defs.h"
#include "cc_path_ctx.h"
#include "cc_path.h"

// todo :: DOCS


// todo :: DOCS
CcMaybeBoolEnum cc_path_cmp_compare_steps( CcPly * ply,
                                           CcStep * path_steps,
                                           CcPathContext * path_ctx );

// todo :: DOCS
CcMaybeBoolEnum cc_path_cmp_compare_ply( CcPly * ply,
                                         CcPathNode * path_ply,
                                         CcPathContext * path_ctx,
                                         CcStep ** steps__o_a );

// todo :: DOCS
CcMaybeBoolEnum cc_path_cmp_compare_all_paths( CcPly * ply,
                                               CcPathNode ** path_node__io,
                                               CcPathContext * path_ctx );


#endif /* __CC_PATH_CMP_H__ */
