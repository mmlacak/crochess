// Copyright (c) 2021 Mario Mlačak, mmlacak@gmail.com
// Licensed under 3-clause (modified) BSD license. See LICENSE for details.

#ifndef __CC_PARSE_MOVE_H__
#define __CC_PARSE_MOVE_H__

#include "cc_chessboard.h"

// #include "cc_step.h"
#include "cc_ply.h"
#include "cc_move.h"

#include "cc_parse_msg.h"



// TODO :: DOCS
CcPly * cc_parse_ply( char const * const restrict ply_str,
                      CcChessboard const * const restrict cb,
                      CcParseMsg ** parse_msgs );

// TODO :: DOCS
CcMove * cc_parse_move( char const * const restrict move_str,
                        CcChessboard const * const restrict cb,
                        CcParseMsg ** parse_msgs );


// TODO :: DOCS
char * cc_parse_next_ply_str_new( char const * const restrict move_str_s,
                                  CcParseMsg ** parse_msgs );


// TODO :: DOCS
CcParseMsg * cc_parse_msg_get_last( CcParseMsg const * const restrict parse_msgs );


#endif /* __CC_PARSE_MOVE_H__ */
