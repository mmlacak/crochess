// Copyright (c) 2021 Mario Mlačak, mmlacak@gmail.com
// Licensed under 3-clause (modified) BSD license. See LICENSE for details.

#ifndef __CC_PARSE_UTILS_H__
#define __CC_PARSE_UTILS_H__

#include <stdbool.h>
#include <stdlib.h>

#include "cc_parse_msg.h"


// TODO :: DOCS
bool cc_parse_char_is_ply_gather( char const c );

// TODO :: DOCS
char const * cc_parse_ply_gathers( char const * const restrict move_str, bool skip_or_stop_at );

// TODO :: DOCS
size_t cc_parse_ply_divider_len( char const * const restrict move_str );

// TODO :: DOCS
char const * cc_parse_ply_divider( char const * const restrict move_str, bool skip_or_stop_at );

// TODO :: DOCS
char * cc_parse_next_ply_str_new( char const * const restrict move_str_s,
                                  CcParseMsg ** parse_msgs_io );


#endif /* __CC_PARSE_UTILS_H__ */
