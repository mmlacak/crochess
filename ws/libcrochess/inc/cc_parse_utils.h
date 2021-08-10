// Copyright (c) 2021 Mario Mlačak, mmlacak@gmail.com
// Licensed under 3-clause (modified) BSD license. See LICENSE for details.

#ifndef __CC_PARSE_UTILS_H__
#define __CC_PARSE_UTILS_H__

#include <stdbool.h>
#include <stdlib.h>

#include "cc_ply.h"


// TODO :: DOCS
bool cc_parse_utils_char_is_ply_gather( char const c );

// TODO :: DOCS
char const * cc_parse_utils_go_ply_gather( char const * const restrict move_str, bool skip_or_stop_at );

// TODO :: DOCS
size_t cc_parse_utils_ply_link_len( char const * const restrict move_str );

// TODO :: DOCS
char const * cc_parse_utils_go_ply_link( char const * const restrict move_str, bool skip_or_stop_at );

// TODO :: DOCS
char * cc_parse_utils_next_ply_str_new( char const * const restrict move_str_s );

// TODO :: DOCS
bool cc_parse_util_get_ply_link( char const * const restrict ply_str,
                                 CcPlyLinkEnum * const restrict link_o );


#endif /* __CC_PARSE_UTILS_H__ */
