// Copyright (c) 2021 Mario Mlačak, mmlacak@gmail.com
// Licensed under 3-clause (modified) BSD license. See LICENSE for details.

#ifndef __CC_STR_UTILS_H__
#define __CC_STR_UTILS_H__

#include <stdbool.h>


bool cc_str_to_case( char * const restrict str, bool is_lower_or_upper );
char * cc_str_to_case_new( char const * const restrict str, bool is_lower_or_upper );


#endif /* __CC_STR_UTILS_H__ */
