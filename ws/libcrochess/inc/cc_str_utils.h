// Copyright (c) 2021 Mario Mlačak, mmlacak@gmail.com
// Licensed under 3-clause (modified) BSD license. See LICENSE for details.

#ifndef __CC_STR_UTILS_H__
#define __CC_STR_UTILS_H__

#include <stdbool.h>


bool cc_str_to_case( char * const restrict str, bool is_lower_or_upper );
char * cc_str_to_case_new( char const * const restrict str, bool is_lower_or_upper );

size_t cc_str_len( char const * const restrict str );
size_t cc_str_len_max( char const * const restrict str, size_t max_len );

char * cc_str_duplicate_new( char const * const restrict str );
char * cc_str_duplicate_len_new( char const * const restrict str, size_t max_len );

char * cc_str_concatenate_new(  char const * const restrict str_1,
                                char const * const restrict str_2 );

char * cc_str_concatenate_len_new(  char const * const restrict str_1,
                                    char const * const restrict str_2,
                                    size_t max_len );


#endif /* __CC_STR_UTILS_H__ */
