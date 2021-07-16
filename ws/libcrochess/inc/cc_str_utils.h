// Copyright (c) 2021 Mario Mlačak, mmlacak@gmail.com
// Licensed under 3-clause (modified) BSD license. See LICENSE for details.

#ifndef __CC_STR_UTILS_H__
#define __CC_STR_UTILS_H__

#include <stdbool.h>


bool cc_str_to_case( char * const restrict str, bool is_lower_or_upper );
char * cc_str_to_case__new( char const * const restrict str, bool is_lower_or_upper );

size_t cc_str_len( char const * const restrict str );
size_t cc_str_len_min( char const * const restrict str, size_t max_len );

char * cc_str_duplicate__new( char const * const restrict str );
char * cc_str_duplicate_len__new( char const * const restrict str, size_t max_len );

char * cc_str_concatenate__new( char const * const restrict str_1,
                                char const * const restrict str_2 );

char * cc_str_concatenate_len__new( char const * const restrict str_1,
                                    char const * const restrict str_2,
                                    size_t max_len );

char * cc_str_concatenate_char__new( char const * const restrict str,
                                     char const chr );

bool cc_str_append_char( char ** const restrict alloc_str,
                         char const chr );

char * cc_str_append__new( char ** restrict alloc_str_1,
                           char ** restrict alloc_str_2 );

char * cc_str_append_len__new( char ** restrict alloc_str_1,
                               char ** restrict alloc_str_2,
                               size_t max_len );

char * cc_str_append_format__new( char ** restrict alloc_str,
                                  char const * const restrict fmt, ... );

char * cc_str_append_format_len__new( char ** restrict alloc_str,
                                      size_t max_len,
                                      char const * const restrict fmt, ... );


#endif /* __CC_STR_UTILS_H__ */
