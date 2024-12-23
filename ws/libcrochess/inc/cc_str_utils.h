// Copyright (c) 2021, 2022 Mario Mlačak, mmlacak@gmail.com
// Licensed under GNU GPL v3+ license. See LICENSING, COPYING files for details.

#ifndef __CC_STR_UTILS_H__
#define __CC_STR_UTILS_H__

#include <stdbool.h>
#include <stdlib.h>
#include <stdio.h>
#include <stdarg.h>


#define CC_MAX_LEN_ZERO_TERMINATED (0)

#define CC_SIZE_BUFFER (8192) // (BUFSIZ) isn't good; BUFSIZ varies, and could be < 256.

#define CC_MAX_LEN_BUFFER (CC_SIZE_BUFFER - 1)

#define CC_SIZE_IGNORE (0)

#define CC_SIZE_CHAR_8 (8)

#define CC_MAX_LEN_CHAR_8 (CC_SIZE_CHAR_8)

#define CC_SIZE_CHAR_16 (16)

#define CC_MAX_LEN_CHAR_16 (CC_SIZE_CHAR_16)

#define CC_SIZE_CHAR_32 (32)

#define CC_MAX_LEN_CHAR_32 (CC_SIZE_CHAR_32)

#define CC_SIZE_CHAR_64 (64)

#define CC_MAX_LEN_CHAR_64 (CC_SIZE_CHAR_64)

#define CC_SIZE_CHAR_128 (128)

#define CC_MAX_LEN_CHAR_128 (CC_SIZE_CHAR_128)

#define CC_SIZE_CHAR_256 (256)

#define CC_MAX_LEN_CHAR_256 (CC_SIZE_CHAR_256)

#define CC_SIZE_CHAR_512 (512)

#define CC_MAX_LEN_CHAR_512 (CC_SIZE_CHAR_512)


typedef int (*cc_ctype_fp_ischar_t)( int ch );


typedef char cc_char_8 [ CC_SIZE_CHAR_8 ];

#define CC_CHAR_8_EMPTY { '\0', '\0', '\0', '\0', '\0', '\0', '\0', '\0' }

typedef char cc_char_16 [ CC_SIZE_CHAR_16 ];

#define CC_CHAR_16_EMPTY { '\0', '\0', '\0', '\0', '\0', '\0', '\0', '\0', \
                           '\0', '\0', '\0', '\0', '\0', '\0', '\0', '\0' }

typedef char cc_char_32 [ CC_SIZE_CHAR_32 ];

#define CC_CHAR_32_EMPTY { '\0', '\0', '\0', '\0', '\0', '\0', '\0', '\0', \
                           '\0', '\0', '\0', '\0', '\0', '\0', '\0', '\0', \
                           '\0', '\0', '\0', '\0', '\0', '\0', '\0', '\0', \
                           '\0', '\0', '\0', '\0', '\0', '\0', '\0', '\0' }

typedef char cc_char_64 [ CC_SIZE_CHAR_64 ];

#define CC_CHAR_64_EMPTY { '\0', '\0', '\0', '\0', '\0', '\0', '\0', '\0', \
                           '\0', '\0', '\0', '\0', '\0', '\0', '\0', '\0', \
                           '\0', '\0', '\0', '\0', '\0', '\0', '\0', '\0', \
                           '\0', '\0', '\0', '\0', '\0', '\0', '\0', '\0', \
                           '\0', '\0', '\0', '\0', '\0', '\0', '\0', '\0', \
                           '\0', '\0', '\0', '\0', '\0', '\0', '\0', '\0', \
                           '\0', '\0', '\0', '\0', '\0', '\0', '\0', '\0', \
                           '\0', '\0', '\0', '\0', '\0', '\0', '\0', '\0' }

typedef char cc_char_128 [ CC_SIZE_CHAR_128 ];

#define CC_CHAR_128_EMPTY { '\0', '\0', '\0', '\0', '\0', '\0', '\0', '\0', \
                            '\0', '\0', '\0', '\0', '\0', '\0', '\0', '\0', \
                            '\0', '\0', '\0', '\0', '\0', '\0', '\0', '\0', \
                            '\0', '\0', '\0', '\0', '\0', '\0', '\0', '\0', \
                            '\0', '\0', '\0', '\0', '\0', '\0', '\0', '\0', \
                            '\0', '\0', '\0', '\0', '\0', '\0', '\0', '\0', \
                            '\0', '\0', '\0', '\0', '\0', '\0', '\0', '\0', \
                            '\0', '\0', '\0', '\0', '\0', '\0', '\0', '\0', \
                            '\0', '\0', '\0', '\0', '\0', '\0', '\0', '\0', \
                            '\0', '\0', '\0', '\0', '\0', '\0', '\0', '\0', \
                            '\0', '\0', '\0', '\0', '\0', '\0', '\0', '\0', \
                            '\0', '\0', '\0', '\0', '\0', '\0', '\0', '\0', \
                            '\0', '\0', '\0', '\0', '\0', '\0', '\0', '\0', \
                            '\0', '\0', '\0', '\0', '\0', '\0', '\0', '\0', \
                            '\0', '\0', '\0', '\0', '\0', '\0', '\0', '\0', \
                            '\0', '\0', '\0', '\0', '\0', '\0', '\0', '\0' }

typedef char cc_char_256 [ CC_SIZE_CHAR_256 ];

#define CC_CHAR_256_EMPTY { '\0', '\0', '\0', '\0', '\0', '\0', '\0', '\0', \
                            '\0', '\0', '\0', '\0', '\0', '\0', '\0', '\0', \
                            '\0', '\0', '\0', '\0', '\0', '\0', '\0', '\0', \
                            '\0', '\0', '\0', '\0', '\0', '\0', '\0', '\0', \
                            '\0', '\0', '\0', '\0', '\0', '\0', '\0', '\0', \
                            '\0', '\0', '\0', '\0', '\0', '\0', '\0', '\0', \
                            '\0', '\0', '\0', '\0', '\0', '\0', '\0', '\0', \
                            '\0', '\0', '\0', '\0', '\0', '\0', '\0', '\0', \
                            '\0', '\0', '\0', '\0', '\0', '\0', '\0', '\0', \
                            '\0', '\0', '\0', '\0', '\0', '\0', '\0', '\0', \
                            '\0', '\0', '\0', '\0', '\0', '\0', '\0', '\0', \
                            '\0', '\0', '\0', '\0', '\0', '\0', '\0', '\0', \
                            '\0', '\0', '\0', '\0', '\0', '\0', '\0', '\0', \
                            '\0', '\0', '\0', '\0', '\0', '\0', '\0', '\0', \
                            '\0', '\0', '\0', '\0', '\0', '\0', '\0', '\0', \
                            '\0', '\0', '\0', '\0', '\0', '\0', '\0', '\0', \
                            '\0', '\0', '\0', '\0', '\0', '\0', '\0', '\0', \
                            '\0', '\0', '\0', '\0', '\0', '\0', '\0', '\0', \
                            '\0', '\0', '\0', '\0', '\0', '\0', '\0', '\0', \
                            '\0', '\0', '\0', '\0', '\0', '\0', '\0', '\0', \
                            '\0', '\0', '\0', '\0', '\0', '\0', '\0', '\0', \
                            '\0', '\0', '\0', '\0', '\0', '\0', '\0', '\0', \
                            '\0', '\0', '\0', '\0', '\0', '\0', '\0', '\0', \
                            '\0', '\0', '\0', '\0', '\0', '\0', '\0', '\0', \
                            '\0', '\0', '\0', '\0', '\0', '\0', '\0', '\0', \
                            '\0', '\0', '\0', '\0', '\0', '\0', '\0', '\0', \
                            '\0', '\0', '\0', '\0', '\0', '\0', '\0', '\0', \
                            '\0', '\0', '\0', '\0', '\0', '\0', '\0', '\0', \
                            '\0', '\0', '\0', '\0', '\0', '\0', '\0', '\0', \
                            '\0', '\0', '\0', '\0', '\0', '\0', '\0', '\0', \
                            '\0', '\0', '\0', '\0', '\0', '\0', '\0', '\0', \
                            '\0', '\0', '\0', '\0', '\0', '\0', '\0', '\0' }

typedef char cc_char_512 [ CC_SIZE_CHAR_512 ];

#define CC_CHAR_512_EMPTY { '\0', '\0', '\0', '\0', '\0', '\0', '\0', '\0', \
                            '\0', '\0', '\0', '\0', '\0', '\0', '\0', '\0', \
                            '\0', '\0', '\0', '\0', '\0', '\0', '\0', '\0', \
                            '\0', '\0', '\0', '\0', '\0', '\0', '\0', '\0', \
                            '\0', '\0', '\0', '\0', '\0', '\0', '\0', '\0', \
                            '\0', '\0', '\0', '\0', '\0', '\0', '\0', '\0', \
                            '\0', '\0', '\0', '\0', '\0', '\0', '\0', '\0', \
                            '\0', '\0', '\0', '\0', '\0', '\0', '\0', '\0', \
                            '\0', '\0', '\0', '\0', '\0', '\0', '\0', '\0', \
                            '\0', '\0', '\0', '\0', '\0', '\0', '\0', '\0', \
                            '\0', '\0', '\0', '\0', '\0', '\0', '\0', '\0', \
                            '\0', '\0', '\0', '\0', '\0', '\0', '\0', '\0', \
                            '\0', '\0', '\0', '\0', '\0', '\0', '\0', '\0', \
                            '\0', '\0', '\0', '\0', '\0', '\0', '\0', '\0', \
                            '\0', '\0', '\0', '\0', '\0', '\0', '\0', '\0', \
                            '\0', '\0', '\0', '\0', '\0', '\0', '\0', '\0', \
                            '\0', '\0', '\0', '\0', '\0', '\0', '\0', '\0', \
                            '\0', '\0', '\0', '\0', '\0', '\0', '\0', '\0', \
                            '\0', '\0', '\0', '\0', '\0', '\0', '\0', '\0', \
                            '\0', '\0', '\0', '\0', '\0', '\0', '\0', '\0', \
                            '\0', '\0', '\0', '\0', '\0', '\0', '\0', '\0', \
                            '\0', '\0', '\0', '\0', '\0', '\0', '\0', '\0', \
                            '\0', '\0', '\0', '\0', '\0', '\0', '\0', '\0', \
                            '\0', '\0', '\0', '\0', '\0', '\0', '\0', '\0', \
                            '\0', '\0', '\0', '\0', '\0', '\0', '\0', '\0', \
                            '\0', '\0', '\0', '\0', '\0', '\0', '\0', '\0', \
                            '\0', '\0', '\0', '\0', '\0', '\0', '\0', '\0', \
                            '\0', '\0', '\0', '\0', '\0', '\0', '\0', '\0', \
                            '\0', '\0', '\0', '\0', '\0', '\0', '\0', '\0', \
                            '\0', '\0', '\0', '\0', '\0', '\0', '\0', '\0', \
                            '\0', '\0', '\0', '\0', '\0', '\0', '\0', '\0', \
                            '\0', '\0', '\0', '\0', '\0', '\0', '\0', '\0', \
                            '\0', '\0', '\0', '\0', '\0', '\0', '\0', '\0', \
                            '\0', '\0', '\0', '\0', '\0', '\0', '\0', '\0', \
                            '\0', '\0', '\0', '\0', '\0', '\0', '\0', '\0', \
                            '\0', '\0', '\0', '\0', '\0', '\0', '\0', '\0', \
                            '\0', '\0', '\0', '\0', '\0', '\0', '\0', '\0', \
                            '\0', '\0', '\0', '\0', '\0', '\0', '\0', '\0', \
                            '\0', '\0', '\0', '\0', '\0', '\0', '\0', '\0', \
                            '\0', '\0', '\0', '\0', '\0', '\0', '\0', '\0', \
                            '\0', '\0', '\0', '\0', '\0', '\0', '\0', '\0', \
                            '\0', '\0', '\0', '\0', '\0', '\0', '\0', '\0', \
                            '\0', '\0', '\0', '\0', '\0', '\0', '\0', '\0', \
                            '\0', '\0', '\0', '\0', '\0', '\0', '\0', '\0', \
                            '\0', '\0', '\0', '\0', '\0', '\0', '\0', '\0', \
                            '\0', '\0', '\0', '\0', '\0', '\0', '\0', '\0', \
                            '\0', '\0', '\0', '\0', '\0', '\0', '\0', '\0', \
                            '\0', '\0', '\0', '\0', '\0', '\0', '\0', '\0', \
                            '\0', '\0', '\0', '\0', '\0', '\0', '\0', '\0', \
                            '\0', '\0', '\0', '\0', '\0', '\0', '\0', '\0', \
                            '\0', '\0', '\0', '\0', '\0', '\0', '\0', '\0', \
                            '\0', '\0', '\0', '\0', '\0', '\0', '\0', '\0', \
                            '\0', '\0', '\0', '\0', '\0', '\0', '\0', '\0', \
                            '\0', '\0', '\0', '\0', '\0', '\0', '\0', '\0', \
                            '\0', '\0', '\0', '\0', '\0', '\0', '\0', '\0', \
                            '\0', '\0', '\0', '\0', '\0', '\0', '\0', '\0', \
                            '\0', '\0', '\0', '\0', '\0', '\0', '\0', '\0', \
                            '\0', '\0', '\0', '\0', '\0', '\0', '\0', '\0', \
                            '\0', '\0', '\0', '\0', '\0', '\0', '\0', '\0', \
                            '\0', '\0', '\0', '\0', '\0', '\0', '\0', '\0', \
                            '\0', '\0', '\0', '\0', '\0', '\0', '\0', '\0', \
                            '\0', '\0', '\0', '\0', '\0', '\0', '\0', '\0', \
                            '\0', '\0', '\0', '\0', '\0', '\0', '\0', '\0', \
                            '\0', '\0', '\0', '\0', '\0', '\0', '\0', '\0' }


bool cc_str_clear( char * str__io, size_t size__d );

char * cc_str_pad__new( char pad, size_t count );

bool cc_str_is_empty( char const * str, bool ignore_spaces );

bool cc_str_count_chars( char const * str,
                         cc_ctype_fp_ischar_t fp_is_char,
                         size_t max_len__d,
                         size_t * count__o );

char const * cc_str_contains_char( char c,
                                   bool case_sensitive,
                                   char const * start,
                                   char const * end__d,
                                   size_t max_len__d );

char const * cc_str_traverse_chars( char const * str,
                                    cc_ctype_fp_ischar_t fp_is_char,
                                    bool skip_or_stop_at,
                                    size_t max_len__d );


bool cc_str_to_case( char * str__io,
                     bool to_upper_or_lower,
                     size_t max_len__d );

char * cc_str_to_case__new( char const * str,
                            bool to_upper_or_lower,
                            size_t max_len__d );


char const * cc_str_end( char const * start,
                         char const * end__d,
                         size_t max_len__d );

size_t cc_str_len( char const * start,
                   char const * end__d,
                   size_t max_len__d );

int cc_str_len_fmt_va( char const * fmt, va_list args );

int cc_str_len_format( char const * fmt, ... );

bool cc_str_is_equal( char const * start_1,
                      char const * end_1__d,
                      char const * start_2,
                      char const * end_2__d,
                      size_t max_len__d );


size_t cc_str_copy( char const * start,
                    char const * end__d,
                    size_t max_len__d,
                    char * dest__o,
                    char const * dest_end__d,
                    size_t size_dest__d );

char * cc_str_copy__new( char const * start,
                         char const * end__d,
                         size_t max_len__d );

char * cc_str_fmt_va__new( size_t max_len__d,
                           char const * fmt,
                           va_list args );

char * cc_str_fmt__new( size_t max_len__d,
                        char const * fmt, ... );

char * cc_str_duplicate__new( char const * str,
                              bool do_reverse,
                              size_t max_len__d );

char * cc_str_append_into( char * str__io,
                           size_t size_dest__d,
                           char const * str,
                           size_t max_len__d );

char * cc_str_append__new( char const * str_1__d,
                           char const * str_2__d,
                           size_t max_len__d );

char * cc_str_append_free__new( char ** str_1__d_f,
                                char ** str_2__d_f,
                                size_t max_len__d );

char * cc_str_append_fmt_va__new( char ** str__d_f,
                                  size_t max_len__d,
                                  char const * fmt,
                                  va_list args );

char * cc_str_append_fmt__new( char ** str__d_f,
                               size_t max_len__d,
                               char const * fmt, ... );

// todo :: rethink :: (?) move / return newly allocated string (?)
bool cc_str_print( char const * start,
                   char const * end__d,
                   size_t max_len__d,
                   char const * fmt_str,
                   size_t fmt_len__d,
                   char const * fmt__d, ... );

#ifdef __CC_STR_PRINT_INFO__
#define CC_STR_PRINT_IF_INFO(start,end__d,max_len__d,fmt_str,fmt_len__d,fmt__d,...)                   \
    cc_str_print( start, end__d, max_len__d, fmt_str, fmt_len__d, fmt__d __VA_OPT__(,) __VA_ARGS__ )
#else // __CC_STR_PRINT_INFO__
#define CC_STR_PRINT_IF_INFO(start,end__d,max_len__d,fmt_str,fmt__d,...) true
#endif // __CC_STR_PRINT_INFO__
// todo :: rethink :: (?) move / return newly allocated string (?)


#endif /* __CC_STR_UTILS_H__ */
