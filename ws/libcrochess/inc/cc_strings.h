// Copyright (c) 2021, 2022 Mario Mlačak, mmlacak@gmail.com
// Licensed under GNU GPL v3+ license. See LICENSING, COPYING files for details.

#ifndef __CC_STRING_H__
#define __CC_STRING_H__

#include <stdbool.h>
#include <stdlib.h>


typedef struct CcStrings
{
    char * str;
    struct CcStrings * next;
} CcStrings;

CcStrings * cc_strings__new( char const * str,
                             size_t max_len__d );

CcStrings * cc_strings_append( CcStrings ** strings__iod_a,
                               char const * str,
                               size_t max_len__d );

CcStrings * cc_strings_append_fmt_va( CcStrings ** strings__iod_a,
                                      size_t max_len__d,
                                      char const * fmt,
                                      va_list args );

CcStrings * cc_strings_append_fmt( CcStrings ** strings__iod_a,
                                   size_t max_len__d,
                                   char const * fmt, ... );

CcStrings * cc_strings_duplicate_all__new( CcStrings * strings );

bool cc_strings_free_all( CcStrings ** strings__f );


#endif /* __CC_STRING_H__ */
