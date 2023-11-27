// Copyright (c) 2021, 2022 Mario Mlačak, mmlacak@gmail.com
// Licensed under GNU GPL v3+ license. See LICENSING, COPYING files for details.

#include <stddef.h>
// #include <ctype.h>
// #include <string.h>
#include <stdarg.h>
// #include <stdio.h>

#include "cc_defines.h"
#include "cc_str_utils.h"
#include "cc_strings.h"

/**
    @file cc_strings.c
    @brief Strings (linked list) functions.
*/


CcStrings * cc_strings__new( char const * restrict str,
                             size_t max_len__d ) {
    CcStrings * str__a = malloc( sizeof( CcStrings ) );
    if ( !str__a ) return NULL;

    str__a->str = cc_str_duplicate__new( str, false, max_len__d );
    str__a->next = NULL;

    return str__a;
}

CcStrings * cc_strings_append( CcStrings ** restrict strings__iod_a,
                               char const * restrict str,
                               size_t max_len__d ) {
    if ( !strings__iod_a ) return NULL;

    CcStrings * str__t = cc_strings__new( str, max_len__d );
    if ( !str__t ) return NULL;

    if ( !*strings__iod_a ) {
        *strings__iod_a = str__t; // Ownership transfer.
    } else {
        CcStrings * s = *strings__iod_a;
        CC_FASTFORWARD( s );
        s->next = str__t; // Append + ownership transfer.
    }

    return str__t; // Weak pointer.
}

CcStrings * cc_strings_append_fmt( CcStrings ** restrict strings__iod_a,
                                   size_t max_len__d,
                                   char const * restrict fmt, ... ) {
    if ( !strings__iod_a ) return NULL;    // To avoid alloc() + free() of str__a;
                                         // even though this is never referenced.

    va_list args;
    va_start( args, fmt );

    char * str__a = cc_str_fmt_va__new( max_len__d, fmt, args );

    va_end( args );

    if ( !str__a ) return NULL;

    CcStrings * pm__w = cc_strings_append( strings__iod_a, str__a, max_len__d );

    CC_FREE( str__a );

    return pm__w;
}

CcStrings * cc_strings_duplicate_all__new( CcStrings * restrict strings ) {
    if ( !strings ) return NULL;

    CcStrings * new__a = cc_strings__new( strings->str, CC_MAX_LEN_ZERO_TERMINATED );
    if ( !new__a ) return NULL;

    CcStrings * s = strings->next;

    while ( s ) {
        if ( !cc_strings_append( &new__a, s->str, CC_MAX_LEN_ZERO_TERMINATED ) ) {
            cc_strings_free_all( &new__a );
            return NULL; }

        ++s;
    }

    return new__a;
}

bool cc_strings_free_all( CcStrings ** restrict strings__f ) {
    if ( !strings__f ) return false;
    if ( !*strings__f ) return true;

    CcStrings * str = *strings__f;
    CcStrings * tmp = NULL;

    while ( str ) {
        CC_FREE( str->str );

        tmp = str->next;
        CC_FREE( str );
        str = tmp;
    }

    *strings__f = NULL;
    return true;
}
