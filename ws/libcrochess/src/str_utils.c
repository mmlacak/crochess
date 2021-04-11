// Copyright (c) 2021 Mario Mlačak, mmlacak@gmail.com
// Licensed under 3-clause (modified) BSD license. See LICENSE for details.

#include <stddef.h>
#include <stdbool.h>
#include <stdlib.h>
#include <ctype.h>
#include <string.h>

#include "str_utils.h"


bool str_to_case(char * const restrict str, bool is_lower_or_upper)
{
    if ( !str ) return false;

    char * s = str;
    while ( *s )
    {
        if ( is_lower_or_upper )
            *s = tolower( *s );
        else
            *s = toupper( *s );

        ++s;
    }

    return true;
}

char * str_to_case_alloc(char const * const restrict str, bool is_lower_or_upper)
{
    if ( !str ) return NULL;

    size_t len = strlen(str);
    char * s = malloc(len + 1);

    char const * pos = str;
    while ( *pos )
    {
        if ( is_lower_or_upper )
            *s = tolower( *pos );
        else
            *s = toupper( *pos );

        ++s;
        ++pos;
    }

    return s;
}
