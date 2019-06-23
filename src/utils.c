/* Copyright (C) 2005, 2007, 2011 Mario Mlačak, mmlacak@gmail.com */

#include <ctype.h>
#include <string.h>

#include "utils.h"

bool is_unsigned_integer( char * s )
{
    if ( !s ) return false;

    while ( *s != '\0' )
    {
        if ( !isdigit( *s ) ) return false;
        ++s;
    }

    return true;
}

char * str_ends_str( const char * s1, const char * s2 )
{
    char * rs1;
    char * rs2;
    size_t len1, len2;
    long int i;

    if ( !s1 ) return NULL;
    if ( !s2 ) return NULL;

    len1 = strlen( s1 );
    len2 = strlen( s2 );
    if ( len1 < len2 ) return NULL;

    rs1 = (char *)s1 + len1;
    rs2 = (char *)s2 + len2;

    for ( i = len2; ( i >= 0 ) && ( *--rs1 == *--rs2 ); --i ) {};

    if ( i <= 0 ) return ++rs1;
    else return NULL;
}

